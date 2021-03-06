#ifndef __STDC_LIMIT_MACROS
#define __STDC_LIMIT_MACROS
#endif

#ifndef __STDC_CONSTANT_MACROS
#define __STDC_CONSTANT_MACROS
#endif

#include "clang/Frontend/FrontendActions.h"
#include "clang/Frontend/TextDiagnosticBuffer.h"
#include "clang/Tooling/CommonOptionsParser.h"
#include "clang/Tooling/Tooling.h"
#include "clang/AST/ASTContext.h"
#include "clang/Parse/ParseAST.h"
#include "clang/AST/ASTConsumer.h"
#include "clang/AST/RecursiveASTVisitor.h"
#include "clang/Basic/SourceManager.h"
#include "clang/Basic/TargetOptions.h"
#include "clang/Basic/TargetInfo.h"
#include "clang/Frontend/CompilerInstance.h"
#include "clang/Frontend/ASTConsumers.h"
#include "clang/Lex/Lexer.h"

#include <boost/python.hpp>

using namespace clang;
using namespace clang::tooling;
using namespace boost::python;

#ifdef DEBUG
#include <iostream>

template <typename... T>
void debug_print_impl(const T&... args) {
    [](auto...) {}((std::cout << args, 0)...);
    std::cout << std::endl;
}
#define debug_print(...) debug_print_impl(__VA_ARGS__)
#else
#define debug_print(...)
#endif

class ASTDictBuilder : public ASTConsumer,
                       public RecursiveASTVisitor<ASTDictBuilder> {

    const CompilerInstance* Compiler;
    const ASTContext* Context;
    list& ast;
    list& errors;
    dict* current_node = nullptr;

    static std::set<Decl::Kind> declSkipList;
    static std::set<Stmt::StmtClass> stmtSkipList;

    int node_counter = 0;
    dict make_node() {
        dict node;
        node["children"] = list{};
        node["id"] = node_counter++;
        return node;
    }

    ///  Adapted from clang CIndex.cpp
    ///
    /// Clang internally represents ranges where the end location points to the
    /// start of the token at the end. However, for external clients it is more
    /// useful to have a CXSourceRange be a proper half-open interval. This
    /// routine does the appropriate translation.
    SourceRange translateSourceRange(const SourceRange& range) const {
        // We want the last character in this location, so we will adjust the
        // location accordingly.
        auto R = CharSourceRange::getTokenRange(range);
        auto& LangOpts = Context->getLangOpts();
        auto& SM = Compiler->getSourceManager();
        SourceLocation EndLoc = R.getEnd();
        if (EndLoc.isValid() && EndLoc.isMacroID() &&
            !SM.isMacroArgExpansion(EndLoc))
            EndLoc = SM.getExpansionRange(EndLoc).second;
        if (R.isTokenRange() && EndLoc.isValid()) {
            unsigned Length =
                Lexer::MeasureTokenLength(SM.getSpellingLoc(EndLoc), SM,
                                          LangOpts);
            EndLoc = EndLoc.getLocWithOffset(Length - 1);
        }

        SourceRange Result = {R.getBegin(), EndLoc};
        return Result;
    }

    template <typename T>
    void addLocation(dict& json_node, const T& ast_node) {
        list start, end;
        dict location;
        auto& SM = Compiler->getSourceManager();
        auto range = ast_node->getSourceRange();
        range = translateSourceRange(range);

        if (!range.isValid())
            return;

        auto start_loc = SM.getExpansionLoc(range.getBegin());
        auto end_loc = SM.getExpansionLoc(range.getEnd());

        auto start_row = SM.getExpansionLineNumber(start_loc);
        auto start_column = SM.getExpansionColumnNumber(start_loc);
        auto end_row = SM.getExpansionLineNumber(end_loc);
        auto end_column = SM.getExpansionColumnNumber(end_loc);

        start.append(start_row);
        start.append(start_column);
        end.append(end_row);
        end.append(end_column);
        location["start"] = start;
        location["end"] = end;
        json_node["location"] = location;
    }

public:
#include "StatementVisitors.inc"
#include "DeclarationVisitors.inc"

    bool TraverseDecl(Decl* D) {
        if (!D || D->isInvalidDecl() || D->isImplicit()) {
            return true;
        }

        auto original_node = current_node;
        if (!std::string(D->getDeclKindName()).empty()) {
            debug_print("Adding node for decl: ", D->getDeclKindName());
            auto node = make_node();
            if (current_node) {
                list children = extract<list>((*current_node)["children"]);
                children.append(node);
            } else {
                debug_print("  Node is at top level");
                ast.append(node);
            }
            current_node = &node;
        }
        RecursiveASTVisitor<ASTDictBuilder>::TraverseDecl(D);
        current_node = original_node;
        return true;
    }

    bool TraverseStmt(Stmt* S) {
        if (!S) {
            return true;
        }

        auto original_node = current_node;
        if (!std::string(S->getStmtClassName()).empty() &&
            !stmtSkipList.count(S->getStmtClass())) {
            debug_print("Adding node for statement: ", S->getStmtClassName());
            auto node = make_node();
            list children = extract<list>((*current_node)["children"]);
            children.append(node);
            current_node = &node;
        } else {
            debug_print("Not adding node for: ", S->getStmtClassName());
        }
        RecursiveASTVisitor<ASTDictBuilder>::TraverseStmt(S);
        current_node = original_node;
        return true;
    }

    virtual bool HandleTopLevelDecl(DeclGroupRef DR) override {
        for (DeclGroupRef::iterator d = DR.begin(), e = DR.end(); d != e; ++d) {
            if (Compiler->getSourceManager().isInMainFile(
                    (*d)->getLocStart())) {
                debug_print("Traversing top level decl: ",
                            (*d)->getDeclKindName());
                TraverseDecl(*d);
            }
        }

        // TODO: This should probably be somewhere else
        auto diag = dynamic_cast<TextDiagnosticBuffer*>(
            Compiler->getDiagnostics().getClient());
        for (auto it = diag->err_begin(); it < diag->err_end(); ++it) {
            dict msg;
            msg["message"] = it->second;

            list location;
            auto row =
                Compiler->getSourceManager().getExpansionLineNumber(it->first);
            auto column = Compiler->getSourceManager().getExpansionColumnNumber(
                it->first);
            location.append(row);
            location.append(column);
            msg["location"] = location;
            errors.append(msg);
        }
        return true;
    }

    virtual void Initialize(clang::ASTContext& Context) override {
        debug_print("Init");
        this->Context = &Context;
    }

    ASTDictBuilder(const CompilerInstance* C, list& ast, list& errors)
        : Compiler{C}, ast{ast}, errors{errors}, current_node{nullptr} {
        auto& diagEngine = C->getDiagnostics();
        auto diagBuffer = new TextDiagnosticBuffer();
        diagEngine.setClient(diagBuffer);
    }
};

std::set<Decl::Kind> ASTDictBuilder::declSkipList = {};

// Currently just the implicit expressions from Stmt::IgnoreImplicit
std::set<Stmt::StmtClass> ASTDictBuilder::stmtSkipList =
    {Stmt::MaterializeTemporaryExprClass, Stmt::ExprWithCleanupsClass,
     Stmt::ImplicitCastExprClass, Stmt::CXXBindTemporaryExprClass};

class BuildDictFrontendAction : public clang::ASTFrontendAction {
    list& ast;
    list& errors;

public:
    BuildDictFrontendAction(list& ast, list& errors)
        : ast{ast}, errors{errors} {}
    virtual std::unique_ptr<clang::ASTConsumer>
    CreateASTConsumer(clang::CompilerInstance& Compiler, llvm::StringRef) {
        return std::unique_ptr<clang::ASTConsumer>(
            new ASTDictBuilder(&Compiler, ast, errors));
    }
};

class CPPParser {
public:
    CPPParser(std::string str) {
        auto action = new BuildDictFrontendAction(ast, errors);
        runToolOnCodeWithArgs(action, str,
                              {"-std=c++14", "-I/usr/lib/clang/3.7.1/include"});
    }

    list ast;
    list errors;
};

BOOST_PYTHON_MODULE(cppparser) {
    class_<CPPParser>("CPPParser", init<std::string>())
        .def_readonly("ast", &CPPParser::ast)
        .def_readonly("errors", &CPPParser::errors);
}
