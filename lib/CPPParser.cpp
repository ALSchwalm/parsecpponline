#ifndef __STDC_LIMIT_MACROS
#define __STDC_LIMIT_MACROS
#endif

#ifndef __STDC_CONSTANT_MACROS
#define __STDC_CONSTANT_MACROS
#endif

#include "clang/Frontend/FrontendActions.h"
#include "clang/Tooling/CommonOptionsParser.h"
#include "clang/Tooling/Tooling.h"
#include "clang/AST/ASTContext.h"
#include "clang/Parse/ParseAST.h"
#include "clang/AST/ASTConsumer.h"
#include "clang/AST/RecursiveASTVisitor.h"
#include "clang/Basic/Diagnostic.h"
#include "clang/Basic/FileManager.h"
#include "clang/Basic/SourceManager.h"
#include "clang/Basic/TargetOptions.h"
#include "clang/Basic/TargetInfo.h"
#include "clang/Frontend/CompilerInstance.h"
#include "clang/Frontend/ASTConsumers.h"

#include <boost/python.hpp>
#include <iostream>

using namespace clang;
using namespace clang::tooling;
using namespace boost::python;

// For now, I don't know how to actually get an argument to
// the ASTConsumer from outside the FrontendFactory, so just use
// some global state
boost::python::list global_ast;

class ASTDictBuilder : public ASTConsumer,
                       public RecursiveASTVisitor<ASTDictBuilder> {

    const SourceManager* SM;
    dict* current_node = nullptr;

    static int node_counter;
    static dict make_node() {
        dict node;
        node["children"] = list{};
        node["id"] = node_counter++;
        return node;
    }

    template <typename T>
    std::pair<std::pair<int, int>, std::pair<int, int>>
    addLocation(dict& json_node, const T& ast_node) {
        list start;
        list end;
        dict location;
        auto range = ast_node->getSourceRange();

        if (!range.isValid())
            return {{0, 0}, {0, 0}};

        auto start_loc = SM->getExpansionLoc(range.getBegin());
        auto end_loc = SM->getExpansionLoc(range.getEnd());

        auto start_row = SM->getExpansionLineNumber(start_loc);
        auto start_column = SM->getExpansionColumnNumber(start_loc);
        auto end_row = SM->getExpansionLineNumber(end_loc);
        auto end_column = SM->getExpansionColumnNumber(end_loc);

        start.append(start_row);
        start.append(start_column);
        end.append(end_row);
        end.append(end_column);
        location["start"] = start;
        location["end"] = end;
        json_node["location"] = location;
        return {{start_row, start_column}, {end_row, end_column}};
    }

public:
    bool VisitStmt(const Stmt* S) {
        auto& node = *current_node;
        node["label"] = S->getStmtClassName();
        addLocation(node, S);
        return true;
    }

    bool VisitDecl(const Decl* D) {
        auto& node = *current_node;
        node["label"] = D->getDeclKindName() + std::string("Decl");
        addLocation(node, D);
        return true;
    }

    bool TraverseDecl(Decl* D) {
        if (!D) {
            return true;
        }
        auto original_node = current_node;
        if (!std::string(D->getDeclKindName()).empty()) {
            auto node = make_node();
            if (current_node) {
                list children = extract<list>((*current_node)["children"]);
                children.append(node);
            } else {
                global_ast.append(node);
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
        if (!std::string(S->getStmtClassName()).empty()) {
            auto node = make_node();
            list children = extract<list>((*current_node)["children"]);
            children.append(node);
            current_node = &node;
        }
        RecursiveASTVisitor<ASTDictBuilder>::TraverseStmt(S);
        current_node = original_node;
        return true;
    }

    virtual bool HandleTopLevelDecl(DeclGroupRef DR) {
        for (DeclGroupRef::iterator d = DR.begin(), e = DR.end(); d != e; ++d) {
            if (SM->isInMainFile((*d)->getLocStart())) {
                TraverseDecl(*d);
            }
        }
        return true;
    }

    ASTDictBuilder(const SourceManager* SM) : SM{SM} { global_ast = list{}; }
};

int ASTDictBuilder::node_counter = 0;

class BuildDictFrontendAction : public clang::ASTFrontendAction {
public:
    virtual std::unique_ptr<clang::ASTConsumer>
    CreateASTConsumer(clang::CompilerInstance& Compiler, llvm::StringRef) {
        return std::unique_ptr<clang::ASTConsumer>(
            new ASTDictBuilder(&Compiler.getSourceManager()));
    }
};

static llvm::cl::OptionCategory MyToolCategory("my-tool options");

class CPPParser {
public:
    CPPParser(std::string str) {
        const char* argv[] = {"parse", "/tmp/main.cpp", "--", "clang++",

                              // TODO: Fix this hard coded path
                              "-std=c++11", "-I/usr/lib/clang/3.7.0/include",
                              "-c"};
        int argc = sizeof(argv) / sizeof(argv[0]);
        CommonOptionsParser OptionsParser(argc, argv, MyToolCategory);
        ClangTool Tool(OptionsParser.getCompilations(), {"/tmp/main.cpp"});
        Tool.mapVirtualFile("/tmp/main.cpp", str);
        Tool.run(newFrontendActionFactory<BuildDictFrontendAction>().get());
    }
};

BOOST_PYTHON_MODULE(cppparser) {
    class_<CPPParser>("CPPParser", init<std::string>())
        .def_readonly("ast", &global_ast);
}
