#ifndef __STDC_LIMIT_MACROS
#define __STDC_LIMIT_MACROS
#endif

#ifndef __STDC_CONSTANT_MACROS
#define __STDC_CONSTANT_MACROS
#endif

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

using namespace clang;
using namespace boost::python;

class ASTDictBuilder : public ASTConsumer,
                       public RecursiveASTVisitor<ASTDictBuilder> {

    const SourceManager* SM;
    list ast;
    dict* current_node = nullptr;

    static int node_counter;
    static dict make_node() {
        dict node;
        node["children"] = list{};
        node["id"] = node_counter++;
        return node;
    }

    template <typename T>
    void addLocation(dict& json_node, const T& ast_node) {
        list start;
        list end;
        dict location;
        auto range = ast_node->getSourceRange();

        if (!range.isValid())
            return;

        auto start_loc = SM->getSpellingLoc(range.getBegin());
        auto end_loc = SM->getSpellingLoc(range.getEnd());
        auto ploc_start = SM->getPresumedLoc(start_loc);
        auto ploc_end = SM->getPresumedLoc(end_loc);
        start.append(ploc_start.getLine());
        start.append(ploc_start.getColumn());
        end.append(ploc_end.getLine());
        end.append(ploc_end.getColumn());
        location["start"] = start;
        location["end"] = end;
        json_node["location"] = location;
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
        if (!D)
            return false;

        auto original_node = current_node;
        if (!std::string(D->getDeclKindName()).empty()) {
            auto node = make_node();
            list children = extract<list>((*current_node)["children"]);
            children.append(node);
            current_node = &node;
        }
        auto ret = RecursiveASTVisitor<ASTDictBuilder>::TraverseDecl(D);
        current_node = original_node;
        return ret;
    }

    bool TraverseStmt(Stmt* S) {
        if (!S)
            return false;

        auto original_node = current_node;
        if (!std::string(S->getStmtClassName()).empty()) {
            auto node = make_node();
            list children = extract<list>((*current_node)["children"]);
            children.append(node);
            current_node = &node;
        }
        auto ret = RecursiveASTVisitor<ASTDictBuilder>::TraverseStmt(S);
        current_node = original_node;
        return ret;
    }

    void HandleTranslationUnit(ASTContext& ctx) override {
        TranslationUnitDecl* D = ctx.getTranslationUnitDecl();
        auto node = make_node();
        ast.append(node);
        current_node = &node;
        TraverseDecl(D);
    }

    ASTDictBuilder(const SourceManager* SM) : SM{SM} {}

    const list& getAST() const { return ast; }
};

int ASTDictBuilder::node_counter = 0;

class CPPParser {
public:
    CPPParser(std::string str) {
        // CompilerInstance will hold the instance of the Clang compiler for us,
        // managing the various objects needed to run the compiler.
        CompilerInstance TheCompInst;
        TheCompInst.createDiagnostics();

        LangOptions& lo = TheCompInst.getLangOpts();

        // Maybe not the best way to enable C++14, but the only
        // one I can find.
        lo.LineComment = 1;
        lo.CPlusPlus = 1;
        lo.CPlusPlus11 = 1;
        lo.CPlusPlus14 = 1;

        // Initialize target info with the default triple for our platform.
        auto TO = std::make_shared<TargetOptions>();
        TO->Triple = llvm::sys::getDefaultTargetTriple();
        TargetInfo* TI =
            TargetInfo::CreateTargetInfo(TheCompInst.getDiagnostics(), TO);
        TheCompInst.setTarget(TI);

        TheCompInst.createFileManager();
        FileManager& FileMgr = TheCompInst.getFileManager();
        TheCompInst.createSourceManager(FileMgr);

        SourceManager& SourceMgr = TheCompInst.getSourceManager();
        TheCompInst.createPreprocessor(TU_Module);
        TheCompInst.createASTContext();

        auto buffer = llvm::MemoryBuffer::getMemBuffer(str.c_str());
        SourceMgr.setMainFileID(
            SourceMgr.createFileID(std::move(buffer), SrcMgr::C_User));

        TheCompInst.getDiagnosticClient()
            .BeginSourceFile(TheCompInst.getLangOpts(),
                             &TheCompInst.getPreprocessor());

        // Create an AST consumer instance which is going to get called by
        // ParseAST.
        ASTDictBuilder builder{&SourceMgr};

        // Parse the file to AST, registering our consumer as the AST consumer.
        ParseAST(TheCompInst.getPreprocessor(), &builder,
                 TheCompInst.getASTContext());

        ast = builder.getAST();
    }
    list ast;
};

BOOST_PYTHON_MODULE(cppparser) {
    class_<CPPParser>("CPPParser", init<std::string>())
        .def_readonly("ast", &CPPParser::ast);
}
