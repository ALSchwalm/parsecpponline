//===--- ASTDumper.cpp - Dumping implementation for ASTs ------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file implements the AST dump methods, which dump out the
// AST in a form that exposes type details and other fields.
//
//===----------------------------------------------------------------------===//

#ifndef __STDC_LIMIT_MACROS
#define __STDC_LIMIT_MACROS
#endif

#ifndef __STDC_CONSTANT_MACROS
#define __STDC_CONSTANT_MACROS
#endif

#include "clang/AST/ASTContext.h"
#include "clang/AST/Attr.h"
#include "clang/AST/CommentVisitor.h"
#include "clang/AST/DeclCXX.h"
#include "clang/AST/DeclLookups.h"
#include "clang/AST/DeclVisitor.h"
#include "clang/AST/StmtVisitor.h"
#include "clang/AST/TypeVisitor.h"
#include "clang/Basic/Builtins.h"
#include "clang/Basic/Module.h"
#include "clang/Basic/SourceManager.h"
#include "llvm/Support/raw_ostream.h"

#include "clang/AST/ASTConsumer.h"
#include "clang/AST/RecursiveASTVisitor.h"
#include "clang/Basic/Diagnostic.h"
#include "clang/Basic/FileManager.h"
#include "clang/Basic/SourceManager.h"
#include "clang/Basic/TargetOptions.h"
#include "clang/Basic/TargetInfo.h"
#include "clang/Frontend/CompilerInstance.h"
#include "clang/Frontend/ASTConsumers.h"
#include "clang/Lex/Preprocessor.h"
#include "clang/Parse/ParseAST.h"
#include "clang/Rewrite/Core/Rewriter.h"
#include "clang/Rewrite/Frontend/Rewriters.h"
#include "llvm/Support/Host.h"
#include "llvm/Support/raw_ostream.h"

#include <boost/python.hpp>

using namespace clang;
using namespace clang::comments;
using namespace boost::python;

//===----------------------------------------------------------------------===//
// ASTDumper Visitor
//===----------------------------------------------------------------------===//

class ASTDictBuilder : public ConstDeclVisitor<ASTDictBuilder>,
                       public ConstStmtVisitor<ASTDictBuilder>,
                       public ConstCommentVisitor<ASTDictBuilder>,
                       public TypeVisitor<ASTDictBuilder> {

    list ast;
    dict* parent_node;

    raw_ostream& OS;
    const CommandTraits* Traits;
    const SourceManager* SM;

    /// Pending[i] is an action to dump an entity at level i.
    llvm::SmallVector<std::function<void()>, 32> Pending;

    /// Indicates whether we're at the top level.
    bool TopLevel;

    /// Indicates if we're handling the first child after entering a new depth.
    bool FirstChild;

    /// Keep track of the last location we print out so that we can
    /// print out deltas from then on out.
    const char* LastLocFilename;
    unsigned LastLocLine;

    static int node_counter;
    static dict make_node() {
        dict node;
        node["children"] = list{};
        node["id"] = node_counter++;
        return node;
    }

    /// Dump a child of the current node.
    template <typename Fn>
    void dumpChild(Fn doDumpChild) {
        OS << "\n";
        // If we're at the top level, there's nothing interesting to do; just
        // run the dumper.
        if (TopLevel) {
            TopLevel = false;
            auto node = make_node();
            parent_node = &node;
            doDumpChild(node);

            while (!Pending.empty()) {
                Pending.back()();
                Pending.pop_back();
            }

            ast.append(node);
            TopLevel = true;
            return;
        }
        auto dumpWithIndent = [this, doDumpChild]() {
            auto original_node = parent_node;
            auto node = make_node();
            parent_node = &node;

            FirstChild = true;
            unsigned Depth = Pending.size();

            doDumpChild(node);

            // If any children are left, they're the last at their nesting
            // level.
            // Dump those ones out now.
            while (Depth < Pending.size()) {
                Pending.back()();
                this->Pending.pop_back();
            }

            parent_node = original_node;
            list children = extract<list>((*parent_node)["children"]);
            children.append(node);
        };

        if (FirstChild) {
            Pending.push_back(std::move(dumpWithIndent));
        } else {
            Pending.back()();
            Pending.back() = std::move(dumpWithIndent);
        }
        FirstChild = false;
    }

public:
    ASTDictBuilder(raw_ostream& OS, const CommandTraits* Traits,
                   const SourceManager* SM)
        : OS(OS),
          Traits(Traits),
          SM(SM),
          TopLevel(true),
          FirstChild(true),
          LastLocFilename(""),
          LastLocLine(~0U) {}

    const list& getAST() const { return ast; }

    void dumpDecl(const Decl* D);
    void dumpStmt(const Stmt* S);

    // Utilities
    void dumpPointer(const void* Ptr);
    void dumpSourceRange(SourceRange R, dict& node);
    void dumpLocation(SourceLocation Loc, list& location);
    void dumpBareType(QualType T, bool Desugar = true);
    void dumpType(QualType T);
    void dumpTypeAsChild(QualType T);
    void dumpTypeAsChild(const Type* T);
    void dumpBareDeclRef(const Decl* Node);
    void dumpDeclRef(const Decl* Node, const char* Label = nullptr);
    void dumpName(const NamedDecl* D);
    bool hasNodes(const DeclContext* DC);
    void dumpDeclContext(const DeclContext* DC);
    void dumpLookups(const DeclContext* DC, bool DumpDecls);
    void dumpAttr(const Attr* A);

    // C++ Utilities
    void dumpAccessSpecifier(AccessSpecifier AS);
    void dumpCXXCtorInitializer(const CXXCtorInitializer* Init);
    void dumpTemplateParameters(const TemplateParameterList* TPL);
    void dumpTemplateArgumentListInfo(const TemplateArgumentListInfo& TALI);
    void dumpTemplateArgumentLoc(const TemplateArgumentLoc& A);
    void dumpTemplateArgumentList(const TemplateArgumentList& TAL);
    void dumpTemplateArgument(const TemplateArgument& A,
                              SourceRange R = SourceRange());

    // Types
    void VisitComplexType(const ComplexType* T) {
        dumpTypeAsChild(T->getElementType());
    }
    void VisitPointerType(const PointerType* T) {
        dumpTypeAsChild(T->getPointeeType());
    }
    void VisitBlockPointerType(const BlockPointerType* T) {
        dumpTypeAsChild(T->getPointeeType());
    }
    void VisitReferenceType(const ReferenceType* T) {
        dumpTypeAsChild(T->getPointeeType());
    }
    void VisitRValueReferenceType(const ReferenceType* T) {
        if (T->isSpelledAsLValue())
            OS << " written as lvalue reference";
        VisitReferenceType(T);
    }
    void VisitMemberPointerType(const MemberPointerType* T) {
        dumpTypeAsChild(T->getClass());
        dumpTypeAsChild(T->getPointeeType());
    }
    void VisitArrayType(const ArrayType* T) {
        switch (T->getSizeModifier()) {
        case ArrayType::Normal:
            break;
        case ArrayType::Static:
            OS << " static";
            break;
        case ArrayType::Star:
            OS << " *";
            break;
        }
        OS << " " << T->getIndexTypeQualifiers().getAsString();
        dumpTypeAsChild(T->getElementType());
    }
    void VisitConstantArrayType(const ConstantArrayType* T) {
        OS << " " << T->getSize();
        VisitArrayType(T);
    }
    void VisitVariableArrayType(const VariableArrayType* T) {
        OS << " ";
        VisitArrayType(T);
        dumpStmt(T->getSizeExpr());
    }
    void VisitDependentSizedArrayType(const DependentSizedArrayType* T) {
        VisitArrayType(T);
        OS << " ";
        dumpStmt(T->getSizeExpr());
    }
    void
    VisitDependentSizedExtVectorType(const DependentSizedExtVectorType* T) {
        // OS << " ";
        // dumpLocation(T->getAttributeLoc());
        // dumpTypeAsChild(T->getElementType());
        // dumpStmt(T->getSizeExpr());
    }
    void VisitVectorType(const VectorType* T) {
        switch (T->getVectorKind()) {
        case VectorType::GenericVector:
            break;
        case VectorType::AltiVecVector:
            OS << " altivec";
            break;
        case VectorType::AltiVecPixel:
            OS << " altivec pixel";
            break;
        case VectorType::AltiVecBool:
            OS << " altivec bool";
            break;
        case VectorType::NeonVector:
            OS << " neon";
            break;
        case VectorType::NeonPolyVector:
            OS << " neon poly";
            break;
        }
        OS << " " << T->getNumElements();
        dumpTypeAsChild(T->getElementType());
    }
    void VisitFunctionType(const FunctionType* T) {
        auto EI = T->getExtInfo();
        if (EI.getNoReturn())
            OS << " noreturn";
        if (EI.getProducesResult())
            OS << " produces_result";
        if (EI.getHasRegParm())
            OS << " regparm " << EI.getRegParm();
        OS << " " << FunctionType::getNameForCallConv(EI.getCC());
        dumpTypeAsChild(T->getReturnType());
    }
    void VisitFunctionProtoType(const FunctionProtoType* T) {
        auto EPI = T->getExtProtoInfo();
        if (EPI.HasTrailingReturn)
            OS << " trailing_return";
        if (T->isConst())
            OS << " const";
        if (T->isVolatile())
            OS << " volatile";
        if (T->isRestrict())
            OS << " restrict";
        switch (EPI.RefQualifier) {
        case RQ_None:
            break;
        case RQ_LValue:
            OS << " &";
            break;
        case RQ_RValue:
            OS << " &&";
            break;
        }
        // FIXME: Exception specification.
        // FIXME: Consumed parameters.
        VisitFunctionType(T);
        for (QualType PT : T->getParamTypes())
            dumpTypeAsChild(PT);
        if (EPI.Variadic)
            dumpChild([=](dict&) { OS << "..."; });
    }
    void VisitUnresolvedUsingType(const UnresolvedUsingType* T) {
        dumpDeclRef(T->getDecl());
    }
    void VisitTypedefType(const TypedefType* T) { dumpDeclRef(T->getDecl()); }
    void VisitTypeOfExprType(const TypeOfExprType* T) {
        dumpStmt(T->getUnderlyingExpr());
    }
    void VisitDecltypeType(const DecltypeType* T) {
        dumpStmt(T->getUnderlyingExpr());
    }
    void VisitUnaryTransformType(const UnaryTransformType* T) {
        switch (T->getUTTKind()) {
        case UnaryTransformType::EnumUnderlyingType:
            OS << " underlying_type";
            break;
        }
        dumpTypeAsChild(T->getBaseType());
    }
    void VisitTagType(const TagType* T) { dumpDeclRef(T->getDecl()); }
    void VisitAttributedType(const AttributedType* T) {
        // FIXME: AttrKind
        dumpTypeAsChild(T->getModifiedType());
    }
    void VisitTemplateTypeParmType(const TemplateTypeParmType* T) {
        OS << " depth " << T->getDepth() << " index " << T->getIndex();
        if (T->isParameterPack())
            OS << " pack";
        dumpDeclRef(T->getDecl());
    }
    void VisitSubstTemplateTypeParmType(const SubstTemplateTypeParmType* T) {
        dumpTypeAsChild(T->getReplacedParameter());
    }
    void
    VisitSubstTemplateTypeParmPackType(const SubstTemplateTypeParmPackType* T) {
        dumpTypeAsChild(T->getReplacedParameter());
        dumpTemplateArgument(T->getArgumentPack());
    }
    void VisitAutoType(const AutoType* T) {
        if (T->isDecltypeAuto())
            OS << " decltype(auto)";
        if (!T->isDeduced())
            OS << " undeduced";
    }
    void VisitTemplateSpecializationType(const TemplateSpecializationType* T) {
        if (T->isTypeAlias())
            OS << " alias";
        OS << " ";
        T->getTemplateName().dump(OS);
        for (auto& Arg : *T)
            dumpTemplateArgument(Arg);
        if (T->isTypeAlias())
            dumpTypeAsChild(T->getAliasedType());
    }
    void VisitInjectedClassNameType(const InjectedClassNameType* T) {
        dumpDeclRef(T->getDecl());
    }

    void VisitAtomicType(const AtomicType* T) {
        dumpTypeAsChild(T->getValueType());
    }
    void VisitAdjustedType(const AdjustedType* T) {
        dumpTypeAsChild(T->getOriginalType());
    }
    void VisitPackExpansionType(const PackExpansionType* T) {
        if (auto N = T->getNumExpansions())
            OS << " expansions " << *N;
        if (!T->isSugared())
            dumpTypeAsChild(T->getPattern());
    }
    // FIXME: ElaboratedType, DependentNameType,
    // DependentTemplateSpecializationType, ObjCObjectType

    // Decls
    void VisitLabelDecl(const LabelDecl* D);
    void VisitTypedefDecl(const TypedefDecl* D);
    void VisitEnumDecl(const EnumDecl* D);
    void VisitRecordDecl(const RecordDecl* D);
    void VisitEnumConstantDecl(const EnumConstantDecl* D);
    void VisitIndirectFieldDecl(const IndirectFieldDecl* D);
    void VisitFunctionDecl(const FunctionDecl* D);
    void VisitFieldDecl(const FieldDecl* D);
    void VisitVarDecl(const VarDecl* D);
    void VisitFileScopeAsmDecl(const FileScopeAsmDecl* D);
    void VisitImportDecl(const ImportDecl* D);

    // C++ Decls
    void VisitNamespaceDecl(const NamespaceDecl* D);
    void VisitUsingDirectiveDecl(const UsingDirectiveDecl* D);
    void VisitNamespaceAliasDecl(const NamespaceAliasDecl* D);
    void VisitTypeAliasDecl(const TypeAliasDecl* D);
    void VisitTypeAliasTemplateDecl(const TypeAliasTemplateDecl* D);
    void VisitCXXRecordDecl(const CXXRecordDecl* D);
    void VisitStaticAssertDecl(const StaticAssertDecl* D);
    template <typename SpecializationDecl>
    void VisitTemplateDeclSpecialization(const SpecializationDecl* D,
                                         bool DumpExplicitInst,
                                         bool DumpRefOnly);
    template <typename TemplateDecl>
    void VisitTemplateDecl(const TemplateDecl* D, bool DumpExplicitInst);
    void VisitFunctionTemplateDecl(const FunctionTemplateDecl* D);
    void VisitClassTemplateDecl(const ClassTemplateDecl* D);
    void VisitClassTemplateSpecializationDecl(
        const ClassTemplateSpecializationDecl* D);
    void VisitClassTemplatePartialSpecializationDecl(
        const ClassTemplatePartialSpecializationDecl* D);
    void VisitClassScopeFunctionSpecializationDecl(
        const ClassScopeFunctionSpecializationDecl* D);
    void VisitVarTemplateDecl(const VarTemplateDecl* D);
    void
    VisitVarTemplateSpecializationDecl(const VarTemplateSpecializationDecl* D);
    void VisitVarTemplatePartialSpecializationDecl(
        const VarTemplatePartialSpecializationDecl* D);
    void VisitTemplateTypeParmDecl(const TemplateTypeParmDecl* D);
    void VisitNonTypeTemplateParmDecl(const NonTypeTemplateParmDecl* D);
    void VisitTemplateTemplateParmDecl(const TemplateTemplateParmDecl* D);
    void VisitUsingDecl(const UsingDecl* D);
    void VisitUnresolvedUsingTypenameDecl(const UnresolvedUsingTypenameDecl* D);
    void VisitUnresolvedUsingValueDecl(const UnresolvedUsingValueDecl* D);
    void VisitUsingShadowDecl(const UsingShadowDecl* D);
    void VisitLinkageSpecDecl(const LinkageSpecDecl* D);
    void VisitAccessSpecDecl(const AccessSpecDecl* D);
    void VisitFriendDecl(const FriendDecl* D);

    // Stmts.
    void VisitStmt(const Stmt* Node);
    void VisitDeclStmt(const DeclStmt* Node);
    void VisitAttributedStmt(const AttributedStmt* Node);
    void VisitLabelStmt(const LabelStmt* Node);
    void VisitGotoStmt(const GotoStmt* Node);
    void VisitCXXCatchStmt(const CXXCatchStmt* Node);

    // Exprs
    void VisitExpr(const Expr* Node);
    void VisitCastExpr(const CastExpr* Node);
    void VisitDeclRefExpr(const DeclRefExpr* Node);
    void VisitPredefinedExpr(const PredefinedExpr* Node);
    void VisitCharacterLiteral(const CharacterLiteral* Node);
    void VisitIntegerLiteral(const IntegerLiteral* Node);
    void VisitFloatingLiteral(const FloatingLiteral* Node);
    void VisitStringLiteral(const StringLiteral* Str);
    void VisitInitListExpr(const InitListExpr* ILE);
    void VisitUnaryOperator(const UnaryOperator* Node);
    void VisitUnaryExprOrTypeTraitExpr(const UnaryExprOrTypeTraitExpr* Node);
    void VisitMemberExpr(const MemberExpr* Node);
    void VisitExtVectorElementExpr(const ExtVectorElementExpr* Node);
    void VisitBinaryOperator(const BinaryOperator* Node);
    void VisitCompoundAssignOperator(const CompoundAssignOperator* Node);
    void VisitAddrLabelExpr(const AddrLabelExpr* Node);
    void VisitBlockExpr(const BlockExpr* Node);
    void VisitOpaqueValueExpr(const OpaqueValueExpr* Node);

    // C++
    void VisitCXXNamedCastExpr(const CXXNamedCastExpr* Node);
    void VisitCXXBoolLiteralExpr(const CXXBoolLiteralExpr* Node);
    void VisitCXXThisExpr(const CXXThisExpr* Node);
    void VisitCXXFunctionalCastExpr(const CXXFunctionalCastExpr* Node);
    void VisitCXXConstructExpr(const CXXConstructExpr* Node);
    void VisitCXXBindTemporaryExpr(const CXXBindTemporaryExpr* Node);
    void VisitCXXNewExpr(const CXXNewExpr* Node);
    void VisitCXXDeleteExpr(const CXXDeleteExpr* Node);
    void VisitMaterializeTemporaryExpr(const MaterializeTemporaryExpr* Node);
    void VisitExprWithCleanups(const ExprWithCleanups* Node);
    void VisitUnresolvedLookupExpr(const UnresolvedLookupExpr* Node);
    void dumpCXXTemporary(const CXXTemporary* Temporary);
    void VisitLambdaExpr(const LambdaExpr* Node) {
        VisitExpr(Node);
        dumpDecl(Node->getLambdaClass());
    }
    void VisitSizeOfPackExpr(const SizeOfPackExpr* Node);
};

int ASTDictBuilder::node_counter = 0;

//===----------------------------------------------------------------------===//
//  Utilities
//===----------------------------------------------------------------------===//

void ASTDictBuilder::dumpPointer(const void* Ptr) { OS << ' ' << Ptr; }

void ASTDictBuilder::dumpLocation(SourceLocation Loc, list& location) {
    if (!SM)
        return;

    SourceLocation SpellingLoc = SM->getSpellingLoc(Loc);

    // The general format we print out is filename:line:col, but we drop pieces
    // that haven't changed since the last loc printed.
    PresumedLoc PLoc = SM->getPresumedLoc(SpellingLoc);

    if (PLoc.isInvalid()) {
        OS << "<invalid sloc>";
        return;
    }

    location.append(PLoc.getLine());
    location.append(PLoc.getColumn());

    if (strcmp(PLoc.getFilename(), LastLocFilename) != 0) {
        OS << PLoc.getFilename() << ':' << PLoc.getLine() << ':'
           << PLoc.getColumn();
        LastLocFilename = PLoc.getFilename();
        LastLocLine = PLoc.getLine();
    } else if (PLoc.getLine() != LastLocLine) {
        OS << "line" << ':' << PLoc.getLine() << ':' << PLoc.getColumn();
        LastLocLine = PLoc.getLine();
    } else {
        OS << "col" << ':' << PLoc.getColumn();
    }
}

void ASTDictBuilder::dumpSourceRange(SourceRange R, dict& node) {
    // Can't translate locations if a SourceManager isn't available.
    if (!SM)
        return;
    dict location;
    list start;
    list end;
    node["location"] = location;

    dumpLocation(R.getBegin(), start);
    dumpLocation(R.getEnd(), end);
    location["start"] = start;
    location["end"] = end;
}

void ASTDictBuilder::dumpBareType(QualType T, bool Desugar) {

    SplitQualType T_split = T.split();
    OS << "'" << QualType::getAsString(T_split) << "'";

    if (Desugar && !T.isNull()) {
        // If the type is sugared, also dump a (shallow) desugared type.
        SplitQualType D_split = T.getSplitDesugaredType();
        if (T_split != D_split)
            OS << ":'" << QualType::getAsString(D_split) << "'";
    }
}

void ASTDictBuilder::dumpType(QualType T) {
    OS << ' ';
    dumpBareType(T);
}

void ASTDictBuilder::dumpTypeAsChild(QualType T) {
    SplitQualType SQT = T.split();
    if (!SQT.Quals.hasQualifiers())
        return dumpTypeAsChild(SQT.Ty);

    dumpChild([=](dict& node) {
        OS << "QualType";
        dumpPointer(T.getAsOpaquePtr());
        OS << " ";
        dumpBareType(T, false);
        OS << " " << T.split().Quals.getAsString();
        dumpTypeAsChild(T.split().Ty);
    });
}

void ASTDictBuilder::dumpTypeAsChild(const Type* T) {
    dumpChild([=](dict& node) {
        if (!T) {
            OS << "<<<NULL>>>";
            return;
        }

        { OS << T->getTypeClassName() << "Type"; }
        dumpPointer(T);
        OS << " ";
        dumpBareType(QualType(T, 0), false);

        QualType SingleStepDesugar =
            T->getLocallyUnqualifiedSingleStepDesugaredType();
        if (SingleStepDesugar != QualType(T, 0))
            OS << " sugar";
        if (T->isDependentType())
            OS << " dependent";
        else if (T->isInstantiationDependentType())
            OS << " instantiation_dependent";
        if (T->isVariablyModifiedType())
            OS << " variably_modified";
        if (T->containsUnexpandedParameterPack())
            OS << " contains_unexpanded_pack";
        if (T->isFromAST())
            OS << " imported";

        TypeVisitor<ASTDictBuilder>::Visit(T);

        if (SingleStepDesugar != QualType(T, 0))
            dumpTypeAsChild(SingleStepDesugar);
    });
}

void ASTDictBuilder::dumpBareDeclRef(const Decl* D) {
    { OS << D->getDeclKindName(); }
    dumpPointer(D);

    if (const NamedDecl* ND = dyn_cast<NamedDecl>(D)) {

        OS << " '" << ND->getDeclName() << '\'';
    }

    if (const ValueDecl* VD = dyn_cast<ValueDecl>(D))
        dumpType(VD->getType());
}

void ASTDictBuilder::dumpDeclRef(const Decl* D, const char* Label) {
    if (!D)
        return;

    dumpChild([=](dict& node) {
        if (Label)
            OS << Label << ' ';
        dumpBareDeclRef(D);
    });
}

void ASTDictBuilder::dumpName(const NamedDecl* ND) {
    if (ND->getDeclName()) {

        OS << ' ' << ND->getNameAsString();
    }
}

bool ASTDictBuilder::hasNodes(const DeclContext* DC) {
    if (!DC)
        return false;

    return DC->hasExternalLexicalStorage() ||
           DC->noload_decls_begin() != DC->noload_decls_end();
}

void ASTDictBuilder::dumpDeclContext(const DeclContext* DC) {
    if (!DC)
        return;

    for (auto* D : DC->noload_decls())
        dumpDecl(D);

    if (DC->hasExternalLexicalStorage()) {
        dumpChild([=](dict&) { OS << "<undeserialized declarations>"; });
    }
}

void ASTDictBuilder::dumpLookups(const DeclContext* DC, bool DumpDecls) {
    dumpChild([=](dict&) {
        OS << "StoredDeclsMap ";
        dumpBareDeclRef(cast<Decl>(DC));

        const DeclContext* Primary = DC->getPrimaryContext();
        if (Primary != DC) {
            OS << " primary";
            dumpPointer(cast<Decl>(Primary));
        }

        bool HasUndeserializedLookups = Primary->hasExternalVisibleStorage();

        DeclContext::all_lookups_iterator I = Primary->noload_lookups_begin(),
                                          E = Primary->noload_lookups_end();
        while (I != E) {
            DeclarationName Name = I.getLookupName();
            DeclContextLookupResult R = *I++;

            dumpChild([=](dict&) {
                OS << "DeclarationName ";
                { OS << '\'' << Name << '\''; }

                for (DeclContextLookupResult::iterator RI = R.begin(),
                                                       RE = R.end();
                     RI != RE; ++RI) {
                    dumpChild([=](dict&) {
                        dumpBareDeclRef(*RI);

                        if ((*RI)->isHidden())
                            OS << " hidden";

                        // If requested, dump the redecl chain for this lookup.
                        if (DumpDecls) {
                            // Dump earliest decl first.
                            std::function<void(Decl*)> DumpWithPrev = [&](
                                Decl* D) {
                                if (Decl* Prev = D->getPreviousDecl())
                                    DumpWithPrev(Prev);
                                dumpDecl(D);
                            };
                            DumpWithPrev(*RI);
                        }
                    });
                }
            });
        }

        if (HasUndeserializedLookups) {
            dumpChild([=](dict&) { OS << "<undeserialized lookups>"; });
        }
    });
}

void ASTDictBuilder::dumpAttr(const Attr* A) {
    dumpChild([=](dict& node) {
        {

            switch (A->getKind()) {
#define ATTR(X)                                                                \
    case attr::X:                                                              \
        OS << #X;                                                              \
        break;
#include "clang/Basic/AttrList.inc"
            default:
                llvm_unreachable("unexpected attribute kind");
            }
            OS << "Attr";
        }
        dumpPointer(A);
        dumpSourceRange(A->getRange(), node);
        if (A->isInherited())
            OS << " Inherited";
        if (A->isImplicit())
            OS << " Implicit";
#include "clang/AST/AttrDump.inc"
    });
}

static void dumpPreviousDeclImpl(raw_ostream& OS, ...) {}

template <typename T>
static void dumpPreviousDeclImpl(raw_ostream& OS, const Mergeable<T>* D) {
    const T* First = D->getFirstDecl();
    if (First != D)
        OS << " first " << First;
}

template <typename T>
static void dumpPreviousDeclImpl(raw_ostream& OS, const Redeclarable<T>* D) {
    const T* Prev = D->getPreviousDecl();
    if (Prev)
        OS << " prev " << Prev;
}

/// Dump the previous declaration in the redeclaration chain for a declaration,
/// if any.
static void dumpPreviousDecl(raw_ostream& OS, const Decl* D) {
    switch (D->getKind()) {
#define DECL(DERIVED, BASE)                                                    \
    case Decl::DERIVED:                                                        \
        return dumpPreviousDeclImpl(OS, cast<DERIVED##Decl>(D));
#define ABSTRACT_DECL(DECL)
#include "clang/AST/DeclNodes.inc"
    }
    llvm_unreachable("Decl that isn't part of DeclNodes.inc!");
}

//===----------------------------------------------------------------------===//
//  C++ Utilities
//===----------------------------------------------------------------------===//

void ASTDictBuilder::dumpAccessSpecifier(AccessSpecifier AS) {
    switch (AS) {
    case AS_none:
        break;
    case AS_public:
        OS << "public";
        break;
    case AS_protected:
        OS << "protected";
        break;
    case AS_private:
        OS << "private";
        break;
    }
}

void ASTDictBuilder::dumpCXXCtorInitializer(const CXXCtorInitializer* Init) {
    dumpChild([=](dict&) {
        OS << "CXXCtorInitializer";
        if (Init->isAnyMemberInitializer()) {
            OS << ' ';
            dumpBareDeclRef(Init->getAnyMember());
        } else if (Init->isBaseInitializer()) {
            dumpType(QualType(Init->getBaseClass(), 0));
        } else if (Init->isDelegatingInitializer()) {
            dumpType(Init->getTypeSourceInfo()->getType());
        } else {
            llvm_unreachable("Unknown initializer type");
        }
        dumpStmt(Init->getInit());
    });
}

void ASTDictBuilder::dumpTemplateParameters(const TemplateParameterList* TPL) {
    if (!TPL)
        return;

    for (TemplateParameterList::const_iterator I = TPL->begin(), E = TPL->end();
         I != E; ++I)
        dumpDecl(*I);
}

void ASTDictBuilder::dumpTemplateArgumentListInfo(
    const TemplateArgumentListInfo& TALI) {
    for (unsigned i = 0, e = TALI.size(); i < e; ++i)
        dumpTemplateArgumentLoc(TALI[i]);
}

void ASTDictBuilder::dumpTemplateArgumentLoc(const TemplateArgumentLoc& A) {
    dumpTemplateArgument(A.getArgument(), A.getSourceRange());
}

void ASTDictBuilder::dumpTemplateArgumentList(const TemplateArgumentList& TAL) {
    for (unsigned i = 0, e = TAL.size(); i < e; ++i)
        dumpTemplateArgument(TAL[i]);
}

void ASTDictBuilder::dumpTemplateArgument(const TemplateArgument& A,
                                          SourceRange R) {
    dumpChild([=](dict& node) {
        OS << "TemplateArgument";
        if (R.isValid())
            dumpSourceRange(R, node);

        switch (A.getKind()) {
        case TemplateArgument::Null:
            OS << " null";
            break;
        case TemplateArgument::Type:
            OS << " type";
            dumpType(A.getAsType());
            break;
        case TemplateArgument::Declaration:
            OS << " decl";
            dumpDeclRef(A.getAsDecl());
            break;
        case TemplateArgument::NullPtr:
            OS << " nullptr";
            break;
        case TemplateArgument::Integral:
            OS << " integral " << A.getAsIntegral();
            break;
        case TemplateArgument::Template:
            OS << " template ";
            A.getAsTemplate().dump(OS);
            break;
        case TemplateArgument::TemplateExpansion:
            OS << " template expansion";
            A.getAsTemplateOrTemplatePattern().dump(OS);
            break;
        case TemplateArgument::Expression:
            OS << " expr";
            dumpStmt(A.getAsExpr());
            break;
        case TemplateArgument::Pack:
            OS << " pack";
            for (TemplateArgument::pack_iterator I = A.pack_begin(),
                                                 E = A.pack_end();
                 I != E; ++I)
                dumpTemplateArgument(*I);
            break;
        }
    });
}

//===----------------------------------------------------------------------===//
//  Decl dumping methods.
//===----------------------------------------------------------------------===//

void ASTDictBuilder::dumpDecl(const Decl* D) {
    dumpChild([=](dict& node) {
        if (!D) {
            OS << "<<<NULL>>>";
            return;
        }

        node["label"] = std::string(D->getDeclKindName()) + "Decl";
        { OS << D->getDeclKindName() << "Decl"; }
        dumpPointer(D);
        if (D->getLexicalDeclContext() != D->getDeclContext())
            OS << " parent " << cast<Decl>(D->getDeclContext());
        dumpPreviousDecl(OS, D);
        dumpSourceRange(D->getSourceRange(), node);
        if (Module* M = D->getImportedOwningModule())
            OS << " in " << M->getFullModuleName();
        else if (Module* M = D->getLocalOwningModule())
            OS << " in (local) " << M->getFullModuleName();
        if (auto* ND = dyn_cast<NamedDecl>(D))
            for (Module* M : D->getASTContext().getModulesWithMergedDefinition(
                     const_cast<NamedDecl*>(ND)))
                dumpChild(
                    [=](dict&) { OS << "also in " << M->getFullModuleName(); });
        if (const NamedDecl* ND = dyn_cast<NamedDecl>(D))
            if (ND->isHidden())
                OS << " hidden";
        if (D->isImplicit())
            OS << " implicit";
        if (D->isUsed())
            OS << " used";
        else if (D->isThisDeclarationReferenced())
            OS << " referenced";
        if (D->isInvalidDecl())
            OS << " invalid";
        if (const FunctionDecl* FD = dyn_cast<FunctionDecl>(D))
            if (FD->isConstexpr())
                OS << " constexpr";

        ConstDeclVisitor<ASTDictBuilder>::Visit(D);

        for (Decl::attr_iterator I = D->attr_begin(), E = D->attr_end(); I != E;
             ++I)
            dumpAttr(*I);

        // Decls within functions are visited by the body.
        if (!isa<FunctionDecl>(*D) && !isa<ObjCMethodDecl>(*D) &&
            hasNodes(dyn_cast<DeclContext>(D)))
            dumpDeclContext(cast<DeclContext>(D));
    });
}

void ASTDictBuilder::VisitLabelDecl(const LabelDecl* D) { dumpName(D); }

void ASTDictBuilder::VisitTypedefDecl(const TypedefDecl* D) {
    dumpName(D);
    dumpType(D->getUnderlyingType());
    if (D->isModulePrivate())
        OS << " __module_private__";
}

void ASTDictBuilder::VisitEnumDecl(const EnumDecl* D) {
    if (D->isScoped()) {
        if (D->isScopedUsingClassTag())
            OS << " class";
        else
            OS << " struct";
    }
    dumpName(D);
    if (D->isModulePrivate())
        OS << " __module_private__";
    if (D->isFixed())
        dumpType(D->getIntegerType());
}

void ASTDictBuilder::VisitRecordDecl(const RecordDecl* D) {
    OS << ' ' << D->getKindName();
    dumpName(D);
    if (D->isModulePrivate())
        OS << " __module_private__";
    if (D->isCompleteDefinition())
        OS << " definition";
}

void ASTDictBuilder::VisitEnumConstantDecl(const EnumConstantDecl* D) {
    dumpName(D);
    dumpType(D->getType());
    if (const Expr* Init = D->getInitExpr())
        dumpStmt(Init);
}

void ASTDictBuilder::VisitIndirectFieldDecl(const IndirectFieldDecl* D) {
    dumpName(D);
    dumpType(D->getType());

    for (auto* Child : D->chain())
        dumpDeclRef(Child);
}

void ASTDictBuilder::VisitFunctionDecl(const FunctionDecl* D) {
    dumpName(D);
    dumpType(D->getType());

    StorageClass SC = D->getStorageClass();
    if (SC != SC_None)
        OS << ' ' << VarDecl::getStorageClassSpecifierString(SC);
    if (D->isInlineSpecified())
        OS << " inline";
    if (D->isVirtualAsWritten())
        OS << " virtual";
    if (D->isModulePrivate())
        OS << " __module_private__";

    if (D->isPure())
        OS << " pure";
    else if (D->isDeletedAsWritten())
        OS << " delete";

    if (const FunctionProtoType* FPT =
            D->getType()->getAs<FunctionProtoType>()) {
        FunctionProtoType::ExtProtoInfo EPI = FPT->getExtProtoInfo();
        switch (EPI.ExceptionSpec.Type) {
        default:
            break;
        case EST_Unevaluated:
            OS << " noexcept-unevaluated " << EPI.ExceptionSpec.SourceDecl;
            break;
        case EST_Uninstantiated:
            OS << " noexcept-uninstantiated "
               << EPI.ExceptionSpec.SourceTemplate;
            break;
        }
    }

    if (const FunctionTemplateSpecializationInfo* FTSI =
            D->getTemplateSpecializationInfo())
        dumpTemplateArgumentList(*FTSI->TemplateArguments);

    for (ArrayRef<NamedDecl *>::iterator
             I = D->getDeclsInPrototypeScope().begin(),
             E = D->getDeclsInPrototypeScope().end();
         I != E; ++I)
        dumpDecl(*I);

    if (!D->param_begin() && D->getNumParams())
        dumpChild([=](dict&) {
            OS << "<<NULL params x " << D->getNumParams() << ">>";
        });
    else
        for (FunctionDecl::param_const_iterator I = D->param_begin(),
                                                E = D->param_end();
             I != E; ++I)
            dumpDecl(*I);

    if (const CXXConstructorDecl* C = dyn_cast<CXXConstructorDecl>(D))
        for (CXXConstructorDecl::init_const_iterator I = C->init_begin(),
                                                     E = C->init_end();
             I != E; ++I)
            dumpCXXCtorInitializer(*I);

    if (D->doesThisDeclarationHaveABody())
        dumpStmt(D->getBody());
}

void ASTDictBuilder::VisitFieldDecl(const FieldDecl* D) {
    dumpName(D);
    dumpType(D->getType());
    if (D->isMutable())
        OS << " mutable";
    if (D->isModulePrivate())
        OS << " __module_private__";

    if (D->isBitField())
        dumpStmt(D->getBitWidth());
    if (Expr* Init = D->getInClassInitializer())
        dumpStmt(Init);
}

void ASTDictBuilder::VisitVarDecl(const VarDecl* D) {
    dumpName(D);
    dumpType(D->getType());
    StorageClass SC = D->getStorageClass();
    if (SC != SC_None)
        OS << ' ' << VarDecl::getStorageClassSpecifierString(SC);
    switch (D->getTLSKind()) {
    case VarDecl::TLS_None:
        break;
    case VarDecl::TLS_Static:
        OS << " tls";
        break;
    case VarDecl::TLS_Dynamic:
        OS << " tls_dynamic";
        break;
    }
    if (D->isModulePrivate())
        OS << " __module_private__";
    if (D->isNRVOVariable())
        OS << " nrvo";
    if (D->hasInit()) {
        switch (D->getInitStyle()) {
        case VarDecl::CInit:
            OS << " cinit";
            break;
        case VarDecl::CallInit:
            OS << " callinit";
            break;
        case VarDecl::ListInit:
            OS << " listinit";
            break;
        }
        dumpStmt(D->getInit());
    }
}

void ASTDictBuilder::VisitFileScopeAsmDecl(const FileScopeAsmDecl* D) {
    dumpStmt(D->getAsmString());
}

void ASTDictBuilder::VisitImportDecl(const ImportDecl* D) {
    OS << ' ' << D->getImportedModule()->getFullModuleName();
}

//===----------------------------------------------------------------------===//
// C++ Declarations
//===----------------------------------------------------------------------===//

void ASTDictBuilder::VisitNamespaceDecl(const NamespaceDecl* D) {
    dumpName(D);
    if (D->isInline())
        OS << " inline";
    if (!D->isOriginalNamespace())
        dumpDeclRef(D->getOriginalNamespace(), "original");
}

void ASTDictBuilder::VisitUsingDirectiveDecl(const UsingDirectiveDecl* D) {
    OS << ' ';
    dumpBareDeclRef(D->getNominatedNamespace());
}

void ASTDictBuilder::VisitNamespaceAliasDecl(const NamespaceAliasDecl* D) {
    dumpName(D);
    dumpDeclRef(D->getAliasedNamespace());
}

void ASTDictBuilder::VisitTypeAliasDecl(const TypeAliasDecl* D) {
    dumpName(D);
    dumpType(D->getUnderlyingType());
}

void ASTDictBuilder::VisitTypeAliasTemplateDecl(
    const TypeAliasTemplateDecl* D) {
    dumpName(D);
    dumpTemplateParameters(D->getTemplateParameters());
    dumpDecl(D->getTemplatedDecl());
}

void ASTDictBuilder::VisitCXXRecordDecl(const CXXRecordDecl* D) {
    VisitRecordDecl(D);
    if (!D->isCompleteDefinition())
        return;

    for (const auto& I : D->bases()) {
        dumpChild([=](dict&) {
            if (I.isVirtual())
                OS << "virtual ";
            dumpAccessSpecifier(I.getAccessSpecifier());
            dumpType(I.getType());
            if (I.isPackExpansion())
                OS << "...";
        });
    }
}

void ASTDictBuilder::VisitStaticAssertDecl(const StaticAssertDecl* D) {
    dumpStmt(D->getAssertExpr());
    dumpStmt(D->getMessage());
}

template <typename SpecializationDecl>
void ASTDictBuilder::VisitTemplateDeclSpecialization(
    const SpecializationDecl* D, bool DumpExplicitInst, bool DumpRefOnly) {
    bool DumpedAny = false;
    for (auto* RedeclWithBadType : D->redecls()) {
        // FIXME: The redecls() range sometimes has elements of a less-specific
        // type. (In particular, ClassTemplateSpecializationDecl::redecls()
        // gives
        // us TagDecls, and should give CXXRecordDecls).
        auto* Redecl = dyn_cast<SpecializationDecl>(RedeclWithBadType);
        if (!Redecl) {
            // Found the injected-class-name for a class template. This will be
            // dumped
            // as part of its surrounding class so we don't need to dump it
            // here.
            assert(isa<CXXRecordDecl>(RedeclWithBadType) &&
                   "expected an injected-class-name");
            continue;
        }

        switch (Redecl->getTemplateSpecializationKind()) {
        case TSK_ExplicitInstantiationDeclaration:
        case TSK_ExplicitInstantiationDefinition:
            if (!DumpExplicitInst)
                break;
        // Fall through.
        case TSK_Undeclared:
        case TSK_ImplicitInstantiation:
            if (DumpRefOnly)
                dumpDeclRef(Redecl);
            else
                dumpDecl(Redecl);
            DumpedAny = true;
            break;
        case TSK_ExplicitSpecialization:
            break;
        }
    }

    // Ensure we dump at least one decl for each specialization.
    if (!DumpedAny)
        dumpDeclRef(D);
}

template <typename TemplateDecl>
void ASTDictBuilder::VisitTemplateDecl(const TemplateDecl* D,
                                       bool DumpExplicitInst) {
    dumpName(D);
    dumpTemplateParameters(D->getTemplateParameters());

    dumpDecl(D->getTemplatedDecl());

    for (auto* Child : D->specializations())
        VisitTemplateDeclSpecialization(Child, DumpExplicitInst,
                                        !D->isCanonicalDecl());
}

void ASTDictBuilder::VisitFunctionTemplateDecl(const FunctionTemplateDecl* D) {
    // FIXME: We don't add a declaration of a function template specialization
    // to its context when it's explicitly instantiated, so dump explicit
    // instantiations when we dump the template itself.
    VisitTemplateDecl(D, true);
}

void ASTDictBuilder::VisitClassTemplateDecl(const ClassTemplateDecl* D) {
    VisitTemplateDecl(D, false);
}

void ASTDictBuilder::VisitClassTemplateSpecializationDecl(
    const ClassTemplateSpecializationDecl* D) {
    VisitCXXRecordDecl(D);
    dumpTemplateArgumentList(D->getTemplateArgs());
}

void ASTDictBuilder::VisitClassTemplatePartialSpecializationDecl(
    const ClassTemplatePartialSpecializationDecl* D) {
    VisitClassTemplateSpecializationDecl(D);
    dumpTemplateParameters(D->getTemplateParameters());
}

void ASTDictBuilder::VisitClassScopeFunctionSpecializationDecl(
    const ClassScopeFunctionSpecializationDecl* D) {
    dumpDeclRef(D->getSpecialization());
    if (D->hasExplicitTemplateArgs())
        dumpTemplateArgumentListInfo(D->templateArgs());
}

void ASTDictBuilder::VisitVarTemplateDecl(const VarTemplateDecl* D) {
    VisitTemplateDecl(D, false);
}

void ASTDictBuilder::VisitVarTemplateSpecializationDecl(
    const VarTemplateSpecializationDecl* D) {
    dumpTemplateArgumentList(D->getTemplateArgs());
    VisitVarDecl(D);
}

void ASTDictBuilder::VisitVarTemplatePartialSpecializationDecl(
    const VarTemplatePartialSpecializationDecl* D) {
    dumpTemplateParameters(D->getTemplateParameters());
    VisitVarTemplateSpecializationDecl(D);
}

void ASTDictBuilder::VisitTemplateTypeParmDecl(const TemplateTypeParmDecl* D) {
    if (D->wasDeclaredWithTypename())
        OS << " typename";
    else
        OS << " class";
    if (D->isParameterPack())
        OS << " ...";
    dumpName(D);
    if (D->hasDefaultArgument())
        dumpTemplateArgument(D->getDefaultArgument());
}

void ASTDictBuilder::VisitNonTypeTemplateParmDecl(
    const NonTypeTemplateParmDecl* D) {
    dumpType(D->getType());
    if (D->isParameterPack())
        OS << " ...";
    dumpName(D);
    if (D->hasDefaultArgument())
        dumpTemplateArgument(D->getDefaultArgument());
}

void ASTDictBuilder::VisitTemplateTemplateParmDecl(
    const TemplateTemplateParmDecl* D) {
    if (D->isParameterPack())
        OS << " ...";
    dumpName(D);
    dumpTemplateParameters(D->getTemplateParameters());
    if (D->hasDefaultArgument())
        dumpTemplateArgumentLoc(D->getDefaultArgument());
}

void ASTDictBuilder::VisitUsingDecl(const UsingDecl* D) {
    OS << ' ';
    D->getQualifier()->print(OS, D->getASTContext().getPrintingPolicy());
    OS << D->getNameAsString();
}

void ASTDictBuilder::VisitUnresolvedUsingTypenameDecl(
    const UnresolvedUsingTypenameDecl* D) {
    OS << ' ';
    D->getQualifier()->print(OS, D->getASTContext().getPrintingPolicy());
    OS << D->getNameAsString();
}

void ASTDictBuilder::VisitUnresolvedUsingValueDecl(
    const UnresolvedUsingValueDecl* D) {
    OS << ' ';
    D->getQualifier()->print(OS, D->getASTContext().getPrintingPolicy());
    OS << D->getNameAsString();
    dumpType(D->getType());
}

void ASTDictBuilder::VisitUsingShadowDecl(const UsingShadowDecl* D) {
    OS << ' ';
    dumpBareDeclRef(D->getTargetDecl());
}

void ASTDictBuilder::VisitLinkageSpecDecl(const LinkageSpecDecl* D) {
    switch (D->getLanguage()) {
    case LinkageSpecDecl::lang_c:
        OS << " C";
        break;
    case LinkageSpecDecl::lang_cxx:
        OS << " C++";
        break;
    }
}

void ASTDictBuilder::VisitAccessSpecDecl(const AccessSpecDecl* D) {
    OS << ' ';
    dumpAccessSpecifier(D->getAccess());
}

void ASTDictBuilder::VisitFriendDecl(const FriendDecl* D) {
    if (TypeSourceInfo* T = D->getFriendType())
        dumpType(T->getType());
    else
        dumpDecl(D->getFriendDecl());
}

//===----------------------------------------------------------------------===//
//  Stmt dumping methods.
//===----------------------------------------------------------------------===//

void ASTDictBuilder::dumpStmt(const Stmt* S) {
    dumpChild([=](dict&) {
        if (!S) {

            OS << "<<<NULL>>>";
            return;
        }

        if (const DeclStmt* DS = dyn_cast<DeclStmt>(S)) {
            VisitDeclStmt(DS);
            return;
        }

        ConstStmtVisitor<ASTDictBuilder>::Visit(S);

        for (const Stmt* SubStmt : S->children())
            dumpStmt(SubStmt);
    });
}

void ASTDictBuilder::VisitStmt(const Stmt* Node) {
    { OS << Node->getStmtClassName(); }
    dumpPointer(Node);
    // dumpSourceRange(Node->getSourceRange());
}

void ASTDictBuilder::VisitDeclStmt(const DeclStmt* Node) {
    VisitStmt(Node);
    for (DeclStmt::const_decl_iterator I = Node->decl_begin(),
                                       E = Node->decl_end();
         I != E; ++I)
        dumpDecl(*I);
}

void ASTDictBuilder::VisitAttributedStmt(const AttributedStmt* Node) {
    VisitStmt(Node);
    for (ArrayRef<const Attr *>::iterator I = Node->getAttrs().begin(),
                                          E = Node->getAttrs().end();
         I != E; ++I)
        dumpAttr(*I);
}

void ASTDictBuilder::VisitLabelStmt(const LabelStmt* Node) {
    VisitStmt(Node);
    OS << " '" << Node->getName() << "'";
}

void ASTDictBuilder::VisitGotoStmt(const GotoStmt* Node) {
    VisitStmt(Node);
    OS << " '" << Node->getLabel()->getName() << "'";
    dumpPointer(Node->getLabel());
}

void ASTDictBuilder::VisitCXXCatchStmt(const CXXCatchStmt* Node) {
    VisitStmt(Node);
    dumpDecl(Node->getExceptionDecl());
}

//===----------------------------------------------------------------------===//
//  Expr dumping methods.
//===----------------------------------------------------------------------===//

void ASTDictBuilder::VisitExpr(const Expr* Node) {
    VisitStmt(Node);
    dumpType(Node->getType());

    {

        switch (Node->getValueKind()) {
        case VK_RValue:
            break;
        case VK_LValue:
            OS << " lvalue";
            break;
        case VK_XValue:
            OS << " xvalue";
            break;
        }
    }

    {

        switch (Node->getObjectKind()) {
        case OK_Ordinary:
            break;
        case OK_BitField:
            OS << " bitfield";
            break;
        case OK_ObjCProperty:
            OS << " objcproperty";
            break;
        case OK_ObjCSubscript:
            OS << " objcsubscript";
            break;
        case OK_VectorComponent:
            OS << " vectorcomponent";
            break;
        }
    }
}

static void dumpBasePath(raw_ostream& OS, const CastExpr* Node) {
    if (Node->path_empty())
        return;

    OS << " (";
    bool First = true;
    for (CastExpr::path_const_iterator I = Node->path_begin(),
                                       E = Node->path_end();
         I != E; ++I) {
        const CXXBaseSpecifier* Base = *I;
        if (!First)
            OS << " -> ";

        const CXXRecordDecl* RD = cast<CXXRecordDecl>(
            Base->getType()->getAs<RecordType>()->getDecl());

        if (Base->isVirtual())
            OS << "virtual ";
        OS << RD->getName();
        First = false;
    }

    OS << ')';
}

void ASTDictBuilder::VisitCastExpr(const CastExpr* Node) {
    VisitExpr(Node);
    OS << " <";
    { OS << Node->getCastKindName(); }
    dumpBasePath(OS, Node);
    OS << ">";
}

void ASTDictBuilder::VisitDeclRefExpr(const DeclRefExpr* Node) {
    VisitExpr(Node);

    OS << " ";
    dumpBareDeclRef(Node->getDecl());
    if (Node->getDecl() != Node->getFoundDecl()) {
        OS << " (";
        dumpBareDeclRef(Node->getFoundDecl());
        OS << ")";
    }
}

void ASTDictBuilder::VisitUnresolvedLookupExpr(
    const UnresolvedLookupExpr* Node) {
    VisitExpr(Node);
    OS << " (";
    if (!Node->requiresADL())
        OS << "no ";
    OS << "ADL) = '" << Node->getName() << '\'';

    UnresolvedLookupExpr::decls_iterator I = Node->decls_begin(),
                                         E = Node->decls_end();
    if (I == E)
        OS << " empty";
    for (; I != E; ++I)
        dumpPointer(*I);
}

void ASTDictBuilder::VisitPredefinedExpr(const PredefinedExpr* Node) {
    VisitExpr(Node);
    OS << " " << PredefinedExpr::getIdentTypeName(Node->getIdentType());
}

void ASTDictBuilder::VisitCharacterLiteral(const CharacterLiteral* Node) {
    VisitExpr(Node);
    OS << " " << Node->getValue();
}

void ASTDictBuilder::VisitIntegerLiteral(const IntegerLiteral* Node) {
    VisitExpr(Node);

    bool isSigned = Node->getType()->isSignedIntegerType();
    OS << " " << Node->getValue().toString(10, isSigned);
}

void ASTDictBuilder::VisitFloatingLiteral(const FloatingLiteral* Node) {
    VisitExpr(Node);

    OS << " " << Node->getValueAsApproximateDouble();
}

void ASTDictBuilder::VisitStringLiteral(const StringLiteral* Str) {
    VisitExpr(Str);

    OS << " ";
    Str->outputString(OS);
}

void ASTDictBuilder::VisitInitListExpr(const InitListExpr* ILE) {
    VisitExpr(ILE);
    if (auto* Filler = ILE->getArrayFiller()) {
        dumpChild([=](dict&) {
            OS << "array filler";
            dumpStmt(Filler);
        });
    }
    if (auto* Field = ILE->getInitializedFieldInUnion()) {
        OS << " field ";
        dumpBareDeclRef(Field);
    }
}

void ASTDictBuilder::VisitUnaryOperator(const UnaryOperator* Node) {
    VisitExpr(Node);
    OS << " " << (Node->isPostfix() ? "postfix" : "prefix") << " '"
       << UnaryOperator::getOpcodeStr(Node->getOpcode()) << "'";
}

void ASTDictBuilder::VisitUnaryExprOrTypeTraitExpr(
    const UnaryExprOrTypeTraitExpr* Node) {
    VisitExpr(Node);
    switch (Node->getKind()) {
    case UETT_SizeOf:
        OS << " sizeof";
        break;
    case UETT_AlignOf:
        OS << " alignof";
        break;
    case UETT_VecStep:
        OS << " vec_step";
        break;
    case UETT_OpenMPRequiredSimdAlign:
        OS << " __builtin_omp_required_simd_align";
        break;
    }
    if (Node->isArgumentType())
        dumpType(Node->getArgumentType());
}

void ASTDictBuilder::VisitMemberExpr(const MemberExpr* Node) {
    VisitExpr(Node);
    OS << " " << (Node->isArrow() ? "->" : ".") << *Node->getMemberDecl();
    dumpPointer(Node->getMemberDecl());
}

void ASTDictBuilder::VisitExtVectorElementExpr(
    const ExtVectorElementExpr* Node) {
    VisitExpr(Node);
    OS << " " << Node->getAccessor().getNameStart();
}

void ASTDictBuilder::VisitBinaryOperator(const BinaryOperator* Node) {
    VisitExpr(Node);
    OS << " '" << BinaryOperator::getOpcodeStr(Node->getOpcode()) << "'";
}

void ASTDictBuilder::VisitCompoundAssignOperator(
    const CompoundAssignOperator* Node) {
    VisitExpr(Node);
    OS << " '" << BinaryOperator::getOpcodeStr(Node->getOpcode())
       << "' ComputeLHSTy=";
    dumpBareType(Node->getComputationLHSType());
    OS << " ComputeResultTy=";
    dumpBareType(Node->getComputationResultType());
}

void ASTDictBuilder::VisitBlockExpr(const BlockExpr* Node) {
    VisitExpr(Node);
    dumpDecl(Node->getBlockDecl());
}

void ASTDictBuilder::VisitOpaqueValueExpr(const OpaqueValueExpr* Node) {
    VisitExpr(Node);

    if (Expr* Source = Node->getSourceExpr())
        dumpStmt(Source);
}

// GNU extensions.

void ASTDictBuilder::VisitAddrLabelExpr(const AddrLabelExpr* Node) {
    VisitExpr(Node);
    OS << " " << Node->getLabel()->getName();
    dumpPointer(Node->getLabel());
}

//===----------------------------------------------------------------------===//
// C++ Expressions
//===----------------------------------------------------------------------===//

void ASTDictBuilder::VisitCXXNamedCastExpr(const CXXNamedCastExpr* Node) {
    VisitExpr(Node);
    OS << " " << Node->getCastName() << "<"
       << Node->getTypeAsWritten().getAsString() << ">"
       << " <" << Node->getCastKindName();
    dumpBasePath(OS, Node);
    OS << ">";
}

void ASTDictBuilder::VisitCXXBoolLiteralExpr(const CXXBoolLiteralExpr* Node) {
    VisitExpr(Node);
    OS << " " << (Node->getValue() ? "true" : "false");
}

void ASTDictBuilder::VisitCXXThisExpr(const CXXThisExpr* Node) {
    VisitExpr(Node);
    OS << " this";
}

void ASTDictBuilder::VisitCXXFunctionalCastExpr(
    const CXXFunctionalCastExpr* Node) {
    VisitExpr(Node);
    OS << " functional cast to " << Node->getTypeAsWritten().getAsString()
       << " <" << Node->getCastKindName() << ">";
}

void ASTDictBuilder::VisitCXXConstructExpr(const CXXConstructExpr* Node) {
    VisitExpr(Node);
    CXXConstructorDecl* Ctor = Node->getConstructor();
    dumpType(Ctor->getType());
    if (Node->isElidable())
        OS << " elidable";
    if (Node->requiresZeroInitialization())
        OS << " zeroing";
}

void ASTDictBuilder::VisitCXXBindTemporaryExpr(
    const CXXBindTemporaryExpr* Node) {
    VisitExpr(Node);
    OS << " ";
    dumpCXXTemporary(Node->getTemporary());
}

void ASTDictBuilder::VisitCXXNewExpr(const CXXNewExpr* Node) {
    VisitExpr(Node);
    if (Node->isGlobalNew())
        OS << " global";
    if (Node->isArray())
        OS << " array";
    if (Node->getOperatorNew()) {
        OS << ' ';
        dumpBareDeclRef(Node->getOperatorNew());
    }
    // We could dump the deallocation function used in case of error, but it's
    // usually not that interesting.
}

void ASTDictBuilder::VisitCXXDeleteExpr(const CXXDeleteExpr* Node) {
    VisitExpr(Node);
    if (Node->isGlobalDelete())
        OS << " global";
    if (Node->isArrayForm())
        OS << " array";
    if (Node->getOperatorDelete()) {
        OS << ' ';
        dumpBareDeclRef(Node->getOperatorDelete());
    }
}

void ASTDictBuilder::VisitMaterializeTemporaryExpr(
    const MaterializeTemporaryExpr* Node) {
    VisitExpr(Node);
    if (const ValueDecl* VD = Node->getExtendingDecl()) {
        OS << " extended by ";
        dumpBareDeclRef(VD);
    }
}

void ASTDictBuilder::VisitExprWithCleanups(const ExprWithCleanups* Node) {
    VisitExpr(Node);
    for (unsigned i = 0, e = Node->getNumObjects(); i != e; ++i)
        dumpDeclRef(Node->getObject(i), "cleanup");
}

void ASTDictBuilder::dumpCXXTemporary(const CXXTemporary* Temporary) {
    OS << "(CXXTemporary";
    dumpPointer(Temporary);
    OS << ")";
}

void ASTDictBuilder::VisitSizeOfPackExpr(const SizeOfPackExpr* Node) {
    VisitExpr(Node);
    dumpPointer(Node->getPack());
    dumpName(Node->getPack());
}

//===----------------------------------------------------------------------===//
// Decl method implementations
//===----------------------------------------------------------------------===//

list buildJSON(const Decl* decl, raw_ostream& OS) {
    ASTDictBuilder P(OS, &decl->getASTContext().getCommentCommandTraits(),
                     &decl->getASTContext().getSourceManager());
    P.dumpDecl(decl);
    return P.getAST();
}

list buildJSON(const Decl* decl) { return buildJSON(decl, llvm::errs()); }

list buildLookups(const DeclContext* dctx, raw_ostream& OS) {
    auto DC = dctx;
    while (!DC->isTranslationUnit())
        DC = DC->getParent();
    ASTContext& Ctx = cast<TranslationUnitDecl>(DC)->getASTContext();
    ASTDictBuilder P(OS, &Ctx.getCommentCommandTraits(),
                     &Ctx.getSourceManager());
    P.dumpLookups(dctx, true);
    return P.getAST();
}

list buildLookups(const DeclContext* dctx) {
    return buildLookups(dctx, llvm::errs());
}

//-----------------------------------------------------
// Wrapper
//-----------------------------------------------------

class ASTBuilder : public ASTConsumer, public RecursiveASTVisitor<ASTBuilder> {
    typedef RecursiveASTVisitor<ASTBuilder> base;

public:
    ASTBuilder() {}

    void HandleTranslationUnit(ASTContext& Context) override {
        TranslationUnitDecl* D = Context.getTranslationUnitDecl();
        ast.extend(print(D));
    }

    bool shouldWalkTypesOfTypeLocs() const { return false; }

    bool TraverseDecl(Decl* D) {
        if (D) {
            print(D);
            // Don't traverse child nodes to avoid output duplication.
            return true;
        }
        return base::TraverseDecl(D);
    }

    list ast;

private:
    list print(Decl* D) {
        auto& Out = llvm::outs();
        if (DeclContext* DC = dyn_cast<DeclContext>(D)) {
            if (DC == DC->getPrimaryContext()) {
                return buildLookups(DC, Out);
            }
        }
        return list{};
    }
};

class CPPParser {
public:
    CPPParser(std::string str) {
        // CompilerInstance will hold the instance of the Clang compiler for us,
        // managing the various objects needed to run the compiler.
        CompilerInstance TheCompInst;
        TheCompInst.createDiagnostics();

        LangOptions& lo = TheCompInst.getLangOpts();
        lo.CPlusPlus = 1;

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

        // Set the main file handled by the source manager to the input file.
        auto buffer = llvm::MemoryBuffer::getMemBuffer(str.c_str());
        SourceMgr.setMainFileID(
            SourceMgr.createFileID(std::move(buffer), SrcMgr::C_User));

        TheCompInst.getDiagnosticClient()
            .BeginSourceFile(TheCompInst.getLangOpts(),
                             &TheCompInst.getPreprocessor());

        // Create an AST consumer instance which is going to get called by
        // ParseAST.
        ASTBuilder builder;

        // Parse the file to AST, registering our consumer as the AST consumer.
        ParseAST(TheCompInst.getPreprocessor(), &builder,
                 TheCompInst.getASTContext());

        ast = builder.ast;
    }
    list ast;
};

BOOST_PYTHON_MODULE(cppparser) {
    class_<CPPParser>("CPPParser", init<std::string>())
        .def_readonly("ast", &CPPParser::ast);
}
