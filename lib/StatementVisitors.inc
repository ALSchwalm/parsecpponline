#ifndef STMT_VISITORS
#define STMT_VISITORS

bool VisitStmt(const Stmt* S) {
    if (stmtSkipList.count(S->getStmtClass())) {
        return true;
    }
    debug_print("Visiting statement: ", S->getStmtClassName());
    auto& node = *current_node;
    node["label"] = S->getStmtClassName();
    addLocation(node, S);
    return true;
}

bool VisitDeclStmt(const DeclStmt*) {
    auto& node = *current_node;
    node["label"] = "Declaration";
    node["description"] =
        "Declarations introduce (or re-introduce) names into a C++ program. "
        "Each kind of entity is declared differently.";
    node["link"] = "http://en.cppreference.com/w/cpp/language/declarations";
    return true;
}

bool VisitCompoundStmt(const CompoundStmt*) {
    auto& node = *current_node;
    node["label"] = "Compound Statement";
    node["description"] = "A compound statement or 'block' is a brace-enclosed "
                          "sequence of statements.";
    node["link"] = "http://en.cppreference.com/w/cpp/language/"
                   "statements#Compound_statement";
    return true;
}

bool VisitCXXConstructExpr(const CXXConstructExpr*) {
    auto& node = *current_node;
    node["label"] = "Constructor Call";
    node["description"] =
        "A constructor is a special non-static member function "
        "of a class that is used to initialize objects of "
        "its class type.";
    node["link"] = "http://en.cppreference.com/w/cpp/language/initializer_list";
    return true;
}

#endif
