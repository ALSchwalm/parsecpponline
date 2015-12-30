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
    node["label"] = "Declaration Statement";
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

bool VisitCXXNewExpr(const CXXNewExpr*) {
    auto& node = *current_node;
    node["label"] = "New Expression";
    node["description"] =
        "New expressions create and initialize objects with dynamic storage "
        "duration, that is, objects whose lifetime is not limited by the scope "
        "in which they were created.";
    node["link"] = "http://en.cppreference.com/w/cpp/language/new";
    return true;
}

bool VisitCXXMemberCallExpr(const CXXMemberCallExpr*) {
    auto& node = *current_node;
    node["label"] = "Member Function Call";
    node["description"] = "A member function call is a function call to a "
                          "non-static member function of a class.";
    node["link"] = "http://en.cppreference.com/w/cpp/language/member_functions";
    return true;
}

bool VisitInitListExpr(const InitListExpr*) {
    auto& node = *current_node;
    node["label"] = "Braced Initializer List";
    node["description"] = "A braced initializer list is a (possibly empty) "
                          "list of expressions or nested lists that is used "
                          "during list initialization.";
    node["link"] =
        "http://en.cppreference.com/w/cpp/language/list_initialization";
    return true;
}

bool VisitCallExpr(const CallExpr*) {
    auto& node = *current_node;
    node["label"] = "Function Call";
    node["description"] = "A call to a function, function pointer, lambda "
                          "expression or object with an 'operator()'.";
    node["link"] = "http://en.cppreference.com/w/cpp/language/functions";
    return true;
}

bool VisitDeclRefExpr(const DeclRefExpr* E) {
    auto& node = *current_node;
    node["label"] = "Declaration Reference";
    node["suffix"] = E->getDecl()->getNameAsString();
    node["description"] = "A declaration reference is a reference to any named "
                          "entity in the program (function, variable, etc)";
    node["link"] = "http://en.cppreference.com/w/cpp/language/declarations";
    return true;
}

bool VisitStringLiteral(const StringLiteral*) {
    auto& node = *current_node;
    node["label"] = "String Literal";
    node["description"] = "A string is a collection of sequential characters "
                          "of various possible encodings.";
    node["link"] = "http://en.cppreference.com/w/cpp/language/string_literal";
    return true;
}

bool VisitIntegerLiteral(const IntegerLiteral*) {
    auto& node = *current_node;
    node["label"] = "Integer Literal";
    node["description"] = "An integer literal is a positive or negative whole "
                          "number expressed in decimal, hexadecimal, octal or "
                          "binary, that can be directly used in expressions.";
    node["link"] = "http://en.cppreference.com/w/cpp/language/integer_literal";
    return true;
}

#endif
