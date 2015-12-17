#ifndef DECL_VISITORS
#define DECL_VISITORS

bool VisitDecl(const Decl* D) {
    debug_print("Visiting decl: ", D->getDeclKindName());
    auto& node = *current_node;
    node["label"] = D->getDeclKindName() + std::string("Decl");
    addLocation(node, D);
    return true;
}

bool VisitFunctionDecl(const FunctionDecl*) {
    auto& node = *current_node;
    node["label"] = "Function Declaration";
    node["link"] = "http://en.cppreference.com/w/cpp/language/function";
    node["description"] = "A function declaration introduces the function name "
                          "and its type. A function definition associates the "
                          "function name/type with the function body.";
    return true;
}

bool VisitVarDecl(const VarDecl*) {
    auto& node = *current_node;
    node["label"] = "Variable Declaration";
    node["link"] = "http://en.cppreference.com/w/cpp/language/declarations";
    node["description"] = "A variable declaration introduces the variable name "
                          "and its type.";
    return true;
}

bool VisitCXXRecordDecl(const CXXRecordDecl*) {
    auto& node = *current_node;
    node["label"] = "Type Declaration";
    list links;
    links.append("http://en.cppreference.com/w/cpp/language/class");
    links.append("http://en.cppreference.com/w/cpp/language/union");
    node["link"] = links;
    node["description"] =
        "A type declaration introduces a struct, class or union type.";
    return true;
}

bool VisitFieldDecl(const FieldDecl*) {
    auto& node = *current_node;
    node["label"] = "Member Variable Declaration";
    node["description"] = "A non-static member variable declaration declares a "
                          "variable that will be stored within any object of "
                          "the enclosing class type.";
    node["link"] =
        "http://en.cppreference.com/w/cpp/language/class#Member_specification";
    return true;
}

#endif
