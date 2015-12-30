#ifndef DECL_VISITORS
#define DECL_VISITORS

bool VisitDecl(const Decl* D) {
    debug_print("Visiting decl: ", D->getDeclKindName());
    auto& node = *current_node;
    node["label"] = D->getDeclKindName() + std::string("Decl");
    addLocation(node, D);
    return true;
}

bool VisitNamedDecl(const NamedDecl* D) {
    auto& node = *current_node;
    node["suffix"] = D->getNameAsString();
    return true;
}

bool VisitFunctionDecl(const FunctionDecl* F) {
    auto& node = *current_node;
    if (F->hasBody()) {
        if (F->isMain()) {
            node["label"] = "Main Definition";
            node["link"] =
                "http://en.cppreference.com/w/cpp/language/main_function";
            node["description"] =
                "The main function is called at program startup after "
                "initialization of the non-local objects with static storage "
                "duration. It is the designated entry point to a "
                "program.";
            return true;
        } else {
            node["label"] = "Function Definition";
        }
    } else {
        node["label"] = "Function Declaration";
    }

    node["link"] = "http://en.cppreference.com/w/cpp/language/function";
    node["description"] = "A function declaration introduces the function name "
                          "and its type. A function definition associates the "
                          "function name/type with the function body.";
    return true;
}

bool VisitCXXConstructorDecl(const CXXConstructorDecl* F) {
    auto& node = *current_node;
    if (F->hasBody()) {
        node["label"] = "Constructor Definition";
    } else {
        node["label"] = "Constructor Declaration";
    }
    node["description"] =
        "A constructor is a special non-static member function "
        "of a class that is used to initialize objects of "
        "its class type.";
    node["link"] = "http://en.cppreference.com/w/cpp/language/initializer_list";
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

bool VisitCXXRecordDecl(const CXXRecordDecl* D) {
    auto& node = *current_node;
    if (D->hasDefinition()) {
        node["label"] = "Type Definition";
    } else {
        node["label"] = "Type Declaration";
    }
    list links;
    links.append("http://en.cppreference.com/w/cpp/language/class");
    links.append("http://en.cppreference.com/w/cpp/language/union");
    node["link"] = links;
    node["description"] = "A type declaration introduces a struct, class or "
                          "union type. A type definition associates a type "
                          "with its body, making it a complete type.";
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

bool VisitValueDecl(const ValueDecl* D) {
    auto& node = *current_node;
    node["type"] = D->getType().getAsString();
    return true;
}

#endif
