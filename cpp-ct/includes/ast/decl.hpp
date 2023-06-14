#if !defined(MK_AST_DECL_HPP)
#define MK_AST_DECL_HPP

#include <string>

namespace mk {

    // enum class DeclarationKind {
    //     unknown,
    //     variable,
    //     function,
    // };

    template<typename VarName, typename T>
    struct VariableDecl {
        using var_name = VarName;
        using type = T;
    };

    template<typename VarName, typename ReturnType, typename... args>
    struct FunctionDecl {
        using var_name = VarName;
        using return_type = ReturnType;
        using arguments = std::tuple<args...>;
    };

} // namespace mk


#endif // MK_AST_DECL_HPP
