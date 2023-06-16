#if !defined(MK_AST_DECL_HPP)
#define MK_AST_DECL_HPP

#include <string>

namespace mk {

    // enum class DeclarationKind {
    //     unknown,
    //     variable,
    //     function,
    // };

    template<typename VarName, typename ReturnType, typename Args, typename Body>
    struct FunctionDecl {
        using id = VarName;
        using return_type = ReturnType;
        using params = Args;
        using body = Body;
    };
    
    template<typename ReturnType, typename Args, typename Body>
    struct AnonFunctionDecl {
        using return_type = ReturnType;
        using params = Args;
        using body = Body;
    };

    template<typename T, typename Id>
    struct ArgType {
        using type = T;
        using id = Id;
    };

    namespace detail {
        
        template<typename T>
        struct is_function_decl : std::false_type {};

        template<typename VarName, typename ReturnType, typename Args, typename Body>
        struct is_function_decl<FunctionDecl<VarName, ReturnType, Args, Body>> : std::true_type {};

        template<typename ReturnType, typename Args, typename Body>
        struct is_function_decl<AnonFunctionDecl<ReturnType, Args, Body>> : std::true_type {};

        template<typename T>
        static constexpr bool is_function_decl_v = is_function_decl<T>::value;

        template<typename T>
        struct is_anon_function_decl : std::false_type {};

        template<typename ReturnType, typename Args, typename Body>
        struct is_anon_function_decl<AnonFunctionDecl<ReturnType, Args, Body>> : std::true_type {};

        template<typename T>
        static constexpr bool is_anon_function_decl_v = is_anon_function_decl<T>::value;

        template<typename T>
        struct is_arg_type : std::false_type {};

        template<typename T, typename Id>
        struct is_arg_type<ArgType<T, Id>> : std::true_type {};

        template<typename T>
        static constexpr bool is_arg_type_v = is_arg_type<T>::value;

    } // namespace detail
    

} // namespace mk


#endif // MK_AST_DECL_HPP
