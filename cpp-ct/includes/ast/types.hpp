#if !defined(MK_AST_TYPES_HPP)
#define MK_AST_TYPES_HPP

namespace mk {
    
    enum class TypeKind {
        unknown,
        illegal,
        int_,
        bool_,
        string,
        fn
    };

    constexpr auto to_string(TypeKind kind) noexcept {
        switch (kind) {
            case TypeKind::unknown: return "unknown";
            case TypeKind::illegal: return "illegal";
            case TypeKind::int_: return "int";
            case TypeKind::bool_: return "bool";
            case TypeKind::string: return "string";
            case TypeKind::fn: return "fn";
        }
        return "unknown";
    }

    template<TypeKind Kind, typename...>
    struct Type;

    template<TypeKind Kind>
    struct Type<Kind> {
        static constexpr auto kind = Kind;
    };
    
    template<typename ReturnType, typename Args>
    struct Type<TypeKind::fn, ReturnType, Args> {
        static constexpr auto kind = TypeKind::fn;
        using params = Args;
        using return_type = ReturnType;
    };

    namespace detail {
        
        template<typename T>
        struct is_type : std::false_type {};

        template<TypeKind Kind, typename... Ts>
        struct is_type<Type<Kind, Ts...>> : std::true_type {};

        template<typename T>
        static constexpr bool is_type_v = is_type<T>::value;

    } // namespace detail
    

} // namespace mk


#endif // MK_AST_TYPES_HPP
