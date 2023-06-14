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

    template<TypeKind Kind>
    struct Type {
        static constexpr auto kind = Kind;
    };

} // namespace mk


#endif // MK_AST_TYPES_HPP
