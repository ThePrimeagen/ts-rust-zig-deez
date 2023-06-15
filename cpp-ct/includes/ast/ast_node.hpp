#if !defined(MK_AST_NODE_HPP)
#define MK_AST_NODE_HPP

namespace mk {
    
    // enum class ASTNodeKind {
    //     unknown,
    //     program,
    //     function,
    //     expression,
    //     statement,
    //     declaration,
    //     identifier,
    //     literal,
    //     type,
    // };

    template<typename... Ts>
    struct ProgramNode {
        static constexpr auto size = sizeof...(Ts);
        using type = std::tuple<Ts...>;
    };
    
} // namespace mk


#endif // MK_AST_NODE_HPP
