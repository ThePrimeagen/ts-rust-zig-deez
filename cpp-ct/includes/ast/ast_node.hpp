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

    template<typename T>
    struct ProgramNode {
        using type = T;
    };
    
} // namespace mk


#endif // MK_AST_NODE_HPP
