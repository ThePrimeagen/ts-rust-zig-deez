%line 1 "ast.asm"

TREE_DATA_SIZE equ 24
TREE_DATA_OFFS equ 16

;; AST node type. This is just a tagged union.
struc tree
    .code resq 1
    .loc  resq 1
    .data resb TREE_DATA_SIZE
endstruc

struc stmt_program, TREE_DATA_OFFS
    .stmts resb vector_size
endstruc

struc stmt_lex, TREE_DATA_OFFS
    .name resq 1
    .expr resq 1
endstruc

static_assert stmt_program_size <= TREE_DATA_SIZE
static_assert stmt_lex_size <= TREE_DATA_SIZE


