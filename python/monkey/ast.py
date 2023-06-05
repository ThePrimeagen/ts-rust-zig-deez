from abc import ABC, abstractmethod
from dataclasses import dataclass
from monkey.token import Token


@dataclass
class Node(ABC):
    token: Token

    def token_literal(self) -> str:
        return self.token.literal


@dataclass
class Statement(Node):
    @abstractmethod
    def statement_node(self) -> None:
        pass


@dataclass
class Expression(Node):
    @abstractmethod
    def expression_node(self) -> None:
        pass


class Program:
    def __init__(self) -> None:
        self.statements: list[Statement] = []

    def token_literal(self) -> str:
        if self.statements:
            return self.statements[0].token_literal()
        return ""


@dataclass
class Identifier(Expression):
    value: str

    def expression_node(self) -> None:
        pass


@dataclass
class IntegerLiteral(Expression):
    value: int = 0

    def expression_node(self) -> None:
        pass


@dataclass
class Boolean(Expression):
    value: bool

    def expression_node(self) -> None:
        pass


@dataclass
class BlockStatement(Expression):
    statements: list[Statement]

    def expression_node(self) -> None:
        pass


@dataclass
class IfExpression(Expression):
    condition: Expression = None
    consequence: BlockStatement = None
    Alternative: BlockStatement = None

    def expression_node(self) -> None:
        pass


@dataclass
class FunctionLiteral(Expression):
    parameters: list[Identifier]
    body: BlockStatement = None

    def expression_node(self) -> None:
        pass


@dataclass
class CallExpression(Expression):
    function: Expression
    arguments: list[Expression]

    def expression_node(self) -> None:
        pass


@dataclass
class PrefixExpression(Expression):
    operator: str
    right: Expression = None

    def expression_node(self) -> None:
        pass


@dataclass
class InfixExpression(Expression):
    left: Expression = None
    operator: str = ""
    right: Expression = None

    def expression_node(self) -> None:
        pass


@dataclass
class LetStatement(Statement):
    name: Identifier = None
    value: Expression = None

    def statement_node(self):
        pass


@dataclass
class ReturnStatement(Statement):
    return_value: Expression = None

    def statement_node(self) -> None:
        pass


@dataclass
class ExpressionStatement(Statement):
    expression: Expression = None

    def statement_node(self) -> None:
        pass
