from abc import ABC, abstractmethod
from dataclasses import dataclass
from monkey.token import Token


class Node(ABC):
    @abstractmethod
    def token_literal(self) -> str:
        pass


class Statement(Node):
    def statement_node(self) -> None:
        pass


class Expression(Node):
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
class Identifier:
    token: Token
    value: str

    def statement_node(self):
        pass

    def token_literal(self) -> str:
        return self.token.literal


@dataclass
class LetStatement:
    token: Token
    name: Identifier = None
    value: Expression = None

    def expression_node(self):
        pass

    def token_literal(self) -> str:
        return self.token.literal
