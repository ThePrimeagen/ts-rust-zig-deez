from lexer.my_token import Token
from dataclasses import dataclass


class Node:
    Token: Token

    def TokenLiteral(self) -> str:
        return self.Token.Literal


class Statement(Node):
    def statementNode(self) -> None:
        ...


class Expression(Node):
    def expressionNode(self) -> None:
        ...


@ dataclass
class Program:
    Statements: list[Statement]

    def TokenLiteral(self) -> str:
        if len(self.Statements) > 0:
            return self.Statements[0].TokenLiteral()
        return ""

    def __str__(self) -> str:
        return "".join(map(str, self.Statements))


@ dataclass
class Identifier(Expression):
    Token: Token
    Value: str

    def expressionNode(self):
        pass

    def TokenLiteral(self) -> str:
        return self.Token.Literal

    def __str__(self):
        return self.Value


@ dataclass
class LetStatement(Statement):
    Token: Token
    Name: Identifier
    Value: Expression

    def statementNode(self):
        pass

    def TokenLiteral(self) -> str:
        return self.Token.Literal

    def __str__(self) -> str:
        out = [f"{self.TokenLiteral()} {str(self.Name)} = "]
        if self.Value != None:
            out.append(str(self.Value))
        out.append(';')
        return "".join(out)


@ dataclass
class ReturnStatement(Statement):
    Token: Token
    ReturnValue: Expression

    def statementNode(self):
        pass

    def TokenLiteral(self) -> str:
        return self.Token.Literal

    def __str__(self) -> str:
        out = [f"{self.TokenLiteral()} "]
        if self.ReturnValue != None:
            out.append(str(self.ReturnValue))
        out.append(';')
        return "".join(out)


@ dataclass
class ExpressionStatement(Statement):
    Token: Token
    Expression: Expression

    def statementNode(self):
        pass

    def TokenLiteral(self) -> str:
        return self.Token.Literal

    def __str__(self) -> str:
        if self.Expression != None:
            return str(self.Expression)
        return ""
