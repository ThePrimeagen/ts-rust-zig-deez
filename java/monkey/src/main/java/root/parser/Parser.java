package root.parser;

import root.Token;
import root.TokenType;
import root.ast.Program;
import root.ast.expressions.*;
import root.ast.statements.ExpressionStatement;
import root.ast.statements.LetStatement;
import root.ast.statements.ReturnStatement;
import root.ast.statements.Statement;
import root.lexer.Lexer;

import java.util.ArrayList;
import java.util.List;

public class Parser {

    private final Lexer lexer;

    private Token currentToken;

    private Token peekToken;

    public final List<String> errors = new ArrayList<>();

    public Parser(Lexer lexer) {
        this.lexer = lexer;

        this.proceedToNextToken();
        this.proceedToNextToken();
    }

    public Program parseProgram() {
        Program program = new Program();

        while (!curTokenIs(TokenType.EOF)) {
            try {
                Statement statement = parseStatement();
                if (statement != null) {
                    program.getStatements().add(statement);
                }
            } catch (ParserException pe) {
                this.errors.add(pe.getMessage());
            }
            proceedToNextToken();
        }

        return program;
    }

    private void peekError(TokenType expected) throws ParserException {
        throw new ParserException("Expected next token to be %s, got %s".formatted(expected.name(), this.currentToken.type().name()));
    }

    private void proceedToNextToken() {
        this.currentToken = this.peekToken;
        this.peekToken = this.lexer.nextToken();
    }

    private Statement parseStatement() throws ParserException {
        return switch (this.currentToken.type()) {
            case LET -> this.parseLetStatement();
            case RETURN -> this.parseReturnStatement();
            default -> this.parseExpressionStatement();
        };
    }

    private ReturnStatement parseReturnStatement() {
        ReturnStatement returnStatement = new ReturnStatement(this.currentToken);

        this.proceedToNextToken();

        // TODO: We're skipping the expressions until we encounter a semicolon or EOF
        while (!this.curTokenIs(TokenType.SEMI) && !this.curTokenIs(TokenType.EOF)) {
            this.proceedToNextToken();
        }

        return returnStatement;
    }

    private LetStatement parseLetStatement() throws ParserException {
        LetStatement letStatement = new LetStatement(this.currentToken);

        this.expectPeek(TokenType.IDENT);

        letStatement.setName(new IdentifierExpression(this.currentToken, this.currentToken.literal()));

        this.expectPeek(TokenType.ASSIGN);

        // TODO: We're skipping the expressions until we encounter a semicolon or EOF
        while (!this.curTokenIs(TokenType.SEMI) && !this.curTokenIs(TokenType.EOF)) {
            this.proceedToNextToken();
        }

        return letStatement;
    }

    private ExpressionStatement parseExpressionStatement() throws ParserException {
        ExpressionStatement statement = new ExpressionStatement(this.currentToken);

        statement.setExpression(this.parseExpression(OperatorPrecedence.LOWEST));

        if (this.peekTokenIs(TokenType.SEMI)) {
            this.proceedToNextToken();
        }

        return statement;
    }

    private Expression parseExpression(OperatorPrecedence precedence) throws ParserException {
        ParserSupplier<Expression> prefixFn = prefixParseFn();

        if (prefixFn == null) {
            throw new ParserException("No prefix parse function found for " + currentToken.type());
        }
        Expression leftExpression = prefixFn.get();

        while (!peekTokenIs(TokenType.SEMI) && precedence.compareTo(peekPrecedence()) < 0) {
            ParserFunction<Expression, Expression> infixFn = infixParseFn(peekToken.type());

            if (infixFn == null) {
                break;
            }

            proceedToNextToken();

            leftExpression = infixFn.apply(leftExpression);
        }

        return leftExpression;
    }

    private ParserSupplier<Expression> prefixParseFn() {
        return switch (currentToken.type()) {
            case IDENT -> () -> new IdentifierExpression(currentToken, currentToken.literal());
            // NumberFormatException should never be thrown here, since we already know it's an INT
            // token and those contain valid int representations in their values
            case INT -> () -> new IntegerLiteralExpression(currentToken, Long.parseLong(currentToken.literal()));
            case BANG, MINUS -> this::parsePrefixExpression;
            default -> null;
        };
    }

    private PrefixExpression parsePrefixExpression() throws ParserException {
        var expression = new PrefixExpression(currentToken, currentToken.literal());
        proceedToNextToken();
        expression.setRight(parseExpression(OperatorPrecedence.PREFIX));
        return expression;
    }

    private ParserFunction<Expression, Expression> infixParseFn(TokenType tokenType) {
        return switch (tokenType) {
            case PLUS, MINUS, SLASH, ASTERISK, EQUAL, NOT_EQUAL, LT, GT -> this::parseInfixExpression;
            default -> null;
        };
    }

    private InfixExpression parseInfixExpression(Expression left) throws ParserException {
        var expression = new InfixExpression(currentToken, currentToken.literal(), left);

        var precedence = curPrecedence();
        proceedToNextToken();
        expression.setRight(parseExpression(precedence));

        return expression;
    }

    private boolean curTokenIs(TokenType type) {
        return this.currentToken.type() == type;
    }

    private OperatorPrecedence curPrecedence() {
        return OperatorPrecedence.precedenceForTokenType(this.currentToken.type());
    }

    private boolean peekTokenIs(TokenType type) {
        return this.peekToken.type() == type;
    }

    private OperatorPrecedence peekPrecedence() {
        return OperatorPrecedence.precedenceForTokenType(this.peekToken.type());
    }

    private void expectPeek(TokenType type) throws ParserException {
        if (this.peekTokenIs(type)) {
            this.proceedToNextToken();
        } else {
            this.peekError(type);
        }
    }
}
