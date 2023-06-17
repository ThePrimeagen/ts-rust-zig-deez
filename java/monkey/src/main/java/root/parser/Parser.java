package root.parser;

import root.Token;
import root.TokenType;
import root.ast.*;
import root.ast.expressions.Expression;
import root.ast.expressions.IdentifierExpression;
import root.ast.expressions.IntegerLiteralExpression;
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

        this.nextToken();
        this.nextToken();
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
            nextToken();
        }

        return program;
    }

    private void peekError(TokenType expected) throws ParserException {
        throw new ParserException("Expected next token to be %s, got %s".formatted(expected.name(), this.currentToken.type().name()));
    }

    private void nextToken() {
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

    private Statement parseReturnStatement() {
        ReturnStatement returnStatement = new ReturnStatement(this.currentToken);

        this.nextToken();

        // TODO: We're skipping the expressions until we encounter a semicolon or EOF
        while (!this.curTokenIs(TokenType.SEMI) && !this.curTokenIs(TokenType.EOF)) {
            this.nextToken();
        }

        return returnStatement;
    }

    private Statement parseLetStatement() throws ParserException {
        LetStatement letStatement = new LetStatement(this.currentToken);

        this.expectPeek(TokenType.IDENT);

        letStatement.setName(new IdentifierExpression(this.currentToken, this.currentToken.literal()));

        this.expectPeek(TokenType.ASSIGN);

        // TODO: We're skipping the expressions until we encounter a semicolon or EOF
        while (!this.curTokenIs(TokenType.SEMI) && !this.curTokenIs(TokenType.EOF)) {
            this.nextToken();
        }

        return letStatement;
    }

    private Statement parseExpressionStatement() throws ParserException {
        ExpressionStatement statement = new ExpressionStatement(this.currentToken);

        statement.setExpression(this.parseExpression(OperatorPrecedence.LOWEST));

        if (this.peekTokenIs(TokenType.SEMI)) {
            this.nextToken();
        }

        return statement;
    }

    private Expression parseExpression(OperatorPrecedence precedence) throws ParserException {
        Expression prefix = this.prefixParse();

        return prefix;
    }

    private Expression prefixParse() throws ParserException {
        return switch (currentToken.type()) {
            case IDENT -> new IdentifierExpression(currentToken, currentToken.literal());
            // NumberFormatException should never be thrown here, since we already know it's an INT
            // token and those contain valid int representations in their values
            case INT -> new IntegerLiteralExpression(currentToken, Long.parseLong(currentToken.literal()));
            default -> null;
        };
    }

    private Expression infixParse(Expression expression) {
        return null;
    }

    private boolean curTokenIs(TokenType type) {
        return this.currentToken.type() == type;
    }

    private boolean peekTokenIs(TokenType type) {
        return this.peekToken.type() == type;
    }

    private void expectPeek(TokenType type) throws ParserException {
        if (this.peekTokenIs(type)) {
            this.nextToken();
        } else {
            this.peekError(type);
        }
    }
}
