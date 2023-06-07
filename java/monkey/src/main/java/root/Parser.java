package root;

import root.ast.*;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

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
            parseStatement().ifPresent(program.statements::add);
            nextToken();;
        }

        return program;
    }

    private void peekError(TokenType expected) {
        errors.add("Expected next token to be %s, got %s".formatted(expected.name(), currentToken.type().name()));
    }

    private void nextToken() {
        this.currentToken = this.peekToken;
        this.peekToken = this.lexer.nextToken();
    }

    private Optional<Statement> parseStatement() {
        return switch (this.currentToken.type()) {
            case LET -> this.parseLetStatement();
            case RETURN -> this.parseReturnStatement();
            default -> Optional.empty();
        };
    }

    private Optional<Statement> parseReturnStatement() {
        ReturnStatement returnStatement = new ReturnStatement(this.currentToken);

        this.nextToken();

        // TODO: We're skipping the expressions until we encounter a semicolon
        while (!this.curTokenIs(TokenType.SEMI)) {
            this.nextToken();
        }

        return Optional.of(returnStatement);
    }

    private Optional<Statement> parseLetStatement() {
        LetStatement letStatement = new LetStatement(this.currentToken);

        if (!this.expectPeek(TokenType.IDENT)) {
            return Optional.empty();
        }

        letStatement.name = new Identifier(this.currentToken, this.currentToken.literal());

        if (!this.expectPeek(TokenType.ASSIGN)) {
            return Optional.empty();
        }

        // TODO: We're skipping the expressions until we encounter a semicolon
        while (!this.curTokenIs(TokenType.SEMI)) {
            this.nextToken();
        }

        return Optional.of(letStatement);
    }
    private boolean curTokenIs(TokenType type) {
        return this.currentToken.type() == type;
    }

    private boolean peekTokenIs(TokenType type) {
        return this.peekToken.type() == type;
    }

    private boolean expectPeek(TokenType type) {
        if (this.peekTokenIs(type)) {
            this.nextToken();
            return true;
        }

        this.peekError(type);
        return false;
    }
}
