package root.parser;

import root.Token;
import root.TokenType;
import root.ast.Program;
import root.ast.expressions.*;
import root.ast.statements.*;
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

        proceedToNextToken();
        proceedToNextToken();
    }

    public Program parseProgram() {
        Program program = new Program();

        while (!currentTokenIs(TokenType.EOF)) {
            try {
                Statement statement = parseStatement();
                if (statement != null) {
                    program.getStatements().add(statement);
                }
            } catch (ParserException pe) {
                errors.add(pe.getMessage());
            }
            proceedToNextToken();
        }

        return program;
    }

    private void proceedToNextToken() {
        currentToken = peekToken;
        peekToken = lexer.nextToken();
    }

    private Statement parseStatement() throws ParserException {
        return switch (currentToken.type()) {
            case LET -> parseLetStatement();
            case RETURN -> parseReturnStatement();
            default -> parseExpressionStatement();
        };
    }

    private ReturnStatement parseReturnStatement() {
        ReturnStatement returnStatement = new ReturnStatement(currentToken);

        this.proceedToNextToken();

        // TODO: We're skipping the expressions until we encounter a semicolon or EOF
        while (!currentTokenIs(TokenType.SEMI) && !currentTokenIs(TokenType.EOF)) {
            proceedToNextToken();
        }

        return returnStatement;
    }

    private LetStatement parseLetStatement() throws ParserException {
        LetStatement letStatement = new LetStatement(currentToken);

        expectPeek(TokenType.IDENT);

        letStatement.setName(new IdentifierExpression(currentToken, currentToken.literal()));

        expectPeek(TokenType.ASSIGN);

        // TODO: We're skipping the expressions until we encounter a semicolon or EOF
        while (!currentTokenIs(TokenType.SEMI) && !currentTokenIs(TokenType.EOF)) {
            proceedToNextToken();
        }

        return letStatement;
    }

    private ExpressionStatement parseExpressionStatement() throws ParserException {
        ExpressionStatement statement = new ExpressionStatement(currentToken);

        statement.setExpression(parseExpression(OperatorPrecedence.LOWEST));

        if (peekTokenIs(TokenType.SEMI)) {
            proceedToNextToken();
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
            case TRUE, FALSE -> () -> new BooleanLiteralExpression(currentToken, currentTokenIs(TokenType.TRUE));
            case BANG, MINUS -> this::parsePrefixExpression;
            case LPAREN -> this::parseGroupedExpression;
            case IF -> this::parseIfExpression;
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

    private Expression parseGroupedExpression() throws ParserException {
        proceedToNextToken();

        Expression expression = parseExpression(OperatorPrecedence.LOWEST);

        expectPeek(TokenType.RPAREN);

        return expression;
    }

    private IfExpression parseIfExpression() throws ParserException {
        IfExpression ifExpression = new IfExpression(currentToken);

        expectPeek(TokenType.LPAREN);
        proceedToNextToken();

        ifExpression.setCondition(parseExpression(OperatorPrecedence.LOWEST));

        expectPeek(TokenType.RPAREN);
        expectPeek(TokenType.LSQIRLY);

        ifExpression.setConsequence(parseBlockStatement());

        if (peekTokenIs(TokenType.ELSE)) {
            proceedToNextToken();
            expectPeek(TokenType.LSQIRLY);

            ifExpression.setAlternative(parseBlockStatement());
        }

        return ifExpression;
    }

    private BlockStatement parseBlockStatement() throws ParserException {
        BlockStatement blockStatement = new BlockStatement(currentToken);

        proceedToNextToken();

        while (!currentTokenIs(TokenType.RSQIRLY) && !currentTokenIs(TokenType.EOF)) {
            blockStatement.addStatement(parseStatement());
            proceedToNextToken();
        }

        if (!currentTokenIs(TokenType.RSQIRLY)) {
            throw new ParserException("Block statement needs to be closed");
        }

        return blockStatement;
    }

    private boolean currentTokenIs(TokenType type) {
        return currentToken.type() == type;
    }

    private OperatorPrecedence curPrecedence() {
        return OperatorPrecedence.precedenceForTokenType(currentToken.type());
    }

    private boolean peekTokenIs(TokenType type) {
        return peekToken.type() == type;
    }

    private OperatorPrecedence peekPrecedence() {
        return OperatorPrecedence.precedenceForTokenType(peekToken.type());
    }

    private void expectPeek(TokenType type) throws ParserException {
        if (peekTokenIs(type)) {
            proceedToNextToken();
        } else {
            throw new ParserException("Expected next token to be %s, got %s".formatted(type.name(), peekToken.type().name()));
        }
    }
}
