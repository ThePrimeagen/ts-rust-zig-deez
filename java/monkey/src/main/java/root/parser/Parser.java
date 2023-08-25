package root.parser;

import root.LocalizedToken;
import root.TokenType;
import root.ast.Program;
import root.ast.expressions.*;
import root.ast.statements.*;
import root.lexer.Lexer;

import java.util.ArrayList;
import java.util.List;

public class Parser {

    private final Lexer lexer;

    private LocalizedToken currentToken;

    private LocalizedToken peekToken;

    private final List<ParserException> errors = new ArrayList<>();

    public Parser(Lexer lexer) {
        this.lexer = lexer;

        proceedToNextToken();
        proceedToNextToken();
    }

    public Program parseProgram() throws ParseProgramException {
        var program = new Program();

        while (!currentTokenIs(TokenType.EOF)) {
            try {
                Statement statement = parseStatement();
                if (statement != null) {
                    program.getStatements().add(statement);
                }
            } catch (ParserException pe) {
                errors.add(pe);
            }
            proceedToNextToken();
        }

        if (!errors.isEmpty()) {
            throw new ParseProgramException(errors);
        }

        return program;
    }

    private void proceedToNextToken() {
        currentToken = peekToken;
        peekToken = lexer.nextLocalized();
    }

    private Statement parseStatement() throws ParserException {
        return switch (currentToken.type()) {
            case LET -> parseLetStatement();
            case RETURN -> parseReturnStatement();
            default -> parseExpressionStatement();
        };
    }

    private ReturnStatement parseReturnStatement() throws ParserException {
        var returnStatement = new ReturnStatement(currentToken);

        proceedToNextToken();

        // A modification I made to support the new Unit type for functions that don't return anything
        if (currentTokenIs(TokenType.SEMI)) {
            returnStatement.setReturnValue(UnitExpression.INSTANCE);
            return returnStatement;
        }

        returnStatement.setReturnValue(parseExpression(OperatorPrecedence.LOWEST));

        if (peekTokenIs(TokenType.SEMI)) {
            proceedToNextToken();
        }

        return returnStatement;
    }

    private LetStatement parseLetStatement() throws ParserException {
        var letStatement = new LetStatement(currentToken);

        expectPeek(TokenType.IDENTIFIER);

        letStatement.setName(new IdentifierExpression(currentToken, currentToken.literal()));

        expectPeek(TokenType.ASSIGN);
        proceedToNextToken();

        letStatement.setValue(parseExpression(OperatorPrecedence.LOWEST));

        if (peekTokenIs(TokenType.SEMI)) {
            proceedToNextToken();
        }

        return letStatement;
    }

    private ExpressionStatement parseExpressionStatement() throws ParserException {
        var statement = new ExpressionStatement(currentToken);

        statement.setExpression(parseExpression(OperatorPrecedence.LOWEST));

        if (peekTokenIs(TokenType.SEMI)) {
            proceedToNextToken();
        }

        return statement;
    }

    private Expression parseExpression(OperatorPrecedence precedence) throws ParserException {
        ParserSupplier<Expression> prefixFn = prefixParseFn();

        if (prefixFn == null) {
            throw new ParserException("Unexpected token found: " + currentToken.literal(), currentToken);
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
            case IDENTIFIER -> () -> new IdentifierExpression(currentToken, currentToken.literal());
            case INT -> () -> new IntegerLiteralExpression(currentToken, Long.parseLong(currentToken.literal()));
            case TRUE, FALSE -> () -> new BooleanLiteralExpression(currentToken, currentTokenIs(TokenType.TRUE));
            case NULL -> () -> new NullLiteralExpression(currentToken);
            case STRING -> () -> new StringLiteralExpression(currentToken);
            case BANG, MINUS -> this::parsePrefixExpression;
            case LPAREN -> this::parseGroupedExpression;
            case IF -> this::parseIfExpression;
            case FUNC -> this::parseFunctionLiteral;
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
            case LPAREN -> this::parseCallExpression;
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

    private CallExpression parseCallExpression(Expression function) throws ParserException {
        var callExpression = new CallExpression(currentToken, function);
        parseCallArguments(callExpression);
        return callExpression;
    }

    private void parseCallArguments(CallExpression callExpression) throws ParserException {
        if (peekTokenIs(TokenType.RPAREN)) {
            proceedToNextToken();
            return;
        }

        proceedToNextToken();
        var argument = parseExpression(OperatorPrecedence.LOWEST);
        callExpression.getArguments().add(argument);

        if (peekTokenIs(TokenType.COMMA)) {
            proceedToNextToken();
            parseCallArguments(callExpression);
        } else {
            expectPeek(TokenType.RPAREN);
        }
    }

    private Expression parseGroupedExpression() throws ParserException {
        proceedToNextToken();

        Expression expression = parseExpression(OperatorPrecedence.LOWEST);

        expectPeek(TokenType.RPAREN);

        return expression;
    }

    private IfExpression parseIfExpression() throws ParserException {
        var ifExpression = new IfExpression(currentToken);

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

    private FunctionLiteralExpression parseFunctionLiteral() throws ParserException {
        var functionLiteral = new FunctionLiteralExpression(currentToken);

        expectPeek(TokenType.LPAREN);

        parseFunctionparameters(functionLiteral);

        expectPeek(TokenType.LSQIRLY);

        functionLiteral.setBody(parseBlockStatement());

        return functionLiteral;
    }

    private void parseFunctionparameters(FunctionLiteralExpression functionLiteral) throws ParserException {
        if (peekTokenIs(TokenType.RPAREN)) {
            proceedToNextToken();
            return;
        }

        proceedToNextToken();
        var identifier = new IdentifierExpression(currentToken, currentToken.literal());
        functionLiteral.getParameters().add(identifier);

        if (peekTokenIs(TokenType.COMMA)) {
            proceedToNextToken();
            parseFunctionparameters(functionLiteral);
        } else {
            expectPeek(TokenType.RPAREN);
        }
    }

    private BlockStatement parseBlockStatement() throws ParserException {
        var blockStatement = new BlockStatement(currentToken);

        proceedToNextToken();

        while (!currentTokenIs(TokenType.RSQIRLY) && !currentTokenIs(TokenType.EOF)) {
            blockStatement.addStatement(parseStatement());
            proceedToNextToken();
        }

        if (!currentTokenIs(TokenType.RSQIRLY)) {
            throw new ParserException("Block statement needs to be closed", currentToken);
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
            throw new ParserException("Expected next token to be %s, got %s".formatted(type.tokenOrName(), peekToken.literal()), currentToken);
        }
    }
}
