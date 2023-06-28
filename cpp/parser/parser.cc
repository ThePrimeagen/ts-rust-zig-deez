#include "parser.hh"

#include <iostream>

Parser::Precedence Parser::precedenceFromToken(token_type tok) {
	switch (tok)
	{
	case token_type::Equal: return EQUALS;
	case token_type::NotEqual: return EQUALS;
	case token_type::LessThan: return LESSGREATER;
	case token_type::GreaterThan: return LESSGREATER;
	case token_type::Plus: return SUM;
	case token_type::Dash: return SUM;
	case token_type::ForwardSlash: return PRODUCT;
	case token_type::Asterisk: return PRODUCT;
	case token_type::LParen: return CALL;
	default: return LOWEST;
	}
}

void Parser::peekError(token_type t) {
	std::string msg("Expected token type to be ");
	msg += getTokenTypeName(t);
	msg += " but got ";
	msg += getTokenTypeName(peek_token.type);
	msg += " instead.";
	errors.emplace_back(std::move(msg));
}

void Parser::checkParserErrors() {
	if (errors.empty()) return;
	std::cerr << "Parser has " << errors.size() << " errors:\n";
	for (const std::string &msg : errors)
		std::cerr << msg << '\n';
	exit(1);
}

std::unique_ptr<Identifier> Parser::parseIdentifier() {
	return std::make_unique<Identifier>(current_token.literal);
}

std::unique_ptr<LetStatement> Parser::parseLet() {
	std::unique_ptr<LetStatement> let_stmt(new LetStatement);

	if (!expectPeek(token_type::Identifier))
		return nullptr;

	let_stmt->name = parseIdentifier();

	if (!expectPeek(token_type::Assign))
		return nullptr;

	advance_tokens();


	let_stmt->value = parseExpression(LOWEST);
	if (peekTokenIs(token_type::Semicolon)) advance_tokens();
	return let_stmt;
}

std::unique_ptr<ReturnStatement> Parser::parseReturn() {
	std::unique_ptr<ReturnStatement> ret_stmt(new ReturnStatement);

	advance_tokens();

	ret_stmt->value = parseExpression(LOWEST);

	if (peekTokenIs(token_type::Semicolon)) advance_tokens();
	return ret_stmt;
}

std::unique_ptr<IntegerLiteral> Parser::parseIntegerLiteral() {

	std::int64_t value;
	auto [ptr, ec] = std::from_chars(current_token.literal.data(), current_token.literal.data() + current_token.literal.size(), value);

	if (ec == std::errc())
		return std::make_unique<IntegerLiteral>(value);
	onError("failed to parse integer literal");
	return nullptr;
}

std::unique_ptr<PrefixExpression> Parser::parsePrefixExpression() {
	auto result = std::unique_ptr<PrefixExpression>(new PrefixExpression);
	result->type = current_token.type;
	advance_tokens();
	result->right = parseExpression(PREFIX);
	return result;
}

std::unique_ptr<InfixExpression> Parser::pareseInfixExpression(expr_ptr &&left) {
	auto result = std::unique_ptr<InfixExpression>(new InfixExpression);
	result->left = std::move(left);
	result->type = current_token.type;
	Precedence precedence = currentPrecedence();
	advance_tokens();
	result->right = parseExpression(precedence);
	return result;
}

std::unique_ptr<Expression> Parser::parseExpression(Precedence precedence)
{
	auto prefix = prefixParseFns.find(current_token.type);
	if (prefix == prefixParseFns.end())
	{
		std::cerr << "No prefix parsing function for " << current_token.literal << "!\n";
		return nullptr;
	}

	std::unique_ptr<Expression> lhs = prefix->second();

	while (!peekTokenIs(token_type::Semicolon) && precedence < peekPrecedence()) {
		auto infix = infixParseFns.find(peek_token.type);
		if (infix == infixParseFns.end()) {
			return lhs;
		}
		advance_tokens();

		lhs = infix->second(std::move(lhs));
	}

	return lhs;
}

std::unique_ptr<ExpressionStatement> Parser::parseExpressionStatement()
{
	std::unique_ptr<Expression> expr = parseExpression(LOWEST);

	if (peekTokenIs(token_type::Semicolon))
		advance_tokens();

	return std::make_unique<ExpressionStatement>(std::move(expr));
}

std::unique_ptr<Statement> Parser::parseStatement()
{
	switch (current_token.type)
	{
		using enum token_type;
	case Let:
		return parseLet();
	case Return:
		return parseReturn();
		/*case If:
				statement = parseIf();
				break;
			*/
	default:
		return parseExpressionStatement();
	}
}

std::unique_ptr<Expression> Parser::parseGroupedExpression() {
	advance_tokens();
	std::unique_ptr<Expression> result = parseExpression(LOWEST);

	if (!expectPeek(token_type::RParen))
		return nullptr;
	return result;
}

std::unique_ptr<BlockStatement> Parser::parseBlockStatement() {
	auto block = std::make_unique<BlockStatement>();
	advance_tokens();

	while (!curTokenIs(token_type::RSquirly) && !curTokenIs(token_type::Eof)) {
		auto stmt = parseStatement();
		if (stmt)
			block->statements.push_back(std::move(stmt));
		advance_tokens();
	}

	return block;
}

std::unique_ptr<IfExpression> Parser::parseIfExpression() {
	auto ifexpr = std::make_unique<IfExpression>();

	if (!expectPeek(token_type::LParen))
		return nullptr;

	advance_tokens();
	ifexpr->condition = parseExpression(LOWEST);

	if (!expectPeek(token_type::RParen))
		return nullptr;

	if (!expectPeek(token_type::LSquirly))
		return nullptr;

	ifexpr->consequence = parseBlockStatement();

	if (peekTokenIs(token_type::Else)) {
		advance_tokens();
		if (!expectPeek(token_type::LSquirly))
			return nullptr;

		ifexpr->alternative = parseBlockStatement();


	}
	return ifexpr;
}

std::optional<std::vector<std::unique_ptr<Identifier>>> Parser::parseFunctionParameters() {
	std::vector<std::unique_ptr<Identifier>> result;

	if (peekTokenIs(token_type::RParen))
	{
		advance_tokens();
		return result;
	}

	advance_tokens();

	result.push_back(std::make_unique<Identifier>(current_token.literal));

	while (peekTokenIs(token_type::Comma)) {
		advance_tokens();
		advance_tokens();
		result.push_back(std::make_unique<Identifier>(current_token.literal));
	}

	if (!expectPeek(token_type::RParen))
	{
		return std::nullopt;
	}

	return result;
}

std::unique_ptr<FunctionLiteral> Parser::parseFunctionLiteral() {
	auto lit = std::make_unique<FunctionLiteral>();

	if (!expectPeek(token_type::LParen))
		return nullptr;

	auto params = parseFunctionParameters();
	if (params) lit->parameters = std::move(*params);
	else return nullptr;

	if (!expectPeek(token_type::LSquirly))
		return nullptr;

	lit->body = parseBlockStatement();

	return lit;
}

std::optional<std::vector<expr_ptr>> Parser::parseCallArguments() {
	std::vector<expr_ptr> args;

	if (peekTokenIs(token_type::RParen)) {
		advance_tokens();
		return args;
	}

	advance_tokens();
	args.push_back(parseExpression(LOWEST));

	while (peekTokenIs(token_type::Comma)) {
		advance_tokens();
		advance_tokens();
		args.push_back(parseExpression(LOWEST));
	}

	if (!expectPeek(token_type::RParen)) {
		return std::nullopt;
	}

	return args;
}

std::unique_ptr<CallExpression> Parser::parseCallExpression(expr_ptr left) {
	std::unique_ptr < CallExpression > call = std::make_unique<CallExpression>();
	call->function = std::move(left);
	auto args = parseCallArguments();
	if (args) call->arguments = std::move(*args);
	else return nullptr;
	return call;
}

Parser::Parser(lexer &l) : lex(l)
{
	lex.next_token(current_token);
	lex.next_token(peek_token);
}

std::unique_ptr<Program> Parser::parse()
{
	std::unique_ptr<Program> program(new Program);
	while (current_token.type != token_type::Eof) {
		stmt_ptr statement = parseStatement();

		if (statement)
			program->statements.emplace_back(std::move(statement));
		advance_tokens();
	}
	return program;
}

void Parser::printErrors() {
	std::cerr << "Encountered errors:\n";
	for (const auto &error : errors)
		std::cerr << '\t' << error << '\n';
}
