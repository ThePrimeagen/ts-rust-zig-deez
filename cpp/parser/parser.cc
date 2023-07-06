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
	case token_type::LBracket: return INDEX;
	case token_type::Ampersand: return AND;
	case token_type::Hat: return XOR;
	case token_type::Pipe: return OR;
	default: return LOWEST;
	}
}

void Parser::peekError(token_type t) {
	std::string msg("Expected token type to be ");
	msg += getTokenTypeName(t);
	msg += " but got ";
	msg += getTokenTypeName(peek_token.type);
	msg += " instead.";
	errors.push_back(std::move(msg));
}

void Parser::printErrors() {
	std::cerr << "Encountered errors:\n";
	for (const auto &error : errors)
		std::cerr << '\t' << error << '\n';
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

	if (!expectPeek(token_type::Identifier))
		return nullptr;

	auto let_stmt = std::make_unique<LetStatement>();
	let_stmt->name = parseIdentifier();

	if (!expectPeek(token_type::Assign))
		return nullptr;
	advance_tokens();

	let_stmt->value = std::unique_ptr<Expression>(parseExpression(LOWEST));
	if (peekTokenIs(token_type::Semicolon)) advance_tokens();
	return let_stmt;
}

std::unique_ptr<ReturnStatement> Parser::parseReturn() {
	advance_tokens();
	auto value = parseExpression(LOWEST);
	if (peekTokenIs(token_type::Semicolon))
		advance_tokens();
	return std::make_unique<ReturnStatement>(std::move(value));
}

std::unique_ptr<IntegerLiteral> Parser::parseIntegerLiteral() {
	std::int64_t value;
	auto [ptr, ec] = std::from_chars(current_token.literal.data(), current_token.literal.data() + current_token.literal.size(), value);
	if (ec == std::errc())
		return std::make_unique<IntegerLiteral>(value);
	onError("failed to parse integer literal");
	return nullptr;
}

std::unique_ptr<IntegerLiteral> Parser::parseHexIntegerLiteral() {
	std::int64_t value;
	auto [ptr, ec] = std::from_chars(current_token.literal.data(), current_token.literal.data() + current_token.literal.size(), value, 16);
	if (ec == std::errc())
		return std::make_unique<IntegerLiteral>(value);
	onError("failed to parse integer literal");
	return nullptr;
}

std::unique_ptr<PrefixExpression> Parser::parsePrefixExpression() {
	auto result = std::make_unique<PrefixExpression>();
	result->type = current_token.type;
	advance_tokens();
	result->right = parseExpression(PREFIX);
	return result;
}

std::unique_ptr<InfixExpression> Parser::parseInfixExpression(expr_ptr &&left) {
	auto result = std::make_unique<InfixExpression>();
	result->left = std::move(left);
	result->type = current_token.type;
	Precedence precedence = currentPrecedence();
	advance_tokens();
	result->right = parseExpression(precedence);
	return result;
}

expr_ptr Parser::parseExpression(Precedence precedence)
{
	auto prefix = prefixParseFns.find(current_token.type);
	if (prefix == prefixParseFns.end())
	{
		std::cerr << "No prefix parsing function for " << current_token.literal << "!\n";
		return nullptr;
	}
	expr_ptr lhs(prefix->second());

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
	auto expr = std::make_unique<ExpressionStatement>(parseExpression(LOWEST));
	if (peekTokenIs(token_type::Semicolon))
		advance_tokens();
	return expr;
}

stmt_ptr Parser::parseStatement()
{
	switch (current_token.type)
	{
	case token_type::Let:
		return parseLet();
	case token_type::Return:
		return parseReturn();
	default:
		return parseExpressionStatement();
	}
}

expr_ptr Parser::parseGroupedExpression() {
	advance_tokens();
	expr_ptr result = parseExpression(LOWEST);
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
	
	if (!expectPeek(token_type::LParen))
		return nullptr;
	advance_tokens();

	auto ifexpr = std::make_unique<IfExpression>();
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

std::shared_ptr<std::vector<std::string>> Parser::parseFunctionParameters() {
	std::shared_ptr<std::vector<std::string>> result=std::make_shared<std::vector<std::string>>();

	if (peekTokenIs(token_type::RParen))
	{
		advance_tokens();
		return result;
	}
	advance_tokens();
	
	result->emplace_back(current_token.literal);
	while (peekTokenIs(token_type::Comma)) {
		advance_tokens();
		advance_tokens();
		result->emplace_back(current_token.literal);
	}

	if (!expectPeek(token_type::RParen))
		return nullptr;
	return result;
}

std::unique_ptr<FunctionLiteral> Parser::parseFunctionLiteral() {
	
	if (!expectPeek(token_type::LParen))
		return nullptr;

	auto lit = std::make_unique<FunctionLiteral>();
	lit->parameters = parseFunctionParameters();
	if (!lit->parameters)
		return nullptr;
	if (!expectPeek(token_type::LSquirly))
		return nullptr;
	lit->body=std::shared_ptr<BlockStatement>(parseBlockStatement());
	return lit;
}

std::optional<std::vector<expr_ptr>> Parser::parseExpressionList(token_type end_tok) {
	std::vector<expr_ptr> args;

	if (peekTokenIs(end_tok)) {
		advance_tokens();
		return args;
	}

	advance_tokens();
	args.push_back(std::move(parseExpression(LOWEST)));

	while (peekTokenIs(token_type::Comma)) {
		advance_tokens();
		advance_tokens();
		args.push_back(std::move(parseExpression(LOWEST)));
	}

	if (!expectPeek(end_tok)) {
		return std::nullopt;
	}

	return args;
}

std::unique_ptr<CallExpression> Parser::parseCallExpression(expr_ptr &&left) {
	auto call = std::make_unique<CallExpression>();
	call->function = std::move(left);
	auto args = parseExpressionList(token_type::RParen);
	if (args) call->arguments = std::move(*args);
	else return nullptr;
	return call;
}

Parser::Parser(lexer &l) : lex(l) {
	lex.next_token(current_token);
	lex.next_token(peek_token);
}

std::shared_ptr<Program> Parser::parse() {
	std::shared_ptr<Program> program(new Program);
	while (current_token.type != token_type::Eof) {
		stmt_ptr statement = parseStatement();

		if (statement)
			program->statements.push_back(std::move(statement));
		advance_tokens();
	}
	return program;
}

std::unique_ptr<ArrayLiteral> Parser::parseArrayLiteral() {
	auto literal = std::make_unique<ArrayLiteral>();
	auto args = parseExpressionList(token_type::RBracket);
	if (args) literal->elements = std::move(*args);
	else return nullptr;
	return literal;
}

std::unique_ptr<IndexExpression> Parser::parseIndexExpression(expr_ptr &&left) {
	auto index = std::make_unique<IndexExpression>();
	index->left = std::move(left);
	advance_tokens();
	index->index = parseExpression(LOWEST);
	if (!expectPeek(token_type::RBracket))
		return nullptr;
	return index;
}

std::unique_ptr<HashLiteral> Parser::parseHashLiteral() {
	auto hash = std::make_unique<HashLiteral>();

	while (!peekTokenIs(token_type::RSquirly)) {
		advance_tokens();
		auto key = parseExpression(LOWEST);
		if (!expectPeek(token_type::Colon))
			return nullptr;

		advance_tokens();
		auto value = parseExpression(LOWEST);
		if (!peekTokenIs(token_type::RSquirly) && !expectPeek(token_type::Comma))
			return nullptr;

		hash->elements.emplace_back(std::move(key), std::move(value));
	}

	if (!expectPeek(token_type::RSquirly))
		return nullptr;
	return hash;
}