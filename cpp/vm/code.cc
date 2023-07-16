#include "vm/code.hh"
#include <format>

std::unordered_map<OpCode, OpCodeDescription> definitions = {
	{OpConstant, {"OpConstant", {2} } },
	{OpPop, { "OpPop", {} } },
	{OpAdd, { "OpAdd", {} } },
	{OpSub, { "OpSub", {} } },
	{OpMul, { "OpMul", {} } },
	{OpDiv, { "OpDiv", {} } },
	{OpOr, { "OpOr", {} } },
	{OpXor, { "OpXor", {} } },
	{OpAnd, { "OpAnd", {} } },
	{OpTrue, { "OpTrue", {} } },
	{OpFalse, { "OpFalse", {} } },
	{OpEqual, { "OpEqual", {} } },
	{OpNotEqual, { "OpNotEqual", {} } },
	{OpGreaterThan, { "OpGreaterThan", {} } },
	{OpMinus, { "OpMinus", {} } },
	{OpBang, { "OpBang", {} } },
	{OpTilde, { "OpTilde", {} } },
	{OpJumpNotTruthy, { "OpJumpNotTruthy", {2} } },
	{OpJumpTruthy, { "OpJumpTruthy", {2} } },
	{OpJump, { "OpJump", {2} } },
	{OpNull, { "OpNull", {} } },
	{OpGetGlobal, { "OpGetGlobal", {2} } },
	{OpSetGlobal, { "OpSetGlobal", {2} } },
	{OpArray, { "OpArray", {2} } },
	{OpHash, { "OpHash,", {2} } },
	{OpIndex, { "OpIndex", {} }},
	{OpCall, { "OpCall", {1} }},
	{OpReturnValue, { "OpReturnValue", {} }},
	{OpReturn, { "OpReturn", {} }},
	{OpSetLocal, { "OpSetLocal", {1} }},
	{OpGetLocal, { "OpGetLocal", {1} }},
	{OpGetBuiltin, { "OpGetBuiltin", {1} }},
	{OpClosure, { "OpClosure", {2, 1} }},
	{OpGetFree, { "OpGetFree", { 1 } }},
	{OpCurrentClosure, { "OpCurrentClosure", {} }},
	//{, { "", {} }},
};


/*		
	,
	,
*/
const OpCodeDescription &lookupDescription(OpCode op) {
	return definitions[op];
}


size_t Instructions::make_code(OpCode op, std::initializer_list<std::int64_t> operands) {
	auto def = definitions.find(op);

	if (def == definitions.end())
		return ~0; // Unkown op
	if (operands.size() != def->second.operandWidths.size())
		return ~0; // bad number of operadns

	size_t instructionLength = 1;
	for (size_t width : def->second.operandWidths)
		instructionLength += width;
	size_t pos = contents.size();
	contents.reserve(contents.size() + instructionLength);
	contents.push_back(op);


	std::initializer_list<std::int64_t>::const_iterator iter = operands.begin();
	for (size_t i = 0; i < operands.size(); ++i, ++iter) {
		std::int64_t operand = *iter;
		switch (def->second.operandWidths[i])
		{
		case 4: contents.push_back((std::uint8_t)(operand >> 24) & 0xFF); [[fallthrough]];
		case 3: contents.push_back((std::uint8_t)(operand >> 16) & 0xFF); [[fallthrough]];
		case 2: contents.push_back((std::uint8_t)(operand >> 8) & 0xFF); [[fallthrough]];
		case 1: contents.push_back((std::uint8_t)(operand) & 0xFF);
		}
	}
	return pos;
}

size_t Instructions::make_code(OpCode op) {
	auto def = definitions.find(op);

	if (def == definitions.end())
		return ~0; // Unkown op
	if (def->second.operandWidths.size() != 0)
		return ~0; // bad number of operadns
	size_t pos = contents.size();
	contents.push_back(op);
	return pos;
}

std::int64_t Instructions::read_int(const_iterator &pos, size_t width) const {
	std::int64_t result = 0;
	switch (width)
	{
	case 4: result |= ((std::int64_t)(*pos++) << 24); [[fallthrough]];
	case 3: result |= ((std::int64_t)(*pos++) << 16); [[fallthrough]];
	case 2: result |= ((std::int64_t)(*pos++) << 8); [[fallthrough]];
	case 1: result |= (std::int64_t)(*pos);
	}
	return result;
}

std::vector<std::int64_t> Instructions::read_operands(const_iterator &pos, const OpCodeDescription &descr) const {
	std::vector<std::int64_t> result;
	result.reserve(descr.operandWidths.size());

	for (size_t size : descr.operandWidths) {
		result.push_back(read_int(pos, size));
		pos++;
	}
	return result;
}

std::string Instructions::to_string() const {
	std::string result;
	const_iterator pos = contents.begin();
	std::vector<std::int64_t> operands;
	while (pos != contents.end()) {
		ptrdiff_t output_pos = std::distance(contents.begin(), pos);
		auto def = definitions.find((OpCode)*(pos++));

		result += std::format("{:04} {}", output_pos, def->second.name);
		operands = read_operands(pos, def->second);

		for (std::int64_t operand : operands)
			result += " " + std::to_string(operand);
		result += '\n';
	}

	return result;
}
