#pragma once
#include <cstdint>
#include <vector>
#include <string>
#include <unordered_map>
#include <initializer_list>
#include <iterator>

enum OpCode : std::uint8_t {
	OpConstant,
	OpPop,
	OpAdd,
	OpSub,
	OpMul,
	OpDiv,
	OpOr,
	OpXor,
	OpAnd,
	OpTrue,
	OpFalse,
	OpEqual,
	OpNotEqual,
	OpGreaterThan,
	OpMinus,
	OpBang,
	OpTilde,
	OpJumpNotTruthy,
	OpJumpTruthy,
	OpJump,
	OpNull,
	OpGetGlobal,
	OpSetGlobal,
	OpArray,
	OpHash,
	OpIndex,
	OpCall,
	OpReturnValue,
	OpReturn,
	OpSetLocal,
	OpGetLocal,
	OpGetBuiltin,
	OpClosure,
	OpGetFree,
	OpCurrentClosure, 
};

struct OpCodeDescription {
	std::string name;
	std::vector<size_t> operandWidths;
};

const OpCodeDescription &lookupDescription(OpCode op);

struct Instructions
{
	std::vector<std::uint8_t> contents;

	using const_iterator = std::vector<std::uint8_t>::const_iterator;
	using iterator = std::vector<std::uint8_t>::iterator;

	inline const_iterator begin() const { return contents.begin(); }
	inline iterator begin() { return contents.begin(); }
	inline const_iterator end() const { return contents.end(); }
	inline iterator end() { return contents.end(); }
	inline size_t size() const { return contents.size(); }
	inline std::uint8_t operator[](size_t index) const { return contents[index]; }

	size_t make_code(OpCode op, std::initializer_list<std::int64_t> operands);
	size_t make_code(OpCode op);

	Instructions() = default;
	Instructions(OpCode op, std::initializer_list<std::int64_t> operands) {
		make_code(op, operands);
	}
	explicit Instructions(OpCode op)	{
		make_code(op);
	}
	template<typename I>
	Instructions(I begin, I end) {
		for (I i = begin; i != end; ++i)
			std::copy(i->begin(), i->end(), std::inserter(contents, contents.end()));
	}
	Instructions(std::initializer_list<std::uint8_t> bytes) {
		std::copy(bytes.begin(), bytes.end(), std::inserter(contents, contents.end()));
	}

	inline bool operator==(const Instructions &other) const {
		return contents == other.contents;
	}

	inline bool operator!=(const Instructions &other) const {
		return contents != other.contents;
	}

	std::string to_string() const;

	std::int64_t read_int(const_iterator &pos, size_t width) const;

	std::vector<std::int64_t> read_operands(const_iterator &pos, const OpCodeDescription &descr) const;
};
