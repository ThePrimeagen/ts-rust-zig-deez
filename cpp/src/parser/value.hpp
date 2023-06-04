#pragma once

#include <string>
#include <variant>
#include <memory>
#include <iosfwd>

struct AbstractFunctionExpression;

struct Environment;
using EnvironmentP = std::shared_ptr<Environment>;

struct NullValue {
	bool operator==(const NullValue&) const { return true; }
};

using BoundFunction = std::pair<const AbstractFunctionExpression*, EnvironmentP>;

using Value = std::variant<
	NullValue,
	bool,
	int64_t,
	std::string,
	BoundFunction
>;

const Value nil{NullValue{}};


// helper type for the visitor #4
template<class... Ts>
struct overloaded : Ts... { using Ts::operator()...; };



std::ostream& operator<<(std::ostream& os, const Value& value);

namespace std
{
	std::string to_string(const Value&);
}