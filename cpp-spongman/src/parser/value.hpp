#pragma once

#include <string>
#include <variant>
#include <memory>
#include <iosfwd>
#include <vector>

struct AbstractFunctionExpression;

struct Environment;
using EnvironmentP = std::shared_ptr<Environment>;


// helper type for std::visit
template<class... Ts>
struct overloaded : Ts... { using Ts::operator()...; };


struct NullValue {
	bool operator==(const NullValue&) const { return true; }
};

using String = std::string;
using Integer = int64_t;
using BoundFunction = std::pair<const AbstractFunctionExpression*, EnvironmentP>;

struct Value;
using ValueType = std::variant<
	NullValue,
	bool,
	Integer,
	String,
	BoundFunction,
	std::vector<Value>
>;

namespace std
{
	std::string to_string(const ValueType&);
}

struct Value
{
	ValueType data;

	template<typename T>
	const T& get() const {
		if (!std::holds_alternative<T>(data))
			throw std::runtime_error("Error trying to convert " + std::to_string(data) + " to " + Value{T{}}.typeName());
		return std::get<T>(data);
	}

	std::string typeName() const;
};
const Value nil;

using Array = std::vector<Value>;	// recursively defined


inline constexpr bool operator==(const Value& v1, const Value& v2)
{
	return std::visit(overloaded{
		[](const NullValue& v1, const NullValue& v2) { return true; },
		[](const bool v1, const bool v2) { return v1 == v2; },
		[](const Integer v1, const Integer v2) { return v1 == v2; },
		[](const String& v1, const String& v2) { return v1 == v2; },
		[](const BoundFunction& v1, const BoundFunction& v2) { return false; },	// TODO: ?
		[](const Array& v1, const Array& v2) {
			return v1.size() == v2.size() && std::equal(v1.begin(), v1.end(), v2.begin());
		},
		[](const auto& v1, const auto& v2) { return false; }
	}, v1.data, v2.data);
}



std::ostream& operator<<(std::ostream& os, const Value& value);
