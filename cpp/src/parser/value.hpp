#pragma once

#include <string>
#include <variant>
#include <memory>
#include <iosfwd>
#include <vector>

struct AbstractFunctionExpression;

struct Environment;
using EnvironmentP = std::shared_ptr<Environment>;

// helper type for the visitor #4
template<class... Ts>
struct overloaded : Ts... { using Ts::operator()...; };



struct NullValue {
	bool operator==(const NullValue&) const { return true; }
};

using BoundFunction = std::pair<const AbstractFunctionExpression*, EnvironmentP>;
using String = std::string;

struct Value
{
	using variant_type = std::variant<
		NullValue,
		bool,
		int64_t,
		String,
		BoundFunction,
		std::vector<Value>
	>;

	variant_type data;
};

using Array = std::vector<Value>;


inline constexpr bool operator==(const Value& v1, const Value& v2)
{
	return std::visit(overloaded{
		[](const NullValue& v1, const NullValue& v2) { return true; },
		[](const bool v1, const bool v2) { return v1 == v2; },
		[](const int64_t v1, const int64_t v2) { return v1 == v2; },
		[](const String& v1, const String& v2) { return v1 == v2; },
		[](const BoundFunction& v1, const BoundFunction& v2) { return false; },	// TODO: ?
		[](const Array& v1, const Array& v2) {
			return v1.size() == v2.size() && std::equal(v1.begin(), v1.end(), v2.begin());
		},
		[](const auto& v1, const auto& v2) { return false; }
	}, v1.data, v2.data);
}

const Value nil;




std::ostream& operator<<(std::ostream& os, const Value& value);

namespace std
{
	//std::string to_string(const Value&);
	std::string to_string(const Value::variant_type&);
}