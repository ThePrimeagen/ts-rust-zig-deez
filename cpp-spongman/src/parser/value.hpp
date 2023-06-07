#pragma once

#include <string>
#include <variant>
#include <memory>
#include <iosfwd>
#include <vector>
#include <unordered_map>

struct AbstractFunctionExpression;

struct Environment;
using EnvironmentP = std::shared_ptr<Environment>;


// helper type for std::visit
template<class... Ts>
struct overloaded : Ts... { using Ts::operator()...; };


struct NullValue
{
	bool operator==(const NullValue&) const { return true; }
};

struct Value;
struct ValueHash { size_t operator()(const Value& value) const; };

using String = std::string;
using Integer = int64_t;
using BoundFunction = std::pair<const AbstractFunctionExpression*, EnvironmentP>;
using Array = std::vector<Value>;
using Hash = std::unordered_map<Value, Value, ValueHash>;

using ValueType = std::variant<
	NullValue,
	bool,
	Integer,
	String,
	BoundFunction,
	Array,
	Hash
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

inline size_t ValueHash::operator()(const Value& value) const
{
	return std::visit(overloaded{
		[](const NullValue& v1, const NullValue& v2) { return size_t{0}; },
		[](const bool val) { return std::hash<bool>{}(val); },
		[](const Integer val) { return std::hash<int64_t>{}(val); },
		[](const String& val) { return std::hash<std::string>{}(val); },
		[](const BoundFunction& val) { return size_t{0}; },	// TODO: ?
		[](const Array& val) {
			size_t h = 0;
			for (const auto& value : val)
				h ^= ValueHash{}(value);
			return h;
		},
		[](const Hash& val) { 
			size_t h = 0;
			for (const auto& [key, value] : val)
				h ^= ValueHash{}(key) ^ ValueHash{}(value);
			return h;
		},
		[](const auto& val) { return size_t{0}; }
	}, value.data);
}


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
		[](const Hash& v1, const Hash& v2) {
			return v1.size() == v2.size() && std::equal(v1.begin(), v1.end(), v2.begin());
		},
		[](const auto& v1, const auto& v2) { return false; }
	}, v1.data, v2.data);
}


std::ostream& operator<<(std::ostream& os, const Value& value);
