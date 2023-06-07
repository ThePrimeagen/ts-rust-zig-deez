#include <iostream>
#include <sstream>

#include "value.hpp"
#include "expression.hpp"

std::string std::to_string(const ValueType& data)
{
	return std::visit(overloaded{
		[](const NullValue& value) -> std::string {
			return "nil";
		},
		[](const bool value) -> std::string {
			return value ? "true" : "false";
		},
		[](const Integer value) -> std::string {
			return std::to_string(value);
		},
		[](const String& value) -> std::string {
			// TODO: escape
			return "\"" + value + "\"";
		},
		[](const BoundFunction& value) -> std::string {
			std::string str = "fn(";
			bool first = true;
			for (const auto& param : value.first->params()) {
				if (!first)
					str += ",";
				first = false;
				str += param;
			}
			return str + ")";
		},
		[](const Array& value) -> std::string {
			std::string str = "[";
			bool first = true;
			for (const auto& element : value) {
				if (!first)
					str += ",";
				first = false;
				str += std::to_string(element.data);
			}
			return str + "]";
		},
		[](const Hash& hash) -> std::string {
			std::string str = "{";
			bool first = true;
			for (const auto& [key, value] : hash) {
				if (!first)
					str += ",";
				first = false;
				str += std::to_string(key.data);
				str += ":";
				str += std::to_string(value.data);
			}
			return str + "}";
		},
		[](const auto& _) -> std::string {
			return "unknown value";
		}
	},
	data);
}

std::ostream& operator<<(std::ostream& os, const Value& value)
{
	os << std::to_string(value.data);
	return os;
}

std::string Value::typeName() const
{
	return std::visit(overloaded{
		[](const NullValue& value) { return "nil"; },
		[](const bool value)       { return "bool"; },
		[](const Integer value)    { return "Integer"; },
		[](const String& value)    { return "String"; },
		[](const BoundFunction& value) { return "fn"; },
		[](const Array& value)     { return "Array"; },
		[](const auto& _)          { return "unknown"; }
	},
	data);
}
