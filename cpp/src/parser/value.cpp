#include <iostream>
#include <sstream>

#include "value.hpp"
#include "expression.hpp"

std::string std::to_string(const Value& value)
{
	return std::visit(overloaded{

			[](const NullValue& value) -> std::string {
				return "nil";
			},
			[](const bool value) -> std::string {
				return value ? "true" : "false";
			},
			[](const int64_t value) -> std::string {
				return std::to_string(value);
			},
			[](const std::string& value) -> std::string {
				// TODO: escape
				return "\"" + value + "\"";
			},
			[](const BoundFunction& value) -> std::string {
				std::string str = "fn(";
				bool first = true;
				for (const auto& param : value.first->params()) {
					if (first)
						str += ", ";
					first = false;
					str += param;
				}
				return str + ")";
			},
			[](const auto& _) -> std::string {
				return "unknown value";
			}
		},
		value);
}

std::ostream& operator<<(std::ostream& os, const Value& value)
{
	os << std::to_string(value);
	return os;
}
