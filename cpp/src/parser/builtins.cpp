#include "builtins.hpp"

#include <unordered_map>


unordered_string_map<BuiltinFunctionExpression> builtins{

	{ "len", {
		"len", {"val"},
		[](const std::vector<Value>& arguments) {
			if (arguments.size() != 1)
				throw std::runtime_error("wrong number of arguments to len(): " + std::to_string(arguments.size()));

			auto value = arguments[0];
			return Value{std::visit(overloaded{
				[](const String& str) { return static_cast<int64_t>(str.length()); },
				[](const std::vector<Value>& array) { return static_cast<int64_t>(array.size()); },
				[](const auto& value) -> int64_t {
					throw std::runtime_error("invalid argument to len(): " + std::to_string(value));
				}
			}, value.data)};
		}
	}},

	{ "first", {
		"first", {"arr"},
		[](const std::vector<Value>& arguments) {
			if (arguments.size() != 1)
				throw std::runtime_error("wrong number of arguments to first(): " + std::to_string(arguments.size()));

			auto value = arguments[0];
			return std::visit(overloaded{
				[](const std::vector<Value>& array) { return array.empty() ? nil : array.front(); },
				[](const auto& value) -> Value {
					throw std::runtime_error("invalid argument to first(): " + std::to_string(value));
				}
			}, value.data);
		}
	}},

	{ "last", {
		"last", {"arr"},
		[](const std::vector<Value>& arguments) {
			if (arguments.size() != 1)
				throw std::runtime_error("wrong number of arguments to last(): " + std::to_string(arguments.size()));

			auto value = arguments[0];
			return std::visit(overloaded{
				[](const std::vector<Value>& array) { return array.empty() ? nil : array.back(); },
				[](const auto& value) -> Value {
					throw std::runtime_error("invalid argument to last(): " + std::to_string(value));
				}
			}, value.data);
		}
	}},

	{ "rest", {
		"rest", {"arr"},
		[](const std::vector<Value>& arguments) {
			if (arguments.size() != 1)
				throw std::runtime_error("wrong number of arguments to last(): " + std::to_string(arguments.size()));

			auto value = arguments[0];
			return std::visit(overloaded{
				[](const std::vector<Value>& array) {
					if (array.empty())
						return nil;
					Array rest;
					std::copy(++array.cbegin(), array.cend(), std::back_inserter(rest));
					return Value{rest};
				},
				[](const auto& value) -> Value {
					throw std::runtime_error("invalid argument to last(): " + std::to_string(value));
				}
			}, value.data);
		}
	}},	
};

