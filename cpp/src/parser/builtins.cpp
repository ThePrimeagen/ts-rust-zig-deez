#include <unordered_map>
#include <string_view>

#include "token.hpp"
#include "builtins.hpp"



// BuiltinFunctionExpression

std::vector<BuiltinFunctionExpression> BuiltinFunctionExpression::builtins{

	{ "len", {"val"},
		[](const std::vector<Value>& arguments) {
			if (arguments.size() != 1)
				throw std::runtime_error("wrong number of arguments to len(): " + std::to_string(arguments.size()));

			auto value = arguments[0];
			return Value{std::visit(overloaded{
				[](const String& str) { return static_cast<Integer>(str.length()); },
				[](const std::vector<Value>& array) { return static_cast<Integer>(array.size()); },
				[](const auto& value) -> Integer {
					throw std::runtime_error("invalid argument to len(): " + std::to_string(value));
				}
			}, value.data)};
		}
	},
	{ "first", {"arr"},
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
	},
	{ "last", {"arr"},
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
	},
	{ "rest", {"arr"},
		[](const std::vector<Value>& arguments) {
			if (arguments.size() != 1)
				throw std::runtime_error("wrong number of arguments to rest(): " + std::to_string(arguments.size()));

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
					throw std::runtime_error("invalid argument to rest(): " + std::to_string(value));
				}
			}, value.data);
		}
	},
};


// BuiltinBinaryFunctionExpression

Value BuiltinBinaryFunctionExpression::call(
	EnvironmentP closureEnv,
	EnvironmentP callerEnv,
	const std::vector<ExpressionP>& arguments
) const
{
	if (arguments.size() != 2)
		throw std::runtime_error("wrong number of arguments to " + name + "(): " + std::to_string(arguments.size()));

	auto leftValue = arguments[0]->eval(callerEnv);
	auto rightValue = arguments[1]->eval(callerEnv);
	return call(leftValue, rightValue);
}


BuiltinBinaryFunctionExpression BuiltinBinaryFunctionExpression::asterisk {
	"*",
	[](const Value& left, const Value& right) {
		return std::visit(overloaded{
			[](const Integer left, const Integer right) { return Value{left * right}; },
			// TODO: function composition?
			[](const auto& left, const auto& right) -> Value {
				BuiltinBinaryFunctionExpression::error("*", left, right); return {};
			}
		}, left.data, right.data);
	}
};
BuiltinBinaryFunctionExpression BuiltinBinaryFunctionExpression::slash {
	"/",
	[](const Value& left, const Value& right) {
		return std::visit(overloaded{
			[](const Integer left, const Integer right) { return Value{left / right}; },
			[](const auto& left, const auto& right) -> Value {
				BuiltinBinaryFunctionExpression::error("/", left, right); return Value{};
			}
		}, left.data, right.data);
	}
};
BuiltinBinaryFunctionExpression BuiltinBinaryFunctionExpression::percent {
	"%",
	[](const Value& left, const Value& right) {
		return std::visit(overloaded{
			[](const Integer left, const Integer right) { return Value{left % right}; },
			[](const auto& left, const auto& right) -> Value {
				BuiltinBinaryFunctionExpression::error("%", left, right); return {};
			}
		}, left.data, right.data);
	}
};
BuiltinBinaryFunctionExpression BuiltinBinaryFunctionExpression::plus {
	"+",
	[](const Value& left, const Value& right) {
		return std::visit(overloaded{
			[](const Integer left, const Integer right) { return Value{left + right}; },
			[](const String& left, const String& right) { return Value{left + right}; },
			[](const Array& left, const Array& right) {
				Array result;
				std::copy(left.cbegin(), left.cend(), std::back_inserter(result));
				std::copy(right.cbegin(), right.cend(), std::back_inserter(result));
				return Value{result};
			},
			[](const auto& left, const auto& right) -> Value {
				BuiltinBinaryFunctionExpression::error("+", left, right); return {};
			}
		}, left.data, right.data);
	}
};
BuiltinBinaryFunctionExpression BuiltinBinaryFunctionExpression::minus {
	"-",
	[](const Value& left, const Value& right) {
		return std::visit(overloaded{
			[](const Integer left, const Integer right) { return Value{left - right}; },
			[](const auto& left, const auto& right) -> Value {
				BuiltinBinaryFunctionExpression::error("-", left, right); return {};
			}
		}, left.data, right.data);
	}
};
BuiltinBinaryFunctionExpression BuiltinBinaryFunctionExpression::bitAnd {
	"&",
	[](const Value& left, const Value& right) {
		return std::visit(overloaded{
			[](const Integer left, const Integer right) { return Value{left & right}; },
			[](const auto& left, const auto& right) -> Value {
				BuiltinBinaryFunctionExpression::error("&", left, right); return {};
			}
		}, left.data, right.data);
	}
};
BuiltinBinaryFunctionExpression BuiltinBinaryFunctionExpression::bitOr {
	"|",
	[](const Value& left, const Value& right) {
		return std::visit(overloaded{
			[](const Integer left, const Integer right) { return Value{left | right}; },
			[](const auto& left, const auto& right) -> Value {
				BuiltinBinaryFunctionExpression::error("|", left, right); return {};
			}
		}, left.data, right.data);
	}
};
BuiltinBinaryFunctionExpression BuiltinBinaryFunctionExpression::bitEor {
	"^",
	[](const Value& left, const Value& right) {
		return std::visit(overloaded{
			[](const Integer left, const Integer right) { return Value{left ^ right}; },
			[](const auto& left, const auto& right) -> Value {
				BuiltinBinaryFunctionExpression::error("^", left, right); return {};
			}
		}, left.data, right.data);
	}
};
BuiltinBinaryFunctionExpression BuiltinBinaryFunctionExpression::lt {
	"<",
	[](const Value& left, const Value& right) {
		return std::visit(overloaded{
			[](const Integer left, const Integer right) { return Value{left < right}; },
			[](const auto& left, const auto& right) -> Value {
				BuiltinBinaryFunctionExpression::error("<", left, right); return {};
			}
		}, left.data, right.data);
	}
};
BuiltinBinaryFunctionExpression BuiltinBinaryFunctionExpression::gt {
	">",
	[](const Value& left, const Value& right) {
		return std::visit(overloaded{
			[](const Integer left, const Integer right) { return Value{left > right}; },
			[](const auto& left, const auto& right) -> Value {
				BuiltinBinaryFunctionExpression::error(">", left, right); return {};
			}
		}, left.data, right.data);
	}
};
BuiltinBinaryFunctionExpression BuiltinBinaryFunctionExpression::le {
	"<=",
	[](const Value& left, const Value& right) {
		return std::visit(overloaded{
			[](const Integer left, const Integer right) { return Value{left <= right}; },
			[](const auto& left, const auto& right) -> Value {
				BuiltinBinaryFunctionExpression::error("<=", left, right); return {};
			}
		}, left.data, right.data);
	}
};
BuiltinBinaryFunctionExpression BuiltinBinaryFunctionExpression::ge {
	">=",
	[](const Value& left, const Value& right) {
		return std::visit(overloaded{
			[](const Integer left, const Integer right) { return Value{left >= right}; },
			[](const auto& left, const auto& right) -> Value {
				BuiltinBinaryFunctionExpression::error(">=", left, right); return {};
			}
		}, left.data, right.data);
	}
};
BuiltinBinaryFunctionExpression BuiltinBinaryFunctionExpression::eq {
	"==",
	[](const Value& left, const Value& right) {
		return std::visit(overloaded{
			[](const Integer left, const Integer right) { return Value{left == right}; },
			[](const bool left, const bool right) { return Value{left == right}; },
			[](const String& left, const String& right) { return Value{left == right}; },
			[](const Array& left, const Array& right) {
				return Value{
					left.size() == right.size() &&
					std::equal(std::begin(left), std::end(left), std::begin(right))
				};
			},
			[](const auto& left, const auto& right) -> Value {
				BuiltinBinaryFunctionExpression::error("==", left, right); return {};
			}
		}, left.data, right.data);
	}
};
BuiltinBinaryFunctionExpression BuiltinBinaryFunctionExpression::neq {
	"!=",
	[](const Value& left, const Value& right) {
		return std::visit(overloaded{
			[](const Integer left, const Integer right) { return Value{left != right}; },
			[](const bool left, const bool right) { return Value{left != right}; },
			[](const String& left, const String& right) { return Value{left != right}; },
			[](const Array& left, const Array& right) {
				return Value{
					left.size() != right.size() ||
					!std::equal(std::begin(left), std::end(left), std::begin(right))
				};
			},
			[](const auto& left, const auto& right) -> Value {
				BuiltinBinaryFunctionExpression::error("!=", left, right); return {};
			}
		}, left.data, right.data);
	}
};
BuiltinBinaryFunctionExpression BuiltinBinaryFunctionExpression::and_ {
	"&&",
	[](const Value& left, const Value& right) {
		return std::visit(overloaded{
			[](const bool left, const bool right) { return Value{left && right}; },
			[](const auto& left, const auto& right) -> Value {
				BuiltinBinaryFunctionExpression::error("&&", left, right); return {};
			}
		}, left.data, right.data);
	}
};
BuiltinBinaryFunctionExpression BuiltinBinaryFunctionExpression::or_ {
	"||",
	[](const Value& left, const Value& right) {
		return std::visit(overloaded{
			[](const bool left, const bool right) { return Value{left || right}; },
			[](const auto& left, const auto& right) -> Value {
				BuiltinBinaryFunctionExpression::error("||", left, right); return {};
			}
		}, left.data, right.data);
	}
};

std::unordered_map<TokenType, BuiltinBinaryFunctionExpression*> BuiltinBinaryFunctionExpression::builtins {
	{ TokenType::Asterisk, &BuiltinBinaryFunctionExpression::asterisk },
	{ TokenType::Slash,    &BuiltinBinaryFunctionExpression::slash    },
	{ TokenType::Percent,  &BuiltinBinaryFunctionExpression::percent  },
	{ TokenType::Plus,     &BuiltinBinaryFunctionExpression::plus     },
	{ TokenType::Minus,    &BuiltinBinaryFunctionExpression::minus    },
	{ TokenType::BitAnd,   &BuiltinBinaryFunctionExpression::bitAnd   },
	{ TokenType::BitOr,    &BuiltinBinaryFunctionExpression::bitOr    },
	{ TokenType::BitEor,   &BuiltinBinaryFunctionExpression::bitEor   },
	{ TokenType::Lt,       &BuiltinBinaryFunctionExpression::lt       },
	{ TokenType::Gt,       &BuiltinBinaryFunctionExpression::gt       },
	{ TokenType::Le,       &BuiltinBinaryFunctionExpression::le       },
	{ TokenType::Ge,       &BuiltinBinaryFunctionExpression::ge       },
	{ TokenType::Eq,       &BuiltinBinaryFunctionExpression::eq       },
	{ TokenType::Not_eq,   &BuiltinBinaryFunctionExpression::neq      },
	{ TokenType::And,      &BuiltinBinaryFunctionExpression::and_     },
	{ TokenType::Or,       &BuiltinBinaryFunctionExpression::or_      },
};
