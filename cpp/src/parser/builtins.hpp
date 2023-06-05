#pragma once

#include "expression.hpp"


// allow string_view lookup in string-keyed map:

namespace detail
{
	struct string_equal {
		using is_transparent = std::true_type;
		bool operator()(std::string_view l, std::string_view r) const noexcept {
			return l == r;
		}
	};

	struct string_hash {
		using is_transparent = std::true_type;
		auto operator()(std::string_view str) const noexcept {
			return std::hash<std::string_view>()(str);
		}
	};
}  // namespace detail

template <typename Value>
using unordered_string_map = std::unordered_map<std::string, Value, detail::string_hash, detail::string_equal>;
//using unordered_string_set = std::unordered_set<std::string, detail::string_hash, detail::string_equal>;


extern unordered_string_map<BuiltinFunctionExpression> builtins;
