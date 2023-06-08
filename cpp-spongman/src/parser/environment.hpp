#pragma once

#include <memory>
#include <string_view>

#include "utils.hpp"

#include "value.hpp"

struct Environment;
using EnvironmentP = std::shared_ptr<Environment>;

struct Environment : public std::enable_shared_from_this<Environment>
{
	EnvironmentP parent{};
	unordered_string_map<Value> values;

	Environment(EnvironmentP parent = {});

	Value get(std::string_view name) const;
	void set(std::string_view name, Value&& value);
};