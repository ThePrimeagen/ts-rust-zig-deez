#include "value.hpp"

#include "environment.hpp"

Environment::Environment(EnvironmentP parent)
: parent{parent}
{
}

Value Environment::get(std::string_view name) const
{
	for (auto ptr = shared_from_this(); ptr; ptr = ptr->parent) {
		if (const auto iter = ptr->values.find(std::string{name}); iter != ptr->values.end())
			return iter->second;
	}
	return nil;
}

void Environment::set(std::string_view name, Value value)
{
	values[std::string{name}] = value;
}