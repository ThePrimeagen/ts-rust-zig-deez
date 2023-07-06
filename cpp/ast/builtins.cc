#include "ast.hh"

#include <iostream>
#include <algorithm>
#include <fstream>
#include <filesystem>
#include <regex>


Object make_args_errror(size_t got, size_t wanted) {
	return Object(Error(std::format("wrong number of arguments. got={}, want={}", got, wanted)));
}


std::unordered_map<std::string, builtin_ptr> builtins{
	{
		// Container support 
		"len",
		[](BuiltinFunctionParameters args) {
			if (args.size() != 1) return make_args_errror(args.size(),1);
			if (args[0].is_a<std::string>())
				return Object((std::int64_t)args[0].get<std::string>().size());
			else if (args[0].is_a<array_ptr>())
				return Object((std::int64_t)args[0].get<array_ptr>()->size());
			else if (args[0].is_a<hashmap_ptr>())
				return Object((std::int64_t)args[0].get<hashmap_ptr>()->size());
			else return Object(Error(std::format("argument to 'len' not supported, got {}", args[0].type_name())));
		}
	},
	{
		"first",
		[](BuiltinFunctionParameters args) {
			if (args.size() != 1) return make_args_errror(args.size(),1);
			else if (args[0].is_a<array_ptr>())
				return args[0].get<array_ptr>()->front();
			else if (args[0].is_a<std::string>())
				return Object(args[0].get<std::string>().front());
			else return Object(Error(std::format("argument to 'first' not supported, got {}", args[0].type_name())));
		}
	},
	{
		"last",
		[](BuiltinFunctionParameters args) {
			if (args.size() != 1) return make_args_errror(args.size(),1);
			else if (args[0].is_a<array_ptr>())
				return args[0].get<array_ptr>()->back();
			else if (args[0].is_a<std::string>())
				return Object(args[0].get<std::string>().back());
			else return Object(Error(std::format("argument to 'last' not supported, got {}", args[0].type_name())));
		}
	},
	{
		"rest",
		[](BuiltinFunctionParameters args) {
			if (args.size() != 1) return make_args_errror(args.size(),1);
			else if (args[0].is_a<array_ptr>())
			{
				array_ptr ar = args[0].get<array_ptr>();
				Array::iterator second = ar->begin()++;
				return Object(std::make_shared<Array>(second, ar->end()));
			}
			else if (args[0].is_a<std::string>())
			{
				const std::string s = args[0].get<std::string>();
				std::string::const_iterator second = s.begin()++;
				return Object(std::string(second, s.end()));
			}
			else return Object(Error(std::format("argument to 'rest' not supported, got {}", args[0].type_name())));
		}
	},
	{
		"push", [](BuiltinFunctionParameters args) {
			if (args.size() != 2) return make_args_errror(args.size(),2);
			else if (args[0].is_a<array_ptr>()) {
				args[0].get<array_ptr>()->push_back(args[1]);
				return args[0];
			}
			else return Object(Error(std::format("argument to 'push' not supported, got {}", args[0].type_name())));
		}
	},
	{
		"pop", [](BuiltinFunctionParameters args) {
			if (args.size() != 1) return make_args_errror(args.size(),1);
			else if (args[0].is_a<array_ptr>()) {
				Object result = args[0].get<array_ptr>()->back();
				args[0].get<array_ptr>()->pop_back();
				return result;
			}
			else return Object(Error(std::format("argument to 'push' not supported, got {}", args[0].type_name())));
		}
	},
	{
		"keys", [](BuiltinFunctionParameters args) {
			if (args.size() != 1) return make_args_errror(args.size(),1);
			else if (args[0].is_a<hashmap_ptr>())
			{
				array_ptr result = make_array();
				hashmap_ptr hash = args[0].get<hashmap_ptr>();
				result->reserve(hash->size());
				for (const auto &elem : *hash)
					result->push_back(elem.first);
				return Object(result);
			}
			else return Object(Error(std::format("argument to 'keys' not supported, got {}", args[0].type_name())));
		}
	},
	{
		"range", [](BuiltinFunctionParameters args) {
			if (args.size() != 3) return make_args_errror(args.size(),3);
			if (!args[1].is_a<std::int64_t>() || !args[2].is_a<std::int64_t>())
				return Object(Error(std::format("argument to 'range' not supported, must be integers")));
			size_t from = args[1].get<std::int64_t>();
			size_t count = args[2].get<std::int64_t>();
			if (args[0].is_a<std::string>()) {
				const std::string &src = args[0].get<std::string>();
				return Object(src.substr(from, count));
			}
			else if (args[0].is_a<array_ptr>()) {
				const array_ptr &src = args[0].get<array_ptr>();
				array_ptr result = make_array();
				for (size_t i = from; i < from + count && i < src->size(); ++i)
					result->push_back((*src)[i]);
				return Object();
			}
			else return Object(Error(std::format("argument to 'range' not supported, got {}", args[0].type_name())));
		}
	},
	{
		"find", [](BuiltinFunctionParameters args) {
			if (args.size() != 2 && args.size() != 3)
				return Object(Error("'find' expects two strings and an optional integers as parameters"));
			if (!args[0].is_a<std::string>() || !args[1].is_a<std::string>())
				return Object(Error("First two argument to 'find' must be strings"));
			const std::string &src = args[0].get<std::string>();
			const std::string &find = args[1].get<std::string>();
			size_t pos = 0;
			if (args.size() == 3) {
				if (!args[1].is_a<std::int64_t>())
					return Object(Error("Third argument to 'find' must be integer"));
				pos = args[2].get<std::int64_t>();
			}
			return Object((std::int64_t)src.find(find,pos));
		}
	},
	{
		"rfind", [](BuiltinFunctionParameters args) {
			if (args.size() != 2 && args.size() != 3)
				return Object(Error("'rfind' expects two strings and an optional integers as parameters"));
			if (!args[0].is_a<std::string>() || !args[1].is_a<std::string>())
				return Object(Error("First two argument to 'find' must be strings"));
			const std::string &src = args[0].get<std::string>();
			const std::string &find = args[1].get<std::string>();
			size_t pos = std::string::npos;
			if (args.size() == 3) {
				if (!args[1].is_a<std::int64_t>())
					return Object(Error("Third argument to 'rfind' must be integer"));
				pos = args[2].get<std::int64_t>();
			}
			return Object((std::int64_t)src.rfind(find,pos));
		}
	},
	{
		"sort", [](BuiltinFunctionParameters args) {
			if (args.size() != 1 && args.size() != 2)
				return Object(Error("'sort' expects an array and an optional compare function as parameters"));
			if (args.size() == 2)
			{
				if (!args[1].is_a<func_ptr>())
					return Object(Error("'sort' expects second parameter to be a compare function"));
				if (args[1].get<func_ptr>()->paramCount() != 2)
					return Object(Error("Compare function for 'sort' must take two paramteters"));
			}

			if (args[0].is_a<array_ptr>()) {
				array_ptr array = args[0].get<array_ptr>();
				if (args.size() == 1)
					std::sort(array->begin(), array->end());
				else {
					func_ptr fn = args[1].get<func_ptr>();
					if (fn->paramCount() != 2)
						return Object(Error("Compare function for 'sort' must take two paramteters"));
					std::sort(array->begin(), array->end(), [&fn](Object &a, Object &b) {
						return fn->call({ a,b }).get<bool>();
						});
				}
				return args[0];
			}
			else if (args[0].is_a<std::string>()) {
				std::string s = args[0].get<std::string>();
				if (args.size() == 1)
					std::sort(s.begin(), s.end());
				else {
					func_ptr fn = args[1].get<func_ptr>();
					std::sort(s.begin(), s.end(), [&fn](char a, char b) {
						return fn->call({ Object(a),Object(b) }).get<bool>();
						});
				}
				return Object(s);
			}
			else return Object(Error(std::format("argument to 'sort' not supported, got {}", args[0].type_name())));
		}
	},
	{
		"reverse", [](BuiltinFunctionParameters args) {
			if (args.size() != 1)
				return Object(Error("'sort' expects an array and an optional compare function as parameters"));
			if (args[0].is_a<array_ptr>()) {
				array_ptr array = args[0].get<array_ptr>();
				std::reverse(array->begin(), array->end());
				return args[0];
			}
			else if (args[0].is_a<std::string>()) {
				std::string s = args[0].get<std::string>();
				std::reverse(s.begin(), s.end());
				return Object(s);
			}
			else return Object(Error(std::format("argument to 'sort' not supported, got {}", args[0].type_name())));
		}
	},
	{
		"unique", [](BuiltinFunctionParameters args) {
			if (args.size() != 1)
				return Object(Error("'sort' expects an array and an optional compare function as parameters"));
			if (args[0].is_a<array_ptr>()) {
				array_ptr array = args[0].get<array_ptr>();
				auto last = std::unique(array->begin(), array->end());
				array->erase(last, array->end());
				return args[0];
			}
			else if (args[0].is_a<std::string>()) {
				std::string s = args[0].get<std::string>();
				auto last = std::unique(s.begin(), s.end());
				s.erase(last, s.end());
				return Object(s);
			}
			else return Object(Error(std::format("argument to 'sort' not supported, got {}", args[0].type_name())));
		}
	},
			// Input/Output
	{
		"puts", [](BuiltinFunctionParameters args) {
			for (const Object &arg : args)
				std::cout << arg.to_string() << '\n';
			return Object();
		}
	},
	{
		"gets", [](BuiltinFunctionParameters args) {
			std::string result;
			std::getline(std::cin, result);
			return Object(result);
		}
	},
	{
		"readfile", [](BuiltinFunctionParameters args) {
			if (args.size() != 1 || !args[0].is_a<std::string>())
				return Object(Error("'readfile' expects a string with the filename as it's single paramter"));
			// read entire file into string
			if (std::ifstream is{ args[0].get<std::string>(), std::ios::binary | std::ios::ate}) {
				auto size = is.tellg();
				std::string str(size, '\0'); // construct string to stream size
				is.seekg(0);
				if (is.read(&str[0], size))
					return Object(str);
			}
			return Object(Error("Failed to read file."));
		}
	},
	{
		"writefile", [](BuiltinFunctionParameters args) {
			if (args.size() != 2 || !args[0].is_a<std::string>() || !args[1].is_a<std::string>())
				return Object(Error("'readfile' expects a string with the filename as first and the string to write as second paramter"));
			std::ofstream ostrm(args[0].get<std::string>(), std::ios::binary);
			const std::string &str = args[1].get<std::string>();
			ostrm.write(&str[0], str.size());
			return Object(ostrm.good());
		}
	},
	{
		"glob",[](BuiltinFunctionParameters args) {
			if (args.size() != 1 || !args[0].is_a<std::string>())
				return Object(Error("'gob' expects a string with the path to search for matching files"));
			array_ptr result = make_array();
			std::filesystem::path path{ args[0].get<std::string>()};
			std::string filename;
			if (path.has_filename())
			{
				filename = path.filename().string();
				path.remove_filename();
			}
			if (path.empty()) path = "./";
			if (filename == "*") filename.clear();
			if (!filename.empty()) {
				std::string file_re;
				for (char c : filename) {
					switch (c) { // Built a regex that simulates shell-like globbing 
					case '.': file_re.append("\\."); break;
					case '*': file_re.append(".*"); break;
					case '?': file_re.append("."); break;
					default: file_re.push_back(c); break;
					}
				}
				std::regex re(file_re);
				for (auto const &dir_entry : std::filesystem::directory_iterator{ path })
				{
					bool match = std::regex_match(dir_entry.path().string(), re);
					if (match) result->push_back(dir_entry.path().string());
				}
			}
			else {
				for (auto const &dir_entry : std::filesystem::directory_iterator{ path })
					result->push_back(dir_entry.path().string());
			}
			return Object(result);
		}
	},
			// Regular expression fun
	{ "regexp",[](BuiltinFunctionParameters args) {
			if (args.size() != 2 || !args[0].is_a<std::string>() || !args[1].is_a<std::string>())
				return Object(Error("'regexp' expects a string with the regular expression as first and the string to match against as second paramter"));
			std::regex re(args[0].get<std::string>());
			return Object(std::regex_match(args[1].get<std::string>(), re));
		}
	},
	{ "regsub",[](BuiltinFunctionParameters args) {
			// For format string description, see https://262.ecma-international.org/5.1/#sec-15.5.4.11
			/* Example:
				let s ="Quick brown fox";
				let re="a|e|i|o|u";
				regsub(s,re,"[$&]");
			*/
			if (args.size() != 3 || !args[0].is_a<std::string>() || !args[1].is_a<std::string>() || !args[2].is_a<std::string>())
				return Object(Error("'regsub' expects a string with the regular expression as first,the string to match against as second, and the subtition string as third paramter"));
			std::regex re(args[0].get<std::string>());
			return Object(std::regex_replace(args[1].get<std::string>(), re, args[2].get<std::string>()));
			}
	},
			// Other system interaction
	{
		"exit",
		[](BuiltinFunctionParameters args) {
			if (args.size() == 0) exit(0);
			else if (args.size() == 1 and args[0].is_a<std::int64_t>()) exit(args[0].get<std::int64_t>());
			else {
				std::cerr << "Bad arguments to 'exit', expected zero or one integer parameter" << std::endl;
				exit(1);
			}
			return Object();
		}
	}
};
