#include "ast.hh"
#include "vm/vm.hh"

#include <iostream>
#include <algorithm>
#include <fstream>
#include <filesystem>
#include <regex>
#include <unordered_set>


Object make_args_errror(size_t got, size_t wanted) {
	return Object(Error(std::format("wrong number of arguments. got={}, want={}", got, wanted)));
}

std::vector<std::pair<std::string, builtin_ptr>> builtin_defs {
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
			{
				array_ptr a = args[0].get<array_ptr>();
				if (a->empty()) return Object();
				else return a->front();
			}
			else if (args[0].is_a<std::string>())
			{
				const std::string &s = args[0].get<std::string>();
				if (s.empty()) return Object();
				else return Object(s.front());
			}
			else return Object(Error(std::format("argument to 'first' not supported, got {}", args[0].type_name())));
		}
	},
	{
		"last",
		[](BuiltinFunctionParameters args) {
			if (args.size() != 1) return make_args_errror(args.size(),1);
			else if (args[0].is_a<array_ptr>())
			{
				array_ptr a = args[0].get<array_ptr>();
				if (a->empty()) return Object();
				else return a->back();
			}
			else if (args[0].is_a<std::string>())
			{
				const std::string &s = args[0].get<std::string>();
				if (s.empty()) return Object();
				else return Object(s.back());
			}
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
				Array::iterator second = ar->begin();
				if (second == ar->end()) return Object(std::make_shared<Array>());
				return Object(std::make_shared<Array>(++second, ar->end()));
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
				if (args[0].get<array_ptr>()->empty()) return Object();
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
		"split", [](BuiltinFunctionParameters args) {
			if (args.size() != 2 || !args[0].is_a<std::string>() || !args[1].is_a<std::string>())
				return Object(Error("'split' expects two strings as parameters"));
			const std::string &src = args[0].get<std::string>();
			const std::string &split = args[1].get<std::string>();
			array_ptr result = make_array();
			if (split.length() == 0) {
				for (char c : src)
					result->push_back(Object(c));
			}
			else if (split.length() == 1) {
				size_t start = 0;
				char s = split[0];
				while (true) {
					size_t end = src.find_first_of(s, start);
					result->push_back(Object(src.substr(start,end - start)));
					if (end == std::string::npos) break;
					start = end + 1;
				}
			}
			else {
				size_t start = 0;
				while (true) {
					size_t end = src.find(split, start);
					result->push_back(Object(src.substr(start, end - start)));
					if (end == std::string::npos) break;
					start = end + split.length();
				}

			}
			return Object(result);
		}
	},
	{
		"join", [](BuiltinFunctionParameters args) {
			if (args.size() != 2 || !args[0].is_a<array_ptr>() || !args[1].is_a<std::string>())
				return Object(Error("'join' expects an array and a string as parameters"));
			array_ptr src = args[0].get<array_ptr>();
			const std::string &join = args[1].get<std::string>();
			std::string result;
			result.reserve(src->size() * 16);
			if (src->size() > 0) {
				auto iter = src->begin();
				result += iter->to_string();
				++iter;
				for (; iter != src->end(); ++iter)
					result += join + iter->to_string();
			}
			return Object(result);
		}
	},
	{
		"sort", [](BuiltinFunctionParameters args) {
			if (args.size() != 1 && args.size() != 2)
				return Object(Error("'sort' expects an array and an optional compare function as parameters"));
			if (args.size() == 2)
			{
				if (args[1].is_a<func_ptr>()) {
					if (args[1].get<func_ptr>()->paramCount() != 2)
						return Object(Error("Compare function for 'sort' must take two parameters"));
				}
				else if (args[1].is_a<closure_ptr>()) {
					if (args[1].get<closure_ptr>()->fn->numParameters != 2)
						return Object(Error("Compare function for 'sort' must take two parameters"));
				}
				else
					return Object(Error("Second parameter to sort must be a comparison function"));
			}

			if (args[0].is_a<array_ptr>()) {
				array_ptr array = args[0].get<array_ptr>();
				if (args.size() == 1)
					std::sort(array->begin(), array->end());
				else if (args[1].is_a<func_ptr>()) {
					func_ptr fn = args[1].get<func_ptr>();
					std::sort(array->begin(), array->end(), [&fn](Object &a, Object &b) {
						return fn->call({ a,b }).get<bool>();
						});
				}
				else if (args[1].is_a<closure_ptr>()) {
					closure_ptr fn = args[1].get<closure_ptr>();
					SwitchVM vm(GlobalData::getLastGlobals());
					std::sort(array->begin(), array->end(), [&vm, &fn](Object &a, Object &b) {
						vm.run(fn, { a, b });
						return vm.last_popped_element().get<bool>();
						});
				}
				return args[0];
			}
			else if (args[0].is_a<std::string>()) {
				std::string s = args[0].get<std::string>();
				if (args.size() == 1)
					std::sort(s.begin(), s.end());
				else if (args[1].is_a<func_ptr>()) {
					func_ptr fn = args[1].get<func_ptr>();
					std::sort(s.begin(), s.end(), [&fn](char a, char b) {
						return fn->call({ Object(a),Object(b) }).get<bool>();
						});
				}
				else if (args[1].is_a<closure_ptr>()) {
					closure_ptr fn = args[1].get<closure_ptr>();
					SwitchVM vm(GlobalData::getLastGlobals());
					std::sort(s.begin(), s.end(), [&vm, &fn](char a, char b) {
						vm.run(fn, { Object(a), Object(b) });
						return vm.last_popped_element().get<bool>();
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
			// Value inspection
	{
		"is_nil", [](BuiltinFunctionParameters args) {
			if (args.size() != 1)
				return Object(Error("Expected exactly one parameter!"));
			return Object(args[0].is_a<std::monostate>());
		}
	},
	{
		"is_error", [](BuiltinFunctionParameters args) {
			if (args.size() != 1)
				return Object(Error("Expected exactly one parameter!"));
			return Object(args[0].is_a<Error>());
		}
	},
	{
		"is_integer", [](BuiltinFunctionParameters args) {
			if (args.size() != 1)
				return Object(Error("Expected exactly one parameter!"));
			return Object(args[0].is_a<std::int64_t>());
		}
	},
	{
		"is_bool", [](BuiltinFunctionParameters args) {
			if (args.size() != 1)
				return Object(Error("Expected exactly one parameter!"));
			return Object(args[0].is_a<bool>());
		}
	},
	{
		"is_array", [](BuiltinFunctionParameters args) {
			if (args.size() != 1)
				return Object(Error("Expected exactly one parameter!"));
			return Object(args[0].is_a<array_ptr>());
		}
	},
	{
		"is_hash", [](BuiltinFunctionParameters args) {
			if (args.size() != 1)
				return Object(Error("Expected exactly one parameter!"));
			return Object(args[0].is_a<hashmap_ptr>());
		}
	},
	{
		"is_function", [](BuiltinFunctionParameters args) {
			if (args.size() != 1)
				return Object(Error("Expected exactly one parameter!"));
			return Object(args[0].is_a<func_ptr>() || args[0].is_a<closure_ptr>());
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
		"file_read", [](BuiltinFunctionParameters args) {
			if (args.size() != 1 || !args[0].is_a<std::string>())
				return Object(Error("'file_read' expects a string with the filename as it's single paramter"));
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
		"file_write", [](BuiltinFunctionParameters args) {
			if (args.size() != 2 || !args[0].is_a<std::string>() || !args[1].is_a<std::string>())
				return Object(Error("'file_write' expects a string with the filename as first and the string to write as second paramter"));
			std::ofstream ostrm(args[0].get<std::string>(), std::ios::binary);
			const std::string &str = args[1].get<std::string>();
			ostrm.write(&str[0], str.size());
			return Object(ostrm.good());
		}
	},
	{
		"file_glob",[](BuiltinFunctionParameters args) {
			if (args.size() != 1 || !args[0].is_a<std::string>())
				return Object(Error("'file_glob' expects a string with the path to search for matching files"));
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
	{ 
		"file_size", [](BuiltinFunctionParameters args) {
			if (args.size() != 1 || !args[0].is_a<std::string>())
				return Object(Error("Argument must be a string contaiing a path"));
			std::filesystem::path path = args[0].get<std::string>();
			return Object((std::int64_t)std::filesystem::file_size(path));
		}
	},
	{ 
		"file_exists", [](BuiltinFunctionParameters args) {
			if (args.size() != 1 || !args[0].is_a<std::string>())
				return Object(Error("Argument must be a string contaiing a path"));
			std::filesystem::path path = args[0].get<std::string>();
			return Object(std::filesystem::exists(path));
		}
	},
	{ 
		"file_curpath", [](BuiltinFunctionParameters args) {
			if (args.size() == 0) {
				std::filesystem::path path = std::filesystem::current_path();
				return Object(path.string());
			}
			else if (args.size() == 1) {
				if (!args[0].is_a<std::string>())
					return Object(Error("optional argument must be path to switch to"));
				std::filesystem::path path = args[0].get<std::string>();
				std::filesystem::current_path(path);
				return Object();
			}
			else 
				return Object(Error("Unexpected number of arguments"));
		}
	},
	{ 
		"file_mkdir", [](BuiltinFunctionParameters args) {
			if (args.size() != 1 || !args[0].is_a<std::string>())
				return Object(Error("Argument must be a string contaiing a path"));
			std::filesystem::path path = args[0].get<std::string>();
			return Object(std::filesystem::create_directories(path));
		}
	},
	{ 
		"file_delete", [](BuiltinFunctionParameters args) {
			if (args.size() != 1 || !args[0].is_a<std::string>())
				return Object(Error("Argument must be a string contaiing a path"));
			std::filesystem::path path = args[0].get<std::string>();
			return Object(std::filesystem::remove(path));
		}
	},
	{ 
		"file_mtime", [](BuiltinFunctionParameters args) {
			if (args.size() != 1 || !args[0].is_a<std::string>())
				return Object(Error("Argument must be a string contaiing a path"));
			std::filesystem::path path = args[0].get<std::string>();
			auto t = std::filesystem::last_write_time(path).time_since_epoch();
			return Object(std::chrono::duration_cast<std::chrono::seconds>(t).count());
		}
	},
	{ 
		"file_rename", [](BuiltinFunctionParameters args) {
			if (args.size() != 2 || !args[0].is_a<std::string>() || !args[1].is_a<std::string>())
				return Object(Error("Arguments must be two strings containing a path each"));
			std::filesystem::path oldpath = args[0].get<std::string>();
			std::filesystem::path newpath = args[1].get<std::string>();
			std::error_code ec;
			std::filesystem::rename(oldpath, newpath, ec);
			if (ec)
				return Object(Error(ec.message()));
			else
				return Object();
		}
	},
			// Regular expressionsm no language is complete without!
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
			// Time
	{
		"time_now",
		[](BuiltinFunctionParameters args) {
			auto t = std::chrono::system_clock::now().time_since_epoch();
			return Object(std::chrono::duration_cast<std::chrono::seconds>(t).count());
		}
	},
	{
		"time_format",
		[](BuiltinFunctionParameters args) {
			if (args.size() != 2 && !args[0].is_a<std::string>() && !args[1].is_a<std::int64_t>())
				return Object(Error("Expected a format specifier followed by a timestamp"));
			//auto p0 = std::chrono::time_point<std::chrono::system_clock>{} + std::chrono::seconds(args[1].get<std::int64_t>());
			auto p0 = std::chrono::time_point<std::chrono::system_clock>(std::chrono::seconds(args[1].get<std::int64_t>()));
			std::string f = "{:" + args[0].get<std::string>() + "}";
			std::string result = std::vformat(f, std::make_format_args(p0));
			return Object(std::move(result));
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

std::unordered_map<std::string, builtin_ptr> builtins{ builtin_defs.begin(), builtin_defs.end() };
