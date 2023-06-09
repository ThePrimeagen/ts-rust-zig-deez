extends MainLoop

func run_tests(tests: PoolStringArray) -> void:
	var test_scripts: Array = []

	for test in tests:
		match test:
			"lexer":
				test_scripts.append(preload("res://test/lexer_tests.gd"))
			"parser":
				push_warning("Parser tests not implemented")
			"ast":
				push_warning("AST tests not implemented")
			_:
				push_error("Unknown test case: %s" % test)

	for test in test_scripts:
		for method in test.get_script_method_list():
			if method.name.begins_with("test_"):
				print("%s: '%s'" % [test.resource_path, method.name])
				test.call(method.name)

func _initialize() -> void:
	for argument in OS.get_cmdline_args():
		if argument.begins_with("--test"):
			run_tests(argument.get_slice("=", 1).split(",", false))

func _iteration(_delta: float) -> bool:
	return true

