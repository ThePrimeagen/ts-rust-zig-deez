package src;

/**
	This is so-called 'static extensions' class.

	It is ideally used with `using CharTools;` and then acts as 
	  an [extension](https://haxe.org/manual/lf-static-extension.html) to the `Int` class.

	I can just write some utility functions here and call them from integers directly. Cool.
**/
class CharTools {
	static public function isNumber(char:Int):Bool {
		return '0'.code <= char && '9'.code >= char;
	}

	static public function isLetter(char:Int):Bool {
		return 'a'.code <= char && 'z'.code >= char || 'A'.code <= char && 'Z'.code >= char || char == '_'.code;
	}
}

/**
	Another cool thing about Haxe is that I can call `"X".code` and it will be replaced with character code at compile time, 
	but if you will try to use `"deez nuts".code`, the compiler will give you an error.
**/
