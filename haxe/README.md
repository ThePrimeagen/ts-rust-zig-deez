# deez-Haxe

[Haxe](https://haxe.org/) has a lot of build targets like JavaScript, Python, C++, C#, etc.

Build examples:

- [Javascript](https://haxe.org/manual/target-javascript.html) `haxe build.tests.hxml --js bin/js/test.js`
- [C++](https://haxe.org/manual/target-cpp.html) `haxe build.tests.hxml --cpp bin/cpp`
- [Python](https://haxe.org/manual/target-python.html) `haxe build.tests.hxml --cpp bin/python/test.py`
- [PHP](https://haxe.org/manual/target-php.html) `haxe build.tests.hxml --php bin/php`
- [Java](https://haxe.org/manual/target-java.html) `haxe build.tests.hxml --java bin/java`
- [JVM](https://haxe.org/manual/target-jvm.html) `haxe build.tests.hxml --java bin/jvm`
- [C#](https://haxe.org/manual/target-cs.html) `haxe build.tests.hxml --cs bin/cs`
- [Lua](https://haxe.org/manual/target-lua.html) `haxe build.tests.hxml --lua bin/lua/test.lua`
- [C](https://haxe.org/manual/target-hl-c-compilation.html) (via HashLink) `haxe build.tests.hxml --hl bin/c/test.c`
- [HashLink](https://haxe.org/manual/target-hl-getting-started.html) `haxe build.tests.hxml --hl bin/hl`
- [Flash](https://haxe.org/manual/target-flash.html) `haxe build.tests.hxml --swf bin/flash/test.swf --swf-version 15 --swf-header 960:640:60:f68712` (Gigachad target, btw)

Actually, [V language](https://vlang.io/) have their own C to V transpiler, so _theoretically_ you can transpile Haxe to V. :D

## Recompilation

Haxe also have it's own [compilation server](https://haxe.org/manual/cr-completion-server.html) to speed up recompilation.

Simply run `haxe -v --wait <PORT>` and then add parameter `--connect 6000` to your command.
