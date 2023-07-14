# Monkeyscript - Dlang edition
Implementation of Monkeyscript written in Dlang. Originally specified in "Writing an Interpreter in Go" by Thorsten Ball.

## Project dependencies
The following dependencies must be used for this project:

- dlang (dmd compiler)
- dub
- dfmt
- dscanner
- linenoise library
    * https://github.com/antirez/linenoise

## Linenoise install
```bash
wget -O linenoise.zip https://github.com/antirez/linenoise/archive/refs/heads/master.zip

unzip linenoise.zip
cd linenoise-master
gcc -c -Os -g -o linenoise.o linenoise.c
ar rcs liblinenoise.a linenoise.o
mkdir -p ../lib

cp liblinenoise.a ../lib
cd ..
rm -rf linenoise-master
```

## Compile project and run tests
```bash
make ready
```

## Compile REPL
```bash
make repl
```

