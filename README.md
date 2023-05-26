<div align="center">

  <h1>Degens Interpreter Competition (DIC)</h1>
  <h5>Based on the awesome book "Writing An Interpreter In Go" by Thorsten Ball</h5>
  <h6><a href="https://interpreterbook.com/">Use the code THEPRIMEAGEN for 30% off</a></h6>

![Work In Progress](https://img.shields.io/badge/Work%20In%20Progress-orange?style=for-the-badge)

</div>

 ## Contributing

Remember: first come, first serve. If there exists an implementation in your language of choice, contribute to it!

If not, start by copying LANG_TEMPLATE folder:

```bash
cp -r .github/LANG_TEMPLATE/ <your_folder_name>
```

It comes with:

```bash
├── .gitignore  # ignores all editor-specific or os-specific files, add additional stuff for your language
├── Dockerfile  # for Docker builds
├── Makefile    # convenient commands runner (make <cmd>) under *NIX systems
└── make.cmd    # convenient commands runner (.\make.cmd <cmd>) under Windows
```

## Running

### Command names

General:

- `help` — outputs all the commands available, same as running the `make` or `make.cmd` without arguments

Without Docker:

- `fmt`
- `lint`
- `test`
- `ready` — should run the three above

Docker:

- `docker-build` — makes the build
- `docker-ready` — runs the build image and executes `fmt`, `lint`, `test` commands inside


### *NIX (Linux, macOS, etc.)

This assumes there is a `Makefile` in the root of the language directory.

```bash
# without docker
make fmt
make lint
make test
make ready  # runs all three above

# docker, assuming there's a Dockerfile
make docker-build  # makes the build
make docker-ready  # runs fmt, lint and test in the built image
```

### Windows

This assumes there is a `make.cmd` in the root of the language directory.

```batchfile
@rem without docker
.\make.cmd fmt
.\make.cmd lint
.\make.cmd test
.\make.cmd ready  @rem runs all three above

@rem docker, assuming there's a Dockerfile
.\make.cmd docker-build  @rem makes the build
.\make.cmd docker-ready  @rem runs fmt, lint and test in the built image
```
