=========
deez_py
=========

.. image:: https://img.shields.io/badge/License-GPLv3-blue.svg
   :target: https://github.com/ThePrimeagen/ts-rust-zig-deez_py/tree/master/python/LICENSE
   :alt: License

.. image:: https://img.shields.io/pypi/v/deez_py.svg
   :target: https://pypi.org/project/deez_py/
   :alt: pypi version

**deez_py** is a python implementation of a lexical analyzer that provides comprehensive scanning, and lookahead capabilities. It also implements a parser that comes with awesome functionalities.

🛠️ Requirements
---------------

**deez_py** requires Python 3.10 or above.

To install Python 3.10.1, we recommend using `pyenv`_.

.. code-block:: bash

   # install pyenv
   git clone https://github.com/pyenv/pyenv ~/.pyenv

   # setup pyenv (you should also put these three lines in .bashrc or similar)
   # if you are using zsh
   cat << EOF >> ~/.zshrc
   # pyenv config
   export PATH="${HOME}/.pyenv/bin:${PATH}"
   export PYENV_ROOT="${HOME}/.pyenv"
   eval "$(pyenv init -)"
   EOF

   # or if you using the default bash shell, do this instead:
   cat << EOF >> ~/.bashrc
   # pyenv config
   export PATH="${HOME}/.pyenv/bin:${PATH}"
   export PYENV_ROOT="${HOME}/.pyenv"
   eval "$(pyenv init -)"
   EOF
   # Close and open a new shell session
   # install Python 3.10.1
   pyenv install 3.10.1

   # make it available globally
   pyenv global system 3.10.1


To manage the Python 3.10.1 virtualenv, we recommend using `poetry`_.

.. code-block:: bash

   # install poetry
   curl -sSL https://install.python-poetry.org | python3 -
   poetry --version
   Poetry \(version 1.5.1\)

   # Having the python executable in your PATH, you can use it:
   poetry env use 3.10.1

   # However, you are most likely to get the following issue:
   Creating virtualenv deez_py-dxc671ba-py3.10 in ~/.cache/pypoetry/virtualenvs

   ModuleNotFoundError

   No module named 'virtualenv.seed.via_app_data'

   at <frozen importlib._bootstrap>:973 in _find_and_load_unlocked

   # To resolve it, you need to reinstall virtualenv through pip
   sudo apt remove --purge python3-virtualenv virtualenv
   python3 -m pip install -U virtualenv

   # Now, you can just use the minor Python version in this case:
   poetry env use 3.10.1
   Using virtualenv: ~/.cache/pypoetry/virtualenvs/deez_py-dxc671ba-py3.10


🚨 Installation
---------------

With :code:`pip`:

.. code-block:: console

   python3 -m pip install deez-py


🚸 Usage
--------

.. code-block:: python3

   >>> from deez_py import Lexer
   >>> lex = Lexer('=+(){},;')
   >>> for _ in range(9):
   >>>     print(lex.get_next_token())
   ...
   Token(type=<TokenType.Equal: '='>, literal='=')
   Token(type=<TokenType.Plus: '+'>, literal='+')
   Token(type=<TokenType.LParen: '('>, literal='(')
   Token(type=<TokenType.RParen: ')'>, literal=')')
   Token(type=<TokenType.LSquirly: '{'>, literal='{')
   Token(type=<TokenType.RSquirly: '}'>, literal='}')
   Token(type=<TokenType.Comma: ','>, literal=',')
   Token(type=<TokenType.Semicolon: ';'>, literal=';')
   Token(type=<TokenType.Eof: 'EOF'>, literal='EOF')


👨‍💻 Development
------------------

For local development, you can install all dependencies by running:

.. code-block:: bash

   make install


🧪 Testing
------------------

.. code-block:: bash

   make test
   # or
   pytest -vv tests


💡 Tips
------------------

To run a subset of tests:

.. code-block:: bash

   make test
   make lint
   make coverage


🚀 Deploying
------------------

A reminder for maintainers on how to deploy. Run the following commands in order:

.. code-block:: bash

   bump2version patch # possible: major / minor / patch
   git push
   git push --tags
   make dist
   make release


📝 License
----------

Todo.

.. _pyenv: https://github.com/pyenv/pyenv
.. _poetry: https://github.com/python-poetry/poetry
