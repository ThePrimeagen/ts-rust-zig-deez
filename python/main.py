import getpass
from lexer.repl import Start


def main():
    username = getpass.getuser()
    print(f"Hello {username}! This is the Monkey programming language!\n")
    print("Feel free to type in commands\n")
    Start()


if __name__ == "__main__":
    main()
