package repl
import "core:fmt"
import "core:os"
import "core:bufio"
import "../lexer"

main :: proc() {
	start(os.stdin)
}
start :: proc(input: os.Handle) {
	buf: [1024]byte
	l := lexer.init_lexer("")
	defer lexer.destroy_lexer(&l)
	for {
		fmt.print(">> ")
		n, err := os.read(os.stdin, buf[:])
		if err < 0 {fmt.panicf("Error: %#v", err)}
		l.data = buf[:n]
		lexer.scan_tokens(&l)
		if len(l.tokens) > 1 { 	// dont print if only the EOF
			for t in l.tokens {fmt.printf("%#v\n", t)}
		}


		lexer.reset_lexer(&l)
	}
}
