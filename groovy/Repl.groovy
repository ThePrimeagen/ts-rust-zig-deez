import lexer.Lexer
import lexer.Token
import lexer.TokenType

/**
 * REPL for the interpreter
 */
class Repl {

    static String helpMessage = '''
.help - show this message
.exit - exit the REPL
    '''

    static void main(String[] args) {
        BufferedReader reader = new BufferedReader(new InputStreamReader(System.in))

        println 'welcome to deez'
        println 'type ".help" for more info'

        String exitMessage = '\n...DEEZ NUTS on your chin lol bye'

        while (true) {
            print '🥜 '

            String input = reader.readLine()

            if (input == '.help') {
                println helpMessage
                continue
            }

            if (input == '.exit' || input == null) {
                break
            }

            Lexer lexer = new Lexer(input)

            Token token = lexer.nextToken()
            while (token.type != TokenType.EOF) {
                println "type: ${token.type}, literal: ${token.literal}"
                token = lexer.nextToken()
            }
        }

        println exitMessage
    }

}
