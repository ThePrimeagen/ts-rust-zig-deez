package root;

public class Main {

    public static void main(String[] args) {
        String username = System.getProperty("user.name");

        System.out.printf("Hello %s! This is the Monkey programming language!%n", username);
        System.out.println("Feel free to type in commands!");

        REPL repl = new REPL(System.in, System.out);
        repl.start();
    }
}