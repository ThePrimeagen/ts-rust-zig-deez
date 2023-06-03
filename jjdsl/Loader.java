import javax.script.*;
import java.nio.file.*;
import java.util.stream.*;
import java.util.*;
class Loader {
    static String entry = "" +
            "function enter(x) {" +
            "return \"Enter was Called with no Issues\";" +
            "}";
    static boolean runTest = false;
    public static void main(String[] args) {
        if (args.length > 0 ) {
            runTest = true;
        }
        try (Stream<Path> stream = Files.list(Paths.get("./scripts"))){
            List<String> files = stream
                    .map(Path::getFileName)
                    .map(Path::toString)
                    .sorted()
                    .collect(Collectors.toList());
            ScriptEngine engine = new ScriptEngineManager().getEngineByName("nashorn");
            for (String file: files) {
                StringBuilder monkey = new StringBuilder();
                Path path = Paths.get("./scripts/" + file);
                for (String line : Files.readAllLines(path)) {
                    monkey.append(line);
                    monkey.append("\n");
                }
                engine.eval(monkey.toString());
            }
            engine.eval(entry);
            Invocable invocable = (Invocable) engine;
            String functionCall = runTest ? "test" : "enter";
            Object result = invocable.invokeFunction(functionCall, "");
            System.out.println(result);
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
}