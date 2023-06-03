import javax.script.*;
import java.nio.file.*;
import java.util.stream.*;
import java.util.*;
class Loader {
    static String entry = "" +
            "function enter(x) {" +
            "return null;" +
            "}";
    public static void main(String[] args) {
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
            Object result = invocable.invokeFunction("enter", "");
            System.out.println(result);
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
}