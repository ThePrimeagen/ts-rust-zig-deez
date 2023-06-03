import javax.script.*;
import java.nio.file.*;
import java.util.stream.*;
import java.util.*;
class Loader {
    public static void main(String[] args) {
        try (Stream<Path> stream = Files.list(Paths.get("./scripts"))){
            List<String> files = stream
                    .map(Path::getFileName)
                    .map(Path::toString)
                    .sorted()
                    .collect(Collectors.toList());
            ScriptEngine engine = new ScriptEngineManager().getEngineByName("nashorn");
            for (String file: files) {
                Path path = Paths.get("./scripts/" + file);
                String content = Files.readAllLines(path).get(0);
                engine.eval(content);
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
}