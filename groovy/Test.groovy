/**
 * a simple unit test framework
 */
class Test {

    static void run(String description, Closure closure) {
        String msg = description
        try {
            closure()
            msg += ': \u001B[92mpassed\u001B[0m'
        } catch (AssertionError e) {
            msg += ': \u001B[91mfailed\u001B[0m'
            println e
        }
        println msg
    }

    /**
     * runs the unit tests
     * looks in every directory in the current directory
     * for a *.test.groovy file and runs it.
     */
    static void main(String[] args) {
        File dir = new File('.')
        File[] files = dir.listFiles()
        files.each { file ->
            if (file.isDirectory()) {
                File[] testFiles = file.listFiles { f -> f.name.endsWith('Test.groovy') }
                testFiles.each { testFile ->
                    GroovyShell shell = new GroovyShell()
                    shell.evaluate(testFile)
                }
            }
        }
    }

}
