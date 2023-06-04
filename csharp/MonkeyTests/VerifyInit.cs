using System.Runtime.CompilerServices;

namespace MonkeyTests;

public static class VerifyInit
{
    [ModuleInitializer]
    public static void Initialize() =>
        UseProjectRelativeDirectory("_snapshots");
}