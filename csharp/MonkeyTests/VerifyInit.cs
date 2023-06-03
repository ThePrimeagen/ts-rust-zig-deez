using System.Runtime.CompilerServices;

namespace MonkeyTests;

public static class StaticSettingsUsage
{
    [ModuleInitializer]
    public static void Initialize() =>
        UseProjectRelativeDirectory("_snapshots");
}