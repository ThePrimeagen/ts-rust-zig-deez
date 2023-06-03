``` ini

BenchmarkDotNet=v0.13.5, OS=Windows 10 (10.0.19045.2965/22H2/2022Update)
Intel Core i7-7700 CPU 3.60GHz (Kaby Lake), 1 CPU, 8 logical and 4 physical cores
.NET SDK=7.0.400-preview.23225.8
  [Host]     : .NET 7.0.5 (7.0.523.17405), X64 RyuJIT AVX2
  DefaultJob : .NET 7.0.5 (7.0.523.17405), X64 RyuJIT AVX2


```
|        Method |        Mean |     Error |    StdDev |         Min |         Max |   Gen0 | Allocated |
|-------------- |------------:|----------:|----------:|------------:|------------:|-------:|----------:|
| SmallCodeTest |    888.8 ns |  11.91 ns |  11.14 ns |    875.1 ns |    906.5 ns | 0.2594 |   1.06 KB |
| LargeCodeTest | 21,557.1 ns | 223.87 ns | 198.46 ns | 21,137.2 ns | 21,704.3 ns | 6.1340 |  25.06 KB |
