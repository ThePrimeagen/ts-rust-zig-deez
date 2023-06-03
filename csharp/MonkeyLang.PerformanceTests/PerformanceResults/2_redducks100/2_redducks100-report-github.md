``` ini

BenchmarkDotNet=v0.13.5, OS=Windows 10 (10.0.19045.2965/22H2/2022Update)
Intel Core i7-7700 CPU 3.60GHz (Kaby Lake), 1 CPU, 8 logical and 4 physical cores
.NET SDK=7.0.400-preview.23225.8
  [Host]     : .NET 7.0.5 (7.0.523.17405), X64 RyuJIT AVX2
  DefaultJob : .NET 7.0.5 (7.0.523.17405), X64 RyuJIT AVX2


```
|        Method |      Mean |     Error |    StdDev |       Min |       Max |   Gen0 | Allocated |
|-------------- |----------:|----------:|----------:|----------:|----------:|-------:|----------:|
| SmallCodeTest |  2.157 μs | 0.0227 μs | 0.0212 μs |  2.123 μs |  2.181 μs | 0.1450 |     616 B |
| LargeCodeTest | 55.294 μs | 0.5759 μs | 0.5387 μs | 54.426 μs | 56.098 μs | 3.6621 |   15400 B |
