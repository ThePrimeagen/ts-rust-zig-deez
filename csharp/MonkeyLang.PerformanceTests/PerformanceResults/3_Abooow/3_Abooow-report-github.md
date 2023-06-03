``` ini

BenchmarkDotNet=v0.13.5, OS=Windows 10 (10.0.19045.2965/22H2/2022Update)
Intel Core i7-7700 CPU 3.60GHz (Kaby Lake), 1 CPU, 8 logical and 4 physical cores
.NET SDK=7.0.400-preview.23225.8
  [Host]     : .NET 7.0.5 (7.0.523.17405), X64 RyuJIT AVX2
  DefaultJob : .NET 7.0.5 (7.0.523.17405), X64 RyuJIT AVX2


```
|        Method |      Mean |     Error |    StdDev |       Min |       Max |   Gen0 | Allocated |
|-------------- |----------:|----------:|----------:|----------:|----------:|-------:|----------:|
| SmallCodeTest |  1.250 μs | 0.0042 μs | 0.0040 μs |  1.243 μs |  1.256 μs | 0.1583 |     664 B |
| LargeCodeTest | 30.897 μs | 0.3221 μs | 0.3013 μs | 30.314 μs | 31.177 μs | 3.9673 |   16600 B |
