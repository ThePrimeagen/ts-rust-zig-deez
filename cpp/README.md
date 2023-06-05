

```
$ make docker-repl
...
repl
> let qsort = fn(xs) if (len(xs)==0) [] else { let p=xs[0]; qsort(filter(fn(a){a<p}, xs)) + [p] + qsort(filter(fn(a){a>p}, xs)) }
fn(xs)

> qsort([9,3,7,4,6,1,8,2,5])
[1,2,3,4,5,6,7,8,9]
```
