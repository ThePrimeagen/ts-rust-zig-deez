
Quick start:
---

```prompt
$ make docker-repl
...
repl
> 1+2
3
```


Extensions:
----

- Boolean Operators:

	```js
	> true && ~false
	true
	```

- Modulo Operator:
	```js
	> 11 % 4
	3
	```

- Binary Operations:
	```js
	> 127 | 128
	255

	> 11 & ~6
	9

	> ~0
	-1

	> 13 ^ 15
	2

	```

- Array Operations

	```js
	> [1,2,3] == [4,5,6]
	false

	> [1,2,3] == [1,2,3]
	true

	> [1,2,3] + [4,5,6]
	[1,2,3,4,5,6]
	```

- Hash Operations

	```js
	> {"a":true} == {"a":false}
	false

	> {"a":true} == {"b":true}
	false

	> {"a":true} == {"a":true}
	true

	> {"a":true} + {"b":false}
	{"b":false,"a":true}
	```

- Integer-to-string conversion
	```js
	> "pi == " + 3 + ", approximately"
	"pi == 3, approximately"
	```

- String Builtin Overloads:
	```js
	> first("Hello")
	"H"

	> rest("Hello")
	"ello"
	```

- Array-of-'char' printing
	```js
	> ["a", "b", "c"]
	"abc"

	> reverse("...si eman eht")
	"the name is..."
	```

- Infix operators
	```js
	> $+
	fn(x,y)

	> $+(1,2)
	3
	```

Examples:
---
- prelude functions:

	```js
	> map(fn(x) "number " + x, [1,2,3,4,5])
	["number 1","number 2","number 3","number 4","number 5"]

	> foldl1($+, [1,2,3,4,5])
	15

	> foldl1($+, map(fn(x) x+" ", ["the", "name", "is", "..."])) + "ThePrimeagen"
	"the name is ... ThePrimeagen"

	> sort([5,4,3,2,1])
	[1,2,3,4,5]

	> take(3, [1,2,3,4,5])
	[1,2,3]

	> hanoi(3)
		move the top disc from A to B
		move the top disc from A to C
		move the top disc from B to C
		move the top disc from A to B
		move the top disc from C to A
		move the top disc from C to B
		move the top disc from A to B
	```

- Quicksort:

	```js
	> let qsort = fn(xs) if (len(xs)==0) [] else { let p=xs[0]; qsort(filter(fn(a) a<p, xs)) + [p] + qsort(filter(fn(a) a>p, xs)) }
	fn(xs)

	> qsort([9,3,7,4,6,1,8,2,5])
	[1,2,3,4,5,6,7,8,9]
