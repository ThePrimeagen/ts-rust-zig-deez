I am sorry.

Btw, I still have no idea how to properly separate COBOL into multiple files.
That's why I put things that go into DATA and PROCEDURE divisions in separate
files, so i can #include them (`COPY` statement) into the correct position.
Also, this separation is completely arbitrary, because I could put everything
into a single file (and it would be easier).

# Running

```
sudo apt install gnucobol4     # I assume you don't have that
cobc -x tests.cbl
./tests
```

I don't know how docker works, sorry ¯\\\_(ツ)_/¯
