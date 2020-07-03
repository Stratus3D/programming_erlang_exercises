# Exercises for Chapter 24

**1. Extend `adapter_db1` so that calling `adapter_db1:new(persistent)` creates a tuple module with a persistent data store.**

Solution in the [exercise_1](exercise_1/) directory.

Compile the Erlang files:

```
erlc *.erl
```

Run the tests in the shell:

```
$ erl
Erlang/OTP 20 [erts-9.3] [source] [64-bit] [smp:4:4] [ds:4:4:10] [async-threads:10] [hipe] [kernel-poll:false]

Eshell V9.3  (abort with ^G)
1> adapter_db1_test:test().
ok
```

**2. Write a key-value store that stores small values in memory and large values on disk. Make an module that makes this have the same interface as the previous adapter in this chapter.**

Solution in the [exercise_2](exercise_2/) directory.

Compile the Erlang files:

```
erlc *.erl
```

Run the tests in the shell:

```
$ erl
Erlang/OTP 20 [erts-9.3] [source] [64-bit] [smp:4:4] [ds:4:4:10] [async-threads:10] [hipe] [kernel-poll:false]

Eshell V9.3  (abort with ^G)
1> adapter_db1_test:test().
ok
```

**3. Make a key value store where some key-value pairs are persistent and others are transient. Calling `put(Key, memory, Val)` will store a key-value pair in memory, `put(Key, disk, Val)` should store the data on disk. Use a pair of processes to do this, one for the persistent store and one for the disk store. Reuse the earlier code in the chapter.**

Solution in the [exercise_3](exercise_3/) directory. It wasn't clear to me what the significance was between exercise 2 and exercise 3 other than the use of two processes. The processes themselves aren't really doing much as I chose to continue to use ETS and DETS tables to store the key value data. The gen_server implementation is a little unique in that it is effectively two different gen_server implementations in the same module. It worked out well enough, but I'm not sure it's a good pattern because there ended up being two distinct sets of code.

Compile the Erlang files:

```
erlc *.erl
```

Run the tests in the shell:

```
$ erl
Erlang/OTP 20 [erts-9.3] [source] [64-bit] [smp:4:4] [ds:4:4:10] [async-threads:10] [hipe] [kernel-poll:false]

Eshell V9.3  (abort with ^G)
1> adapter_db1_test:test().
ok
```
