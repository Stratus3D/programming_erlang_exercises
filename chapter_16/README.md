# Exercises for Chapter 16

**1. Write a program that compares the last modified timestamp of a .erl file and its .beam counterpart**

Solution in the `exercise_1/` directory. I put the code in an escript so it is easy to use on the command line.

```bash
$ ./stale_beam ../../chapter_15/exercise_3/cpu_info.erl
```

**2. Use erlang:md5/1 to compute the checksum of a small file**

Solution in the `exercise_2/` directory. I put the code in an escript so it is easy to use on the command line.

```bash
$ ./emd5sum emd5sum
07357C74F0992326718D133D7DDD5F01
```

**3. Repeat the previous exercise for a large file (a few hundred megabytes). Read the file in chunks and use `erlang:md5_init`, `erlang:md5_update` and `erlang:md5_final` to compute the sum.**

Solution in the `exercise_3/` directory. I put the code in an escript so it is easy to use on the command line.

```bash
# Generate a large file
$ fallocate -l 256mb exercise_3/bigfile.bin
# Compute md5 sum
$ ./emd5sum emd5sum
07357C74F0992326718D133D7DDD5F01
```

**4. Use the `lib_find` module to find all `.jpg` files on your computer. Check for identical files by computing the MD5 sum of each file and comparing the computed sums.**

**5. Write a caching mechanism that computes the MD5 sum of a file and stores it with the last modified time of the file. When the sum is requested check if the file has changed and return the cached sum if it hasn't**

Solution in the `exercise_5/` directory.

```
$ erlc md5_cache.erl
$ erl
Erlang/OTP 19 [erts-8.2] [source] [64-bit] [smp:4:4] [async-threads:10] [hipe] [kernel-poll:false]
Eshell V8.2  (abort with ^G)
1> md5_cache:start_link().
{ok,<0.66.0>}
2> md5_cache:get_md5("md5_cache.erl").
MD5 hash is not up to date
{ok,<<230,12,220,207,212,102,157,69,193,161,120,23,149,
      187,231,52>>}
3> md5_cache:get_md5("md5_cache.erl").
MD5 hash is up to date
{ok,<<230,12,220,207,212,102,157,69,193,161,120,23,149,
      187,231,52>>}
```

**6. Tweets are exactly 140 bytes long. Write a random access twit storage module that exports the following functions: `init(N)` allocates space for N number of tweets. `store(N, Buf)` to store tweet (140 byte Buf) at N location. `fetch(N)` fetches the data for tweet number N.**

Solution in the `exercise_6/` directory.
```
erl
1> tweet_store:init(3).
ok
2> tweet_store:store(2, <<"A short tweet">>).
ok
3> tweet_store:store(3, <<"Another short tweet">>).
ok
4> tweet_store:fetch(2).
<<"A short tweet">>
5> tweet_store:fetch(3).
<<"Another short tweet">>
6> tweet_store:fetch(1).
<<>>
7>
```
