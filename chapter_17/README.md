# Exercises for Chapter 17

**1. Modify the code for `nano_get_url` by adding the appropriate headers where necessary and performing redirects if needed in order to fetch any webpage**

Solution to exercise in [exercise_1](exercise_1/) directory.

```
erl
1> nano_get_url:get_url().
Redirected to: http://www.google.com/webhp
{"200",
 [{"date","Tue, 19 May 2020 12:56:02 GMT"},
  {"expires","-1"},
  {"cache-control","private, max-age=0"},
  {"content-type","text/html; charset=ISO-8859-1"},
  {"p3p",
   "CP=\"This is not a P3P policy! See g.co/p3phelp for more info.\""},
  {"server","gws"},
  {"x-xss-protection","0"},
  {"x-frame-options","SAMEORIGIN"},
  {"set-cookie",
   "1P_JAR=2020-05-19-12; expires=Thu, 18-Jun-2020 12:56:02 GMT; path=/; domain=.google.com; Secure"},
  {"set-cookie",
   "NID=204=0DCMqJF2Ile8j7aDSW3HPuCPI7mGgttfvnahCPE_bl4ViwbvXd6wa6e-pw4dC_4hmlkaaRZSn6to3int8t8WKylCs9IsZYGoBo63tloqAGx5Llldh1MRs66OHqFzY_DUsg9BWfbBiLwqckyYatgYO3hMji8QkaWwgcKlsOcKUek; expires=Wed, 18-Nov-2020 12:56:02 GMT; path=/; domain=.google.com; HttpOnly"},
  {"accept-ranges","none"},
  {"vary","Accept-Encoding"}],
 <<"<!doctype html><html itemscope=\"\" itemtype=\"http://schema.org/WebPage\" lang=\"en\"><head><meta content=\"Se"...>>}
2>
```

**2. Enter the cdoe for a simple TCP server on page 267. Modify the code to receive a `{Mod, Func, Args}` tuple instead of a string. Compute `Reply = apply(Mod, Func, Args)` and send the value back to the socket. Write equivalent client code that encodes the `Mod`, `Func`, and `Args` in a form understood by the server.**

Solution to the exercise in the [exercise_2](exercise_2/) directory.

In one `erl` shell run the server:

```
erl
1> nano_server:start().
Server received binary = <<131,104,3,100,0,6,101,114,108,97,110,103,100,0,13,
                           108,105,115,116,95,116,111,95,116,117,112,108,101,
                           108,0,0,0,1,107,0,1,5,106>>
Server (unpacking)  {erlang,list_to_tuple,[[5]]}
Server replying = {5}
Server socket closed
```

And in another run the client:

```
erl
1> nano_client:send(erlang, list_to_tuple, [[5]]).
Client received binary = <<131,104,1,97,5>>
Client result = {5}
```

**3. Repeat the previous exercise using UDP.**

**4. Add a layer of cryptography by encoding the binary before sending it to the outgoing socket and decoding it after it is received on the incoming socket.**

**5. Make a simple "email-like" system. use Eralng terms as messages and store them in `$HOME/mbox`.**
