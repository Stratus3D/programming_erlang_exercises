# Exercises for Chapter 18

**1. The process started in `shell1.erl` is very simple. If it crashes the web application cannot recover. Add error recovery to the application. Add a replay command feature.**

Solution to exercise in [exercise_1](exercise_1/) directory. I chose not to implement the replay feature as I didn't feel like I would learn much from implementing it. Somewhere (whether in the client or on the server) you'd need to store a list of previously executed commands and then expose a replay button to run them again.

You'll need to have rebar3 installed to run the exercise. In an Erl shell run:

```
rebar3 shell
1> webserver:start(2233).
```

Open `shell1.html` in your web browser. You can crash the shell by entering some expressions that crash it:

```
Starting Erlang shell:
1 > A = 1.
1
#> {{unbound_var,'B'},[{erl_eval,exprs,2,[]}]}
Starting Erlang shell:
```

**2. Read the code in `websockets.js` and trace what happens when you click the button in the browser. How does the message you get when you click the button find the Erlang processes involved?**

No code for this exercise.

The code in websocket.js from exercise 1 opens a connection to a websocket endpoint at `/websocket/shell1`. The connection code also draws buttons and inputs on the screen and binds them to callback functions. The `start_session` JavaScript function is then invoked, which binds other functions to the websocket callbacks exposed by the JavaScript WebSockets API. When a user interacts with a button or input field, websocket messages are sent via the callback functions. When the Erlang server receives the messages and sends the response via the socket, the `onMessage` callback is invoked and message is parsed and passed to the `do_cmds` function, which looks at the message and invokes the right function to handle it (for the `shell1` program this is only `append_div`).

**3. The IRC lite program is a fully functioning chat program. Check that it works. You may find it does not work because of firewalls or other network reasons. Find the specification of the real RFC protocol and you'll see it is much longer. Why is this?**

No code for this exercise.

There are a couple reasons for this. First is that the real IRC specification defines many more commands than the sample chat program. It has three commands, whereas the IRC spec defines over two dozen different commands. There are also other things the real IRC spec must define that don't need to be defined in the sample chat program. The sample chat program reuses existing protocols like websockets and makes assumptions about the client because we've written the JavaScript for it ourselves. The real IRC spec must define the details of the protocol and address things like DNS.

**4. The IRC program uses a centralized server. Could you change it so as to eliminate the central server and use a network of peers? How about adding SVG graphics to the client and using the audio interface in HTML5 to send and receive sounds?**

I chose not to write code for this exercise.

You could change from a centralized server to a federated system where users connect to their own server, and the servers broadcast messages to each other. This would introduce a lot more complexity as servers themselves would have to be able to find each other on the network and then subscribe to messages from all the other servers in order to relay them to the users using each instance. Adding SVG graphics and audio clips would probably be easier, as they would just be new message types that would be displayed different on the webpage client.
