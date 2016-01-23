#Exercises for Chapter 20

##1. Build a database for a website. Database needs to have three tables: users, tips and abuse.
`erlang_tips.erl` is stored in `exercise_1/`. Example usage:

    1> erlang_tips:module_info(exports).
    [{all_users,0},
     {get_user,1},
     {add_user,3},
     {all_tips,0},
     {get_tip,1},
     {add_tip,3},
     {all_abuse,0},
     {get_abuse,1},
     {add_abuse,2},
     {start_database,0},
     {create_database,0},
     {reset_tables,0},
     {module_info,0},
     {module_info,1}]
    2> erlang_tips:create_database().
    ok
    3> erlang_tips:add_tip("google.com", "Use this site to find stuff", undefined).
    {atomic,ok}
    4> erlang_tips:add_tip("google.com", "Use this site to find stuff", undefined).
    {atomic,ok}
    5> erlang_tips:add_tip("stratus3d.com", "This site has tons of info on Erlang!", undefined).
    {atomic,ok}
    6> erlang_tips:all_tips().
    [{tip,"stratus3d.com",
          "This site has tons of info on Erlang!",undefined},
           {tip,"google.com","This site is a fake",undefined}]
           5>

##2. Take the previous exercise and get it run with ram and disk copies on two nodes.
`erlang_tips.erl` is stored in `exercise_2/`. To run the `erlang_tips` database on two nodes open up two terminal sessions and start two Erlang nodes. The first:

    cd exercise_2/
    erl -sname node1@localhost -setcookie cookie

The second:

    cd exercise_2/
    erl -sname node2@localhost -setcookie cookie

Note that the node names are hardcoded at the top of the `exercise_2/erlang_tips.erl` module. If you need to change them alter the `NODE_NAMES` macro (as well as the commands shown above). Once both nodes are running create the database and run a few commands on the first node:

    (node1@localhost)1> erlang_tips:create_database().
    ok
    (node1@localhost)2> erlang_tips:add_tip("stratus3d.com", "This site contains a lot of great articles on Erlang!", undefined).
    {atomic,ok}
    (node1@localhost)3> erlang_tips:add_tip("google.com", "Use this site to find stuff", undefined).
    {atomic,ok}
    (node1@localhost)4> erlang_tips:add_tip("google.com", "Use this site to find stuff", undefined).
    {atomic,ok}
    (node1@localhost)5>

Then run the `all_tips` function to retrieve the tips on node 2:

    (node2@localhost)1> erlang_tips:all_tips().
    [{tip,"stratus3d.com",
        "This site contains a lot of great articles on Erlang!",
        undefined},
    {tip,"google.com","Use this site to find stuff",undefined}]
    (node2@localhost)2>
