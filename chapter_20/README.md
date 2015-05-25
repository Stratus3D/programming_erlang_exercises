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
    3> erlang_tips:add_tip("google.com", "This site is a fake", undefined).
    {atomic,ok}
    4> erlang_tips:add_tip("google.com", "This site is a fake", undefined).
    {atomic,ok}
    5> erlang_tips:add_tip("stratus3d.com", "This site has tons of info on Erlang!", undefined).
    {atomic,ok}
    6> erlang_tips:all_tips().
    [{tip,"stratus3d.com",
          "This site has tons of info on Erlang!",undefined},
           {tip,"google.com","This site is a fake",undefined}]
           5>

