#Exercises for Chapter 3

##1. Memorize the commands on page 27.

##2. Execute the `help()` function in the shell

    $ erl
    Erlang R16B01 (erts-5.10.2) [source] [64-bit] [smp:8:8] [async-threads:10] [hipe] [kernel-poll:false]

    Eshell V5.10.2  (abort with ^G)
    1> help().
    ** shell internal commands **
    b()        -- display all variable bindings
    e(N)       -- repeat the expression in query <N>
    f()        -- forget all variable bindings
    f(X)       -- forget the binding of variable X
    h()        -- history
    history(N) -- set how many previous commands to keep
    results(N) -- set how many previous command results to keep
    catch_exception(B) -- how exceptions are handled
    v(N)       -- use the value of query <N>
    rd(R,D)    -- define a record
    rf()       -- remove all record information
    rf(R)      -- remove record information about R
    rl()       -- display all record information
    rl(R)      -- display record information about R
    rp(Term)   -- display Term using the shell's record information
    rr(File)   -- read record information from File (wildcards allowed)
    rr(F,R)    -- read selected record information from file(s)
    rr(F,R,O)  -- read selected record information with options
    ** commands in module c **
    bt(Pid)    -- stack backtrace for a process
    c(File)    -- compile and load code in <File>
    cd(Dir)    -- change working directory
    flush()    -- flush any messages sent to the shell
    help()     -- help info
    i()        -- information about the system
    ni()       -- information about the networked system
    i(X,Y,Z)   -- information about pid <X,Y,Z>
    l(Module)  -- load or reload module
    lc([File]) -- compile a list of Erlang modules
    ls()       -- list files in the current directory
    ls(Dir)    -- list files in directory <Dir>
    m()        -- which modules are loaded
    m(Mod)     -- information about module <Mod>
    memory()   -- memory allocation information
    memory(T)  -- memory allocation information of type <T>
    nc(File)   -- compile and load code in <File> on all nodes
    nl(Module) -- load module on all nodes
    pid(X,Y,Z) -- convert X,Y,Z to a Pid
    pwd()      -- print working directory
    q()        -- quit - shorthand for init:stop()
    regs()     -- information about registered processes
    nregs()    -- information about all registered processes
    xm(M)      -- cross reference check a module
    y(File)    -- generate a Yecc parser
    ** commands in module i (interpreter interface) **
    ih()       -- print help for the i module
    true
    2>

##3. Try representing a house as a tuple and a street as a list of houses (tuples).

So we are tasked with the job of representing a house as a tuple. A house could be represented many different ways in Erlang. Since the details are a little vague we will represent a house as a tuple of 4 elements taking this form: `{Address, NumberOfBedrooms, NumberOfBathrooms, ListOfAdditionalFeatures}`. Examples:

    {"1111 1st Street", 3, 2, [garage]}
    {"1113 1st Street", 3, 3, [pool, garage]}


Now that we have defined the structure of the house tuples, we can create lists of tuples to represent streets:

    [{"1111 1st Street", 3, 2, [garage]},
    {"1113 1st Street", 3, 3, [pool, garage]},
    {"1116 1st Street", 3, 2, []}]

