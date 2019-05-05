# CheatSheet


## Naming Conventions

Git
* branch-name

Erlang
* modules_names (snake_case)
* function_names (snake_case)


## Tags

* `-module(tut).` 
* `-export([double/1]).` 
* `-import(module).`
* `-behaviour(application).` 


## Commands

* run function from bash `erl -run module function`
* shutdown system   `halt()`
* compile `c(module_name)`
* see exception at line 12  `v(12).`
* wildcard `_`
* void function returns `ok` atom
* comparison operators (<, >, >= geq, =< leq, **== equal**, **/= not equal**)
* variables are immutable
* higer order functions (return a function)

```{erl}
    Xf = fun(X) -> X * 2 end. 
    Xf(5).


```


## Data Structures

* **tuples** `{element1, element2, ...}`
    * pattern matching `{X, Y} = {paris, {f, 28}}.`
* **lists** `[element1, element2, ...]` (allows heterogeneous elements)
    * `[First | Rest] = [1,2,3,4]`, `First = 1` and `Rest = [2,3,4]`
    * `[E1, E2 | Rest] = [1,2,rest].`
    * `[A, B | C] = [1, 2].`, `C = []`

**Foreach** takes a list and apply a fun to every element and **map** creates a new list by applying a function to every element. 

```
    %%% MAP
    Add_3 = fun(X) -> X + 3 end. 
    lists:map(Add_3, [1,2,3]). 
    > [4,5,6] 

    %%% FOREACH - run side effects
    Print_City = fun({City, {X, Temp}}) -> io:format("~-15w ~w ~w~n", [City, X, Temp end.
    lists:foreach(Print_City, [{moscow, {c, -10}}, {cape_town, {f, 70}}]). 
```

**Sort** allows to define your sorting criterion with a fun
```
    lists:sort(
        fun({_, {c, Temp1}}, {_, {c, Temp2}}) -> Temp1 < Temp2 end, 
        [ {cape_town, {c, 10}}, {moscow, {c, 15}} ] ). 
```

List of tuples have **keysomething** operations like:

```
    lists:keymember(a, 2, [{b,a,c}, {x,y,z}]).  % works on lists of tuples
    > true

    lists:keysearch(a, 2, [{b,a,c}, {x,y,z}]).  % the same of keymember with tuple return
    > {value,{b,a,c}}
    
    lists:keydelete(a, 2, [{p, p}, {b,a,c}, {x,y,z}]).
    >[{p,p},{x,y,z}]
```

* **map** (dictionary)
    * assign with => `Dst = #{ "key" => 42 }.`
    * update with := `Dst#{red := 1, green := 2, ... }; ` 
    * pattern matching with := 

```{erl}
alpha( #{alpha := SA}, #{alpha := DA} ) ->
    SA + DA * (1.0 - SA). 
```


## Flow Control

* **if**

```{erl}
if 
    A > B, B > C ->     % AND
        io:format("side effect", []),
        greater; 
    A < B ; B > C ->    % OR
        less;
    A == B ->
        equal;
    true ->
        error
end.
```

* **case**

```{erl}
case Length of
    {centimeter, X} ->
        {inch, X / 2.54};
    {inch, Y} ->
        {centimeter, Y * 2.54}
end. 
```

### Macros

* **By repalce**

```{erl}
-define(placeholder, replacement)
```

* **By function**

```{erl}
-define(is_channel(V), (is_float(V) andalso V >= 0.0 andalso V =< 1.0)). 

%%% evaluation using ?placeholder
new(R,G,B,A) when ?is_channel(R) -> ... 
```

> Useful with guards cause guards don't allow normal function evaluation

* `?MODULE.` the name of the current module
* `?FILE.` the current file name.
* `?LINE.` the current line number.
* `?MACHINE.` the current machine name {'JAM', 'BEAM' or 'VEE'}


### io:format

```
    io:format("~w~n", [hello]). 
```

* `~w` word
    * `~-15w` print the "term" with a field length (width) of 15 and left justify it
* `~p`
* `~n` new line

```{erl}
is_atom(io:format("text ~p ~p", [atom, "string"])).
> text atom "string" true
is_atom(io:format("text ~p ~p", [atom, list_to_atom("string")])).
> text atom string true      
```


### BIFs 

Built in functions (ie. `trunc` is equivalent to `erlang:trunc`)

* `trunc` taglia i decimali
* `rem` resto divisione intera
* `round` arrotonda per eccesso
* `float(4)`
* `is_atom(hello)`
* `atom_to_list(hello)`


## Best Practices

### Tail recursion


<br/>
------------------------
# Concurrent programming


## Spawn 

> In erlang each thread of execution is called a process
>> Processes are independent, concurrent and don't share data
>> Threads are processes that share data in some way

* `spawn` create a process that execute a function, ***returns PID**
* `self()` returns current PID

```{erl}
-module(processes)

say_something(What, 0) ->
    done; 
say_something(What, Times) ->
    io:format("~p~n", [What]),
    say_something(What, Times - 1). 

start() ->
    spawn(processes, say_something, [hello, 3]),
    spawn(processes, say_something, [goodbye, 3]),
```


## Send Messages

Send messages through threads using `!` **send operator**

```{erl}
    Pong_PID ! {ping, self()}.
```

> **Receive** is blocking

```{erl}
receive
    finished ->
        io:format("Pong finished~n", []);
    {ping, Ping_PID} ->
        io:format("Pong received ping~n", []),
        Ping_PID ! pong,pong() 
end. 
```


## Register 

> **Register** used to send messages without knowing the PID

```{erl}
    register(pong, spawn(?MODULE, pong, [])).
    ...
    pong ! finished.

    whereis(pong).      % {undefined, _}
```

<br/>
-------------------------
# Distributed Programming

Erlang systems in order to talk with each others requires a **magic cookie**; use a file called `.erlang.cookie` in the home direcotory

```{sh}
$ cd 
$ cat > .erlang.cookie 
this_is_very_secret 
$ chmod 400 .erlang.cookie      
# requirement, only the file owner can access the file content
```

Start **erlang with a name**:   

```{sh}
erl -sname my_name
``` 

In this way u can run multiple nodes on the same system

```
erl -sname pong
ping_pong:start_pong().  
node()                      % get node name

erl -sname ping 
ping_pong:start_ping(node_name). 
```