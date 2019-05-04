%% @author http://erlang.org/doc/reference_manual/users_guide.html
%% @copyright Copyright © 2003-2019 Ericsson AB. All Rights Reserved.
%% @doc snippet from <a href="http://erlang.org/doc/reference_manual/users_guide.html"> user manual</a>

-module(ping_pong_dist). 
-export([start_ping/0, start_ping/1, start_pong/0,  ping/2, pong/0]). 

ping(0, Pong_Node) ->
    {pong, Pong_Node} ! finished,
    io:format("ping finished~n", []); 
ping(N, Pong_Node) ->
    {pong, Pong_Node} ! {ping, self()},
    receive
        pong ->
            io:format("Ping received pong~n", [])
    end,
    ping(N - 1, Pong_Node). 
    
pong() ->
    receive
        finished ->
            io:format("Pong finished~n", []);
        {ping, Ping_PID} ->
            io:format("Pong received ping~n", []),
            Ping_PID ! pong,
            pong()
    end. 
    
start_pong() ->
    register(pong, spawn(?MODULE, pong, [])). 
    
start_ping(Pong_Node) ->
    spawn(?MODULE, ping, [3, Pong_Node]). 

start_ping() ->
    spawn(?MODULE, ping, [3, pong@eddy]). 
    

%% erl -sname ping 
%%ping_pong:start_ping(). 

%%erl -sname pong
%%ping_pong:start_pong().  