%% @author http://erlang.org/doc/reference_manual/users_guide.html
%% @copyright Copyright © 2003-2019 Ericsson AB. All Rights Reserved.
%% @doc snippet from <a href="http://erlang.org/doc/reference_manual/users_guide.html"> user manual</a>

-module(ping_pong). 
-export([start/0, ping/2, pong/0]). 

ping(0, Pong_PID) ->
    Pong_PID ! finished,
    io:format("ping finished~n", []); 
ping(N, Pong_PID) ->
    Pong_PID ! {ping, self()},      % send ping e PID
    receive                         % bloccante
        pong ->
            io:format("Ping received pong~n", [])
    end,
    ping(N - 1, Pong_PID).          % recursive call

pong() ->
    receive
        finished ->
            io:format("Pong finished~n", []);
        {ping, Ping_PID} ->
            io:format("Pong received ping~n", []),
            Ping_PID ! pong,
            pong()              % loop until finished
    end. 
    
start() ->
    Pong_PID = spawn(ping_pong, pong, []),
    spawn(ping_pong, ping, [3, Pong_PID]).