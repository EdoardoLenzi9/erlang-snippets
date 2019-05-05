%% @author http://erlang.org/doc/reference_manual/users_guide.html
%% @copyright Copyright © 2003-2019 Ericsson AB. All Rights Reserved.
%% @doc snippet from <a href="http://erlang.org/doc/reference_manual/users_guide.html"> user manual</a>


-module(ping_pong_robust). 
-export([start/1,  ping/2, pong/0]). 


ping(N, Pong_Pid) ->
    link(Pong_Pid),         % creates a bidirectional link with the spawned process
    ping1(N, Pong_Pid). 
    

ping1(0, _) ->
    exit(ping);             % launches an abnormal exit signal 

ping1(N, Pong_Pid) ->
    Pong_Pid ! {ping, self()},
    receive
        pong ->
            io:format("Ping received pong~n", [])
    end,
    ping1(N - 1, Pong_Pid). 
    

pong() ->
    process_flag(trap_exit, true),      % override default signal behaviour
    pong1(). 


pong1() ->
    receive
        {ping, Ping_PID} ->
            io:format("Pong received ping~n", []),
            Ping_PID ! pong,
            pong1();
        {'EXIT', From, Reason} ->       % exit signal is converted into this tuple and managed
            io:format("pong exiting, got ~p~n", [{'EXIT', From, Reason}])   
        % process ends (skips the recursive call)
    end. 
    

start(Ping_Node) ->                     % name of any node
    PongPID = spawn(?MODULE, pong, []),
    spawn(Ping_Node, ?MODULE, ping, [3, PongPID]). 
    %spawn_link(Ping_Node, ?MODULE, ping, [3, PongPID]).