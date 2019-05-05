%%% Configure the location of the server node, 
% -define(server_node, root@server). 

server_node() ->
    {ok, HostName} = inet:gethostname(),
    %io:format("server@~w", [list_to_atom(HostName)]).
    list_to_atom(lists:concat(["server@", HostName])). 