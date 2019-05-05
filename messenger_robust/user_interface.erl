%% @author http://erlang.org/doc/reference_manual/users_guide.html
%% @copyright Copyright © 2003-2019 Ericsson AB. All Rights Reserved.
%% @doc snippet from <a href="http://erlang.org/doc/reference_manual/users_guide.html"> user manual</a>


%%% User interface to the messenger program 
%%% login(Name) 
%%%     One user at a time can log in from each Erlang node in the 
%%%     system messenger: and choose a suitable Name. If the Name 
%%%     is already logged in at another node or if someone else is 
%%%     already logged in at the same node, login will be rejected 
%%%     with a suitable error message. 

%%% logoff() 
%%%     Logs off anybody at that node 

%%% message(ToName, Message) 
%%%     sends Message to ToName. Error messages if the user of this 
%%%     function is not logged on or if ToName is not logged on at 
%%%     any node. 


-module(user_interface). 
-export([logonA/0, logonB/0, logon/1, logoff/0, message/2]). 
-include("mess_interface.hrl"). 
-include("mess_config.hrl"). 


logonA() ->
    logon(a).


logonB() ->
    logon(b).


logon(Name) ->
    case whereis(mess_client) of 
        undefined ->
            register(mess_client, spawn(mess_client, client, [server_node(), Name]));
        _ -> 
            already_logged_on
    end. 


logoff() ->
    mess_client ! logoff. 


message(ToName, Message) ->
    case whereis(mess_client) of % Test if the client is running
        undefined ->
            not_logged_on;
        _ -> 
            mess_client ! #message_to{to_name=ToName, message=Message},
            ok 
    end. 