%% @author http://erlang.org/doc/reference_manual/users_guide.html
%% @copyright Copyright © 2003-2019 Ericsson AB. All Rights Reserved.
%% @doc snippet from <a href="http://erlang.org/doc/reference_manual/users_guide.html"> user manual</a>

-module(leap).
-export([month_length/2, start/0]). 

month_length(Year, Month) ->
    %% All years divisible by 400 are leap
    %% Years divisible by 100 are not leap (except the 400 rule above)
    %% Years divisible by 4 are leap (except the 100 rule above)

    Leap = if
        trunc(Year / 400) * 400 == Year ->
            leap;
        trunc(Year / 100) * 100 == Year ->
            not_leap;
        trunc(Year / 4) * 4 == Year ->
            leap;
        true ->
            not_leap
    end, 
    case Month of
        sep -> 30;
        apr -> 30;
        jun -> 30;
        nov -> 30;
        feb when Leap == leap -> 29;
        feb -> 28;
        jan -> 31;
        mar -> 31;
        may -> 31;
        jul -> 31;
        aug -> 31;
        oct -> 31;
        dec -> 31
    end. 

start() ->
    io:fwrite("~w~n", [leap:month_length(2004, feb)]), 
    io:fwrite("~w~n", [leap:month_length(2003, feb)]), 
    io:fwrite("~w~n", [leap:month_length(1947, aug)]). 