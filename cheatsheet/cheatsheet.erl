%% @author http://erlang.org/doc/reference_manual/users_guide.html
%% @copyright Copyright © 2003-2019 Ericsson AB. All Rights Reserved.
%% @doc snippet from <a href="http://erlang.org/doc/reference_manual/users_guide.html"> user manual</a>

-module(cheatsheet).

-export([fac/1]).
-export([convert/2]). 
-export([convert_length/1]). 


% -module(factorial).

fac(1) ->
    1;
fac(N) ->
    N * fac(N - 1).


%- module(atoms)

convert(M, inch) ->     
    M / 2.54; 
convert(N, centimeter) ->
    N * 2.54. 


% -module(tuples)

convert_length({centimeter, X}) ->
    {inch, X / 2.54}; 
convert_length({inch, Y}) ->
    {centimeter, Y * 2.54}. 


% -module(nested tuples). 
    
format_temps([])->                        
    ok;                                     % void function
format_temps([City | Rest]) ->
    print_temp(convert_to_celsius(City)),
    format_temps(Rest). 
    
convert_to_celsius({Name, {c, Temp}}) ->  
    {Name, {c, Temp}}; 
convert_to_celsius({Name, {f, Temp}}) ->  
    {Name, {c, (Temp - 32) * 5 / 9}}. 
            
print_temp({Name, {c, Temp}}) ->
    io:format("~-15w ~w c~n", [Name, Temp]). 


% -module(lists)

list_tut() ->
    [First | TheRest] = [1,2,3,4,5],
    % First = 1, TheRest = [2,3,4,5]
    
    [E1, E2 | R] = [1,2,3,4,5,6,7],

    [A, B | C] = [1, 2]. 
    % C = []

list_pattern_matching_cattivo([{Name, {c, Temp}} | Rest]) ->
    io:format("~-15w ~w c~n", [Name, Temp]).

list_length([]) ->
    0;    
list_length([First | Rest]) ->
    1 + list_length(Rest). 


% -module(maps)

maps_tut() ->
    dict = #{ "key" => 42 }. 


% -module(if)

test_if(A, B) ->
    if 
        A == 5 ->
            io:format("A == 5~n", []),
            a_equals_5;
        B == 6 ->
            io:format("B == 6~n", []),
            b_equals_6;
        A == 2, B == 3 -> 
            %That is A equals 2 and B equals 3
            io:format("A == 2, B == 3~n", []),
            a_equals_2_b_equals_3;
        A == 1 ; B == 7 ->                     
            %That is A equals 1 or B equals 7
            io:format("A == 1 ; B == 7~n", []),
            a_equals_1_or_b_equals_7;
        true ->
            default %no ; 
    end. 


% -module(case)

test_case(Length) ->
    case Length of
        {centimeter, X} ->
            {inch, X / 2.54};
        {inch, Y} ->
            {centimeter, Y * 2.54}
    end. 

%%% Both if and case have return value

