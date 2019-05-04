%% @author http://erlang.org/doc/reference_manual/users_guide.html
%% @copyright Copyright © 2003-2019 Ericsson AB. All Rights Reserved.
%% @doc snippet from <a href="http://erlang.org/doc/reference_manual/users_guide.html"> user manual</a>

-module(color). 
-export([new/4, blend/2]). 
-define(is_channel(V), (is_float(V) andalso V >= 0.0 andalso V =< 1.0)).  % macro

% u need to use macros in guards, functions aren't allowed
new(R,G,B,A) when ?is_channel(R), ?is_channel(G),
                  ?is_channel(B), ?is_channel(A) ->
    #{red => R, green => G, blue => B, alpha => A}. % new map, assign with => 

blend(Src,Dst) ->
    blend(Src,Dst,alpha(Src,Dst)). 
    
blend(Src,Dst,Alpha) when Alpha > 0.0 ->
    Dst#{   %override map Dst
        red   := red(Src,Dst) / Alpha, % update with :=
        green := green(Src,Dst) / Alpha,
        blue  := blue(Src,Dst) / Alpha,
        alpha := Alpha}; 
    
blend(_,Dst,_) ->
    Dst#{
        red   := 0.0,
        green := 0.0,
        blue  := 0.0,
        alpha := 0.0}. 
    
alpha(#{alpha := SA}, #{alpha := DA}) -> % pattern matching, assign a name to map component
    SA + DA*(1.0 - SA). 

red(#{red := SV, alpha := SA}, #{red := DV, alpha := DA}) ->
    SV*SA + DV*DA*(1.0 - SA). 

green(#{green := SV, alpha := SA}, #{green := DV, alpha := DA}) ->
    SV*SA + DV*DA*(1.0 - SA). 

blue(#{blue := SV, alpha := SA}, #{blue := DV, alpha := DA}) ->
    SV*SA + DV*DA*(1.0 - SA). 