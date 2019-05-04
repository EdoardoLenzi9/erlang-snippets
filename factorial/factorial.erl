%% @author http://erlang.org/doc/reference_manual/users_guide.html
%% @copyright Copyright © 2003-2019 Ericsson AB. All Rights Reserved.
%% @doc snippet from <a href="http://erlang.org/doc/reference_manual/users_guide.html"> user manual</a>

-module(factorial).
-export([fac/1]).

fac(1) ->
    1;
fac(N) ->
    N * fac(N - 1).