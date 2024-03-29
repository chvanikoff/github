-module(github_app).
-author('Chvanikoff <chvanikoff@gmail.com>').

-behaviour(application).

%% Application callbacks
-export([
	start/2,
	stop/1
]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    github_sup:start_link().

stop(_State) ->
    ok.
