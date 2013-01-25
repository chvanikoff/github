-module(github).
-author('Chvanikoff <chvanikoff@gmail.com>').

%% API
-export([
	start/0,
	start/2
]).

-define(SERVER, github_srv).

%% ===================================================================
%% API functions
%% ===================================================================

start() ->
	ok = sync:go(),
	{ok, [{login, Login}, {password, Password}]} = file:consult("priv/github.config"),
	start(Login, Password).

start(Login, Password) ->
	ensure_started([crypto, public_key, ssl, inets, github]),
	gen_server:cast(?SERVER, {set_credentials, Login, Password}).

%% ===================================================================
%% Internal functions
%% ===================================================================

ensure_started([]) -> ok;
ensure_started([App | Apps]) ->
	Msg = case application:start(App) of
		ok ->
			"started";
		{error, {already_started, App}} ->
			"was already started";
		Error ->
			io:format("Error starting ~p:~n~p~n", [App, Error]),
			throw(Error)
	end,
	io:format("~p " ++ Msg ++ "~n", [App]),
	ensure_started(Apps).