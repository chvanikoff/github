-module(github_srv).
-author('chvanikoff <chvanikoff@gmail.com>').

-behaviour(gen_server).

%% Gen_server callbacks
-export([
	init/1,
	handle_call/3,
	handle_cast/2,
	handle_info/2,
	terminate/2,
	code_change/3
]).

%% API
-export([
	start_link/0,
	http_get/1, http_get/2,
	http_put/1, http_put/2,
	remaining/0
]).

-record(state, {
	remaining = 0,
	auth_headers = [],
	username = ""
}).

-define(SERVER, ?MODULE).
-define(API_URL, "https://api.github.com/").

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
	gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).


http_get(Url) ->
	http_get(Url, []).


http_get(Url, Params) ->
	gen_server:call(?SERVER,
		{get, build_url(Url, proplists:get_value(params, Params, [])), proplists:get_value(headers, Params, [])}).


http_put(Url) ->
	http_put(Url, []).


http_put(Url, Params) ->
	gen_server:call(?SERVER,
		{put, build_url(Url, proplists:get_value(params, Params, [])), proplists:get_value(headers, Params, [])}).


remaining() ->
	gen_server:call(?SERVER, remaining).

%% ===================================================================
%% Gen_server callbacks
%% ===================================================================

init([]) ->
	{ok, #state{}}.


handle_call(remaining, _From, State = #state{remaining = Remaining}) ->
	{reply, Remaining, State};

handle_call({get, Url, Headers}, _From, State = #state{auth_headers = Auth_headers}) ->
	{ok, {{"HTTP/1.1", 200, "OK"}, Response_headers, Response}} = httpc:request(get, {
		Url,
		lists:append(Headers, Auth_headers)}, [], []),
	Remaining = list_to_integer(proplists:get_value("x-ratelimit-remaining", Response_headers)),
	{reply, Response, State#state{remaining = Remaining}};

handle_call({put, Url, Headers}, _From, State = #state{auth_headers = Auth_headers}) ->
	{ok, {{"HTTP/1.1", 200, "OK"}, Response_headers, Response}} = httpc:request(put, {
		Url,
		lists:append(Headers, Auth_headers)}, [], []),
	Remaining = list_to_integer(proplists:get_value("x-ratelimit-remaining", Response_headers)),
	{reply, Response, State#state{remaining = Remaining}};

handle_call(_Request, _From, State) ->
	{reply, ignored, State}.


handle_cast({put, Url, Headers}, State = #state{auth_headers = Auth_headers}) ->
	{ok, {{"HTTP/1.1", 200, "OK"}, Response_headers, _Response}} = httpc:request(put, {
		Url,
		lists:append(Headers, Auth_headers)}, [], []),
	Remaining = list_to_integer(proplists:get_value("x-ratelimit-remaining", Response_headers)),
	{noreply, State#state{remaining = Remaining}};

handle_cast({set_credentials, Login, Password}, State) ->
	{noreply, State#state{auth_headers = auth_headers(Login, Password), username = Login}};

handle_cast(_Msg, State) ->
	{noreply, State}.


handle_info(_Info, State) ->
	{noreply, State}.


terminate(_Reason, _State) ->
	ok.


code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%% ===================================================================
%% Internal functions
%% ===================================================================

auth_headers(Login, Password) ->
	[{"Authorization", "Basic " ++ base64:encode_to_string(Login ++ ":" ++ Password)},
	{"Content-Type", "text/json"}].

build_url(Url, Ql) ->
	?API_URL ++ Url ++ build_qs(Ql).

build_qs([]) ->
	"";
build_qs(Ql) ->
	{K1, V1} = hd(Ql),
	{K, V} = {to_list(K1), to_list(V1)},
	lists:foldl(fun(E, A) ->
		{K1, V1} = E,
		{K, V} = {to_list(K1), to_list(V1)},
		A ++ "&" ++ K ++ "=" ++ V
	end, "?" ++ K ++ "=" ++ V, tl(Ql)).

to_list(A) when is_atom(A) -> atom_to_list(A);
to_list(I) when is_integer(I) -> integer_to_list(I);
to_list(B) when is_binary(B) -> binary_to_list(B).
