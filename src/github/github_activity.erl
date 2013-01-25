-module(github_activity).
-author('Chvanikoff <chvanikoff@gmail.com>').

%% API
-export([
	list_events/0,
	list_repo_events/2,
	list_repo_issue_events/2,
	list_public_repo_events/2,
	list_org_public_events/1,
	list_received_events/1,
	list_received_public_events/1,
	list_events_by_user/1,
	list_public_events_by_user/1,
	list_org_events/2
]).
-export([
	list_notifications/0, list_notifications/1,
	list_repo_notifications/2, list_repo_notifications/3,
	mark_notifications_read/0, mark_notifications_read/1,
	mark_repo_notifications_read/2, mark_repo_notifications_read/3,
	read_notifications_thread/1,
	mark_notifications_thread_read/1, mark_notifications_thread_read/2
]).

-define(SERVER, github_srv).

%% ===================================================================
%% API functions
%% ===================================================================

%% ###################################################################
%% Events
%% ###################################################################

list_events() ->
	?SERVER:http_get("events").

list_repo_events(Owner, Repo) ->
	?SERVER:http_get("repos/" ++ Owner ++ "/" ++ Repo ++ "/events").

list_repo_issue_events(Owner, Repo) ->
	?SERVER:http_get("repos/" ++ Owner ++ "/" ++ Repo ++ "/issues/events").

list_public_repo_events(Owner, Repo) ->
	?SERVER:http_get("networks/" ++ Owner ++ "/" ++ Repo ++ "/events").

list_org_public_events(Organization) ->
	?SERVER:http_get("orgs/" ++ Organization ++ "/events").

list_received_events(User) ->
	?SERVER:http_get("users/" ++ User ++ "/received_events").

list_received_public_events(User) ->
	?SERVER:http_get("users/" ++ User ++ "/received_events/public").

list_events_by_user(User) ->
	?SERVER:http_get("users/" ++ User ++ "/events").

list_public_events_by_user(User) ->
	?SERVER:http_get("users/" ++ User ++ "/events/public").

list_org_events(User, Organization) ->
	?SERVER:http_get("users/" ++ User ++ "/events/orgs/" ++ Organization).

%% ###################################################################
%% Notifications
%% ###################################################################

list_notifications() ->
	list_notifications([]).

list_notifications(Params) ->
	?SERVER:http_get("notifications", [{params, Params}]).

list_repo_notifications(Owner, Repo) ->
	list_repo_notifications(Owner, Repo, []).

list_repo_notifications(Owner, Repo, Params) -> 
	?SERVER:http_get("repos/" ++ Owner ++ "/" ++ Repo ++ "/notifications", [{params, Params}]).

mark_notifications_read() ->
	mark_notifications_read([]).

mark_notifications_read(Params) ->
	?SERVER:http_put("notifications", [{params, Params}]).

mark_repo_notifications_read(Owner, Repo) ->
	mark_repo_notifications_read(Owner, Repo, []).

mark_repo_notifications_read(Owner, Repo, Params) ->
	?SERVER:http_put("repos/" ++ Owner ++ "/" ++ Repo ++ "/notifications", [{params, Params}]).

read_notifications_thread(Thread) ->
	?SERVER:http_get("notifications/threads/" ++ Thread).

mark_notifications_thread_read(Thread) ->
	mark_notifications_thread_read(Thread, []).

mark_notifications_thread_read(Thread, Params) ->
	?SERVER:http_put("notifications/threads/" ++ Thread, [{params, Params}]).





