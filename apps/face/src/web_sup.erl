-module(web_sup).
-behaviour(supervisor).
-export([start_link/0, init/1]).
-compile(export_all).
-include_lib ("n2o/include/wf.hrl").
-include("users.hrl").
-define(APP, face).

start_link() -> supervisor:start_link({local, ?MODULE}, ?MODULE, []).

-define(USERS, [#user{id="maxim",email="maxim@synrc.com"},
                #user{id="doxtop",email="doxtop@synrc.com"},
                #user{id="roman",email="roman@github.com"}]).

init([]) ->

    {ok, _} = cowboy:start_http(http, 100, [{port, wf:config(n2o,transition_port,8000)}],
                                           [{env, [{dispatch, dispatch_rules()}]}]),

    PrivDir = code:priv_dir(face),

    {ok, _} = cowboy:start_https(https, 100, [
        {port, wf:config(n2o,port,8443)},
        {cacertfile, PrivDir ++ "/ssl/cowboy-ca.crt"},
        {certfile, PrivDir ++ "/ssl/server.crt"},
        {keyfile, PrivDir ++ "/ssl/server.key"} ], [{env, [{dispatch, dispatch_rules()}]}]),

    users:init(),
    users:populate(?USERS),

    {ok, {{one_for_one, 5, 10}, []}}.

dispatch_rules() ->
    cowboy_router:compile(
        [{'_', [
            {"/static/[...]", cowboy_static,
                {priv_dir, ?APP, <<"static">>,[{mimetypes,cow_mimetypes,all}]}},
            {"/rest/:resource", rest_cowboy, []},
            {"/rest/:resource/:id", rest_cowboy, []},
            {"/ws/[...]", bullet_handler, [{handler, n2o_bullet}]},
            {'_', n2o_cowboy, []}
    ]}]).
