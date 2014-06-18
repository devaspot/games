-module(web_sup).
-behaviour(supervisor).
-export([start_link/0, init/1]).
-compile(export_all).
-include_lib ("n2o/include/wf.hrl").
-include("users.hrl").
-define(APP, web).

start_link() -> supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->

    {ok, _} = cowboy:start_http(http, 100, [{port, wf:config(n2o,transition_port,8080)}],
                                           [{env, [{dispatch, dispatch_rules()}]}]),

    case file:read_file("/home/kakauser/cert/2014/startssl.ca") of
      {error,_} -> skip;
      {ok,_} ->
        {ok, _} = cowboy:start_https(https, 100, [
        {port, wf:config(n2o,port,8443)},
        {cacertfile, "/home/kakauser/cert/2014/startssl.ca"},
        {certfile, "/home/kakauser/cert/2014/kakaranet.com.crt"},
        {keyfile, "/home/kakauser/cert/2014/kakaranet.dec.key"} ],
             [{env, [{dispatch, dispatch_rules()}]}])
     end,


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
