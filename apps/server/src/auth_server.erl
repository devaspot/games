-module(auth_server).

-include_lib("server/include/conf.hrl").
-include_lib("server/include/settings.hrl").
-include_lib("server/include/log.hrl").
-include_lib("server/include/authtoken.hrl").
-include_lib("server/include/requests.hrl").

-behaviour(gen_server).

-compile(export_all).
-export([store_token/3,start_link/0,
         robot_credentials/0,
         fake_credentials/0,
         get_user_info/1, get_user_info/2,
         get_user_info_by_user_id/1
        ]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(SPARE_LOGINS, [#'PlayerInfo'{name = <<"Abe">>, surname="Kobo", login = <<"dunes">>, avatar_url = <<"/files/users/user_dunes/avatar/1-small.jpg">>},
                       #'PlayerInfo'{name = <<"Herman">>, surname="Hesse", login = <<"wolves">>, avatar_url = <<"/files/users/user_wolves/avatar/1-small.jpg">>},
                       #'PlayerInfo'{name = <<"Ernest">>, surname = <<"Hemingway">>, login = <<"oldman">>, avatar_url = <<"/files/users/user_oldman/avatar/1-small.jpg">>},
                       #'PlayerInfo'{name = <<"Erich Maria">>, surname = <<"Remarque">>, login = <<"imwesten">>, avatar_url = <<"/files/users/user_imwesten/avatar/1-small.jpg">>}]).

-record(state, {
          spare = ?SPARE_LOGINS,
          tokens
         }).

%% definition of user from zealot/include/user.hrl
-record(user_info,
        {username,
         name,
         surname,
         age,
	 avatar_url,
         sex,
         skill :: integer(),
         score :: integer()}).

start_link() -> gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).
store_token(GameId, Token, UserId) when is_list(Token) -> store_token(GameId, list_to_binary(Token), UserId);
store_token(GameId, Token, UserId) when is_binary(Token) -> gen_server:call(?SERVER, {store_token, GameId, Token, UserId}).
get_user_info(Token) when is_list(Token)  -> get_user_info(list_to_binary(Token));
get_user_info(Token) when is_binary(Token) -> gen_server:call(?SERVER, {get_user_info, Token}).
get_user_info(Token, Id) when is_list(Token) -> get_user_info(list_to_binary(Token), Id);
get_user_info(Token, Id) when is_list(Id) -> get_user_info(Token, list_to_binary(Id));
get_user_info(Token, Id) when is_binary(Token), is_binary(Id) -> gen_server:call(?SERVER, {get_user_info, Token, Id}).
get_user_info_by_user_id(UserId) when is_list(UserId) -> get_user_info_by_user_id(list_to_binary(UserId));
get_user_info_by_user_id(UserId) -> user_info(UserId).
fake_credentials() -> gen_server:call(?SERVER, {fake_credentials}).
robot_credentials() -> gen_server:call(?SERVER, {robot_credentials}).

init([]) ->
    Tokens = ets:new(tokens, [private, ordered_set, {keypos, #authtoken.token}]),
    store_token(0,Tokens, <<?TEST_TOKEN>>, "maxim"),
    store_token(0,Tokens, <<?TEST_TOKEN2>>, "alice"),
    {ok, #state{tokens = Tokens}}.

handle_call({store_token, GameId, Token, UserId}, _From, #state{tokens = E} = State) ->
    store_token(GameId, E, Token, UserId),
    {reply, Token, State};

handle_call({get_user_info, Token}, _From, #state{tokens = E} = State) ->
    gs:info("checking token: ~p", [Token]),
    case ets:lookup(E, Token) of
        [] ->
            gs:info("token not found", []),
            {reply, false, State};
        List ->
            {authtoken, _, UserId} = hd(List),
            gs:info("token was registred, getting user info for ~p",[UserId]),
            Reply = case user_info(UserId) of
                {ok, UserInfo} ->
                    gs:info("..user info retrieved", []),
                    UserInfo;
                {error, user_not_found} ->
                    gs:info("..no such user info, providing fake credentials", []),
                    fake_credentials0(State#state.spare); %% for eunit tests. FIX
                {badrpc, _} ->
                    gs:info("..bad rpc, providing fake credentials", []),
                    fake_credentials0(State#state.spare)  %% for eunit tests. FIX
            end,
            {reply, Reply, State}
    end;

handle_call({get_user_info, Token, Id}, _From, #state{tokens = E} = State) ->
    gs:info("checking token: ~p", [Token]),
    case ets:lookup(E, Token) of
        [] ->
            gs:error("token not found", []),
            {reply, false, State};
        _List ->
            Reply0 = fake_credentials0(State#state.spare),
            Reply = Reply0#'PlayerInfo'{id = Id},
            {reply, Reply, State}
    end;


handle_call({fake_credentials}, _From, #state{spare = Spare} = State) -> H = fake_credentials0(Spare), {reply, H, State};
handle_call({robot_credentials}, _From, #state{spare = Spare} = State) -> H = fake_credentials0(Spare), {reply, H#'PlayerInfo'{robot = true}, State};
handle_call(_Request, _From, State) -> Reply = ok, {reply, Reply, State}.
handle_cast(_Msg, State) -> {noreply, State}.
handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

fake_credentials0(Spare) ->
    Pos = crypto:rand_uniform(1, length(Spare)),
    H0 = lists:nth(Pos, Spare),
    Id = list_to_binary(binary_to_list(H0#'PlayerInfo'.login) ++
         integer_to_list(id_generator:get_id2())),
    H0#'PlayerInfo'{id = Id}.

store_token(GameId, E, Token, UserId) ->
    gs:info("storing token: ~p", [Token]),
    Data = #authtoken{token = Token, id = UserId},
    ets:insert(E, Data).

user_info(UserId) ->
    case nsm_auth:get_user_info(UserId) of
        {ok, UserData} ->
            gs:info("User Data: ~p",[UserData]),
            {ok, #'PlayerInfo'{id = list_to_binary(UserData#user_info.username),
                               login = list_to_binary(UserData#user_info.username),
                               name = wf:to_binary(UserData#user_info.name),
                               avatar_url = wf:to_binary(UserData#user_info.avatar_url),
                               skill = UserData#user_info.skill,
                               score = UserData#user_info.score,
                               surname = wf:to_binary(UserData#user_info.surname)}};
        Error ->
            Error
    end.
