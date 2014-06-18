-module(auth_server).

-include_lib("server/include/settings.hrl").
-include_lib("server/include/authtoken.hrl").
-include_lib("server/include/requests.hrl").
-include_lib("kvs/include/user.hrl").

-behaviour(gen_server).
-compile(export_all).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-define(SPARE_LOGINS, [
    #'PlayerInfo'{name = <<"Hürrem">>, sex = female, surname = <<"Sultan">>, login = <<"peace">>, robot = true},
    #'PlayerInfo'{name = <<"Albert">>, sex = male, surname= <<"Einstein">>, login = <<"quantum">>, robot = true },
    #'PlayerInfo'{name = <<"Marie">>, sex = female, surname= <<"Curie">>, login = <<"radio">>, robot = true },
    #'PlayerInfo'{name = <<"Ilya">>, sex = male, surname = <<"Prigogine">>, login = <<"synergetics">>, robot = true}
    ]).

-record(state, {spare = ?SPARE_LOGINS,tokens}).

spare() -> [ P#'PlayerInfo'{id =wf:to_binary(wf:to_list(P#'PlayerInfo'.login) ++
              wf:to_list(id_generator:get_id2()))} || P <- ?SPARE_LOGINS ].

start_link() -> gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).
store_token(GameId, Token, UserId) -> gen_server:call(?SERVER, {store_token, GameId, Token, UserId}).
get_user_info(Token) -> gen_server:call(?SERVER, {get_user_info, Token}).
get_user_info_by_user_id(UserId) -> user_info(UserId).
generate_token(Game,User) -> T = base64:encode(crypto:rand_bytes(100)), store_token(Game,T,User).

init([]) ->
    Tokens = ets:new(tokens, [private, ordered_set, {keypos, #authtoken.token}]),
    {ok, #state{tokens = Tokens}}.

handle_call({store_token, GameId, Token, UserId}, _From, #state{tokens = E} = State) ->
    store_token(GameId, E, Token, UserId),
    {reply, Token, State};

handle_call({get_user_info, Token}, _From, #state{tokens = E} = State) ->
    case ets:lookup(E, Token) of
        [] ->
            gas:info(?MODULE,"Token not found. Denied.", []),
            {reply, false, State};
        List ->
            {authtoken, _, UserId} = hd(List),
            Reply = case user_info(UserId) of
                {error, not_found} ->
                    gas:info(?MODULE,"User is not in DB", []),
                    user_info(#user{id = UserId });
                UserInfo ->
                    gas:info(?MODULE,"Registered User", []),
                    UserInfo
            end,
            {reply, Reply, State}
    end;

handle_call(_Request, _From, State) -> Reply = ok, {reply, Reply, State}.
handle_cast(_Msg, State) -> {noreply, State}.
handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

robot_credentials() ->
    Pos = crypto:rand_uniform(1, length(?SPARE_LOGINS) + 1),
    H0 = lists:nth(Pos, ?SPARE_LOGINS),
    Id = wf:to_binary(wf:to_list(H0#'PlayerInfo'.login) ++ wf:to_list(id_generator:get_id2())),
    H0#'PlayerInfo'{id = Id}.

store_token(GameId, E, Token, UserId) ->
    gas:info(?MODULE,"Storing token: ~p", [Token]),
    Data = #authtoken{token = Token, id = UserId},
    ets:insert(E, Data).

player_name(#'PlayerInfo'{login = Id, name = Name, surname = Surname}) ->
    wf:to_binary([case Name of <<"undefined">> -> Id;
              _ -> wf:to_list(Name) ++ case Surname of
                     <<"undefined">> -> ""; _ -> " " ++ wf:to_list(Surname) end end]).

user_info(#user{}=UserData) ->
%    gas:info(?MODULE,"PlayerInfo by #user: ~p",[UserData]),
    #'PlayerInfo'{id = wf:to_binary(UserData#user.id),
        login = wf:to_binary(UserData#user.username),
        name = wf:to_binary(UserData#user.names),
        avatar_url = wf:to_binary(UserData#user.avatar),
        skill = 0,
        score = 0,
        surname = wf:to_binary(UserData#user.surnames)};


user_info(UserId) ->
    case kvs:get(user,UserId) of
        {ok, UserData} ->
%            gas:info(?MODULE,"User Data: ~p",[UserData]),
            user_info(UserData);
        Error -> Error end.
