-define(LOG(Format, Args),
	io:format("~p:~p:~p:~p::: " ++ Format ++ "~n", [self(), ?MODULE, ?LINE, erlang:localtime() | Args])).

-define(PP(Format, Args),
	io:format("~p:~p:~p::: " ++ Format ++ "~n", [self(), ?MODULE, ?LINE | Args])).

% -define(DEBUG(Format, Args),
%         io:format("~p:~p:~p::: " ++ Format ++ "~n", [self(), ?MODULE, ?LINE | Args])).

-define(DP(Format, Args),
	io:format(user, "~p:~p:~p::: " ++ Format ++ "~n", [self(), ?MODULE, ?LINE | Args])).

%% -define(PP(F, A), ok).
%% -define(DP(F, A), ok).
-define(DEBUG(F,A), ok).
%% -define(LOG(F, A), ok).
