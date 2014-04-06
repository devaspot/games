%%----------------------------------------------------------------------
%% @author Vladimir Baranov <vladimir.b.n.b@gmail.com>
%% @copyright Paynet Internet ve Bilisim Hizmetleri A.S. All Rights Reserved.
%% @doc
%% Definitions file for accounts and transactions related records and macroses
%% @end
%%--------------------------------------------------------------------

-ifndef(NSM_ACCOUNTS).
-define(NSM_ACCOUNTS, true).

-type currency()         :: kakaush | quota | game_point | money.
%% game events when points affected
-type game_event_type()  :: round_start | round_end | game_start | game_end | tour_start.
-type tournament_type()  :: standalone | elimination | pointing | lucky. %% FIXME: Copypasted from game_okey.hrl
-type game_name()        :: okey | tavla. %% TODO: add other game names
-type game_mode()        :: standard | evenodd | color | countdown | feellucky |
                            esli | kakara. %%  TODO: add other game types
-type account_id()       :: {string(), currency()}. %% {username, currency}.
-type transaction_id()   :: string().

-define(SYSTEM_ACCOUNT_ID, system).

%% common fields for all accounts

-record(account, {id          :: account_id(),
				  debet       :: integer(),
				  credit      :: integer(),
				  last_change :: integer() %% can be negative. Store last change
				                           %% to resolve conflicts just by applying
                                           %% all changes to one of the conflicting

				  }).

-define(ACC_ID(Account, Currency), {Account, Currency}). %% account id helper

-record(pointing_rule,
		{
		 id,            %% {Game, GameType, Rounds} | {Game, GameType}
		 game,
		 game_type,
		 rounds,        %% rounds | points | undefined
		 kakush_winner, %% kakush points will be assigned for the winner of the game.
		 kakush_other,  %% kakush points will be assigned for the rest of the
 		                %% players other then winner, per player.
		 quota,         %% quota, no of quota will be charged to players.
		 game_points    %% game points will be assigned at the end of the game
		                %% to winner player
		}).

%% Transaction info definitions. This records should be used as
%% values in #transaction.info. 'ti' prefix = 'transaction info'.

-record(ti_game_event,
        {
         id                           :: integer(),         %% GameId
         type                         :: game_event_type(),
         game_name                    :: game_name(),
         game_mode                    :: game_mode(),
         double_points                :: integer(),
         tournament_type = standalone :: tournament_type()
        }).

-record(ti_payment,
		{
		 id                         :: integer()
		}).

-record(ti_admin_change,
		{
		 reason                     :: binary()
		 }).

-record(ti_default_assignment,
		{

		 }).

-type transaction_info() :: #ti_game_event{} | #ti_payment{} | #ti_admin_change{}|#ti_default_assignment{}.

-record(user_transaction, {user,top}).

-record(transaction,
		{
		 id                         :: transaction_id(),
		 commit_time                :: erlang:now(),
		 amount                     :: integer(),    %% amount to move between accounts
		 remitter                   :: account_id(), %% accout that gives money/points
		 acceptor                   :: account_id(), %% account receive money/points
		 currency                   :: currency(),   %% some of the points or money
		 info                       :: transaction_info(),
                 next,
                 prev
		 }).

%% Currencies

-define(CURRENCY_KAKUSH,               kakush).
-define(CURRENCY_KAKUSH_CURRENCY,      kakush_currency). % used for gifts section, charged only when user buy package
-define(CURRENCY_MONEY,       money).
-define(CURRENCY_GAME_POINTS, game_point).
-define(CURRENCY_QUOTA,       quota).

-endif.
