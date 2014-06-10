Kakaranet Game Server
=====================

Game Server API
---------------

Game Server provides very clean, like a sky, Erlang based API for communication with
underlying transports. The talk protocol is based on two kind of messages:
actions (client requests) and events (server responses). Each client is driven
by game_session gen_server.

    ├── README.md                   you are reading this file just now
    ├── game.erl                    API for crating game rooms, tournaments, etc.
    ├── game_session.erl            front API for Game Protocol
    ├── game_app.erl
    ├── game_sup.erl
    ├── server.app.src

Please refer to requests.hrl for common Game Server protocol envelopment.
Each game could have specific actions and events.

### Actions

```erlang
-record(session_attach, {token}).
-record(logout, {}).
-record(join_game, {game :: 'GameId'() }).
-record(stats_action, {player :: 'PlayerId'(), game_type}).
-record(pause_game, {table, game :: 'GameId'(), action}).
-record(game_action, {game :: 'GameId'(), action, args = []}).
```

### Events

```erlang
-record(game_event, {game :: 'GameId'(), event, args = [] }).
-record(stats_event, {player :: 'PlayerId'(), games, reveals, protocol}).
-record(game_paused, {table, game :: 'GameId'(), action, who :: 'PlayerId'(), retries}).
-record(disconnect, {code :: integet(), reason}).
-record(player_left, {player :: 'PlayerId'(), human_replaced, replacement :: 'PlayerId'()}).
```

Token Server
------------

Password-free token server assign permanent token to empty cookie or unknown device
automatically. Default unique user name would be assigned also. Later on you can
change your name or merge your account with other devices after subscription.

    ├── auth 
    │   ├── anonymous.erl           anonymous names generation
    │   └── auth_server.erl         gen_server

Game Tables
-----------

Each game has playing rules (desk), scroring constraints (scoring),
table message protocol (table) and default bot implementation. Please follow
this composition during new games development. Each table represents single game.

    ├── okey/tavla
    │   ├── game_bot.erl            game bot
    │   ├── game_desk.erl           desk rules, game rules
    │   ├── game_scoring.erl        scoring
    │   ├── game_table.erl          table messaging between players
    │   └── game_test.erl           game testing

Tournaments
-----------

Games could be organized into a set of tables with tournament rules. By default
Game Server provides several kind of default tournament modules: elimination tounaments,
full cycle tournaments, infinite score-free tournaments for new players and regular
standalone tables (which is treated as simpliest tournaments with just one table).

    ├── modules
    │   ├── elimination.erl         tournaments
    │   ├── matrix.erl              declarative tournaments DSL language
    │   ├── lucky.erl               free play mode, endless tournaments or lobby mode
    │   ├── relay.erl               relay between tournaments and tables
    │   └── standalone.erl          regular single-table games

Services
--------

Each tournaments and games are driven by supervisors. Also Game Server has game logging
serices along with auth token sever which are also under supervision.

    ├── sup
    │   ├── journal.erl             persist game protocol statistics in KV store
    │   ├── id_generator.erl        unique ids
    │   ├── lucky_sup.erl           lucky games supervisor
    │   ├── okey_sup.erl            okey pre-created games
    │   └── tavla_sup.erl           tavla pre-created games

Aux libraries
-------------

Useful libraries for shuflling cards/tashes/piles, logging and validation libraries.

    └── lib
        ├── deck.erl                shuffle machine
        ├── gas.erl                 logging switcher
        ├── known_records.erl       strict allowed protocol records
        └── midict.erl              multi-indexed dictionary

Size
----

Game Server is about 640KiB and 10K LOC of sources.

License
-------

For licensing Kakaranet Game Server for commercial use please contact:

* Sinan Ustel
* Ahmet Tez

Authors
-------

* Sergei Polkovnikov
* Maksym Sokhatskyi

OM A HUM
