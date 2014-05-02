Kakaranet Game Server
=====================

Token server
------------

    ├── auth 
    │   ├── anonymous.erl           Anonymous Names Generation
    │   └── auth_server.erl         gen_server

Aux libraries
-------------

    ├── lib
    │   ├── deck.erl                shuffle machine
    │   ├── gas.erl                 logging switcher
    │   ├── known_records.erl       strict allowed protocol records
    │   └── midict.erl              multi-indexed dictionary

Tournament modes
----------------

    ├── modules
    │   ├── elimination.erl         tournaments
    │   ├── matrix.erl              tournament matrix
    │   ├── lucky.erl               free play mode, infinite tournament, lobby mode
    │   ├── relay.erl               relay between tournaments and tables
    │   └── standalone.erl          standalone games

Node services under supervision
-------------------------------

    ├── sup
    │   ├── game_log.erl            persist game protocol statistics
    │   ├── id_generator.erl        unique ids
    │   ├── lucky_sup.erl           lucky games supervisor
    │   ├── okey_sup.erl            okey pre-created games
    │   └── tavla_sup.erl           tavla pre-created games

Common structure for all games
------------------------------

    ├── okey/tavla
    │   ├── game_bot.erl                game bot
    │   ├── game_desk.erl               desk rules, game rules
    │   ├── game_scoring.erl            scoring
    │   ├── game_table.erl              table messaging between players
    │   └── game_test.erl               game testing

Game Server API
---------------

    ├── README.md
    ├── game.erl                        API for crating game rooms, tournaments, etc.
    ├── game_app.erl
    ├── game_session.erl                front API for Game Protocol
    ├── game_sup.erl
    └── server.app.src

Credits
-------

* Sinan Ustel
* Ahmet Tez

OM A HUM
