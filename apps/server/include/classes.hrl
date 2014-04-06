%%%
%%% #object can be passed to amf:encode
%%%
-type amf0() :: any().

-record(object, {
          name          :: string(),
          members = []  :: list(tuple(atom(), amf0()))
         }).
%%%
%%% Next four are packed into #object before amf:encode
%%%
-record('KamfRequest', {
          id        :: any(),
          method    :: string(),
          args = [] :: list(tuple(atom(), any()))
         }).

-record('KamfResponse', {
          id      :: any(),
          success :: boolean(),
          result  :: any()
         }).

-record('KamfMessage', {
          id         :: any(),
          event_type :: string(),
          args = []  :: list(tuple(atom(), any()))
         }).

-record('KamfFatalError', {
          type   :: atom(), %% request or message
          id     :: any(),
          reason :: string()
         }).

