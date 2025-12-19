%%% ===========================================================================
%%% Comprehensive Erlang Sample - Syntax Highlighting Demonstration
%%% ===========================================================================
%%%
%%% @doc This module demonstrates all major Erlang language features
%%% for syntax highlighting purposes.
%%%
%%% @author Sample Author
%%% @copyright 2024
%%% @version 1.0.0
%%% ===========================================================================

-module(sample).

%% API exports
-export([main/0, factorial/1, fibonacci/1, quicksort/1]).
-export([start_server/0, start_link/1, stop/1]).
-export([handle_call/3, handle_cast/2, handle_info/2]).
-export([init/1, terminate/2, code_change/3]).

%% Types and callbacks
-export_type([status/0, user/0, result/1]).

%% Behaviour
-behaviour(gen_server).

%% Include files
-include_lib("kernel/include/logger.hrl").
%% -include("sample.hrl").

%% ===========================================================================
%% Type Definitions
%% ===========================================================================

-type status() :: active | inactive | pending | {error, term()}.
-type user_id() :: pos_integer().
-type email() :: binary().

-type user() :: #{
    id := user_id(),
    name := binary(),
    email := email(),
    age => non_neg_integer(),
    active := boolean()
}.

-type result(T) :: {ok, T} | {error, term()}.
-type callback_result() :: {reply, term(), state()}
                         | {noreply, state()}
                         | {stop, term(), state()}.

-opaque state() :: #{
    counter := integer(),
    users := #{user_id() => user()}
}.

%% Callback specifications
-callback init(Args :: term()) -> {ok, state()} | {stop, Reason :: term()}.
-callback handle_request(Request :: term(), State :: state()) -> result(term()).

%% Optional callbacks
-optional_callbacks([handle_request/2]).

%% ===========================================================================
%% Records
%% ===========================================================================

-record(person, {
    id :: user_id(),
    name :: binary(),
    age = 0 :: non_neg_integer(),
    email :: email() | undefined
}).

-record(config, {
    host = "localhost" :: string(),
    port = 8080 :: pos_integer(),
    timeout = 5000 :: timeout(),
    options = [] :: [term()]
}).

%% ===========================================================================
%% Macros
%% ===========================================================================

-define(DEFAULT_TIMEOUT, 5000).
-define(MAX_RETRIES, 3).
-define(LOG_INFO(Msg), io:format("[INFO] ~s~n", [Msg])).
-define(LOG_ERROR(Msg, Args), io:format("[ERROR] " ++ Msg ++ "~n", Args)).
-define(SQUARE(X), ((X) * (X))).
-define(IS_POSITIVE(X), is_integer(X) andalso X > 0).

%% Conditional compilation
-ifdef(DEBUG).
-define(DEBUG_LOG(Msg), io:format("[DEBUG] ~p~n", [Msg])).
-else.
-define(DEBUG_LOG(_Msg), ok).
-endif.

%% ===========================================================================
%% Literals
%% ===========================================================================

%% Integer literals
integer_examples() ->
    DecimalInt = 42,
    NegativeInt = -17,
    HexInt = 16#FF,           % 255
    OctalInt = 8#755,         % 493
    BinaryInt = 2#101010,     % 42
    BaseN = 36#HELLO,         % Base-36 number
    CharCode = $A,            % 65
    EscapeChar = $\n,         % 10
    {DecimalInt, NegativeInt, HexInt, OctalInt, BinaryInt, BaseN, CharCode, EscapeChar}.

%% Float literals
float_examples() ->
    SimpleFloat = 3.14159,
    Scientific = 6.022e23,
    NegativeFloat = -2.71828,
    SmallFloat = 1.0e-10,
    {SimpleFloat, Scientific, NegativeFloat, SmallFloat}.

%% Atom literals
atom_examples() ->
    SimpleAtom = hello,
    QuotedAtom = 'hello world',
    SpecialAtom = 'with-dashes',
    UnicodeAtom = 'λ',
    BooleanTrue = true,
    BooleanFalse = false,
    UndefinedAtom = undefined,
    {SimpleAtom, QuotedAtom, SpecialAtom, UnicodeAtom, BooleanTrue, BooleanFalse, UndefinedAtom}.

%% String and binary literals
string_examples() ->
    String = "Hello, Erlang!",
    EscapedString = "Line 1\nLine 2\tTabbed",
    UnicodeString = "Hello, 世界!",
    Binary = <<"Hello, Binary!">>,
    Utf8Binary = <<"Hello, 世界!"/utf8>>,
    RawBytes = <<1, 2, 3, 255>>,
    BitString = <<1:1, 0:1, 1:1, 0:4>>,
    {String, EscapedString, UnicodeString, Binary, Utf8Binary, RawBytes, BitString}.

%% ===========================================================================
%% Data Structures
%% ===========================================================================

%% Lists
list_examples() ->
    SimpleList = [1, 2, 3, 4, 5],
    MixedList = [1, atom, "string", 3.14],
    NestedList = [[1, 2], [3, 4], [5, 6]],
    ConsCell = [Head | Tail] = [1, 2, 3],
    ListComprehension = [X * 2 || X <- [1, 2, 3, 4, 5], X rem 2 == 0],
    {SimpleList, MixedList, NestedList, Head, Tail, ListComprehension}.

%% Tuples
tuple_examples() ->
    Pair = {ok, "success"},
    Triple = {error, 404, "Not Found"},
    NestedTuple = {{1, 2}, {3, 4}},
    RecordTuple = #person{id = 1, name = <<"Alice">>, age = 30},
    {Pair, Triple, NestedTuple, RecordTuple}.

%% Maps
map_examples() ->
    SimpleMap = #{name => <<"Alice">>, age => 30},
    UpdatedMap = SimpleMap#{age := 31},
    NestedMap = #{
        user => #{name => <<"Bob">>, email => <<"bob@example.com">>},
        settings => #{theme => dark, language => en}
    },
    #{name := Name} = SimpleMap,
    {SimpleMap, UpdatedMap, NestedMap, Name}.

%% ===========================================================================
%% Pattern Matching
%% ===========================================================================

%% Function clause patterns
pattern_match_function(0) -> zero;
pattern_match_function(N) when N > 0 -> positive;
pattern_match_function(N) when N < 0 -> negative;
pattern_match_function(_) -> unknown.

%% List patterns
pattern_match_list([]) -> empty;
pattern_match_list([_]) -> single;
pattern_match_list([_, _]) -> pair;
pattern_match_list([H | T]) -> {head, H, tail_length, length(T)}.

%% Tuple patterns
pattern_match_tuple({ok, Value}) -> {success, Value};
pattern_match_tuple({error, Reason}) -> {failure, Reason};
pattern_match_tuple({Tag, V1, V2}) -> {tagged, Tag, V1, V2}.

%% Map patterns
pattern_match_map(#{type := request, id := Id, payload := Payload}) ->
    {request, Id, Payload};
pattern_match_map(#{type := response, id := Id, result := Result}) ->
    {response, Id, Result};
pattern_match_map(#{type := Type}) ->
    {unknown_type, Type}.

%% Record patterns
pattern_match_record(#person{name = Name, age = Age}) when Age >= 18 ->
    {adult, Name};
pattern_match_record(#person{name = Name, age = Age}) ->
    {minor, Name, Age}.

%% Binary patterns
pattern_match_binary(<<Type:8, Length:16/big, Payload:Length/binary, Rest/binary>>) ->
    {packet, Type, Payload, Rest};
pattern_match_binary(<<Header:4/binary, Body/binary>>) ->
    {message, Header, Body};
pattern_match_binary(_) ->
    invalid.

%% ===========================================================================
%% Functions
%% ===========================================================================

%% Simple function
-spec add(number(), number()) -> number().
add(A, B) -> A + B.

%% Recursive function
-spec factorial(non_neg_integer()) -> pos_integer().
factorial(0) -> 1;
factorial(N) when N > 0 -> N * factorial(N - 1).

%% Tail-recursive function
-spec factorial_tail(non_neg_integer()) -> pos_integer().
factorial_tail(N) -> factorial_tail(N, 1).

factorial_tail(0, Acc) -> Acc;
factorial_tail(N, Acc) -> factorial_tail(N - 1, N * Acc).

%% Fibonacci with multiple clauses
-spec fibonacci(non_neg_integer()) -> non_neg_integer().
fibonacci(0) -> 0;
fibonacci(1) -> 1;
fibonacci(N) when N > 1 -> fibonacci(N - 1) + fibonacci(N - 2).

%% QuickSort
-spec quicksort([T]) -> [T] when T :: term().
quicksort([]) -> [];
quicksort([Pivot | Rest]) ->
    Smaller = [X || X <- Rest, X =< Pivot],
    Larger = [X || X <- Rest, X > Pivot],
    quicksort(Smaller) ++ [Pivot] ++ quicksort(Larger).

%% Function with guards
-spec classify(term()) -> atom().
classify(N) when is_integer(N), N < 0 -> negative;
classify(N) when is_integer(N), N == 0 -> zero;
classify(N) when is_integer(N), N > 0 -> positive;
classify(N) when is_float(N) -> float_value;
classify(A) when is_atom(A) -> atom_value;
classify(L) when is_list(L) -> list_value;
classify(T) when is_tuple(T) -> tuple_value;
classify(M) when is_map(M) -> map_value;
classify(_) -> unknown.

%% Anonymous functions (funs)
higher_order_examples() ->
    %% Lambda syntax
    Double = fun(X) -> X * 2 end,

    %% Multiple clauses in fun
    Describe = fun
        (0) -> zero;
        (N) when N > 0 -> positive;
        (_) -> negative
    end,

    %% Capturing module function
    AddFun = fun add/2,

    %% External function reference
    MapFun = fun lists:map/2,

    %% Using funs
    Result1 = Double(5),
    Result2 = Describe(-3),
    Result3 = AddFun(1, 2),
    Result4 = MapFun(Double, [1, 2, 3]),

    {Result1, Result2, Result3, Result4}.

%% ===========================================================================
%% Control Flow
%% ===========================================================================

control_flow_examples(Value) ->
    %% Case expression
    Result1 = case Value of
        0 -> zero;
        N when N > 0 -> positive;
        N when N < 0 -> negative;
        _ -> unknown
    end,

    %% If expression
    Result2 = if
        Value < 0 -> negative;
        Value == 0 -> zero;
        Value > 0 -> positive;
        true -> unknown
    end,

    %% Begin-end block
    Result3 = begin
        A = Value * 2,
        B = A + 1,
        B * 3
    end,

    %% Catch expression
    Result4 = catch 1/0,

    {Result1, Result2, Result3, Result4}.

%% Try-catch-after
exception_handling_examples() ->
    %% Try with catch classes
    Result1 = try
        throw(my_error)
    catch
        throw:Error -> {caught_throw, Error};
        error:Reason -> {caught_error, Reason};
        exit:Reason -> {caught_exit, Reason};
        _:_ -> caught_unknown
    after
        io:format("Cleanup code~n")
    end,

    %% Try with of
    Result2 = try factorial(5) of
        N when N > 100 -> large;
        N when N > 10 -> medium;
        _ -> small
    catch
        error:_ -> error_occurred
    end,

    %% Raising exceptions
    %% throw(reason),          % Throw
    %% error(reason),          % Error
    %% exit(reason),           % Exit
    %% erlang:raise(Class, Reason, Stacktrace),

    {Result1, Result2}.

%% ===========================================================================
%% List Comprehensions and Generators
%% ===========================================================================

comprehension_examples() ->
    %% Simple list comprehension
    Squares = [X * X || X <- [1, 2, 3, 4, 5]],

    %% With filter
    EvenSquares = [X * X || X <- lists:seq(1, 10), X rem 2 == 0],

    %% Multiple generators
    Pairs = [{X, Y} || X <- [1, 2, 3], Y <- [a, b, c]],

    %% Nested with pattern matching
    Flattened = [X || {_, L} <- [{a, [1, 2]}, {b, [3, 4]}], X <- L],

    %% Binary comprehension
    Bytes = << <<(X * 2)>> || <<X>> <= <<1, 2, 3, 4, 5>> >>,

    %% Generator with guard
    Pythagorean = [{A, B, C} ||
        A <- lists:seq(1, 20),
        B <- lists:seq(A, 20),
        C <- lists:seq(B, 20),
        A * A + B * B == C * C
    ],

    {Squares, EvenSquares, Pairs, Flattened, Bytes, Pythagorean}.

%% ===========================================================================
%% Processes and Concurrency
%% ===========================================================================

%% Spawn process
spawn_example() ->
    Pid = spawn(fun() ->
        receive
            {hello, From} ->
                From ! {world, self()};
            quit ->
                ok
        after 5000 ->
            timeout
        end
    end),
    Pid.

%% Spawn with link
spawn_link_example() ->
    spawn_link(fun() ->
        timer:sleep(1000),
        exit(normal)
    end).

%% Spawn with monitor
spawn_monitor_example() ->
    {Pid, Ref} = spawn_monitor(fun() ->
        timer:sleep(1000),
        done
    end),
    receive
        {'DOWN', Ref, process, Pid, Reason} ->
            {completed, Reason}
    after 5000 ->
        timeout
    end.

%% Process loop
server_loop(State) ->
    receive
        {call, From, Ref, Request} ->
            {Reply, NewState} = handle_request(Request, State),
            From ! {reply, Ref, Reply},
            server_loop(NewState);
        {cast, Request} ->
            NewState = handle_async_request(Request, State),
            server_loop(NewState);
        stop ->
            ok;
        _Unknown ->
            server_loop(State)
    after ?DEFAULT_TIMEOUT ->
        {timeout, State}
    end.

handle_request({get, Key}, State) ->
    Value = maps:get(Key, State, undefined),
    {Value, State};
handle_request({put, Key, Value}, State) ->
    {ok, State#{Key => Value}};
handle_request(_, State) ->
    {error, State}.

handle_async_request({log, Message}, State) ->
    io:format("Log: ~p~n", [Message]),
    State;
handle_async_request(_, State) ->
    State.

%% ===========================================================================
%% gen_server Implementation
%% ===========================================================================

-record(server_state, {
    counter = 0 :: integer(),
    data = #{} :: map()
}).

%% API Functions
start_server() ->
    gen_server:start({local, ?MODULE}, ?MODULE, [], []).

start_link(Args) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Args, []).

stop(Pid) ->
    gen_server:stop(Pid).

%% gen_server Callbacks
-spec init(Args :: term()) -> {ok, state()}.
init(_Args) ->
    process_flag(trap_exit, true),
    State = #server_state{counter = 0, data = #{}},
    {ok, State}.

-spec handle_call(Request :: term(), From :: {pid(), term()}, State :: state()) ->
    callback_result().
handle_call(get_counter, _From, #server_state{counter = Counter} = State) ->
    {reply, Counter, State};
handle_call(increment, _From, #server_state{counter = Counter} = State) ->
    NewState = State#server_state{counter = Counter + 1},
    {reply, Counter + 1, NewState};
handle_call({get, Key}, _From, #server_state{data = Data} = State) ->
    Value = maps:get(Key, Data, undefined),
    {reply, Value, State};
handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

-spec handle_cast(Request :: term(), State :: state()) ->
    {noreply, state()} | {stop, term(), state()}.
handle_cast({put, Key, Value}, #server_state{data = Data} = State) ->
    NewData = Data#{Key => Value},
    {noreply, State#server_state{data = NewData}};
handle_cast(reset, State) ->
    {noreply, State#server_state{counter = 0, data = #{}}};
handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast(_Request, State) ->
    {noreply, State}.

-spec handle_info(Info :: term(), State :: state()) -> {noreply, state()}.
handle_info(timeout, State) ->
    io:format("Timeout received~n"),
    {noreply, State};
handle_info({'EXIT', _Pid, Reason}, State) ->
    io:format("Linked process exited: ~p~n", [Reason]),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(Reason :: term(), State :: state()) -> ok.
terminate(Reason, _State) ->
    io:format("Server terminating: ~p~n", [Reason]),
    ok.

-spec code_change(OldVsn :: term(), State :: state(), Extra :: term()) ->
    {ok, state()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ===========================================================================
%% Binary Processing
%% ===========================================================================

binary_processing() ->
    %% Constructing binaries
    Packet = <<1:8, 1024:16/big, "hello">>,

    %% Binary pattern matching
    <<Type:8, Length:16/big, Payload:5/binary>> = Packet,

    %% Bitstring operations
    BitFlags = <<1:1, 0:1, 1:1, 0:1, 1:4>>,

    %% UTF-8 encoding
    Utf8 = <<"Hello, 世界!"/utf8>>,

    %% Binary comprehension
    Doubled = << <<(X * 2)>> || <<X>> <= <<1, 2, 3, 4, 5>> >>,

    %% Type specifiers
    <<Int32:32/signed-big-integer>> = <<255, 255, 255, 255>>,
    <<Float64:64/float>> = <<64, 9, 33, 251, 84, 68, 45, 24>>,

    {Packet, Type, Length, Payload, BitFlags, Utf8, Doubled, Int32, Float64}.

%% ===========================================================================
%% ETS (Erlang Term Storage)
%% ===========================================================================

ets_examples() ->
    %% Create tables
    Set = ets:new(my_set, [set, named_table, public]),
    OrderedSet = ets:new(my_ordered_set, [ordered_set]),
    Bag = ets:new(my_bag, [bag]),
    DuplicateBag = ets:new(my_dup_bag, [duplicate_bag]),

    %% Insert data
    ets:insert(Set, {key1, value1}),
    ets:insert(Set, [{key2, value2}, {key3, value3}]),

    %% Lookup
    [{key1, Value1}] = ets:lookup(Set, key1),

    %% Match
    MatchResult = ets:match(Set, {'$1', '$2'}),

    %% Select
    MatchSpec = [{{key, '$1'}, [{'>', '$1', 10}], ['$1']}],
    SelectResult = ets:select(Set, MatchSpec),

    %% Cleanup
    ets:delete(Set),
    ets:delete(OrderedSet),
    ets:delete(Bag),
    ets:delete(DuplicateBag),

    {Value1, MatchResult, SelectResult}.

%% ===========================================================================
%% Module Attributes and Compiler Directives
%% ===========================================================================

-compile([export_all, nowarn_export_all]).
-compile({inline, [add/2, factorial_tail/2]}).

-on_load(on_load_function/0).

on_load_function() ->
    io:format("Module loaded~n"),
    ok.

%% Dialyzer attributes
-dialyzer({nowarn_function, risky_function/1}).
-dialyzer([no_return, no_unused]).

risky_function(X) when is_integer(X) ->
    1 / X.

%% ===========================================================================
%% Main Entry Point
%% ===========================================================================

-spec main() -> ok.
main() ->
    io:format("=== Erlang Sample Program ===~n"),

    %% Basic functions
    io:format("Factorial 10: ~p~n", [factorial(10)]),
    io:format("Fibonacci 20: ~p~n", [fibonacci(20)]),
    io:format("Quicksort: ~p~n", [quicksort([3, 1, 4, 1, 5, 9, 2, 6])]),

    %% Pattern matching
    io:format("Pattern match: ~p~n", [pattern_match_function(42)]),

    %% Records
    Person = #person{id = 1, name = <<"Alice">>, age = 30},
    io:format("Person: ~p~n", [Person]),

    %% Maps
    User = #{id => 1, name => <<"Bob">>, active => true},
    io:format("User: ~p~n", [User]),

    %% Comprehensions
    Squares = [X * X || X <- lists:seq(1, 10)],
    io:format("Squares: ~p~n", [Squares]),

    io:format("Done!~n"),
    ok.
