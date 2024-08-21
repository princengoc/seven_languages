%Typist battle. Refactored with OTP gen_server
-module(game_server).
-behavior(gen_server).

%game constants
-define(MAXTME, 3). %max time per line in 10x of seconds
-define(DEFAULT_OTHER_NODE_IP, "bong@192.168.1.70").
-define(SUPER_POWER_SCORE, 3). %minimum score where super power can be activated
-define(SUPER_WORD, "oye"). 

%API
-export([start/1, stop/0, loop/0]).

% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

% record: a data structure like a namedtuple. 
% do not store the timer pid, instead use send_after
-record(state, {words_to_type=[], next_lines=[],score=0, time=?MAXTME+1, other_ip = none :: pid()}).


%% Helper start funcion
connect(OtherNodeIP, MaxRetry) ->
    io:format("Trying to connect to ~s ... \n", [OtherNodeIP]),
    Connected = net_adm:ping(OtherNodeIP) == pong,
    CanRetry = MaxRetry > 0,
    case {Connected, CanRetry} of
        {false, false} ->
            io:format("Unable to connect. Check your connections and restart. \n"), 
            exit(self(), unable_to_connect);
        {false, true} ->
            timer:sleep(1000),
            connect(OtherNodeIP, MaxRetry-1);
        {true, _} -> ok
    end.

%% API Functions
start(Battle) ->
    gen_server:start({local, typist}, ?MODULE, [Battle], []), 
    util:clear_screen(),
    % launch initial screen after 500ms delay
    erlang:send_after(500, typist, update_timer),
    loop().

stop() ->
    gen_server:call(typist, stop).

%% gen_server Callbacks
init([Battle]) ->
    % Initialize state
    if 
        Battle == single -> 
            util:clear_screen(), 
            io:format("Starting game in single player mode. If you want to battle, use battle()~n"),
            OtherNodeIP = list_to_atom(?DEFAULT_OTHER_NODE_IP);
        true -> 
            OtherNodeIP = list_to_atom(string:trim(io:get_line("Enter the other node address, for example bong@192.168.1.70 \n"))),
            % validate connection
            connect(OtherNodeIP, 10)
    end,
    [FirstLine | NextLines] = util:get_lines_from_file(),
    {ok, #state{words_to_type = util:get_wordlist(FirstLine), next_lines=NextLines, other_ip = OtherNodeIP}}.

handle_call(stop, _From, State) ->
    {stop, normal, ok, State}.

handle_cast(other_user_won, State) -> 
    io:format("Oh dear you lost. Try again next time! \n"),
    {stop, normal, State};
handle_cast({challenge, Words}, State) -> 
    WordsToType = State#state.words_to_type,
    NewState = State#state{words_to_type = WordsToType ++ Words}, 
    display_state(NewState),
    {noreply, NewState};
handle_cast({user_input, TypedWords}, State) -> 
    Score = State#state.score,
    WordsToType = State#state.words_to_type,
    OtherNodeIP = State#state.other_ip,
    NextLines = State#state.next_lines,
    HasSuperPower = Score > ?SUPER_POWER_SCORE-1,
    %Use super power
    Super = lists:member(?SUPER_WORD, TypedWords) andalso HasSuperPower,
    % check user_input vs TypedWords
    WordsRemain = util:match_head(TypedWords, WordsToType), 
    case {Super, length(WordsRemain)} of 
        {true, _} -> 
            Challenge = lists:filter(fun(X) -> X =/= ?SUPER_WORD end, TypedWords),
            % send the rest of the typed words as challenge to the other node
            gen_server:cast({typist, OtherNodeIP}, {challenge, Challenge}),
            % update score budget after using 
            NewScore = max(0, Score-?SUPER_POWER_SCORE),
            NewState = State#state{score = NewScore, words_to_type = WordsRemain, time = ?MAXTME}, 
            display_state(NewState),
            {noreply, NewState};
        {false, 0} -> 
            %No more words remain: update score, add new words, new round
            case length(NextLines) of 
                0 -> 
                    %No more next lines, declare victory
                    io:format("Congratulations!!! You have WON!!\n"), 
                    gen_server:cast({typist, OtherNodeIP}, other_user_won),
                    {stop, normal, State};
                _ -> 
                    %No more words remain: update score, add new words, new round
                    [NextLine|NewNextLines] = State#state.next_lines,
                    NewWords = util:get_wordlist(NextLine),
                    NewState = State#state{score = Score+1, words_to_type = NewWords, next_lines = NewNextLines, time=?MAXTME}, 
                    display_state(NewState), 
                    {noreply, NewState}
            end;
        {false, L} when L > 0 -> 
            % did not use Super power, still has stuff to type
            NewState = State#state{words_to_type = WordsRemain}, 
            display_state(NewState), 
            {noreply, NewState}
    end.



handle_info(update_timer, State) ->
    Time = State#state.time,
    if 
        Time > 1 -> 
            NewState = State#state{time = Time-1};
        true -> 
            NewScore = State#state.score-1,
            NewState = State#state{time = ?MAXTME, score = NewScore}
    end,
    display_state(NewState),
    erlang:send_after(10000, typist, update_timer),
    {noreply, NewState};
handle_info(_Info, State) -> 
    {noreply, State}.

terminate(_Reason, _State) ->
    io:format("\n Game over. Hope you enjoyed!\n"),
    ok.

%% iput loop
loop() -> 
    Input = io:get_line(""),
    [InputTrimmed|_] = string:split(Input, "\n"),
    WordsList = util:get_wordlist(InputTrimmed),
    gen_server:cast(typist, {user_input, WordsList}), 
    loop().

%% Helper Functions

clear_line() -> io:format("~c[2K", [27]).

display_state(State) -> 
    %move cursor
    Score = State#state.score,
    WordsToType = State#state.words_to_type,    
    io:format("~c[H", [27]),
    clear_line(),
    io:format("Score: ~p~n", [Score]),
    clear_line(),
    io:format("Time left: ~p0 seconds ~n", [State#state.time]),
    HasSuperPower = Score > 2,
    case HasSuperPower of
        false -> io:format("Type this! Score of 3 and you can activate SUPER POWER!! ~n");
        true -> io:format("Super power available! Type oye then a message for the poozer. ~n")
    end,
    %concatenate list of words to type into a string and print to screen
    WordsString = lists:foldl(fun(Word, Acc) -> Acc ++ Word ++ " " end, "", WordsToType),
    clear_line(),
    io:format("~s ~n", [string:trim(WordsString)]),
    clear_line().
