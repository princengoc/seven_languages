%Typist Battle!
%Simple implementation
-module(game).
-export([single/0, battle/0, get_maxtime/0]).

get_maxtime() -> 3. %30 seconds per line

% formatting functions
clear_line() -> io:format("~c[2K", [27]).

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
        {true, _} -> 
            start(OtherNodeIP)
    end.

battle() ->
    util:clear_screen(),
    io:format("Starting the battle! \n"),
    OtherNodeIP = list_to_atom(string:trim(io:get_line("Enter the other node address, for example bong@192.168.1.70 \n"))),
    connect(OtherNodeIP, 10).

single() -> 
    util:clear_screen(),
    io:format("Starting game in single player mode. If you want to battle, use battle()~n"),
    OtherNodeIP = list_to_atom("phantom@1.2.3.4"),
    start(OtherNodeIP).

start(OtherNodeIP) -> 
    util:clear_screen(), % ANSI code to clear the screen
    % Spawn the game loop with initial state
    util:clear_screen(),
    [FirstLine | NextLines] = util:get_lines_from_file(),
    util:clear_screen(),
    GameLoopPid = spawn(fun() -> game_loop({0, util:get_wordlist(FirstLine), NextLines, get_maxtime(), OtherNodeIP}) end),
    register(typist, GameLoopPid),
    % Spawn a process to handle user input, passing the game loop pid
    input_loop().    

input_loop() ->
    Input = io:get_line(""),
    [InputTrimmed|_] = string:split(Input, "\n"),
    WordsList = util:get_wordlist(InputTrimmed),
    typist ! {user_input, WordsList},
    input_loop().

game_loop({Score, WordsToType, NextLines, Time, OtherNodeIP}) ->
    % Move the cursor to the top-left corner (row 1, column 1)
    io:format("~c[H", [27]),   % ANSI code to move the cursor to the top-left corner
    io:format("Score: ~p~n", [Score]),
    io:format("Time left: ~p~n", [Time]),
    clear_line(),
    HasSuperPower = Score > 2,
    case HasSuperPower of
        false -> io:format("Type this! Score of 3 and you can activate SUPER POWER!! ~n");
        true -> io:format("Super power available! Type oye then a message for the poozer. ~n")
    end,

    %concatenate list of words to type into a string and print to screen
    WordsString = lists:foldl(fun(Word, Acc) -> Acc ++ Word ++ " " end, "", WordsToType),
    clear_line(),
    io:format("~s ~n", [string:trim(WordsString)]),
    clear_line(),

    %refresh score every 10 seconds
    Timer = erlang:start_timer(10000, self(), Time-1),
    
    Maxtime = get_maxtime(),

    receive
        {other_user_won} -> 
            io:format("Oh dear you lost. Rematch? \n"),
            init:stop();
        {challenge, Words} -> 
            %append the adversary's challenge current state
            WordsRemain = WordsToType ++ Words, 
            % start another round
            erlang:cancel_timer(Timer),
            game_loop({Score, WordsRemain, NextLines, Maxtime, OtherNodeIP});
        {user_input, TypedWords} ->
            erlang:cancel_timer(Timer),
            % check if user meets the condition for super power: Score is at least 3 AND user typed the word "oye"
            Super = lists:member("oye", TypedWords) andalso HasSuperPower,
            % check user_input vs TypedWords
            WordsRemain = util:match_head(TypedWords, WordsToType), 
            case {Super, length(WordsRemain)} of 
                {true, _} -> 
                    Challenge = lists:filter(fun(X) -> X =/= "oye" end, TypedWords),
                    % send the rest of the typed words as challenge to the other node
                    {typist, OtherNodeIP} ! {challenge, Challenge},
                    % use up 3 points to send the challenge
                    NewScore = max(0, Score-3),
                    game_loop({NewScore, WordsRemain, NextLines, Maxtime, OtherNodeIP});
                {false, 0} -> 
                    %No more words remain: update score, add new words, new round
                    case length(NextLines) of 
                        0 -> 
                            %No more next lines, declare victory
                            io:format("Congratulations!!! You have WON!! Rematch? \n"), 
                            {typist, OtherNodeIP} ! {other_user_won},
                            init:stop();
                        _ -> 
                            %No more words remain: update score, add new words, new round
                            game_nextline(Score+1, NextLines, Maxtime, OtherNodeIP)                            
                    end;
                {false, L} when L > 0 -> 
                    game_loop({Score, WordsRemain, NextLines, Time, OtherNodeIP})
            end;
        %handling timeout events. Timeout happens every 1 second since we need to update the score.
        {timeout, _, TimeLeft} -> 
            if
                TimeLeft == 0 -> 
                    %timed out
                    NewScore = Score - 1,
                    game_loop({NewScore, WordsToType, NextLines, Maxtime, OtherNodeIP});
                TimeLeft > 0 -> 
                    game_loop({Score, WordsToType, NextLines, TimeLeft, OtherNodeIP})
            end
    end.

game_nextline(Score, NextLines, Maxtime, OtherNodeIP) -> 
    [NextLine|NewNextLines] = NextLines, 
    NewWords = util:get_wordlist(NextLine),                
    % start another round
    game_loop({Score, NewWords, NewNextLines, Maxtime, OtherNodeIP}).