%utility module
-module(util). 
-export([get_wordlist/1, clear_screen/0, match_head/2, get_lines_from_file/0]).

clear_screen() -> 
    io:format("~c[2J", [27]), 
    % move cursor to top left
    io:format("~c[H", [27]).

%find longest matching heads of TypedWords vs WordsToType, 
%take away the match and returns the tail of WordsToType
match_head(_, []) -> [];
match_head([], WordsToType) -> WordsToType;
match_head([H1|T1], [Word|WordsToType]) -> 
    if 
        H1 =:= Word -> match_head(T1, WordsToType);
        H1 =/= Word -> [Word|WordsToType]
    end.

% get words from each line
get_wordlist(Line) -> string:split(Line, " ", all).

% read in from a file
% default filename: scarecrow.txt
get_lines_from_file() -> 
    Input = string:trim(io:get_line("Specify which text you want. We have scarecrow, fish and test \n")),
    TargetFile = "data/" ++ Input ++ ".txt",
    case filelib:is_file(TargetFile) of 
        true -> 
            ChosenFile = TargetFile;
        false -> 
            io:format("~p not found. Using scarecrow as default ~n", [TargetFile]),
            ChosenFile = "data/scarecrow.txt"
    end,
    {ok, Binary} = file:read_file(ChosenFile), 
    clear_screen(),
    Content = binary_to_list(Binary), 
    Lines = string:split(Content, ["\n"], all),
    Lines.
