-module(music).
-compile(export_all).
-include_lib("kernel/include/file.hrl").

run() ->
    Files = list_dir("./"),
    lists:sort(Files).

list_dir(Path) ->
    case file:list_dir(Path) of
        {ok,[]} ->
            [];
        {ok,Contents} ->
            [ if_dir_list(Path++"/"++Filename) || Filename <- Contents ];
        {error,_} ->
            []
    end.

% / file
% / dir / file
% / dir / dir / file

if_dir_list(Filename) ->
    case file:read_file_info(Filename) of 
        {ok,FileInfo} when FileInfo#file_info.type == directory ->
            list_dir(Filename);
        {ok,_} ->
            Filename;
        {error,Reason} ->
            [{error,Reason}]
    end.