-module(gb_trees_fs).

%% Use something like inotify ( create/ delete )
%% To be able to update the tree.

-export([
    stop/0,
    run/1
]).

% a
% ├── b
% │   └── e
% │       └── file
% └── c
%     └── f
%         └── file


% a = registered (gb_trees_fs) (start work when all parents report done)

% b = (parent) ( start work when all children reported done)
% e = leaf ( start-work, when done, call parent)
% c = (parent) ( start work when all children reported done)



% parent
% └── child1
%     ├── child2
%     │   └── file2
%     └── file1


stop() ->
    erlang:exit(whereis(?MODULE), kill).

run(Dir) ->
    S = self(),
    ParentPid = spawn(fun() -> tree_registry(S) end),
    % io:format("Top parent ~p\n", [ParentPid]),
    true = erlang:register(?MODULE, ParentPid),
    receive
        ready ->
            % io:format("root tree ready\n")
            ok
    end,
    create_dir_tree(Dir, ParentPid).

create_dir_tree(Dir, ParentPid) ->
    recurse_dir(
        filelib:is_dir(Dir),
        Dir,
        ParentPid
    ).

recurse_dir(true, Dir, ParentPid) ->
    % io:format("create parent ~p\n", [Dir]),
    XPid = spawn( fun() -> child(Dir) end ),
    XPid ! {new_child, ParentPid},
    recurse_dir_files(file:list_dir_all(Dir), Dir, XPid);
recurse_dir(false, _Dir, _ParentPid) ->
    file.

recurse_dir_files({error, Reason}, _Dir, _XPid) ->
    io:format("tell parent i'm done - error ~p!\n", [Reason]);
recurse_dir_files({ok, Filenames}, Dir, XPid) ->
    recurse_dir_files(Filenames, Dir, XPid);
recurse_dir_files([], _Dir, XPid) ->
    XPid ! done,
    % io:format("tell parent i'm done (~p)!\n", [Dir]),
    done;
recurse_dir_files([FileOrDir | Filenames], Dir, XPid) ->
    Key = filename:join(Dir, FileOrDir),
    case create_dir_tree(Key, XPid) of
        done ->
            recurse_dir_files(Filenames, Dir, XPid);
        file ->
            % io:format("New leaf ~p \n", [Key]),
            XPid ! {new_leaf, Key},
            recurse_dir_files(Filenames, Dir, XPid);
        NestedTree ->
            % io:format("New node ~p \n", [Key]),
            XPid ! {new_tree, FileOrDir, NestedTree},
            recurse_dir_files(Filenames, Dir, XPid)
    end.

child(Dir) ->
    child(Dir, gb_trees:empty(), []).

%% mmm, list as ChildNodes, maybe better choice needed.
%% Child A, parent pids = [whereis(?MODULE)],
%% Child B, parent pids = [PidA]
%% Child E, parent pids = [PidA, PidB]
child(Dir, Tree, ParentPids) ->
    receive
        {new_child, XPid} ->
            ?FUNCTION_NAME(
                Dir,
                Tree,
                lists:append([XPid], ParentPids)
            );
        {new_tree, FileOrDir, NestedTree} ->
            io:format("store tree ~p ~p\n", [Dir, NestedTree]),
            ?FUNCTION_NAME(
                Dir,
                gb_trees:insert(FileOrDir, NestedTree, Tree),
                ParentPids
            );
        {new_leaf, Leaf} ->
            % io:format("store leaf ~p ~p\n", [Dir, Leaf]),
            ?FUNCTION_NAME(
                Dir,
                gb_trees:insert(Leaf, Leaf, Tree),
                ParentPids
            );
        done ->
            %% Tell my parent that i'm done.
            % io:format("---> Parent ~p tree ~p\n", [Dir, Tree]),
            % io:format("~p Done TREE: ~p\n", [Dir, gb_trees:to_list(Tree)]),
            % io:format("need to tell parents ~p\n", [ParentPids]),
            lists:foreach(
                fun(P) ->
                    P ! {new_tree, Dir, Tree},
                    P ! done
                end,
                ParentPids
            ),
            ?FUNCTION_NAME(
                Dir,
                Tree,
                ParentPids
            );
        _X ->
            ?FUNCTION_NAME(
                Dir,
                Tree,
                ParentPids
            )
    end.

tree_registry(S) ->
    ?FUNCTION_NAME(S, gb_trees:empty(), false).

tree_registry(S, FullTree, true) ->
    receive
        done ->
            % io:format("root parent done ~p\n", [gb_trees:to_list(FullTree)]),
            ?FUNCTION_NAME(
                S,
                FullTree,
                true
            );
        {new_tree, FileOrDir, NestedTree} ->
            ?FUNCTION_NAME(
                S,
                gb_trees:insert(FileOrDir, NestedTree, FullTree),
                true
            )
    end;
tree_registry(S, FullTree, false) ->
    % io:format("tree_registry not ready\n"),
    S ! ready,
    % io:format("tree_registry Ready\n"),
    ?FUNCTION_NAME(S, FullTree, true).

%% 8774272

% -export([create_dir_tree/1]).

% create_dir_tree(Dir) ->
%     recurse_dir(
%         filelib:is_dir(Dir),
%         Dir
%     ).

% recurse_dir(true, Dir) ->
%     % io:format("list all dir ~p\n", [Dir]),
%     recurse_dir_files(file:list_dir_all(Dir), Dir);
% recurse_dir(false, _Dir) ->
%     false.

% recurse_dir_files({error, eacces}, _Dir) ->
%     error;
% recurse_dir_files({ok, Filenames}, Dir) ->
%     recurse_dir_files(Filenames, Dir, gb_trees:empty()).

% recurse_dir_files([], _Dir, Tree) ->
%     Tree;
% recurse_dir_files([FileOrDir | Filenames], Dir, Tree) ->
%     Key = filename:join(Dir, FileOrDir),
%     Tree2 =
%         case create_dir_tree(Key) of
%             error ->
%                 Tree;
%             false ->
%                 _File = FileOrDir,
%                 %% Chop filename
%                 gb_trees:insert(Key, FileOrDir, Tree);
%             NestedTree ->
%                 _Dir = FileOrDir,
%                 gb_trees:insert(Key, NestedTree, Tree)
%         end,
%     recurse_dir_files(Filenames, Dir, Tree2).