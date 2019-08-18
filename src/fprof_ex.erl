-module(fprof_ex).

-export([

]).

func() ->
    ok.

    % fprof:apply(foo, create_file_slow, [junk, 1024]).


%     fprof:start().
%     fprof:trace(cpu_time).
%     fprof:profile().
%     fprof:stop().





%fprof:start().
%fprof:trace([start, {procs, all}]).
%timer:sleep(1000).
%fprof:trace([stop]).
%fprof:profile().
%fprof:analyse([totals, {dest, "fprof.analysis"}]).
%fprof:stop().


% sudo apt install kcachegrind
% git clone https://github.com/isacssouza/erlgrind
