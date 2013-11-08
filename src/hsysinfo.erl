-module(hsysinfo).
-export([
         out/0,
         cnt/0,
         mem/0,
         etop/0,
         gc/1,
         pro/0
        ]).

out() ->
    process_infos().

cnt() ->
    erlang:system_info(process_count).

mem() ->
    filelib:ensure_dir("../logs/"),
    File = "../logs/pro_memory.log",
    {ok, Fd} = file:open(File, [write, raw, binary, append]), 
    Info = io_lib:format("=>~p \n\n",[erlang:memory()]),
    case  filelib:is_file(File) of
        true   ->   file:write(Fd, Info);
        false  ->
            file:close(Fd),
            {ok, NewFd} = file:open(File, [write, raw, binary, append]),
            file:write(NewFd, Info)
    end.

etop() ->
    spawn(
      fun() -> 
              etop:start([{output, text}, {interval, 30}, {lines, 20}, {sort, memory}]) 
      end).   

gc(Pid) ->
    erlang:garbage_collect(Pid).

process_infos() ->   
    filelib:ensure_dir("../logs/"),
    File = "../logs/processes_infos.log",
    {ok, Fd} = file:open(File, [write, raw, binary, append]), 
    Fun = fun(Pi) ->
                  Info = io_lib:format("=>~p \n\n",[Pi]),
                  case  filelib:is_file(File) of
                      true   ->   file:write(Fd, Info);
                      false  ->
                          file:close(Fd),
                          {ok, NewFd} = file:open(File, [write, raw, binary, append]),
                          file:write(NewFd, Info)
                  end,
                  timer:sleep(20)
          end,
    [   Fun(erlang:process_info(P)) ||   P <- erlang:processes()].  

pro() ->   
    filelib:ensure_dir("../logs/"),
    File = "../logs/pro_registered_name.log",
    {ok, Fd} = file:open(File, [write, raw, binary, append]), 
    Fun = fun(Pi) ->
                  Info = io_lib:format("=>~w \n",[Pi]),
                  case  filelib:is_file(File) of
                      true   ->   file:write(Fd, Info);
                      false  ->
                          file:close(Fd),
                          {ok, NewFd} = file:open(File, [write, raw, binary, append]),
                          file:write(NewFd, Info)
                  end,
                  timer:sleep(20)
          end,
    [   Fun(erlang:process_info(P, registered_name)) ||   P <- erlang:processes()].
   
