%%%
%%Generic TCP server 
%%% Place holder til a new protocol is developed




-module(tcp_receiver_enroll).

-author('mosheadormeo@gmail.com').

-export([listen/1,listen/0,start/1,start/0,stop/0]).

-define(TCP_OPTIONS, [binary, {packet, 0}, {active, false}, {reuseaddr, true}]).
-define(PORT, 15001).

start() ->
    listen().
start(Port) ->
      listen(Port).


% Call echo:listen() to start the server.
listen(Port) ->
    {ok, LSocket} = gen_tcp:listen(Port, ?TCP_OPTIONS),
    spawn(fun() -> accept(LSocket) end).
listen() ->
    
    {ok, LSocket} = gen_tcp:listen(?PORT, ?TCP_OPTIONS),
     register(tcp_receiver, spawn(fun() -> accept(LSocket) end)),
     {ok,self()}.
    %% accept(LSocket).


% Wait for incoming connections and spawn a process that will process incoming packets.
accept(LSocket) ->
    {ok, Socket} = gen_tcp:accept(LSocket),
    Pid = spawn(fun() ->
        io:format("Connection accepted ~n", []),
        loop(Socket)
    end),
    gen_tcp:controlling_process(Socket, Pid),
    accept(LSocket).


loop(Sock) ->
      io:format("in L one"),
    inet:setopts(Sock, [{active, once}]),
     
    receive
    {tcp, Socket, Data} ->
        io:format("Got packet: ~p~n", [Data]),
        %%gen_tcp:send(Socket, Data),
      
         File = "",
       %%  loop(Socket,File),
         %% put identification trigger here
	 FeedBack =  uhac_port: check_face({enroll,Data}),
         gen_tcp:send(Socket,FeedBack)
          ;
    {tcp_closed, Socket}->
        io:format("Socket ~p closed~n", [Socket]);
    {tcp_error, Socket, _Reason} ->
        io:format("Error on socket ~p reason: ~p~n", [Socket, _Reason])
    end.

loop(Sock,File) ->
    inet:setopts(Sock, [{active, once}]),
    receive
    {tcp,_Socket,<<"DONE">>} -> file:close(File);
         
    {tcp, Socket, Data} ->
       
        
        
         ok = file:write(File,Data),
         
         loop(Socket,File);
      {error,_Reason} ->
     
         file:close(File)
   after 
            1000 ->   file:close(File)
    end.

stop()->
    timer:kill_after(1000, whereis(tcp_receiver)),
    ok.
