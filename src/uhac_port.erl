%%%-------------------------------------------------------------------
%%% @author Moshe Adormeo <madormeo@localhost.localdomain>
%%% @copyright (C) 2016, Moshe Adormeo
%%% @doc
%%%
%%% @end
%%% Created : 27 Aug 2016 by Moshe Adormeo <madormeo@localhost.localdomain>
%%%-------------------------------------------------------------------
-module(uhac_port).

-behaviour(gen_server).

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3,call_url/2]).

-export([check_face/1]).

-define(SERVER, ?MODULE).

-record(state, {port}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(ExtProg) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, ExtProg, []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([ExtProg]) ->
     process_flag(trap_exit, true),
   %% Port = open_port({spawn, ExtProg}, [stream, {line, get_maxline()}]),
   Opts = [{packet, 4},binary,exit_status, use_stdio],
    Port = open_port({spawn_executable, ExtProg}, Opts),    
    {ok, #state{port=Port}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
%%handle_call(_Request, _From, State) ->
%%    Reply = ok,
%%    {reply, Reply, State}.

handle_call({verifyface, Msg}, _From, #state{port = Port} = State) ->
    io:fwrite(Msg),


  
    port_command(Port, Msg),
    case collect_response(Port) of
        {response, Response} -> 
            {reply, Response, State};
        timeout -> 
            {stop, port_timeout, State}
    end;

handle_call({enrollface, Msg}, _From, #state{port = Port} = State) ->
    Message = Msg ++ "|enroll",

    port_command(Port,Message),
    case collect_response(Port) of
        {response, Response} -> 
         [Acct,_Confidence,Uname,MSISDN|_Rest] =  binary:split(Response,<<",">>, [global]),
	     {ok, {{_Version, 200, _ReasonPhrase}, _Headers, Body}} = call_url(enroll,{Acct,Uname,MSISDN}),
	    
	    
            
            {reply, Response, State};
        timeout -> 
            {stop, port_timeout, State}
    end.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%%%%
%%%% External Calls

check_face({verify,FileName}) ->
    Len = length(FileName),
    
     if 
                Len < 99 ->
		    P =   string:left(FileName,99) ++ "\n",
                       gen_server:call(?MODULE,{verifyface,P});
		 Len == 99 -> gen_server:call(?MODULE,{verifyface,FileName ++ "\n"});
                true -> {error, "Wrong file format"}
       end;

check_face({enroll,FileName}) ->
          Len = length(FileName),
    
     if 
                Len < 99 ->
		    P =   string:left(FileName,99) ++ "\n",
                       gen_server:call(?MODULE,{enrollface,P});
		Len == 99 -> gen_server:call(?MODULE,{enrollface,FileName ++ "\n"});
                true -> {error, "Wrong file format"}
       end.
     
       


%%%===================================================================
%%% Internal functions
%%%===================================================================


collect_response(Port)->
  
    receive
        {Port, {data, AnswerData}} ->
          
           {response,  AnswerData};
        {Port, {exit_status, _Status}} ->
            erlang:error(port_exit);
	 _X -> io:fwrite(_X)
    end.

call_url(enroll,Data) ->
     {Acct,Uname,MSISDN} = Data,
     httpc:request(post, {"http://192.168.254.6/uhack/acct.php",[],
					      
"application/x-www-form-urlencoded","acct="++
						  binary_to_list(Acct)++"&uname="++binary_to_list(Uname) ++"&msisdn=" 
					     ++ binary_to_list(MSISDN)  },
		  [],[]);
call_url(verified,Data) ->
         Data.

