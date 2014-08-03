 -module(erlog_make_server).
 %% Licensed under the Apache License, Version 2.0 (the "License");
 %% you may not use this file except in compliance with the License.
 %% You may obtain a copy of the License at
 %%
 %%     http://www.apache.org/licenses/LICENSE-2.0
 %%
 %% Unless required by applicable law or agreed to in writing, software
 %% distributed under the License is distributed on an "AS IS" BASIS,
 %% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 %% See the License for the specific language governing permissions and
 %% limitations under the License.

 %% File    : erlog_make_server.erl
 %% Author  : Zachary Kessin
 %% Purpose : Convert an erlog .pl file to an erlang gen_server





-compile(export_all).
-compile({parse_transform, seqbind}).
-include_lib("eunit/include/eunit.hrl").
-export([erlog/2, compile_file/2]).
-ifdef(TEST).
-compile(export_all).
-endif.

erlog(_Config, _AppFile) ->
    ErlogDir = filename:join([rebar_utils:get_cwd(), "erlog"]),
    compile_files(filelib:is_dir(ErlogDir), ErlogDir).
    

compile_files(false,_) ->
    {ok,[]};
compile_files(true, Directory) ->
    Files = filelib:wildcard(Directory ++"/*.pl"),
    ModuleNames = lists:map(fun(File) ->
				    {ok, _Module, Binary}	= compile_file(File, make_module_name(File)),
				    BeamFile			= filename:join([rebar_utils:get_cwd(),"ebin", atom_to_list(make_module_name(File)) ++ ".beam"]),
				    ok			= file:write_file(BeamFile, Binary),
				    make_module_name(File)
			    end, Files),
    {ok,ModuleNames}.

make_module_name(File) -> list_to_atom(filename:basename(File, ".pl")).


 %% -spec(compile_buffer(atom(), iolist()) ->
 %% 	     {ok, atom()}).




compile_file(File,Module) when is_atom(Module)->
    {ok, PL@}            = erlog:new(),
    {ok, PL@}            = erlog:consult(PL@,File),
    {ok, PL@}	         = erlog:consult(PL@, "src/erlog_make_server.pl"),
    {ok, PL@}            = record_defs(PL@),
    {Exports,PL@}        = find_exports(PL@),
    {ok, AST@}           = load_base_ast(),
    {ok, AST@}           = replace_filename(AST@, File),
    {ok, AST@}           = replace_module_name(AST@, Module),
    {ok, AST@}           = replace_start_link(AST@, Module),
    {ok, AST@}           = load_db_state(AST@, PL@),
    {ok, AST@}           = make_interface_functions(AST@, Exports, PL@),
    {ok, AST@}           = add_exports(AST@, Exports, PL@),
    {ok, AST@}           = make_handler_clauses(AST@,Exports,PL@),
    case compile:forms(AST@, [from_ast, debug_info, return]) of
	 {ok, Module, Binary,Errors} ->
	     file:write_file("errors", io_lib:format("% -*- Erlang -*- ~n~n~p~n",[Errors])),
	     {module, Module}     = code:load_binary(Module, File, Binary),
	     {ok, Module, Binary};
	 E ->
	     E
     end.


 -spec(load_base_ast() -> {ok, term()}).
 load_base_ast()->
    FileName   = "priv/custom_server.erl",
    {ok, AST}  = epp:parse_file(FileName,[],[]),
    {ok, AST}.

replace_filename([FileLine|Rest] = _AST, PrologFile) ->
    {attribute,_, file, {_, _}} = FileLine,
    {ok,[{attribute,1, file, {PrologFile, 1}}|Rest]}.

replace_module_name([FirstLine,ModuleLine|Rest], PLModule) ->
    {attribute,_, module,_} = ModuleLine,
    {ok,[FirstLine,
         {attribute,3,module, PLModule}| Rest]}.

replace_start_link(AST, PLModule) ->
    AST1 = lists:keyreplace(start_link, 3, AST, 
                            {function,31,start_link,0,
                             [{clause,31,[],[],
                               [{call,32,
                                 {remote,32,{atom,32,gen_server},{atom,32,start_link}},
                                 [{atom,32,PLModule},{nil,32},{nil,32}]}]}]}),
    {ok, AST1}.
                         
print_exports(AST) ->   
    Exports = lists:filter(fun({attribute,_,export,_}) -> true;
                              (_)  -> false
                           end, AST),
    Exports.

update_exports(Exports, PL) ->
    lists:map(fun({Pr, Ar}) ->
		      case get_return_value({'/', Pr, Ar},PL) of
			  none ->
			       {Pr, Ar + 1};
			  last ->
			      {Pr, Ar }
		      end
	      end, Exports).

add_exports(AST, PLExports, PL) ->  
    print_exports(AST),
    Exports = update_exports(PLExports,PL),

    AST1 = lists:map(fun({attribute,Line,export,[]}) ->
                                   {attribute,Line,export,Exports};
                              (X) -> X
                           end,AST),
    print_exports(AST1),
    {ok,AST1}.

make_supervisor_childspec(AST,PLModule) ->
    Line = 23,
    AST1 = lists:keyreplace(make_supervisor_childspec,3, AST,
                             {function,Line,make_supervisor_childspec,0,
                              [{clause,Line,[],[],
                                [{tuple,21,
                                  [{atom,21,ok},
                                   {tuple,21,
                                    [{atom,21,PLModule},
                                     {tuple,22,
                                      [{atom,22,PLModule},
                                       {atom,22,start_link},
                                       {nil,22}]},
                                     {atom,23,permanent},
                                     {integer,23,100},
                                     {atom,23,worker},
                                     {cons,23,{atom,23,PLModule},{nil,23}}]}]}]}]}),

    {ok,AST1}.
    

find_exports(PL) ->
    PL@ = PL,
    case erlog:prove(PL@, {find_exports, {'Exports'}}) of 
	{{succeed, Res},PL@} ->
	    Exports = [{Fun, Arity } || {'/', Fun,Arity} <- proplists:get_value('Exports', Res)],
	    
	    {Exports,PL@};
	{fail,PL@} ->
	   {[], PL@}
    end.

field_names(Fields) ->
    [FieldName || {record_field, _, {atom, _, FieldName}} <- Fields].

get_records(AST) ->
    AST@ = lists:filter(fun({attribute, _, record, _}) ->
				true;
			   (_) -> false
			end, AST),
    AST@ = lists:map(fun({attribute, _, record, {RecordName, Fields}}) ->
			     {record, RecordName, field_names(Fields)}
		     end, AST@),

    {ok, AST@}.

record_defs(PL) ->
    {ok,PL@}                            = erlog:consult(PL, "priv/records.pl"),
    {{succeed, [{import, Files}]}, PL@}	= erlog:prove(PL@, {find_imports,{import}}),
    PL@ = lists:foldl(fun(File, PL) ->
			      Erlog@ = PL,
			      {ok, AST}  = epp:parse_file(File,[],[]), 
			      {ok,Records} = get_records(AST),
			      {{succeed,_}, Erlog@} = erlog:prove(Erlog@,{records, Records}),
			      Erlog@
		      end, PL@, Files),
    {ok,PL@}.

load_db_state(AST, E0) ->
    DB             = erlog:get_db(E0),
    AbstractDB    = abstract(DB),
    AST1          = lists:keyreplace(db_state, 3, AST, 
                             {function,26,db_state,0,
                              [{clause,26,[],[],[{tuple,27,[{atom,27,ok},AbstractDB]}]}]}),
    {ok, AST1}. 



make_interface_function({Function, Arity},PL) when is_atom(Function) and is_integer(Arity)->
    Line = 31,
    RetVal = get_return_value({'/',Function,Arity},PL),

    Params = case RetVal of 
		 last -> make_param_list(Arity -1, Line);
		 none -> make_param_list(Arity , Line)
	     end,
    EfArity = case RetVal of 
		  last -> Arity;
		  none -> Arity + 1
	      end,
    FN = {function,Line,Function, EfArity ,
     [{clause,Line,
       [{var,Line,'Pid'}|Params],
       [],
       [{call,Line,
         {remote,Line,{atom,30,gen_server},{atom,30,call}},
         [{var,Line,'Pid'},
          {tuple,Line,
             [{atom, Line, Function}| Params]
          }
	 ]}]}]},
    FN.


insert_interface_inner([],_, Acc) ->
    {ok,Acc};
insert_interface_inner([{function,_, interface,_,_}|Rest], [], Acc) ->
    insert_interface_inner(Rest,[], Acc);
insert_interface_inner(AST = [{function,_, interface,_,_}|_], [E|ERest], Acc) ->
    insert_interface_inner(AST,ERest, Acc ++ [E]);
insert_interface_inner([F|Rest], E, Acc) ->
    insert_interface_inner(Rest, E, Acc ++ [F]).
    

insert_interface(AST, Exports) ->
    insert_interface_inner(AST,Exports,[]).


make_interface_functions(AST, Exports, PL) ->
    InterfaceFns = [make_interface_function(Fn,PL)|| Fn <-Exports],    
    insert_interface(AST, InterfaceFns).


make_handler_clauses(AST, Exports,PL) ->
    NewClauses = [base_fn_clause(Predicate, Arity, PL) ||{Predicate, Arity} <- Exports],
    AST1       = lists:keyreplace(handle_call,3, AST,
                                  {function, 39, handle_call, 3,
                                   NewClauses}),
    {ok,AST1}.


make_param_list(ParamCount,Line) when is_integer(ParamCount)->   
    PList     = lists:seq(65,65 + ParamCount -1),
    PAtomList = [list_to_atom([$_,P]) ||P <-PList],
    [{var, Line, PAtom}|| PAtom <- PAtomList].


get_return_value(Function, PL) ->
    {{succeed, [{'RetVal', RetVal}]},_}= erlog:prove(PL, {find_return, Function, {'RetVal'}}),
    RetVal.

     
base_fn_clause(Predicate, ParamCount,PL ) when is_atom(Predicate) andalso is_integer(ParamCount) ->
    RetVal = get_return_value({'/',Predicate,ParamCount},PL),
    
    IParamList = [{atom,46,Predicate}] ++ case RetVal of 
					     none -> make_param_list(ParamCount,46);
					     last -> make_param_list(ParamCount -1, 46)
					 end,
    ParamList =   case RetVal of 
		      none -> IParamList;
		      last -> IParamList ++ [{tuple, 55, [{atom, 55,'Return'}]}]
		  end,
    {clause,54,
     [{match,54,
       {var,54,'_Request'},
       {tuple,54,
	IParamList
       }},
      {var,54,'_From'},
      {var,54,'Erlog'}],
     [],
     [{call,55,
       {atom,55,handle_prolog},
       [{tuple,55,ParamList},{var,55,'Erlog'},{atom,55,RetVal}]}
     ]}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Taken from ulf wiger's parse_trans module

%% abstract/1 - modified from erl_eval:abstract/1:
-type abstract_expr() :: term().
-spec abstract(Data) -> AbsTerm when
      Data :: term(),
      AbsTerm :: abstract_expr().
abstract(T) when is_function(T) ->
    case erlang:fun_info(T, module) of
	{module, erl_eval} ->
	    case erl_eval:fun_data(T) of
		{fun_data, _Imports, Clauses} ->
		    {'fun', 0, {clauses, Clauses}};
		false ->
		    erlang:error(function_clause)  % mimicking erl_parse:abstract(T)
	    end;
	_ ->
	    erlang:error(function_clause)
    end;
abstract(T) when is_integer(T) -> {integer,0,T};
abstract(T) when is_float(T) -> {float,0,T};
abstract(T) when is_atom(T) -> {atom,0,T};
abstract([]) -> {nil,0};
abstract(B) when is_bitstring(B) ->
    {bin, 0, [abstract_byte(Byte, 0) || Byte <- bitstring_to_list(B)]};
abstract([C|T]) when is_integer(C), 0 =< C, C < 256 ->
    abstract_string(T, [C]);
abstract([H|T]) ->
    {cons,0,abstract(H),abstract(T)};
abstract(Tuple) when is_tuple(Tuple) ->
    {tuple,0,abstract_list(tuple_to_list(Tuple))}.

abstract_string([C|T], String) when is_integer(C), 0 =< C, C < 256 ->
    abstract_string(T, [C|String]);
abstract_string([], String) ->
    {string, 0, lists:reverse(String)};
abstract_string(T, String) ->
    not_string(String, abstract(T)).

not_string([C|T], Result) ->
    not_string(T, {cons, 0, {integer, 0, C}, Result});
not_string([], Result) ->
    Result.

abstract_list([H|T]) ->
    [abstract(H)|abstract_list(T)];
abstract_list([]) ->
    [].

abstract_byte(Byte, Line) when is_integer(Byte) ->
    {bin_element, Line, {integer, Line, Byte}, default, default};
abstract_byte(Bits, Line) ->
    Sz = bit_size(Bits),
    <<Val:Sz>> = Bits,
    {bin_element, Line, {integer, Line, Val}, {integer, Line, Sz}, default}.


