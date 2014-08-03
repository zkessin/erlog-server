-module(erlog_make_server_tests).
-include_lib("eqc/include/eqc.hrl").
-include_lib("eunit/include/eunit.hrl").
-compile(export_all).
-compile({parse_transform, seqbind}).

pl_arity() ->
    choose(2,4).

clause_name() ->
    elements(['edge','connected', ancestor, descendent,path, travel]).

ret_val() ->
    oneof([last, none]).

erl_export() ->
    {erl_export, {'/', clause_name(), pl_arity()}, return, ret_val()}.



prop_find_exported_clauses() ->
    ?FORALL(Clauses, non_empty(list(erl_export())),
	    begin
		{ok, PL@}       = erlog:new(),
                PL@             = add_export_clauses(Clauses, PL@),
		{ok,PL@}        = erlog:consult(PL@, "src/erlog_make_server.pl"),
		{Exports,_}     = erlog_make_server:find_exports(PL@),
                ?assertEqual(length(Clauses),length(Exports)),
		?assert(lists:all(fun(_Export = {Fun, Arity}) ->
					  lists:keymember({'/',Fun, Arity}, 2, Clauses)
			      end, Exports)),
		true
	    end).



add_export_clauses(Clauses, PL0) ->
    lists:foldl(fun(CL,PL) ->
			{{succeed, _},PL1} = erlog:prove(PL,{asserta, CL}),
			PL1
                end, PL0, Clauses).

purge(ModName) ->
    %% Purge any results of old runs
    code:purge(ModName),
    code:delete(ModName).
prop_load_base_ast()->
    application:start(erlog),
    application:start(erlog_server),
    {ok, AST}  = erlog_make_server:load_base_ast(),
    lists:all(fun(F) ->
                      is_tuple(F)
              end, AST).
erlog_modules() ->
    ['fleck',
     'et',
     't1',
     'trees',
     'test',
     'family',
     'homer',
     'timer',
     'edges_pl',
     'po_set',
     'erlog_make_server',
     'example_dcg',
     'graph',
     'finite_dcg',
     'po_set'].
    
erlog_files() ->
    ["examples/fleck.pl",
     "examples/et.pl",
     "examples/t1.pl",
     "examples/trees.pl",
     "examples/test.pl",
     "examples/family.pl",
     "examples/homer.pl",
     "examples/timer.pl",
     "priv/edges_pl.pl",
     "priv/po_set.pl",
     "src/erlog_make_server.pl",
     "example_dcg.pl",
     "test/graph.pl",
     "test/finite_dcg.pl",
     "test/po_set.pl"].

prop_replace_file() ->
    {ok, AST}  = erlog_make_server:load_base_ast(),
    ?FORALL(PrologFile,
            elements(erlog_files()),
            begin
                {ok, AST1} = erlog_make_server:replace_filename(AST,PrologFile),
                [{attribute,1,file, {PrologFile,1}}|_] = AST1,
                {ok, _,_} = compile:forms(AST1, [from_ast,debug_info]),
                true
            end).

prop_replace_module_name() ->
    application:start(erlog_server),
    {ok, [ASTHead,_|Rest] = AST}  = erlog_make_server:load_base_ast(),
    ?FORALL(PrologModule,
            elements(erlog_modules()),
            begin

                {ok, [ASTHead,NewModuleLine|Rest] = AST1} = erlog_make_server:replace_module_name(AST, PrologModule),
                ?assertEqual({attribute,3,module,PrologModule}, NewModuleLine),
                {ok, _,_} = compile:forms(AST1, [from_ast,debug_info]),
                true
            end).

%%TODO, compile module and check that this is a valid child spec

prop_set_child_spec() ->
    application:start(erlog),
    {ok, AST}  = erlog_make_server:load_base_ast(),
    ?FORALL(PrologModule,
            elements(erlog_modules()),
            begin
                {ok, AST1}                 = erlog_make_server:make_supervisor_childspec(AST,PrologModule),
                ?assertEqual(length(AST1),length(AST)),

                _ChildSpec                 = lists:keyfind(make_supervisor_childspec,3,AST1),

                {ok,custom_server, Binary} = compile:forms(AST1,[]),
                code:load_binary(custom_server, "custom_server.erl",Binary),
                {ok,ChildSpec}             = custom_server:make_supervisor_childspec(),
                ok                         = supervisor:check_childspecs([ChildSpec]),
                true
            end).



exports(AST) ->
    lists:flatten(lists:map(
                    fun({attribute,_,export,Exports}) ->
                            Exports
                    end,
                    lists:filter(fun({attribute,_,export, _Exports}) ->
                                       true;
                                         (_) ->
                                       false
                                   end,AST))).

prop_add_prolog_export_clauses() ->
    application:start(erlog),
    {ok,  AST}  = erlog_make_server:load_base_ast(),
    BaseExports = exports(AST), 
    ?FORALL({_ModName, Clauses},
	    {'edges_pl',
	     non_empty(list(erl_export()))},
	    ?IMPLIES(length(Clauses) =:= length(lists:usort(Clauses)),
                     ?WHENFAIL(
                        begin
                            ?debugVal(Clauses)
                        end, 
                        begin
                            {ok,PL@ }       = erlog:new(),
                            PL@             = add_export_clauses(Clauses, PL@),
                            {Exports,_PL3}  = erlog_make_server:find_exports(PL@),
                            {ok, AST1}   = erlog_make_server:add_exports(AST,Exports,PL@),
                            ?assertEqual(length(AST1), length(AST)),
                            NewExports = exports(AST1),
                            ?assertEqual(NewExports, BaseExports++Exports),
                            true
                        end))).
    

prop_db_state() ->
    application:start(erlog),
    {ok,  PL@}                 = erlog:new(),
    {ok,  PL@}                 = erlog:consult(PL@, "priv/po_set.pl"),

    {ok,  AST@}                = erlog_make_server:load_base_ast(),
    {ok,  AST@}                = erlog_make_server:load_db_state(AST, PL@),
    {ok,custom_server, Binary} = compile:forms(AST@,[]),
    {module, _}                = code:load_binary(custom_server, "custom_server.erl",Binary),
    {ok, _DBState}             = custom_server:db_state(),
    {ok, E@}                   = custom_server:init([]),
    case erlog:prove(E@, {path, a, f, {'Path'}}) of
        {{succeed, [{'Path', _Path}]},_} ->
            true;
        fail ->
            false
    end.

prop_record_defs()->
    {ok,  PL@}		= erlog:new(),
    {ok,  PL@}		= erlog:consult(PL@, "src/erlog_make_server.pl"),
    {ok,  PL@}		= erlog:consult(PL@, "priv/po_set.pl"),
    {ok,  PL@}          = erlog_make_server:record_defs(PL@),
    {{succeed, _}, PL@} = erlog:prove(PL@, {clause, {est, {'W'}, {'X'}, {'Y'}}, {'Z'}}),
    {{succeed, _}, _}   = erlog:prove(PL@, {clause, {est, {'A'}, {'W'}, {'X'}, {'Y'}}, {'Z'}}),
    true.



rop_make_param_list() ->
    ?FORALL({ParamCount},
            {
             choose(2,10)},
            begin
                Vars = erlog_make_server:make_param_list( ParamCount, 66),

                ?assertEqual(ParamCount, length(Vars)),
                ?assertEqual(Vars, lists:usort(Vars)),
                lists:all(fun({var,Line, PAtom}) when is_integer(Line) andalso is_atom(PAtom) ->
                                  true
                          end, Vars)

            end).

set_node() ->
    elements([a,b,c,d,e,f]).
edge() ->
    {edge, set_node(), set_node()}.
edges() ->
    non_empty(list(edge())).

% prop_supervisor_spec() ->
%     erlog_make_server_tests_sup:start_link(),
%     ModName		= po_set,
%     {ok,ModName}	= erlog_make_server:compile_file("priv/po_set.pl",ModName),
%     {ok,R}     		= ModName:make_child_spec(make_ref()),
%     ?debugVal(R),
%     ok			= supervisor:check_childspecs([R]),
%     {ok,Pid}		= supervisor:start_child(erlog_make_server_tests_sup, R),
%     is_process_alive(Pid).
    
    



assert_functions() ->
    Functions   = po_set:module_info(functions),
    ?assert(lists:member({db_state,0},                   Functions)),
    ?assert(lists:member({make_supervisor_childspec, 0}, Functions)),
    ?assert(lists:member({start_link, 0},                Functions)),
    ?assert(lists:member({init,1} ,                      Functions)),
    ?assert(lists:member({handle_call, 3},               Functions)),
    ?assert(lists:member({handle_cast, 2},               Functions)),
    ?assert(lists:member({handle_info, 2},               Functions)),
    ?assert(lists:member({handle_prolog, 3},             Functions)),
    ?assert(lists:member({path, 3},                      Functions)),
    ?assert(lists:member({add_edge, 3},                  Functions)),
    ?assert(lists:member({sib, 3},                       Functions)),
    true.

prop_compile_file() ->
    {ok,po_set,_} = erlog_make_server:compile_file("priv/po_set.pl", po_set),
    true          = assert_functions(),
    {ok, Pid}     = po_set:start_link(),
    ?assert(is_process_alive(Pid)),
    true.

prop_execute_code() ->
    {ok,po_set,_} = erlog_make_server:compile_file("priv/po_set.pl", po_set),
    {ok, Pid}     = po_set:start_link(),
    ?assert(is_process_alive(Pid)),
    Path          = po_set:path(Pid,a,f),
    ?assertEqual([a,b,f], Path),
    ?assertNot(po_set:sib(Pid,a,b)),
    ?assert(po_set:sib(Pid,c,b)),
    ?assert(po_set:add_edge(Pid,f,z)),
    Path2   = po_set:path(Pid,a,z),
    ?assertEqual([a,b,f,z], Path2),
    true.

