erlog-server
============

This is an effort to allow you to build a gen_server from a prolog
file with a custom API that can be defined in prolog. It is a very
early stage right now everything can and may change.

Pull Requests welcome!

This is designed so that you can create a prolog module .pl and
compile it down to a beam file which will run as a gen_server. 

In your prolog file you should tell it which functiors to export by
using the `erl_export/3` predicate, where the first item is the
functor to return, the second is the atom `return` and the final is
either the name of the parameter to return or boolean. (the examples
are taken from the _po_sets.pl_ file in the priv directory.


````prolog
erl_export(path/3, return, 'Path').
path(A,B,Path) :-
       travel(A,B,[A],Q), 
       reverse(Q,Path).

````

This will create a module that has the erlang function that takes the
PID of the server and the other parameters and returns the return
value specified in prolog.

````erlang
path(Erlog, A,B) ->
	Path.
	
````

If you ask for a return type of `boolean` then it will simply return
true if the predicate succeded and false if it failed.

Note that as of now this does not work yet! I am working on it. 

There is also a plan for a rebar plugin

