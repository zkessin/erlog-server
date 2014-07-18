erlog-server
============

[![Build Status](https://travis-ci.org/zkessin/erlog-server.svg?branch=master)](https://travis-ci.org/zkessin/erlog-server)


This is an effort to allow you to build a gen_server from a prolog
file with a custom API that can be defined in prolog. It is a very
early stage right now everything can and may change.

Pull Requests welcome!

This is designed so that you can create a prolog module .pl and
compile it down to a beam file which will run as a gen_server. 

In your prolog file you should tell it which functors to export by
using the `erl_export/3` predicate, where the first item is the
functor to return, the second is the atom `return` and the final is
either `last` or `none` Last will return the last parameter, none will
just return `ok`. (the examples are taken from the _po_sets.pl_ file
in the priv directory.


````prolog
erl_export(path/3, return, last).
path(A,B,Path) :-
       travel(A,B,[A],Q), 
       reverse(Q,Path).

````

This will create a module that has the Erlang function that takes the
PID of the server and the other parameters and returns the return
value specified in prolog.

````erlang
path(Erlog, A,B) ->
	Path.
````

````prolog
erl_export(add_edge/2, return, none).
add_edge(A,B) :-
	...
````
````erlang
add_edge(Erlog, A, B) ->
    %code here
	ok.
````


If you ask for a return type of `none` then it will simply return
ok if the predicate succeed and crash if it failed.

Currently to compile a file you the function
`erlog_make_server:compile_file/2` pass it the path to the file and an
atom for the erlang module name. A rebar plugin will happen sooner or later.


If you want to have your erlog server running from a supervisor, then
the generated module will include a function
`make_supervisor_childspec` which will generate a basic child_spec
that will enable you to hook it up to a supervision tree.
