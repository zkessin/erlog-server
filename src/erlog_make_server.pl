% -*- Prolog-*-

find_export(Funct) :-
	erl_export(Funct, return, _RetField).

find_exports(Exports) :-
	findall(Export, find_export(Export), Exports).
		   
find_return(Funct, Return) :-
	erl_export(Funct, return, Return).


find_imports(Imports) :-
	findall(Import, import(Import), Imports).
