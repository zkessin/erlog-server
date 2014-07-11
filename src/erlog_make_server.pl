% -*- Prolog-*-

find_export(Funct) :-
	erl_export(Funct, return, RetField),
	asserta(export_ret_val(Funct, RetField)).


find_exports(Exports) :-
	findall(Export, find_export(Export), Exports).
		   

