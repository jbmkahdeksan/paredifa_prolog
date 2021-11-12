:- [fa]. %include fa.
:- reset_gensym.

test :-
    FAjson = fa{
        id:fa01, 
        vocabulary:[0,1], 
        states:[s0, s1, s2, s3], 
        initial:s0, finals:[s2, s3],
        moves:[s0/0==>s1]
    },
    
    json_to_fa(FAjson, FA),
    format('fa with id ~w was created~n', [FA]),
    fa_initial(FA, S0), 
    format('fa has initial state ~w~n', [S0]),
    fa_finals(FA, Finals),
    forall( member(F, Finals), format('Final = ~w~n', [F]) )
. 