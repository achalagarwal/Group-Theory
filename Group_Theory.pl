

%-------------Check membership-----------------------

belongs(X,[X|_]).
belongs(X,[_|T]):-belongs(X,T).

%----------------------------------------------------



%-----------------Check closure-----------------------

closed(_,[]).
closed(L,[[X,Y,Z]|Tail]):-(((\+belongs(X,L)),write(X),write(" is invalid ")),1=0);((\+belongs(Y,L)),write(Y),write(" is invalid ")),1=0;belongs(X,L),belongs(Y,L),belongs(Z,L),closed(L,Tail).

%-----------------------------------------------------



%------------------Return result of X op Y ----------------

operatable([[X,Y,C]|_],X,Y,Z):-Z=C.
operatable([[_,_,_]|T],A,B,Z):-operatable(T,A,B,Z).

%----------------------------------------------------------



checkTriple([[_,_,_]|_],[[_,_,_]|_],[]).
checkTriple([[X,Y,Z]|Tail],[[A,B,C]|Tail2],[H|T]):-operatable([[X,Y,Z]|Tail],C,H,Q),
                                   operatable([[X,Y,Z]|Tail],B,H,R),
                                   operatable([[X,Y,Z]|Tail],A,R,S),
                                   S=Q,
    							   checkTriple([[X,Y,Z]|Tail],[[A,B,C]|Tail2],T).




%-------------------Check associativity---------------

associative([[_,_,_]|_],[],_).
associative([[X,Y,Z]|Tail],[[A,B,C]|Tail2],[H|T]):-checkTriple([[X,Y,Z]|Tail],[[A,B,C]|Tail2],[H|T]),
    							   associative([[X,Y,Z]|Tail],Tail2,[H|T]).

%-----------------------------------------------------




%-----------Compute power of an element---------------

power(A,_,[_|[]],[[_,_,_]|_],A).
power(A,B,[_|T],[[X,Y,Z]|Tail],P):-
  operatable([[X,Y,Z]|Tail],A,B,C),
  power(C,B,T,[[X,Y,Z]|Tail],P).

%-----------------------------------------------------


%-----------Check existence of identity element-------------

identity([_|[]],_,[[_,_,_]|_],_).
identity([H,J|T],Set,[[X,Y,Z]|Tail],I):-
  power(H,H,Set,[[X,Y,Z]|Tail],P),
  power(J,J,Set,[[X,Y,Z]|Tail],Q),
  P=Q,
  identity([J|T],Set, [[X,Y,Z]|Tail],I),
  I=P.

%------------------------------------------------------



%---------------Check existence of Inverse-------------------

inverse([],_).
inverse([H|T],L):-identity([H|T],L,L,I),operatable(L,H,I,Z),Z=H,operatable(L,I,H,K),K=H,inverse(T,L).
inverse([],_,_).
inverse([H|T],L,I):-operatable(L,H,Z,I),operatable(L,Z,H,I),inverse(T,L,I).
%------------------------------------------------------------


%----------------------------Check abelian--------------------------------

abelian([],_).
abelian([[A,B,C]|T],L):-(\+operatable(L,B,A,C),write(" is not abelian "),1=0);(operatable(L,B,A,C),abelian(T,L)).

%-------------------------------------------------------------------------

group(S,Op):-closed(S,Op),associativeHelper(S,Op),identityHelper(S,Op,I),inverse(S,Op,I).


%----------------------------Helper Functions-----------------------------

checkTripleHelper(OpList,SymList):-checkTriple(OpList,OpList,SymList).
associativeHelper(SymList,OpList):-associative(OpList,OpList,SymList).
identityHelper(SymList,OpList,I):-identity(SymList,SymList,OpList,I).
abelianHelper(OpList):-abelian(OpList,OpList).
%-------------------------------------------------------------------------
