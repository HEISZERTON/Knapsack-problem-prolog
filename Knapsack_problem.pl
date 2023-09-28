
sum_WorV([],_,0).									%emtpty bag value = 0
sum_WorV([H|T],IW,W):- sum_WorV(T,IW,W2), nth0(H,IW, W1), W is W1 + W2.  		%sum the weights or values of a bag


buildtable(0, []). 									%base case knapsack is empty
buildtable(N, T):- N > 0, N2 is N - 1, buildtable(N2, T2), append(T2,[[]],T).		%Builds list of emtpy knapsacks


nextrow(I,IW, IV,CR,T):- nextrow(I,0,IW, IV,CR,[],T).					%starts to build the next row initiating with an empty list and j = 0

nextrow(I,0,IW, IV,CR,NR,T) :- !, append(NR,[[]], NR2), nextrow(I,1,IW, IV,CR,NR2,T).	 %case for j = 0

nextrow(_,J,_, _,CR,NR,T) :- length(CR,X), J >= X, ! , T = NR. 			 %return full list as the next row in the table

nextrow(I,J,IW, IV,CR,NR,T):- 				
	nth0(I,IW, W), W > J, !, 	 						% case for item does not fit in bag
	nth0(J,CR, LB),			
	append(NR,[LB], NR2), JN is J + 1, nextrow(I,JN,IW, IV,CR,NR2,T).

nextrow(I,J,IW, IV,CR,NR,T):- 								% else case
	nth0(J,CR, LB),									%last bag
	nth0(I,IW, W), JP is J - W, nth0(JP,CR, OB),					%biggest bag of capacity j that fits item I
	append(OB,[I], NB), max(LB,NB,IV,KS),						%add item to old bag and find max value bag of the 2
	append(NR,[KS], NR2), JN is J + 1, nextrow(I,JN,IW, IV,CR,NR2,T).			


max(K1,K2,IV,K) :- sum_WorV(K1,IV,V1), sum_WorV(K2,IV,V2), V1 > V2,!, put(V1),K = K1. 	%if value of bag 1 is bigger than bag 2 return bag 1	
max(_,K2,_,K) :- K = K2.								%otherwise return bag 2


lastrow(N,IW, _, CR, ER):- length(IW,X), N >= X, ! , ER = CR.				%loops through for each item and returns las row in the table
lastrow(N,IW, IV, CR, ER):- nextrow(N,IW, IV,CR,NR), N2 is N + 1, lastrow(N2,IW, IV, NR, ER).

knapsack(C, IW, IV, V, IL):- 					
	TS is C + 1, buildtable(TS, T),					%build first row
	lastrow(0,IW, IV, T, LW),					%gets last row
	nth0(C,LW, IL), sum_WorV(IL,IV,V).				%gets max bag from last row


solveKnapsack(FN, V, IL):- 
	open(FN,read,Str), 
	read_line_to_string(Str,L1),
	atom_number(L1,NI),
	buildlists(Str,NI,IN,IV,IW),
	read_line_to_string(Str,LE),
	atom_number(LE,C),
	knapsack(C, IW, IV, V, KS),
	tonames(KS,0,IN,[],IL).


tonames([],_,_,_,[]).							%convert a list of indexs to a list of names								
tonames(KS,N,_,KN,T) :- length(KS,X), N >= X, ! , T = KN. 
tonames(KS,N,IN,KN,T) :- nth0(N,KS, X), nth0(X,IN, NAME), N2 is N + 1, append(KN,[NAME],KN2), tonames(KS,N2,IN,KN2,T).


buildlists(_,0,[],[],[]).			%no items left to read
buildlists(Str,N,A,B,C) :- N > 0,		%reads N items from stream and returns list of names,values and weights
	N1 is  N - 1,
	buildlists(Str,N1,IN1,IV1,IW1),
	read_line_to_string(Str,L),
	split_string(L, " ", " ", LL),
	nth0(0,LL, L0),
	nth0(1,LL, L1),atom_number(L1,LI1),
	nth0(2,LL, L2),atom_number(L2,LI2),
	append(IN1,[L0],A),
	append(IV1,[LI1],B),
	append(IW1,[LI2],C).	
	