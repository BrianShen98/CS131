%Question 1
%remove n matching elements from the beginning of a list
remove_first_n(_,[],[]).
remove_first_n(E,[E|L],ANS):-
    remove_first_n(E,L,ANS).
remove_first_n(E,[Y|L],[Y|L]):-
    E \== Y.
	    
signal_morse([],[]).
%one 1 case
signal_morse([1|L],[.|ANS]) :-
    L \=[1|_],signal_morse(L,ANS).
%two consecutive 1 case
signal_morse([1,1|L],[.|ANS]):-
    L \=[1|_],signal_morse(L,ANS).
signal_morse([1,1|L],[-|ANS]):-
    L \= [1|_],signal_morse(L,ANS).
%three or more consecutive 1
signal_morse([1,1,1| L],[-|ANS]) :-
    remove_first_n(1,L,XS),signal_morse(XS,ANS).

%one 0 case
signal_morse([0|L],ANS):-
    L\=[0|_],signal_morse(L,ANS).
%two consecutive 0 case
signal_morse([0,0|L],ANS):-
    L\=[0|_],signal_morse(L,ANS).
signal_morse([0,0|L],[^|ANS]):-
    L\=[0|_],signal_morse(L,ANS).
%three or four consecutive 0 case
signal_morse([0,0,0|L],[^|ANS]):-
    %(L = [0|XS], XS \= [0|_],signal_morse(XS,ANS));
    L \= [0|_],signal_morse(L,ANS).
signal_morse([0,0,0,0|L],[^|ANS]):-
    L \= [0|_],signal_morse(L,ANS).
%five consecutive 0 case
signal_morse([0,0,0,0,0|L],[^|ANS]):-
    L \= [0|_], signal_morse(L,ANS).
signal_morse([0,0,0,0,0|L],[#|ANS]):-
    L \= [0|_], signal_morse(L,ANS).
%six or more consecutive 0
signal_morse([0,0,0,0,0,0| L],[#|ANS]) :-
    remove_first_n(0,L,XS),signal_morse(XS,ANS).


%Question 2
morse(a, [.,-]).           % A
morse(b, [-,.,.,.]).	   % B
morse(c, [-,.,-,.]).	   % C
morse(d, [-,.,.]).	   % D
morse(e, [.]).		   % E
morse('e''', [.,.,-,.,.]). % É (accented E)
morse(f, [.,.,-,.]).	   % F
morse(g, [-,-,.]).	   % G
morse(h, [.,.,.,.]).	   % H
morse(i, [.,.]).	   % I
morse(j, [.,-,-,-]).	   % J
morse(k, [-,.,-]).	   % K or invitation to transmit
morse(l, [.,-,.,.]).	   % L
morse(m, [-,-]).	   % M
morse(n, [-,.]).	   % N
morse(o, [-,-,-]).	   % O
morse(p, [.,-,-,.]).	   % P
morse(q, [-,-,.,-]).	   % Q
morse(r, [.,-,.]).	   % R
morse(s, [.,.,.]).	   % S
morse(t, [-]).	 	   % T
morse(u, [.,.,-]).	   % U
morse(v, [.,.,.,-]).	   % V
morse(w, [.,-,-]).	   % W
morse(x, [-,.,.,-]).	   % X or multiplication sign
morse(y, [-,.,-,-]).	   % Y
morse(z, [-,-,.,.]).	   % Z
morse(0, [-,-,-,-,-]).	   % 0
morse(1, [.,-,-,-,-]).	   % 1
morse(2, [.,.,-,-,-]).	   % 2
morse(3, [.,.,.,-,-]).	   % 3
morse(4, [.,.,.,.,-]).	   % 4
morse(5, [.,.,.,.,.]).	   % 5
morse(6, [-,.,.,.,.]).	   % 6
morse(7, [-,-,.,.,.]).	   % 7
morse(8, [-,-,-,.,.]).	   % 8
morse(9, [-,-,-,-,.]).	   % 9
morse(., [.,-,.,-,.,-]).   % . (period)
morse(',', [-,-,.,.,-,-]). % , (comma)
morse(:, [-,-,-,.,.,.]).   % : (colon or division sign)
morse(?, [.,.,-,-,.,.]).   % ? (question mark)
morse('''',[.,-,-,-,-,.]). % ' (apostrophe)
morse(-, [-,.,.,.,.,-]).   % - (hyphen or dash or subtraction sign)
morse(/, [-,.,.,-,.]).     % / (fraction bar or division sign)
morse('(', [-,.,-,-,.]).   % ( (left-hand bracket or parenthesis)
morse(')', [-,.,-,-,.,-]). % ) (right-hand bracket or parenthesis)
morse('"', [.,-,.,.,-,.]). % " (inverted commas or quotation marks)
morse(=, [-,.,.,.,-]).     % = (double hyphen)
morse(+, [.,-,.,-,.]).     % + (cross or addition sign)
morse(@, [.,-,-,.,-,.]).   % @ (commercial at)

% Error.
morse(error, [.,.,.,.,.,.,.,.]). % error - see below

% Prosigns.
morse(as, [.,-,.,.,.]).          % AS (wait A Second)
morse(ct, [-,.,-,.,-]).          % CT (starting signal, Copy This)
morse(sk, [.,.,.,-,.,-]).        % SK (end of work, Silent Key)
morse(sn, [.,.,.,-,.]).          % SN (understood, Sho' 'Nuff)


%convert morse to english words
%end case
morse_to_word(F,[],[X]):-
    morse(X,F).

%trailing ^ case
morse_to_word(F,[^],[X]):-
    morse(X,F).
%beginning ^ case
morse_to_word([^],[M|R],X):-
    morse_to_word([M],R,X).
%beginning # case
morse_to_word([#],[M|R],[#|X]):-
    morse_to_word([M],R,X).
%trailing # case
morse_to_word(F,[#],[X|[#]]):-
    morse(X,F).

%normal case to process #
morse_to_word(F,[#|R],[X,#|XS]):-
    morse(X,F), R = [H|T],morse_to_word([H],T,XS).

morse_to_word(F,[^|R],[X|XS]):-
    morse(X,F),R = [H|T],morse_to_word([H],T,XS).

morse_to_word(F,[M|R],X):-	
    M \== (^) , append(F,[M],FM), morse_to_word(FM,R,X).

%empty case
without_error(Lastword,[],Lastword).

%trailing # case
without_error(Lastword,[#],R):-
    append(Lastword,[#],R).
% word # error pattern
without_error(Lastword,[#,error|T],R):-
    Lastword \== [], without_error([],T,R).
%word error pattern
without_error(Lastword,[error|T],R):-
    Lastword \==[],without_error([],T,R).
%error error pattern
without_error([],[error|T],[error|XS]):-
    without_error([],T,XS).
%error # error pattern
without_error([],[#,error|T],[#,error|XS]):-
    without_error([],T,XS).
%# not followed by error, push last word to result, update last word
without_error(Lastword,[#,M|T],R):-
    M \== error,append(Lastword,[#],X),without_error([],[M|T],XS),append(X,XS,R).%normal case, add thing to Lastword
without_error(Lastword,[F|T],R):-
    F \== #, F\== error,append(Lastword,[F],N),without_error(N,T,R).


signal_message(Signal,R):-
    signal_morse(Signal,M),M = [X|XS],morse_to_word([X],XS,Middle),without_error([],Middle,R).


