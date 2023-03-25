

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Building Knowledge Base Phase

build_kb:-
	write('Welcome to Pro-Wordle!'),
	nl,
	write('----------------------'),
	nl,
	enter.
	
enter:- 
	write('Please enter a word and its category on separate lines:'),
	nl,
	read(W),
	( W = done,write('Done building the words database...'); read(C),assert(word(W,C)),enter).

	

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Play Phase

play:- 
	write('The available categories are: '),
	categories(L),
	write(L),
	
	chooseCategory(C),
	chooseLength(L),
	
	T is L + 1,
	write('Game started. You have '),
	write(T),
	write(' guesses.'),
	nl,
	game(C,L,T).
	
chooseCategory(C):-
	write('Choose a category: '),
	nl,
	read(Category),
	(
	
	\+is_category(Category),
	write('This category does not exist.'),
	chooseCategory(C)
	
	;
	
	is_category(Category),
	C = Category
	
	).

chooseLength(L):-
	write('Choose a length: '),
	nl,
	read(Length),
	(
	
	\+available_length(Length),
	write('There are no words of this length.'),
	chooseLength(L)
	
	;
	
	available_length(Length),
	L = Length
	
	).
	


game(C,L,T):-

	write('Enter a word composed of '),
	write(L),
	write('letters: '),
	nl,
	read(X),
	(
		X \== L,
		write('Word is not composed of '),
		write(L),
		write(' letters. Try again.'),
		nl,
		write('Remaining Guesses are '),
		write(T),
		game(C,L,T)
		
	;
		%pick_word(W,L,C),
		(
		
				W\==X,
				
				% convert word from KB into a list
				
				atomic_list_concat(L1,'',W),
				
				% convert word input to a list
				
				atomic_list_concat(L2,'',X),
				
				 correct_letters(L1,L2,CL),
				 correct_positions(L1,L2,CP),
				T1 is T-1,
			(
				
				T1 \== 0,
				write('Correct letters are: '),
				write(CL),
				nl,
				write('Correct letters in correct positions are: '),
				write(CP),
				nl,
				write('Remaining Guesses are '),
				write(T1),
				game(C,L,T1)
				
				;
				
				write('You lost!')
			)
		;
				write('You Won!')
		
		)
	).
	
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% IMPLEMENTATION

is_category(C):-
	word(_,C).
	
categories(L):-
	bagof(C,is_category(C),List),
	unique(List,[],L).

% category helper predicate
% Base Case

unique([],A,A).

% Recursive Case

unique([H|T],A,L):-
	\+member(H,A),
	append(A,[H],A1),
	unique(T,A1,L).
	
unique([H|T],A,L):-
	member(H,A),
	unique(T,A,L).
	
	
% atom_length(word,len) true if word has length len	
	
available_length(L):-
	word(W,_),
	atom_length(W,L).
	
	
pick_word(W,L,C):-
	word(W,C),
	atom_length(W,L).
	
	
% Intersection of Both Letters

% Base Case

correct_letters([],_,[]).

% Recursive Case

correct_letters(L1,L2,CL):-
	L1 = [H1|T1],
	member(H1,L2),
	CL = [H1|CT],
	correct_letters(T1,L2,CT).
	
correct_letters(L1,L2,CL):-
	L1 = [H1|T1],
	\+member(H1,L2),
	correct_letters(T1,L2,CL).
	
% Base Case

correct_positions([],[],[]).


% Recursive Case

correct_positions(L1,L2,PL):-
	L1 = [H1|T1],
	L2 = [H1|T2],
	PL = [H1|PT],
	correct_positions(T1,T2,PT).
	
correct_positions(L1,L2,PL):-
	L1 = [H1|T1],
	L2 = [H2|T2],
	H1 \== H2,
	correct_positions(T1,T2,PL).
	
	
	
similar([],_,0).

similar([H1|T1],W,N):-
	\+member(H1,W),
	similar(T1,W,N),
	
similar([H1|T1],W,N):-
	member(H1,W),
	similar(T1,W,N1),
	N is N1 + 1 .
	
	
find_nearst([H1|T1],L2,W):-
	atomic_list_concat(L1,'',H1),
	similar(L1,L2,N),
	

pick_nearst_similar(L,C,X,W):-
	bagof(W1,pick_word(W1,L,C),Bag),
	find_nearst(Bag,X,W).
	
	
	
	
	
	
	
	