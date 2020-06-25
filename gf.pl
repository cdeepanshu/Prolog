:- module(gf,[generate_scenario/1, generate_scenario/2,generate_scenario/3, get_user_desired_theorem/1]).
:- include('../../gfutils').
:- use_module(library(random)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
find_unit_vector:-

   setName(Result),
   assert(objectList(find_unit_vector(Result))).
check_whether_given_two_vectors_are_equal_or_not:-
	setName(Result),
   assert(objectList(check_whether_given_two_vectors_are_equal_or_not(Result))).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%