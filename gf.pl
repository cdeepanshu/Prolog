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
find_vector_in_direction_of_vector_that_has_magnitude:-
	setName(Result),
   assert(objectList(find_vector_in_direction_of_vector_that_has_magnitude(Result))).
calculate_direction_cosines:-
	setName(Result),
   assert(objectList(calculate_direction_cosines(Result))).
find_the_vector_joining_two_points:-
	setName(Result),
   assert(objectList(find_the_vector_joining_two_points(Result))).
find_position_vector_of_a_point_which_divide_line_segment:-   
	setName(Result),
   assert(objectList(find_position_vector_of_a_point_which_divide_line_segment(Result))).
find_given_points_are_vertices_of_a_right_angle_triangle_or_not:-
	setName(Result),
   assert(objectList(find_given_points_are_vertices_of_a_right_angle_triangle_or_not(Result))).
compute_magnitude_of_vector:-
	setName(Result),
   assert(objectList(compute_magnitude_of_vector(Result))).
find_scalar_and_vector_components:-
	setName(Result),
   assert(objectList(find_scalar_and_vector_components(Result))).
find_sum_of_vector:-
	setName(Result),
   assert(objectList(find_sum_of_vector(Result))).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%