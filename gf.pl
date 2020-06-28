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
find_unit_vector_in_diection_of_the_sum_of_vectors:-
	setName(Result),
   assert(objectList(find_unit_vector_in_diection_of_the_sum_of_vectors(Result))).
check_whether_two_vectors_are_collinear_or_not:-
	setName(Result),
   assert(objectList(check_whether_two_vectors_are_collinear_or_not(Result))).
check_whether_vectors_is_equally_inclined_to_the_axes_or_not:-
	setName(Result),
   assert(objectList(check_whether_vectors_is_equally_inclined_to_the_axes_or_not(Result))).
find_position_vector_of_the_mid_point_of_the_vector:-
   setName(Result),
   assert(objectList(find_position_vector_of_the_mid_point_of_the_vector(Result))).
find_angle_between_two_vectors_with_given_magnitude_and_product:-
	setName(Result),
   assert(objectList(find_angle_between_two_vectors_with_given_magnitude_and_product(Result))).
find_angle_between_the_vectors:-
	setName(Result),
   assert(objectList(find_angle_between_the_vectors(Result))).	
check_whether_vectors_are_perpendicular_or_not:-
	setName(Result),
   assert(objectList(check_whether_vectors_are_perpendicular_or_not(Result))).
find_projection_of_the_vector_on_another_vector:-
	setName(Result),
   assert(objectList(find_projection_of_the_vector_on_another_vector(Result))).	
find_magnitude_of_differnce_of_vectors:-
	setName(Result),
   assert(objectList(find_magnitude_of_differnce_of_vectors(Result))).
given_a_unit_vector_and_equation_find_magnitude_of_variable:-
	setName(Result),
   assert(objectList(given_a_unit_vector_and_equation_find_magnitude_of_variable(Result))).
check_whether_points_are_collinear_or_not:-
	setName(Result),
   assert(objectList(check_whether_points_are_collinear_or_not(Result))).
check_for_unit_vector_and_mutually_perpendicular:-
	setName(Result),
   assert(objectList(check_for_unit_vector_and_mutually_perpendicular(Result))).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%