%:- include('../utils').
:- include('vector_algebra/vector_main').
:- include('vector_algebra/vector_main2').
:- include('vector_algebra/vector_main3').
:- include('vector_algebra/vector_main4').


eof2 ==> write('exercise'),nl,handle_include_for_cqt([vector_algebra]).


:- chr_constraint

    find_unit_vector1/1,
    check_whether_given_two_vectors_are_equal_or_not1/1,
    find_vector_in_direction_of_vector_that_has_magnitude1/1,
    calculate_direction_cosines1/1,
    find_the_vector_joining_two_points1/1,
    find_position_vector_of_a_point_which_divide_line_segment_internally1/1,
    find_position_vector_of_a_point_which_divide_line_segment_externally1/1,
	find_given_points_are_vertices_of_a_right_angle_triangle_or_not1/1,
    compute_magnitude_of_vector1/1,
    find_scalar_components1/1,
    find_vector_components1/1,
	find_sum_of_vector1/1,
    find_unit_vector_in_diection_of_the_sum_of_vectors1/1,
    check_whether_two_vectors_are_collinear_or_not1/1,
    check_whether_vectors_is_equally_inclined_to_the_axes_or_not1/1,
    find_position_vector_of_the_mid_point_of_the_vector1/1,
    find_angle_between_two_vectors_with_given_magnitude_and_product1/1,
    find_angle_between_the_vectors1/1,
    check_whether_vectors_are_perpendicular_or_not1/1,
    find_projection_of_the_vector_on_another_vector1/1,
    find_magnitude_of_differnce_of_vectors1/1,
    given_a_unit_vector_and_equation_find_magnitude_of_variable1/1,
    check_whether_points_are_collinear_or_not1/1,
    check_for_unit_vector1/1,
    check_for_mutually_perpendicular_vector1/1,
	find_variable_magnitude1/1,
    evaluate_the_product1/1,
    find_magnitude_of_vectors_given_magnitude_angle_scalar_product1/1,
    find_value_of_lambda1/1,
    find_value_of_expression1/1,
    find_angle_between_the_vectors_with_given_vertices_of_triangle1/1,
    find_magnitude_of_cross_product1/1,
    find_unit_vector_perpendicular_to_each_of_the_vector1/1,
    find_area_of_triangle_having_point_as_it_vertices1/1,
    find_lambda_and_mu_in_the_expression1/1,
    find_expression_value1/1,
    evaluate_the_quantity_mu1/1,
    find_value_of_variable_x1/1,
    find_a_vector_of_given_magnitude_and_parallel_to_resultant_of_vectors1/1,
    find_a_unit_vector_parallel_to_the_vector1/1,
    find_a_vector_perpendicular_to_given_vectors1/1.
    

name1(A,B) ==> check_fact_exists(name1(A,B)) | assert(is_fact_exists(name1(A,B))).
%term1(A,B) ==> check_fact_exists(term1(A,B)) | assert(is_fact_exists(term1(A,B))).

find_unit_vector1(Result)==>check_fact_exists( find_unit_vector1(Result))|
assert(is_fact_exists( find_unit_vector1(Result))).

check_whether_given_two_vectors_are_equal_or_not1(Result)==>check_fact_exists( check_whether_given_two_vectors_are_equal_or_not1(Result))|
assert(is_fact_exists( check_whether_given_two_vectors_are_equal_or_not1(Result))).

find_vector_in_direction_of_vector_that_has_magnitude1(Result)==>check_fact_exists( find_vector_in_direction_of_vector_that_has_magnitude1(Result))|
assert(is_fact_exists( find_vector_in_direction_of_vector_that_has_magnitude1(Result))).

calculate_direction_cosines1(Result)==>check_fact_exists(calculate_direction_cosines1(Result))|
assert(is_fact_exists(calculate_direction_cosines1(Result))).

find_the_vector_joining_two_points1(Result)==>check_fact_exists(find_the_vector_joining_two_points1(Result))|
assert(is_fact_exists(find_the_vector_joining_two_points1(Result))).

find_position_vector_of_a_point_which_divide_line_segment_internally1(Result)==>check_fact_exists(find_position_vector_of_a_point_which_divide_line_segment_internally1(Result))|
assert(is_fact_exists( find_position_vector_of_a_point_which_divide_line_segment_internally1(Result))).

find_position_vector_of_a_point_which_divide_line_segment_externally1(Result)==>check_fact_exists(find_position_vector_of_a_point_which_divide_line_segment_externally1(Result))|
assert(is_fact_exists(find_position_vector_of_a_point_which_divide_line_segment_externally1(Result))).

find_given_points_are_vertices_of_a_right_angle_triangle_or_not1(Result)==>check_fact_exists( find_given_points_are_vertices_of_a_right_angle_triangle_or_not1(Result))|
assert(is_fact_exists( find_given_points_are_vertices_of_a_right_angle_triangle_or_not1(Result))).

compute_magnitude_of_vector1(Result)==>check_fact_exists( compute_magnitude_of_vector1(Result))|
assert(is_fact_exists( compute_magnitude_of_vector1(Result))).

find_scalar_components1(Result)==>check_fact_exists(find_scalar_components1(Result))|
assert(is_fact_exists(find_scalar_components1(Result))).

find_vector_components1(Result)==>check_fact_exists(find_vector_components1(Result))|
assert(is_fact_exists(find_vector_components1(Result))).

find_sum_of_vector1(Result)==>check_fact_exists( find_sum_of_vector1(Result))|
assert(is_fact_exists( find_sum_of_vector1(Result))).

find_unit_vector_in_diection_of_the_sum_of_vectors1(Result)==>check_fact_exists( find_unit_vector_in_diection_of_the_sum_of_vectors1(Result))|
assert(is_fact_exists(find_unit_vector_in_diection_of_the_sum_of_vectors1(Result))).

check_whether_two_vectors_are_collinear_or_not1(Result)==>check_fact_exists(check_whether_two_vectors_are_collinear_or_not1(Result))|
assert(is_fact_exists( check_whether_two_vectors_are_collinear_or_not1(Result))).

check_whether_vectors_is_equally_inclined_to_the_axes_or_not1(Result)==>check_fact_exists(check_whether_vectors_is_equally_inclined_to_the_axes_or_not1(Result))|
assert(is_fact_exists( check_whether_vectors_is_equally_inclined_to_the_axes_or_not1(Result))).

find_position_vector_of_the_mid_point_of_the_vector1(Result)==>check_fact_exists(find_position_vector_of_the_mid_point_of_the_vector1(Result))|
assert(is_fact_exists(find_position_vector_of_the_mid_point_of_the_vector1(Result))).

find_angle_between_two_vectors_with_given_magnitude_and_product1(Result)==>check_fact_exists(find_angle_between_two_vectors_with_given_magnitude_and_product1(Result))|
assert(is_fact_exists(find_angle_between_two_vectors_with_given_magnitude_and_product1(Result))).

find_angle_between_the_vectors1(Result)==>check_fact_exists(find_angle_between_the_vectors1(Result))|
assert(is_fact_exists(find_angle_between_the_vectors1(Result))).

check_whether_vectors_are_perpendicular_or_not1(Result)==>check_fact_exists(check_whether_vectors_are_perpendicular_or_not1(Result))|
assert(is_fact_exists(check_whether_vectors_are_perpendicular_or_not1(Result))).

find_projection_of_the_vector_on_another_vector1(Result)==>check_fact_exists(find_projection_of_the_vector_on_another_vector1(Result))|
assert(is_fact_exists(find_projection_of_the_vector_on_another_vector1(Result))).

find_magnitude_of_differnce_of_vectors1(Result)==>check_fact_exists(find_magnitude_of_differnce_of_vectors1(Result))|
assert(is_fact_exists(find_magnitude_of_differnce_of_vectors1(Result))).

given_a_unit_vector_and_equation_find_magnitude_of_variable1(Result)==>check_fact_exists(given_a_unit_vector_and_equation_find_magnitude_of_variable1(Result))|
assert(is_fact_exists(given_a_unit_vector_and_equation_find_magnitude_of_variable1(Result))).

check_whether_points_are_collinear_or_not1(Result)==>check_fact_exists(check_whether_points_are_collinear_or_not1(Result))|
assert(is_fact_exists(check_whether_points_are_collinear_or_not1(Result))).

check_for_unit_vector1(Result)==>check_fact_exists(check_for_unit_vector1(Result))|
assert(is_fact_exists(check_for_unit_vector1(Result))).

check_for_mutually_perpendicular_vector1(Result)==>check_fact_exists(check_for_mutually_perpendicular_vector1(Result))|
assert(is_fact_exists(check_for_mutually_perpendicular_vector1(Result))).

find_variable_magnitude1(Result)==>check_fact_exists(find_variable_magnitude1(Result))|
assert(is_fact_exists(find_variable_magnitude1(Result))).

evaluate_the_product1(Result)==>check_fact_exists(evaluate_the_product1(Result))|
assert(is_fact_exists(evaluate_the_product1(Result))).

find_magnitude_of_vectors_given_magnitude_angle_scalar_product1(Result)==>check_fact_exists(find_magnitude_of_vectors_given_magnitude_angle_scalar_product1(Result))|
assert(is_fact_exists(find_magnitude_of_vectors_given_magnitude_angle_scalar_product1(Result))).

find_value_of_lambda1(Result)==>check_fact_exists(find_value_of_lambda1(Result))|
assert(is_fact_exists(find_value_of_lambda1(Result))).

find_value_of_expression1(Result)==>check_fact_exists(find_value_of_expression1(Result))|
assert(is_fact_exists(find_value_of_expression1(Result))).

find_angle_between_the_vectors_with_given_vertices_of_triangle1(Result)==>check_fact_exists(find_angle_between_the_vectors_with_given_vertices_of_triangle1(Result))|
assert(is_fact_exists(find_angle_between_the_vectors_with_given_vertices_of_triangle1(Result))).

find_magnitude_of_cross_product1(Result)==>check_fact_exists(find_magnitude_of_cross_product1(Result))|
assert(is_fact_exists(find_magnitude_of_cross_product1(Result))).

find_unit_vector_perpendicular_to_each_of_the_vector1(Result)==>check_fact_exists(find_unit_vector_perpendicular_to_each_of_the_vector1(Result))|
assert(is_fact_exists(find_unit_vector_perpendicular_to_each_of_the_vector1(Result))).

find_area_of_triangle_having_point_as_it_vertices1(Result)==>check_fact_exists(find_area_of_triangle_having_point_as_it_vertices1(Result))|
assert(is_fact_exists(find_area_of_triangle_having_point_as_it_vertices1(Result))).

find_lambda_and_mu_in_the_expression1(Result)==>check_fact_exists(find_lambda_and_mu_in_the_expression1(Result))|
assert(is_fact_exists(find_lambda_and_mu_in_the_expression1(Result))).

find_expression_value1(Result)==>check_fact_exists(find_expression_value1(Result))|
assert(is_fact_exists(find_expression_value1(Result))).

evaluate_the_quantity_mu1(Result)==>check_fact_exists(evaluate_the_quantity_mu1(Result))|
assert(is_fact_exists(evaluate_the_quantity_mu1(Result))).

find_value_of_variable_x1(Result)==>check_fact_exists(find_value_of_variable_x1(Result))|
assert(is_fact_exists(find_value_of_variable_x1(Result))).

find_a_vector_of_given_magnitude_and_parallel_to_resultant_of_vectors1(Result)==>check_fact_exists(find_a_vector_of_given_magnitude_and_parallel_to_resultant_of_vectors1(Result))|
assert(is_fact_exists(find_a_vector_of_given_magnitude_and_parallel_to_resultant_of_vectors1(Result))).

find_a_unit_vector_parallel_to_the_vector1(Result)==>check_fact_exists(find_a_unit_vector_parallel_to_the_vector1(Result))|
assert(is_fact_exists(find_a_unit_vector_parallel_to_the_vector1(Result))).

find_a_vector_perpendicular_to_given_vectors1(Result)==>check_fact_exists(find_a_vector_perpendicular_to_given_vectors1(Result))|
assert(is_fact_exists(find_a_vector_perpendicular_to_given_vectors1(Result))).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/*****************************check_whether_given_two_vectors_are_equal_or_not1***************/
eof1,check_whether_given_two_vectors_are_equal_or_not1(Result)==> check_fact_exists(term1(Result,_))|

generate_random_list_equal_or_unequal_vectors(List1,List2),
generate_question_vector_ex_5(List1,List2,Question),
remove_square_brackets_from_string(Question,Q1_main_part),
generate_answer_vector_ex_5(List1,List2,Ans,Answer_type_str),
generate_solution_vector_ex_5(List1,List2,Solution),
remove_square_brackets_from_string(Solution,Solution_steps),
term1(Result,Ans),
New_element_formed = term(Result,Ans),
Reasoning_list = [],
New_reasoning_list =[check_whether_given_two_vectors_are_equal_or_not(Result)],
Current_config = New_reasoning_list,
Concept = equal_vector, 
Main_predicate = New_element_formed,
Answer_type =  [Answer_type_str],
Answer = [[Ans]],
string_concatenate(["[string(Check i, j, k components of both the vectors.)]"],"",Hint_str),
Hint = [Hint_str],
%string_concatenate([],"",Solution_steps),
Steps = Solution_steps,
%string_concatenate(["[\"string(",Q,")\"]"],"",Q1_main_part),
%string_concatenate(["[\" string()\"]"],"",Q1_side_part),
%string_concatenate([],"",Added_solution_step1),
%Ques_config_list = [[Q1_side_part,Q1_main_part,Added_solution_step1,qt1]],

Q1_side_part = "",
Added_solution_step1 = "",
Ques_config_list = [[[Q1_side_part],[Q1_main_part],Added_solution_step1,qt1]],

get_qid_per_ques_representation(Ques_config_list, Qid_per_ques_representation),
complete_rule_by_adding_properties(New_element_formed,Reasoning_list,New_reasoning_list,Concept,
Main_predicate,Current_config,Answer_type,Answer,Steps,Qid_per_ques_representation,Hint,Ques_config_list,1).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
/*****************************find_unit_vector***************/
eof1,find_unit_vector1(Result)==> check_fact_exists(term1(Result,_))|

%write(1),nl,read(_),
Var1=a,
generate_list(List1),
generate_question_vector_ex_6(List1,Question),
remove_square_brackets_from_string(Question,Q1_main_part),
generate_answer_vector_ex_6(List1,Ans),
term_string(Ans,Temp_ans),
generate_solution_vector_ex_6(List1,Solution),
remove_square_brackets_from_string(Solution,Solution_steps),

term1(Result,Temp_ans),
New_element_formed = term(Result,Temp_ans),
Reasoning_list = [],
New_reasoning_list =[find_unit_vector(Result)],
Current_config = New_reasoning_list,
Concept = unit_vector, 
Main_predicate = New_element_formed,
string_concatenate(["answer_types([[string(latex(a\\\\hat{i} + b\\\\hat{j} + c\\\\hat{k}=)),type(algebra_form),placeholder(string(Enter the values of a,b,c in radical form and separated by comma)),unit()]])"],"",Answer_type_str),
Answer_type =  [Answer_type_str],
Answer = [3,[Ans]],
string_concatenate(["[string(Unit vector is given as :latex(\\\\hat{a} = \\\\frac{1}{|\\\\overrightarrow{a}|} \\\\overrightarrow{a}).)]"],"",Hint_str),
Hint = [Hint_str],
%string_concatenate([],"",Solution_steps),
Steps = Solution_steps,
%string_concatenate(["[\"string(",Q,")\"]"],"",Q1_main_part),
%string_concatenate(["[\" string()\"]"],"",Q1_side_part),
%string_concatenate([],"",Added_solution_step1),
%Ques_config_list = [[Q1_side_part,Q1_main_part,Added_solution_step1,qt1]],

Q1_side_part = "",
Added_solution_step1 = "",
Ques_config_list = [[[Q1_side_part],[Q1_main_part],Added_solution_step1,qt1]],
get_qid_per_ques_representation(Ques_config_list, Qid_per_ques_representation),
complete_rule_by_adding_properties(New_element_formed,Reasoning_list,New_reasoning_list,Concept,
Main_predicate,Current_config,Answer_type,Answer,Steps,Qid_per_ques_representation,Hint,Ques_config_list,1).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/*****************************find_vector_in_direction_of_vector_that_has_magnitude***************/
eof1,find_vector_in_direction_of_vector_that_has_magnitude1(Result)==> check_fact_exists(term1(Result,_))|

%write(0),nl,read(_),
Var1=a,
generate_list(List1),
generate_magnitude(Magnitude),
generate_question_vector_ex_7(List1,Magnitude,Question),
remove_square_brackets_from_string(Question,Q1_main_part),
generate_answer_vector_ex_7(List1,Magnitude,Ans),
term_string(Ans,Temp_ans),
generate_solution_vector_ex_7(List1,Magnitude,Solution),
remove_square_brackets_from_string(Solution,Solution_steps),
term1(Result,Temp_ans),
New_element_formed = term(Result,Temp_ans),
Reasoning_list = [],
New_reasoning_list =[find_vector_in_direction_of_vector_that_has_magnitude(Result)],
Current_config = New_reasoning_list,
Concept = magitude_product,
Main_predicate = New_element_formed,
string_concatenate(["answer_types([[string(latex(a\\\\hat{i} + b\\\\hat{j} + c\\\\hat{k}=)),type(algebra_form),placeholder(string(Enter the values of a,b,c in radical form and separated by comma)),unit()]])"],"",Answer_type_str),
Answer_type =  [Answer_type_str],
Answer = [3,[Ans]],
string_concatenate(["[string(Unit vector is given as :latex(\\\\hat{a} = \\\\frac{1}{|\\\\overrightarrow{a}|} \\\\overrightarrow{a}).)]"],"",Hint_str),
Hint = [Hint_str],
%string_concatenate([],"",Solution_steps),
Steps = Solution_steps,

Q1_side_part = "",
Added_solution_step1 = "",
Ques_config_list = [[[Q1_side_part],[Q1_main_part],Added_solution_step1,qt1]],

get_qid_per_ques_representation(Ques_config_list, Qid_per_ques_representation),
complete_rule_by_adding_properties(New_element_formed,Reasoning_list,New_reasoning_list,Concept,
Main_predicate,Current_config,Answer_type,Answer,Steps,Qid_per_ques_representation,Hint,Ques_config_list,1).

/*****************************calculate_direction_cosines***************/
eof1,calculate_direction_cosines1(Result)==> check_fact_exists(term1(Result,_))|

%write(0),nl,read(_),
Var1=a,
generate_list(List1),
generate_question_vector_ex_9(List1,Question),
remove_square_brackets_from_string(Question,Q1_main_part),
generate_answer_vector_ex_9(List1,Ans),
term_string(Ans,Temp_ans),
generate_solution_vector_ex_9(List1,Solution),
remove_square_brackets_from_string(Solution,Solution_steps),

term1(Result,Temp_ans),

New_element_formed = term(Result,Temp_ans),
Reasoning_list = [],

New_reasoning_list =[calculate_direction_cosines(Result)],
Current_config = New_reasoning_list,
Concept = direction_cosines,
Main_predicate = New_element_formed,
string_concatenate(["answer_types([[string(Direction Ratio=),type(algebra_form),placeholder(string(Enter the values of a,b,c in integer form and separated by comma)),unit()]]),answer_types([[string(Direction Cosines=),type(algebra_form),placeholder(string(Enter the values of l,m,n in integer or radical form and separated by comma)),unit()]])"],"",Answer_type_str),
Answer_type =  [Answer_type_str],
Answer = [3,[Ans]],
string_concatenate(["[string(Direction ratios of a vector are just the respective componets of the vector.)]"],"",Hint_str),
Hint = [Hint_str],
%string_concatenate([],"",Solution_steps),
Steps = Solution_steps,

Q1_side_part = "",
Added_solution_step1 = "",
Ques_config_list = [[[Q1_side_part],[Q1_main_part],Added_solution_step1,qt1]],

get_qid_per_ques_representation(Ques_config_list, Qid_per_ques_representation),
complete_rule_by_adding_properties(New_element_formed,Reasoning_list,New_reasoning_list,Concept,
Main_predicate,Current_config,Answer_type,Answer,Steps,Qid_per_ques_representation,Hint,Ques_config_list,1).

/*****************************find_the_vector_joining_two_points***************/
eof1,find_the_vector_joining_two_points1(Result)==> check_fact_exists(term1(Result,_))|

%write(0),nl,read(_),
generate_list(List1),
generate_list(List2),
generate_question_vector_ex_10(List1,List2,Question),
remove_square_brackets_from_string(Question,Q1_main_part),

generate_answer_vector_ex_10(List1,List2,Ans),
term_string(Ans,Temp_ans),
generate_solution_vector_ex_10(List1,List2,Solution),
remove_square_brackets_from_string(Solution,Solution_steps),

term1(Result,Temp_ans),

New_element_formed = term(Result,Temp_ans),
Reasoning_list = [],

New_reasoning_list =[find_the_vector_joining_two_points(Result)],
Current_config = New_reasoning_list,
Concept = vector_joining_the_points,
Main_predicate = New_element_formed,
string_concatenate(["answer_types([[string(latex(a\\\\hat{i} + b\\\\hat{j} + c\\\\hat{k}=)),type(algebra_form),placeholder(string(Enter the values of a,b,c in radical form and separated by comma)),unit()]])"],"",Answer_type_str),
Answer_type =  [Answer_type_str],
Answer = [3,[Ans]],
string_concatenate(["[string(Here P is the initial point and Q is the terminal point)]"],"",Hint_str),
Hint = [Hint_str],
%string_concatenate([],"",Solution_steps),
Steps = Solution_steps,

Q1_side_part = "",
Added_solution_step1 = "",
Ques_config_list = [[[Q1_side_part],[Q1_main_part],Added_solution_step1,qt1]],

get_qid_per_ques_representation(Ques_config_list, Qid_per_ques_representation),
complete_rule_by_adding_properties(New_element_formed,Reasoning_list,New_reasoning_list,Concept,
Main_predicate,Current_config,Answer_type,Answer,Steps,Qid_per_ques_representation,Hint,Ques_config_list,1).

/*****************************find_position_vector_of_a_point_which_divide_line_segment_internally***************/
eof1,find_position_vector_of_a_point_which_divide_line_segment_internally1(Result)==> check_fact_exists(term1(Result,_))|

%write(0),nl,read(_),
generate_list(Point_list_1),
generate_list(Point_list_2),

generate_magnitude(Ratio1),
generate_magnitude(Ratio2),
generate_question_vector_ex_11_1(Point_list_1,Point_list_2,Ratio1,Ratio2,Question),
remove_square_brackets_from_string(Question,Q1_main_part),
generate_answer_vector_ex_11_1(Point_list_1,Point_list_2,Ratio1,Ratio2,Ans),
term_string(Ans,Temp_ans),
generate_solution_vector_ex_11_1(Point_list_1,Point_list_2,Ratio1,Ratio2,Solution),
remove_square_brackets_from_string(Solution,Solution_steps),
term1(Result,Temp_ans),

New_element_formed = term(Result,Temp_ans),
Reasoning_list = [],

New_reasoning_list =[find_position_vector_of_a_point_which_divide_line_segment_internally(Result)],
Current_config = New_reasoning_list,
Concept = section_formula_internally,
Main_predicate = New_element_formed,
string_concatenate(["answer_types([[string(latex(a\\\\hat{i} + b\\\\hat{j} + c\\\\hat{k}=)),type(algebra_form),placeholder(string(Enter the values of a,b,c in radical form and separated by comma)),unit()]])"],"",Answer_type_str),
Answer_type =  [Answer_type_str],
Answer = [3,[Ans]],
string_concatenate(["[string(Use section formula.)]"],"",Hint_str),
Hint = [Hint_str],
%string_concatenate([],"",Solution_steps),
Steps = Solution_steps,

Q1_side_part = "",
Added_solution_step1 = "",
Ques_config_list = [[[Q1_side_part],[Q1_main_part],Added_solution_step1,qt1]],

get_qid_per_ques_representation(Ques_config_list, Qid_per_ques_representation),
complete_rule_by_adding_properties(New_element_formed,Reasoning_list,New_reasoning_list,Concept,
Main_predicate,Current_config,Answer_type,Answer,Steps,Qid_per_ques_representation,Hint,Ques_config_list,1).

/*****************************find_position_vector_of_a_point_which_divide_line_segment_externally***************/
eof1,find_position_vector_of_a_point_which_divide_line_segment_externally1(Result)==> check_fact_exists(term1(Result,_))|

%write(0),nl,read(_),
generate_list(Point_list_1),
generate_list(Point_list_2),
generate_magnitude(Ratio1),
generate_magnitude(Ratio2),
generate_question_vector_ex_11_2(Point_list_1,Point_list_2,Ratio1,Ratio2,Question),
remove_square_brackets_from_string(Question,Q1_main_part),
generate_answer_vector_ex_11_2(Point_list_1,Point_list_2,Ratio1,Ratio2,Ans),
term_string(Ans,Temp_ans),
generate_solution_vector_ex_11_2(Point_list_1,Point_list_2,Ratio1,Ratio2,Solution),
remove_square_brackets_from_string(Solution,Solution_steps),
term1(Result,Temp_ans),
New_element_formed = term(Result,Temp_ans),
Reasoning_list = [],
New_reasoning_list =[find_position_vector_of_a_point_which_divide_line_segment_externally(Result)],
Current_config = New_reasoning_list,
Concept = section_formula_externally,
Main_predicate = New_element_formed,
string_concatenate(["answer_types([[string(latex(a\\\\hat{i} + b\\\\hat{j} + c\\\\hat{k}=)),type(algebra_form),placeholder(string(Enter the values of a,b,c in radical form and separated by comma)),unit()]])"],"",Answer_type_str),
Answer_type =  [Answer_type_str],
Answer = [3,[Ans]],
string_concatenate(["[string(Use section formula.)]"],"",Hint_str),
Hint = [Hint_str],
%string_concatenate([],"",Solution_steps),
Steps = Solution_steps,

Q1_side_part = "",
Added_solution_step1 = "",
Ques_config_list = [[[Q1_side_part],[Q1_main_part],Added_solution_step1,qt1]],

get_qid_per_ques_representation(Ques_config_list, Qid_per_ques_representation),
complete_rule_by_adding_properties(New_element_formed,Reasoning_list,New_reasoning_list,Concept,
Main_predicate,Current_config,Answer_type,Answer,Steps,Qid_per_ques_representation,Hint,Ques_config_list,1).

/*****************************find_given_points_are_vertices_of_a_right_angle_triangle_or_not***************/
eof1,find_given_points_are_vertices_of_a_right_angle_triangle_or_not1(Result)==> check_fact_exists(term1(Result,_))|

%write(2),nl,read(_),
generate_random_list_for_right_angle_triangle_point_list(Point_list_1,Point_list_2,Point_list_3),

generate_question_vector_ex_12(Point_list_1,Point_list_2,Point_list_3,Question),
remove_square_brackets_from_string(Question,Q1_main_part),
generate_answer_vector_ex_12(Point_list_1,Point_list_2,Point_list_3,Ans),
term_string(Ans,Temp_ans),
generate_solution_vector_ex_12(Point_list_1,Point_list_2,Point_list_3,Solution),
remove_square_brackets_from_string(Solution,Solution_steps),

term1(Result,Temp_ans),

New_element_formed = term(Result,Temp_ans),
Reasoning_list = [],

New_reasoning_list =[find_given_points_are_vertices_of_a_right_angle_triangle_or_not(Result)],
Current_config = New_reasoning_list,
Concept = pythagoras_theorem,
Main_predicate = New_element_formed,
string_concatenate(["answer_types([[string(latex(|\\\\overrightarrow{AB}|^2,|\\\\overrightarrow{BC}|^2,|\\\\overrightarrow{AC}|^2) =),type(algebra_form),placeholder(string(Values of latex(|\\\\overrightarrow{AB}|^2,|\\\\overrightarrow{BC}|^2,|\\\\overrightarrow{AC}|^2) in integer form seperated by comma )),unit()]]),objective_answer_types([[string(False)],[string(True)]])"],"",Answer_type_str),
Answer_type =  [Answer_type_str],
Answer = [3,[Ans]],
string_concatenate(["[string(Find position vector and then check for pythagoras_theorem condition.)]"],"",Hint_str),
Hint = [Hint_str],
%string_concatenate([],"",Solution_steps),
Steps = Solution_steps,

Q1_side_part = "",
Added_solution_step1 = "",
Ques_config_list = [[[Q1_side_part],[Q1_main_part],Added_solution_step1,qt1]],

get_qid_per_ques_representation(Ques_config_list, Qid_per_ques_representation),
complete_rule_by_adding_properties(New_element_formed,Reasoning_list,New_reasoning_list,Concept,
Main_predicate,Current_config,Answer_type,Answer,Steps,Qid_per_ques_representation,Hint,Ques_config_list,1).

/*****************************compute_magnitude_of_vector***************/
eof1,compute_magnitude_of_vector1(Result)==> check_fact_exists(term1(Result,_))|

%write(2),nl,read(_),
generate_list(Vector_list),
generate_question_vector_q_1(Vector_list,Question),
remove_square_brackets_from_string(Question,Q1_main_part),
generate_answer_vector_q_1(Vector_list,Ans),
term_string(Ans,Temp_ans),
generate_solution_vector_q_1(Vector_list,Solution),
remove_square_brackets_from_string(Solution,Solution_steps),
term1(Result,Temp_ans),
New_element_formed = term(Result,Temp_ans),
Reasoning_list = [],
New_reasoning_list =[compute_magnitude_of_vector(Result)],
Current_config = New_reasoning_list,
Concept = vector_magnitude,
Main_predicate = New_element_formed,
string_concatenate(["answer_types([[string(Magnitude = ),type(algebra_form),placeholder(string(Enter your answer in radical form)),unit()]])"],"",Answer_type_str),
Answer_type =  [Answer_type_str],
Answer = [3,[Ans]],
string_concatenate(["[string(Magnitude of a = latex(|a|).)]"],"",Hint_str),
Hint = [Hint_str],
%string_concatenate([],"",Solution_steps),
Steps = Solution_steps,

Q1_side_part = "",
Added_solution_step1 = "",
Ques_config_list = [[[Q1_side_part],[Q1_main_part],Added_solution_step1,qt1]],

get_qid_per_ques_representation(Ques_config_list, Qid_per_ques_representation),
complete_rule_by_adding_properties(New_element_formed,Reasoning_list,New_reasoning_list,Concept,
Main_predicate,Current_config,Answer_type,Answer,Steps,Qid_per_ques_representation,Hint,Ques_config_list,1).

/*****************************find_scalar_components***************/
eof1,find_scalar_components1(Result)==> check_fact_exists(term1(Result,_))|

%write(2),nl,read(_),
generate_list(Point_list_1),
generate_list(Point_list_2),
generate_question_vector_q_5_1(Point_list_1,Point_list_2,Question),
remove_square_brackets_from_string(Question,Q1_main_part),
generate_answer_vector_q_5_1(Point_list_1,Point_list_2,Ans),
term_string(Ans,Temp_ans),
generate_solution_vector_q_5_1(Point_list_1,Point_list_2,Solution),
remove_square_brackets_from_string(Solution,Solution_steps),
term1(Result,Temp_ans),
New_element_formed = term(Result,Temp_ans),
Reasoning_list = [],
New_reasoning_list =[find_scalar_components(Result)],
Current_config = New_reasoning_list,
Concept = scalar_components,
Main_predicate = New_element_formed,
string_concatenate(["answer_types([[string(Scalar Components(a,b,c) = ),type(algebra_form),placeholder(string(Enter the values of a,b,c and separated by comma)),unit()]])"],"",Answer_type_str),
Answer_type =  [Answer_type_str],
Answer = [3,[Ans]],
string_concatenate(["[string(Find Position vector PQ)]"],"",Hint_str),
Hint = [Hint_str],
%string_concatenate([],"",Solution_steps),
Steps = Solution_steps,
Q1_side_part = "",
Added_solution_step1 = "",
Ques_config_list = [[[Q1_side_part],[Q1_main_part],Added_solution_step1,qt1]],
get_qid_per_ques_representation(Ques_config_list, Qid_per_ques_representation),
complete_rule_by_adding_properties(New_element_formed,Reasoning_list,New_reasoning_list,Concept,
Main_predicate,Current_config,Answer_type,Answer,Steps,Qid_per_ques_representation,Hint,Ques_config_list,1).

/*****************************find_vector_components***************/
eof1,find_vector_components1(Result)==> check_fact_exists(term1(Result,_))|

%write(2),nl,read(_),
generate_list(Point_list_1),
generate_list(Point_list_2),
generate_question_vector_q_5_2(Point_list_1,Point_list_2,Question),
remove_square_brackets_from_string(Question,Q1_main_part),
generate_answer_vector_q_5_2(Point_list_1,Point_list_2,Ans),
term_string(Ans,Temp_ans),
generate_solution_vector_q_5_2(Point_list_1,Point_list_2,Solution),
remove_square_brackets_from_string(Solution,Solution_steps),
term1(Result,Temp_ans),
New_element_formed = term(Result,Temp_ans),
Reasoning_list = [],
New_reasoning_list =[find_vector_components(Result)],
Current_config = New_reasoning_list,
Concept = vector_components,
Main_predicate = New_element_formed,
string_concatenate(["answer_types([[string(Vector Componentslatex(a\\\\hat{i},b\\\\hat{j},c\\\\hat{k}=)),type(algebra_form),placeholder(string(Enter the values of a,b,c  and separated by comma)),unit()]])"],"",Answer_type_str),
Answer_type =  [Answer_type_str],
Answer = [3,[Ans]],
string_concatenate(["[string(Find Position vector PQ)]"],"",Hint_str),
Hint = [Hint_str],
%string_concatenate([],"",Solution_steps),
Steps = Solution_steps,
Q1_side_part = "",
Added_solution_step1 = "",
Ques_config_list = [[[Q1_side_part],[Q1_main_part],Added_solution_step1,qt1]],
get_qid_per_ques_representation(Ques_config_list, Qid_per_ques_representation),
complete_rule_by_adding_properties(New_element_formed,Reasoning_list,New_reasoning_list,Concept,
Main_predicate,Current_config,Answer_type,Answer,Steps,Qid_per_ques_representation,Hint,Ques_config_list,1).

/*****************************find_sum_of_vector***************/
eof1,find_sum_of_vector1(Result)==> check_fact_exists(term1(Result,_))|

%write(2),nl,read(_),
generate_list(List1),
generate_list(List2),
generate_list(List3),
generate_question_vector_q_6(List1,List2,List3,Question),
remove_square_brackets_from_string(Question,Q1_main_part),
generate_answer_vector_q_6(List1,List2,List3,Ans),
term_string(Ans,Temp_ans),
generate_solution_vector_q_6(List1,List2,List3,Solution),
remove_square_brackets_from_string(Solution,Solution_steps),

term1(Result,Temp_ans),

New_element_formed = term(Result,Temp_ans),
Reasoning_list = [],

New_reasoning_list =[find_sum_of_vector(Result)],
Current_config = New_reasoning_list,
Concept = sum_of_vector,
Main_predicate = New_element_formed,
string_concatenate(["answer_types([[string(latex(a\\\\hat{i} + b\\\\hat{j} + c\\\\hat{k}=)),type(algebra_form),placeholder(string(Enter the values of a,b,c in radical form and separated by comma)),unit()]])"],"",Answer_type_str),
Answer_type =  [Answer_type_str],
Answer = [3,[Ans]],
string_concatenate(["[string(Add i,j,k components of each vector respectively.)]"],"",Hint_str),
Hint = [Hint_str],
%string_concatenate([],"",Solution_steps),
Steps = Solution_steps,

Q1_side_part = "",
Added_solution_step1 = "",
Ques_config_list = [[[Q1_side_part],[Q1_main_part],Added_solution_step1,qt1]],

get_qid_per_ques_representation(Ques_config_list, Qid_per_ques_representation),
complete_rule_by_adding_properties(New_element_formed,Reasoning_list,New_reasoning_list,Concept,
Main_predicate,Current_config,Answer_type,Answer,Steps,Qid_per_ques_representation,Hint,Ques_config_list,1).

/*****************************find_unit_vector_in_diection_of_the_sum_of_vectors***************/
eof1,find_unit_vector_in_diection_of_the_sum_of_vectors1(Result)==> check_fact_exists(term1(Result,_))|

%write(2),nl,read(_),
generate_list(List1),
generate_list(List2),
generate_question_vector_q_9(List1,List2,Question),
remove_square_brackets_from_string(Question,Q1_main_part),
generate_answer_vector_q_9(List1,List2,Ans),
term_string(Ans,Temp_ans),
generate_solution_vector_q_9(List1,List2,Solution),
remove_square_brackets_from_string(Solution,Solution_steps),

term1(Result,Temp_ans),

New_element_formed = term(Result,Temp_ans),
Reasoning_list = [],

New_reasoning_list =[find_unit_vector_in_diection_of_the_sum_of_vectors(Result)],
Current_config = New_reasoning_list,
Concept = unit_vector_in_diection_of_the_sum_of_vectors,
Main_predicate = New_element_formed,
string_concatenate(["answer_types([[string(latex(a\\\\hat{i} + b\\\\hat{j} + c\\\\hat{k}=)),type(algebra_form),placeholder(string(Enter the values of a,b,c in radical form and separated by comma)),unit()]])"],"",Answer_type_str),
Answer_type =  [Answer_type_str],
Answer = [3,[Ans]],
string_concatenate(["[string(Unit vector is given as :latex(\\\\hat{a} = \\\\frac{1}{|\\\\overrightarrow{a}|} \\\\overrightarrow{a}).)]"],"",Hint_str),
Hint = [Hint_str],
%string_concatenate([],"",Solution_steps),
Steps = Solution_steps,

Q1_side_part = "",
Added_solution_step1 = "",
Ques_config_list = [[[Q1_side_part],[Q1_main_part],Added_solution_step1,qt1]],

get_qid_per_ques_representation(Ques_config_list, Qid_per_ques_representation),
complete_rule_by_adding_properties(New_element_formed,Reasoning_list,New_reasoning_list,Concept,
Main_predicate,Current_config,Answer_type,Answer,Steps,Qid_per_ques_representation,Hint,Ques_config_list,1).

/*****************************check_whether_two_vectors_are_collinear_or_not***************/
eof1,check_whether_two_vectors_are_collinear_or_not1(Result)==> check_fact_exists(term1(Result,_))|

%write(2),nl,read(_),

generate_random_list_collinear_or_non_collinear(List1,List2),
generate_question_vector_q_11(List1,List2,Question),
remove_square_brackets_from_string(Question,Q1_main_part),
generate_answer_vector_q_11(List1,List2,Ans),
generate_solution_vector_q_11(List1,List2,Solution),
remove_square_brackets_from_string(Solution,Solution_steps),

term1(Result,Ans),

New_element_formed = term(Result,Ans),
Reasoning_list = [],

New_reasoning_list =[check_whether_two_vectors_are_collinear_or_not(Result)],
Current_config = New_reasoning_list,
Concept = collinearity,
Main_predicate = New_element_formed,
string_concatenate(["objective_answer_types([[string(False)],[string(True)]])"],"",Answer_type_str),
Answer_type =  [Answer_type_str],
Answer = [[Ans]],
string_concatenate(["[string(For Vectors to be collinear latex(\\\\\\\\overrightarrow{b} = \\\\\\\\lambda.\\\\\\\\overrightarrow{a}))]"],"",Hint_str),
Hint = [Hint_str],
%string_concatenate([],"",Solution_steps),
Steps = Solution_steps,

Q1_side_part = "",
Added_solution_step1 = "",
Ques_config_list = [[[Q1_side_part],[Q1_main_part],Added_solution_step1,qt1]],

get_qid_per_ques_representation(Ques_config_list, Qid_per_ques_representation),
complete_rule_by_adding_properties(New_element_formed,Reasoning_list,New_reasoning_list,Concept,
Main_predicate,Current_config,Answer_type,Answer,Steps,Qid_per_ques_representation,Hint,Ques_config_list,1).

/*****************************check_whether_vectors_is_equally_inclined_to_the_axes_or_not***************/
eof1,check_whether_vectors_is_equally_inclined_to_the_axes_or_not1(Result)==> check_fact_exists(term1(Result,_))|

%write(2),nl,read(_),
generate_random_list_equal_or_unequal(List1),
generate_question_vector_q_14(List1,Question),
remove_square_brackets_from_string(Question,Q1_main_part),
generate_answer_vector_q_14(List1,Ans),
generate_solution_vector_q_14(List1,Solution),
remove_square_brackets_from_string(Solution,Solution_steps),
term1(Result,Ans),
New_element_formed = term(Result,Ans),
Reasoning_list = [],
New_reasoning_list =[check_whether_vectors_is_equally_inclined_to_the_axes_or_not(Result)],
Current_config = New_reasoning_list,
Concept = equally_inclined_vector,
Main_predicate = New_element_formed,
string_concatenate(["objective_answer_types([[string(False)],[string(True)]])"],"",Answer_type_str),
Answer_type =  [Answer_type_str],
Answer = [[Ans]],
string_concatenate(["[string(Check for direction cosines of the vector)]"],"",Hint_str),
Hint = [Hint_str],
%string_concatenate([],"",Solution_steps),
Steps = Solution_steps,

Q1_side_part = "",
Added_solution_step1 = "",
Ques_config_list = [[[Q1_side_part],[Q1_main_part],Added_solution_step1,qt1]],

get_qid_per_ques_representation(Ques_config_list, Qid_per_ques_representation),
complete_rule_by_adding_properties(New_element_formed,Reasoning_list,New_reasoning_list,Concept,
Main_predicate,Current_config,Answer_type,Answer,Steps,Qid_per_ques_representation,Hint,Ques_config_list,1).

/*****************************find_position_vector_of_the_mid_point_of_the_vector***************/
eof1,find_position_vector_of_the_mid_point_of_the_vector1(Result)==> check_fact_exists(term1(Result,_))|

%write(2),nl,read(_),
generate_list(Point_list_1),
generate_list(Point_list_2),
generate_question_vector_q_16(Point_list_1,Point_list_2,Question),
remove_square_brackets_from_string(Question,Q1_main_part),
generate_answer_vector_q_16(Point_list_1,Point_list_2,Ans),
term_string(Ans,Temp_Ans),
generate_solution_vector_q_16(Point_list_1,Point_list_2,Solution),
remove_square_brackets_from_string(Solution,Solution_steps),
term1(Result,Temp_Ans),
New_element_formed = term(Result,Temp_Ans),
Reasoning_list = [],
New_reasoning_list =[find_position_vector_of_the_mid_point_of_the_vector(Result)],
Current_config = New_reasoning_list,
Concept = mid_point_vector,
Main_predicate = New_element_formed,
string_concatenate(["answer_types([[string(latex(a\\\\hat{i} + b\\\\hat{j} + c\\\\hat{k}=)),type(algebra_form),placeholder(string(Enter the values of a,b,c in radical form and separated by comma)),unit()]])"],"",Answer_type_str),
Answer_type =  [Answer_type_str],
Answer = [3,[Ans]],
string_concatenate(["[string(Use Section formula with ratio 1:1)]"],"",Hint_str),
Hint = [Hint_str],
%string_concatenate([],"",Solution_steps),
Steps = Solution_steps,

Q1_side_part = "",
Added_solution_step1 = "",
Ques_config_list = [[[Q1_side_part],[Q1_main_part],Added_solution_step1,qt1]],

get_qid_per_ques_representation(Ques_config_list, Qid_per_ques_representation),
complete_rule_by_adding_properties(New_element_formed,Reasoning_list,New_reasoning_list,Concept,
Main_predicate,Current_config,Answer_type,Answer,Steps,Qid_per_ques_representation,Hint,Ques_config_list,1).


/*****************************find_angle_between_two_vectors_with_given_magnitude_and_product***************/
eof1,find_angle_between_two_vectors_with_given_magnitude_and_product1(Result)==> check_fact_exists(term1(Result,_))|

%write(2),nl,read(_),
generate_magnitude(Mag1),
generate_magnitude(Mag2),
generate_magnitude(Mag3),
generate_question_vector_ex_13(Mag1,Mag2,Mag3,Question),
remove_square_brackets_from_string(Question,Q1_main_part),
generate_answer_vector_ex_13(Mag1,Mag2,Mag3,Ans),
term_string(Ans,Temp_ans),
generate_solution_vector_ex_13(Mag1,Mag2,Mag3,Solution),
remove_square_brackets_from_string(Solution,Solution_steps),
term1(Result,Temp_ans),
New_element_formed = term(Result,Temp_ans),
Reasoning_list = [],
New_reasoning_list =[find_angle_between_two_vectors_with_given_magnitude_and_product(Result)],
Current_config = New_reasoning_list,
Concept = basic_trignometry,
Main_predicate = New_element_formed,
string_concatenate(["answer_types([[string(Angle =),type(algebra_form),placeholder(string(Example of writing the answer: latex(sin^{-1}(\\\\frac{a}{b})) or latex(\\\\frac{\\\\pi}{a})  )),unit()]])"],"",Answer_type_str),
Answer_type =  [Answer_type_str],
Answer = [3,[Ans]],
string_concatenate(["[string(latex(\\\\\\\\overrightarrow{a}.\\\\\\\\overrightarrow{b} = |\\\\\\\\overrightarrow{a}|.|\\\\\\\\overrightarrow{b}| cos\\\\\\\\theta) )]"],"",Hint_str),
Hint = [Hint_str],
%string_concatenate([],"",Solution_steps),
Steps = Solution_steps,

Q1_side_part = "",
Added_solution_step1 = "",
Ques_config_list = [[[Q1_side_part],[Q1_main_part],Added_solution_step1,qt1]],

get_qid_per_ques_representation(Ques_config_list, Qid_per_ques_representation),
complete_rule_by_adding_properties(New_element_formed,Reasoning_list,New_reasoning_list,Concept,
Main_predicate,Current_config,Answer_type,Answer,Steps,Qid_per_ques_representation,Hint,Ques_config_list,1).

/*****************************find_angle_between_the_vectors***************/
eof1,find_angle_between_the_vectors1(Result)==> check_fact_exists(term1(Result,_))|

%write(2),nl,read(_),
generate_list(List1),
generate_list(List2),
generate_question_vector_ex_14(List1,List2,Question),
remove_square_brackets_from_string(Question,Q1_main_part),
generate_answer_vector_ex_14(List1,List2,Ans),
term_string(Ans,Temp_Ans),
generate_solution_vector_ex_14(List1,List2,Solution),
remove_square_brackets_from_string(Solution,Solution_steps),
term1(Result,Temp_Ans),
New_element_formed = term(Result,Temp_Ans),
Reasoning_list = [],
New_reasoning_list =[find_angle_between_the_vectors(Result)],
Current_config = New_reasoning_list,
Concept = trignometry_and_scalar_product,
Main_predicate = New_element_formed,
string_concatenate(["answer_types([[string(Angle =),type(algebra_form),placeholder(string(Example of writing the answer: latex(sin^{-1}(\\\\frac{a}{b})) or latex(\\\\frac{\\\\pi}{a})  )),unit()]])"],"",Answer_type_str),
Answer_type =  [Answer_type_str],
Answer = [3,[Ans]],
string_concatenate(["[string(latex(\\\\\\\\overrightarrow{a}.\\\\\\\\overrightarrow{b} = |\\\\\\\\overrightarrow{a}|.|\\\\\\\\overrightarrow{b}| cos\\\\\\\\theta) )]"],"",Hint_str),
Hint = [Hint_str],
%string_concatenate([],"",Solution_steps),
Steps = Solution_steps,

Q1_side_part = "",
Added_solution_step1 = "",
Ques_config_list = [[[Q1_side_part],[Q1_main_part],Added_solution_step1,qt1]],

get_qid_per_ques_representation(Ques_config_list, Qid_per_ques_representation),
complete_rule_by_adding_properties(New_element_formed,Reasoning_list,New_reasoning_list,Concept,
Main_predicate,Current_config,Answer_type,Answer,Steps,Qid_per_ques_representation,Hint,Ques_config_list,1).

/*****************************check_whether_vectors_are_perpendicular_or_not***************/
eof1,check_whether_vectors_are_perpendicular_or_not1(Result)==> check_fact_exists(term1(Result,_))|

%write(2),nl,read(_),
generate_random_list_for_perpendicular_vector(List1,List2),
generate_question_vector_ex_15(List1,List2,Question),
remove_square_brackets_from_string(Question,Q1_main_part),
generate_answer_vector_ex_15(List1,List2,Ans),
term_string(Ans,Temp_ans),
generate_solution_vector_ex_15(List1,List2,Solution),
remove_square_brackets_from_string(Solution,Solution_steps),
term1(Result,Temp_ans),

New_element_formed = term(Result,Temp_ans),
Reasoning_list = [],

New_reasoning_list =[check_whether_vectors_are_perpendicular_or_not(Result)],
Current_config = New_reasoning_list,
Concept = perpendicular_vectors,
Main_predicate = New_element_formed,
string_concatenate(["answer_types([[string(latex((\\\\overrightarrow{a} + \\\\overrightarrow{b}).(\\\\overrightarrow{a} - \\\\overrightarrow{b})) =),type(algebra_form),placeholder(string(Values of latex((\\\\overrightarrow{a} + \\\\overrightarrow{b}).(\\\\overrightarrow{a} - \\\\overrightarrow{b})) in integer form)),unit()]]),objective_answer_types([[string(False)],[string(True)]])"],"",Answer_type_str),
Answer_type =  [Answer_type_str],
Answer = [3,[Ans]],
string_concatenate(["[string(If latex(\\\\\\\\overrightarrow{a}.\\\\\\\\overrightarrow{b} = 0)), then latex(\\\\\\\\overrightarrow{a}) and latex(\\\\\\\\overrightarrow{b}) are perpendicular vectors. )]"],"",Hint_str),
Hint = [Hint_str],
%string_concatenate([],"",Solution_steps),
Steps = Solution_steps,

Q1_side_part = "",
Added_solution_step1 = "",
Ques_config_list = [[[Q1_side_part],[Q1_main_part],Added_solution_step1,qt1]],
get_qid_per_ques_representation(Ques_config_list, Qid_per_ques_representation),
complete_rule_by_adding_properties(New_element_formed,Reasoning_list,New_reasoning_list,Concept,
Main_predicate,Current_config,Answer_type,Answer,Steps,Qid_per_ques_representation,Hint,Ques_config_list,1).

/*****************************find_projection_of_the_vector_on_another_vector***************/
eof1,find_projection_of_the_vector_on_another_vector1(Result)==> check_fact_exists(term1(Result,_))|

%write(2),nl,read(_),
generate_list(List1),
generate_list(List2),
generate_question_vector_ex_16(List1,List2,Question),
remove_square_brackets_from_string(Question,Q1_main_part),
generate_answer_vector_ex_16(List1,List2,Ans),
term_string(Ans,Temp_Ans),

generate_solution_vector_ex_16(List1,List2,Solution),
remove_square_brackets_from_string(Solution,Solution_steps),
term1(Result,Temp_Ans),

New_element_formed = term(Result,Temp_Ans),
Reasoning_list = [],

New_reasoning_list =[find_projection_of_the_vector_on_another_vector(Result)],
Current_config = New_reasoning_list,
Concept = projection_of_vetor,
Main_predicate = New_element_formed,
string_concatenate(["answer_types([[string(Answer = ),type(algebra_form),placeholder(string(Enter your answer in radical form)),unit()]])"],"",Answer_type_str),
Answer_type =  [Answer_type_str],
Answer = [3,[Ans]],
string_concatenate(["[string(Projection of vector latex(\\\\\\\\overrightarrow{a}) on the vector latex(\\\\\\\\overrightarrow{b}) is given by), string(latex(\\\\\\\\frac{1}{|\\\\\\\\overrightarrow{b}|}(\\\\\\\\overrightarrow{a}.\\\\\\\\overrightarrow{b}) ) )]"],"",Hint_str),
Hint = [Hint_str],
%string_concatenate([],"",Solution_steps),
Steps = Solution_steps,

Q1_side_part = "",
Added_solution_step1 = "",
Ques_config_list = [[[Q1_side_part],[Q1_main_part],Added_solution_step1,qt1]],
get_qid_per_ques_representation(Ques_config_list, Qid_per_ques_representation),
complete_rule_by_adding_properties(New_element_formed,Reasoning_list,New_reasoning_list,Concept,
Main_predicate,Current_config,Answer_type,Answer,Steps,Qid_per_ques_representation,Hint,Ques_config_list,1).

/*****************************find_magnitude_of_differnce_of_vectors***************/
eof1,find_magnitude_of_differnce_of_vectors1(Result)==> check_fact_exists(term1(Result,_))|

%write(2),nl,read(_),
generate_magnitude(Mag1),
generate_magnitude(Mag2),
generate_magnitude(Mag3),
generate_question_vector_ex_17(Mag1,Mag2,Mag3,Question),
remove_square_brackets_from_string(Question,Q1_main_part),
generate_answer_vector_ex_17(Mag1,Mag2,Mag3,Ans),
term_string(Ans,Temp_Ans),

generate_solution_vector_ex_17(Mag1,Mag2,Mag3,Solution),
remove_square_brackets_from_string(Solution,Solution_steps),
term1(Result,Temp_Ans),

New_element_formed = term(Result,Temp_Ans),
Reasoning_list = [],

New_reasoning_list =[find_magnitude_of_differnce_of_vectors(Result)],
Current_config = New_reasoning_list,
Concept = magnitude_of_differnce,
Main_predicate = New_element_formed,
string_concatenate(["answer_types([[string(),type(continuous(string(Magnitude  = ),input(string(),type(algebra_form)),string((Enter your answer in integer or radical form)),input(string(),type())))]])"],"",Answer_type_str),
Answer_type =  [Answer_type_str],
Answer = [3,[Ans]],
string_concatenate(["[string(latex(|\\\\\\\\overrightarrow{a}-\\\\\\\\overrightarrow{b}|^2 = (\\\\\\\\overrightarrow{a}-\\\\\\\\overrightarrow{b}).(\\\\\\\\overrightarrow{a}-\\\\\\\\overrightarrow{b}) ) )]"],"",Hint_str),
Hint = [Hint_str],
%string_concatenate([],"",Solution_steps),
Steps = Solution_steps,

Q1_side_part = "",
Added_solution_step1 = "",
Ques_config_list = [[[Q1_side_part],[Q1_main_part],Added_solution_step1,qt1]],
get_qid_per_ques_representation(Ques_config_list, Qid_per_ques_representation),
complete_rule_by_adding_properties(New_element_formed,Reasoning_list,New_reasoning_list,Concept,
Main_predicate,Current_config,Answer_type,Answer,Steps,Qid_per_ques_representation,Hint,Ques_config_list,1).

/*****************************given_a_unit_vector_and_equation_find_magnitude_of_variable***************/
eof1,given_a_unit_vector_and_equation_find_magnitude_of_variable1(Result)==> check_fact_exists(term1(Result,_))|

%write(2),nl,read(_),
generate_magnitude(Mag1),
generate_question_vector_ex_18(Mag1,Question),
remove_square_brackets_from_string(Question,Q1_main_part),
generate_answer_vector_ex_18(Mag1,Ans),
term_string(Ans,Temp_Ans),
generate_solution_vector_ex_18(Mag1,Solution),
remove_square_brackets_from_string(Solution,Solution_steps),
term1(Result,Temp_Ans),
New_element_formed = term(Result,Temp_Ans),
Reasoning_list = [],
New_reasoning_list =[given_a_unit_vector_and_equation_find_magnitude_of_variable(Result)],
Current_config = New_reasoning_list,
Concept = magnitude_of_variable,
Main_predicate = New_element_formed,
string_concatenate(["answer_types([[string(),type(continuous(string(Magnitude  = ),input(string(),type(algebra_form)),string((Enter your answer in integer or radical form)),input(string(),type())))]])"],"",Answer_type_str),
Answer_type =  [Answer_type_str],
Answer = [3,[Ans]],
string_concatenate(["[string(Unit vector, latex(|\\\\\\\\overrightarrow{a}| = 1.) )]"],"",Hint_str),
Hint = [Hint_str],
%string_concatenate([],"",Solution_steps),
Steps = Solution_steps,

Q1_side_part = "",
Added_solution_step1 = "",
Ques_config_list = [[[Q1_side_part],[Q1_main_part],Added_solution_step1,qt1]],
get_qid_per_ques_representation(Ques_config_list, Qid_per_ques_representation),
complete_rule_by_adding_properties(New_element_formed,Reasoning_list,New_reasoning_list,Concept,
Main_predicate,Current_config,Answer_type,Answer,Steps,Qid_per_ques_representation,Hint,Ques_config_list,1).

/*****************************check_whether_points_are_collinear_or_not***************/
eof1,check_whether_points_are_collinear_or_not1(Result)==> check_fact_exists(term1(Result,_))|

%write(2),nl,read(_),


generate_random_list_for_collinear_point_list(Point_list_1,Point_list_2,Point_list_3),
generate_question_vector_ex_21(Point_list_1,Point_list_2,Point_list_3,Question),
remove_square_brackets_from_string(Question,Q1_main_part),
generate_answer_vector_ex_21(Point_list_1,Point_list_2,Point_list_3,Ans),

term_string(Ans,Temp_ans),
generate_solution_vector_ex_21(Point_list_1,Point_list_2,Point_list_3,Solution),
remove_square_brackets_from_string(Solution,Solution_steps),
term1(Result,Temp_ans),
New_element_formed = term(Result,Temp_ans),
Reasoning_list = [],
New_reasoning_list =[check_whether_points_are_collinear_or_not(Result)],
Current_config = New_reasoning_list,
Concept = collinear_points,
Main_predicate = New_element_formed,
%string_concatenate(["objective_answer_types([[string(A),type(continuous(string(True, latex(|AB|,|BC|,|AC|) = ),input(string(),type(algebra_form),placeholder(string(Values of |AB|,|BC|,|AC| in integer or radical form seperated by comma)),unit()),string()))],[string(B),type(continuous(string(False, latex(|AB|,|BC|,|AC|) = ),input(string(),type(algebra_form),placeholder(string(Values of |AB|,|BC|,|AC| in integer or radical form seperated by comma)),unit()),string()))]])"],"",Answer_type_str),
string_concatenate(["answer_types([[string(latex(|AB|,|BC|,|AC|) =),type(algebra_form),placeholder(string(Values of |AB|,|BC|,|AC| in integer or radical form seperated by comma   )),unit()]]),objective_answer_types([[string(False)],[string(True)]])"],"",Answer_type_str),
Answer_type =  [Answer_type_str],
Answer = [3,[Ans]],
string_concatenate(["[string(If latex(|\\\\overrightarrow{AB}|) = latex(|\\\\overrightarrow{BC}|) + latex(|\\\\overrightarrow{AC}|), Then the points A,B,C are collinear. )]"],"",Hint_str),
Hint = [Hint_str],
%string_concatenate([],"",Solution_steps),
Steps = Solution_steps,

Q1_side_part = "",
Added_solution_step1 = "",
Ques_config_list = [[[Q1_side_part],[Q1_main_part],Added_solution_step1,qt1]],
get_qid_per_ques_representation(Ques_config_list, Qid_per_ques_representation),
complete_rule_by_adding_properties(New_element_formed,Reasoning_list,New_reasoning_list,Concept,
Main_predicate,Current_config,Answer_type,Answer,Steps,Qid_per_ques_representation,Hint,Ques_config_list,1).

/*****************************check_for_unit_vector***************/
eof1,check_for_unit_vector1(Result)==> check_fact_exists(term1(Result,_))|
%write(2),nl,read(_),
generate_random_list_magnitude_for_unit_vector(List1,Mag1),
generate_question_vector_10_3_5_1(Mag1,List1,Question),
remove_square_brackets_from_string(Question,Q1_main_part),
generate_answer_vector_10_3_5_1(Mag1,List1,Ans),
term_string(Ans,Temp_ans),
generate_solution_vector_10_3_5_1(Mag1,List1,Solution),
remove_square_brackets_from_string(Solution,Solution_steps),
term1(Result,Temp_ans),
New_element_formed = term(Result,Temp_ans),
Reasoning_list = [],
New_reasoning_list =[check_for_unit_vector(Result)],
Current_config = New_reasoning_list,
Concept = check_unit_vector,
Main_predicate = New_element_formed,
%string_concatenate(["objective_answer_types([[string(A),type(continuous(string(True, latex(|a|) = ),input(string(),type(algebra_form),placeholder(string(Magnitude of a in integer or radical form )),unit()),string()))],[string(B),type(continuous(string(False, latex(|a|) = ),input(string(),type(algebra_form),placeholder(string(Magnitude of a in integer or radical form )),unit()),string()))]])"],"",Answer_type_str),
string_concatenate(["answer_types([[string(latex(|a|) =),type(algebra_form),placeholder(string(Magnitude of a in integer or radical form   )),unit()]]),objective_answer_types([[string(False)],[string(True)]])"],"",Answer_type_str),

Answer_type =  [Answer_type_str],
Answer = [3,[Ans]],
string_concatenate(["[string(Magnitude of Unit vector, latex(|\\\\\\\\overrightarrow{a}| = 1.))]"],"",Hint_str),
Hint = [Hint_str],
%string_concatenate([],"",Solution_steps),
Steps = Solution_steps,

Q1_side_part = "",
Added_solution_step1 = "",
Ques_config_list = [[[Q1_side_part],[Q1_main_part],Added_solution_step1,qt1]],
get_qid_per_ques_representation(Ques_config_list, Qid_per_ques_representation),
complete_rule_by_adding_properties(New_element_formed,Reasoning_list,New_reasoning_list,Concept,
Main_predicate,Current_config,Answer_type,Answer,Steps,Qid_per_ques_representation,Hint,Ques_config_list,1).

/*****************************check_for_mutually_perpendicular_vector***************/
eof1,check_for_mutually_perpendicular_vector1(Result)==> check_fact_exists(term1(Result,_))|
%write(2),nl,read(_),
generate_random_list_for_mutually_perpendicular_vector(List1,List2),
generate_magnitude(Mag1),
generate_magnitude(Mag2),
generate_question_vector_10_3_5_2(Mag1,Mag2,List1,List2,Question),
remove_square_brackets_from_string(Question,Q1_main_part),
generate_answer_vector_10_3_5_2(Mag1,Mag2,List1,List2,Ans),
term_string(Ans,Temp_ans),
generate_solution_vector_10_3_5_2(Mag1,Mag2,List1,List2,Solution),
remove_square_brackets_from_string(Solution,Solution_steps),
term1(Result,Temp_ans),
New_element_formed = term(Result,Temp_ans),
Reasoning_list = [],
New_reasoning_list =[check_for_mutually_perpendicular_vector(Result)],
Current_config = New_reasoning_list,
Concept = check_mutually_perpendicular_vector,
Main_predicate = New_element_formed,
%string_concatenate(["objective_answer_types([[string(A),type(continuous(string(True, latex((\\\\overrightarrow{a}.\\\\overrightarrow{b})) = ),input(string(),type(algebra_form),placeholder(string(Magnitude of latex((\\\\overrightarrow{a}.\\\\overrightarrow{b})) in integer or radical form )),unit()),string()))],[string(B),type(continuous(string(False, latex((\\\\overrightarrow{a}.\\\\overrightarrow{b})) = ),input(string(),type(algebra_form),placeholder(string(Magnitude of latex((\\\\overrightarrow{a}.\\\\overrightarrow{b})) in integer or radical form )),unit()),string()))]])"],"",Answer_type_str),
string_concatenate(["answer_types([[string(latex((\\\\overrightarrow{a}.\\\\overrightarrow{b})) =),type(algebra_form),placeholder(string(Magnitude of latex((\\\\overrightarrow{a}.\\\\overrightarrow{b})) in integer or radical form   )),unit()]]),objective_answer_types([[string(False)],[string(True)]])"],"",Answer_type_str),
Answer_type =  [Answer_type_str],
Answer = [3,[Ans]],
string_concatenate(["[string(If latex(\\\\\\\\overrightarrow{a}.\\\\\\\\overrightarrow{b} = \\\\\\\\overrightarrow{b}.\\\\\\\\overrightarrow{c} = \\\\\\\\overrightarrow{c}.\\\\\\\\overrightarrow{a} = 0) then latex(\\\\\\\\overrightarrow{a},\\\\\\\\overrightarrow{b},\\\\\\\\overrightarrow{c}) are mutually perpendicular)]"],"",Hint_str),
Hint = [Hint_str],
%string_concatenate([],"",Solution_steps),
Steps = Solution_steps,

Q1_side_part = "",
Added_solution_step1 = "",
Ques_config_list = [[[Q1_side_part],[Q1_main_part],Added_solution_step1,qt1]],
get_qid_per_ques_representation(Ques_config_list, Qid_per_ques_representation),
complete_rule_by_adding_properties(New_element_formed,Reasoning_list,New_reasoning_list,Concept,
Main_predicate,Current_config,Answer_type,Answer,Steps,Qid_per_ques_representation,Hint,Ques_config_list,1).
/*****************************find_variable_magnitude***************/
eof1,find_variable_magnitude1(Result)==> check_fact_exists(term1(Result,_))|

%write(2),nl,read(_),
generate_magnitude(Mag1),
generate_magnitude(Mag2),
generate_question_vector_10_3_6(Mag1,Mag2,Question),
remove_square_brackets_from_string(Question,Q1_main_part),
generate_answer_vector_10_3_6(Mag1,Mag2,Ans),
term_string(Ans,Temp_Ans),
generate_solution_vector_10_3_6(Mag1,Mag2,Solution),
remove_square_brackets_from_string(Solution,Solution_steps),
term1(Result,Temp_Ans),
New_element_formed = term(Result,Temp_Ans),
Reasoning_list = [],
New_reasoning_list =[find_variable_magnitude(Result)],
Current_config = New_reasoning_list,
Concept = value_substitution,
Main_predicate = New_element_formed,
string_concatenate(["answer_types([[string(),type(continuous(string(Magnitude of a,b = ),input(string(),type(algebra_form)),string((Enter your answer separated by comma)),input(string(),type())))]])"],"",Answer_type_str),
Answer_type =  [Answer_type_str],
Answer = [3,[Ans]],
string_concatenate(["[string(latex((\\\\\\\\overrightarrow{a} + \\\\\\\\overrightarrow{b}).(\\\\\\\\overrightarrow{a} - \\\\\\\\overrightarrow{b}) = |\\\\\\\\overrightarrow{a}|^2 - |\\\\\\\\overrightarrow{b}|^2 ))]"],"",Hint_str),
Hint = [Hint_str],
%string_concatenate([],"",Solution_steps),
Steps = Solution_steps,

Q1_side_part = "",
Added_solution_step1 = "",
Ques_config_list = [[[Q1_side_part],[Q1_main_part],Added_solution_step1,qt1]],
get_qid_per_ques_representation(Ques_config_list, Qid_per_ques_representation),
complete_rule_by_adding_properties(New_element_formed,Reasoning_list,New_reasoning_list,Concept,
Main_predicate,Current_config,Answer_type,Answer,Steps,Qid_per_ques_representation,Hint,Ques_config_list,1).

/*****************************evaluate_the_product***************/
eof1,evaluate_the_product1(Result)==> check_fact_exists(term1(Result,_))|

generate_list_of_two(List1),
generate_list_of_two(List2),
generate_question_vector_10_3_7(List1,List2,Question),
remove_square_brackets_from_string(Question,Q1_main_part),
generate_answer_vector_10_3_7(List1,List2,Ans,Answer_type_str),
generate_solution_vector_10_3_7(List1,List2,Solution),
remove_square_brackets_from_string(Solution,Solution_steps),
term1(Result,Ans),
New_element_formed = term(Result,Ans),
Reasoning_list = [],
New_reasoning_list =[evaluate_the_product(Result)],
Current_config = New_reasoning_list,
Concept = evaluate_product,
Main_predicate = New_element_formed,
Answer_type =  [Answer_type_str],
Answer = [[Ans]],
string_concatenate(["[string(latex((\\\\\\\\overrightarrow{a} - \\\\\\\\overrightarrow{b}).(\\\\\\\\overrightarrow{a} + \\\\\\\\overrightarrow{b}) = \\\\\\\\overrightarrow{a}.\\\\\\\\overrightarrow{a} + \\\\\\\\overrightarrow{a}.\\\\\\\\overrightarrow{b} - \\\\\\\\overrightarrow{b}.\\\\\\\\overrightarrow{a} - \\\\\\\\overrightarrow{b}.\\\\\\\\overrightarrow{b}))]"],"",Hint_str),
Hint = [Hint_str],
%string_concatenate([],"",Solution_steps),
Steps = Solution_steps,
Q1_side_part = "",
Added_solution_step1 = "",
Ques_config_list = [[[Q1_side_part],[Q1_main_part],Added_solution_step1,qt1]],
get_qid_per_ques_representation(Ques_config_list, Qid_per_ques_representation),
complete_rule_by_adding_properties(New_element_formed,Reasoning_list,New_reasoning_list,Concept,
Main_predicate,Current_config,Answer_type,Answer,Steps,Qid_per_ques_representation,Hint,Ques_config_list,1).

/*****************************find_magnitude_of_vectors_given_magnitude_angle_scalar_product***************/
eof1, find_magnitude_of_vectors_given_magnitude_angle_scalar_product1(Result)==> check_fact_exists(term1(Result,_))|

%write(2),nl,read(_),
generate_magnitude(Scalar_product),
generate_angle(Angle),
generate_question_vector_10_3_8(Angle,Scalar_product,Question),
remove_square_brackets_from_string(Question,Q1_main_part),
generate_answer_vector_10_3_8(Angle,Scalar_product,Ans),
term_string(Ans,Temp_Ans),
generate_solution_vector_10_3_8(Angle,Scalar_product,Solution),
remove_square_brackets_from_string(Solution,Solution_steps),
term1(Result,Temp_Ans),
New_element_formed = term(Result,Temp_Ans),
Reasoning_list = [],
New_reasoning_list =[find_magnitude_of_vectors_given_magnitude_angle_scalar_product(Result)],
Current_config = New_reasoning_list,
Concept = magnitude_of_vectors_using_trignometry,
Main_predicate = New_element_formed,
string_concatenate(["answer_types([[string(),type(continuous(string(Magnitude  = ),input(string(),type(algebra_form)),string((Enter your answer in integer or radical form)),input(string(),type())))]])"],"",Answer_type_str),
Answer_type =  [Answer_type_str],
Answer = [3,[Ans]],
string_concatenate(["[string(latex(\\\\\\\\overrightarrow{a}.\\\\\\\\overrightarrow{b} = |\\\\\\\\overrightarrow{a}|.|\\\\\\\\overrightarrow{b}| cos\\\\\\\\theta) )]"],"",Hint_str),
Hint = [Hint_str],
%string_concatenate([],"",Solution_steps),
Steps = Solution_steps,
Q1_side_part = "",
Added_solution_step1 = "",
Ques_config_list = [[[Q1_side_part],[Q1_main_part],Added_solution_step1,qt1]],
get_qid_per_ques_representation(Ques_config_list, Qid_per_ques_representation),
complete_rule_by_adding_properties(New_element_formed,Reasoning_list,New_reasoning_list,Concept,
Main_predicate,Current_config,Answer_type,Answer,Steps,Qid_per_ques_representation,Hint,Ques_config_list,1).

/*****************************find_value_of_lambda***************/
eof1, find_value_of_lambda1(Result)==> check_fact_exists(term1(Result,_))|

%write(2),nl,read(_),
generate_list_with_condition(List1,List2,List3),
generate_question_vector_10_3_10(List1,List2,List3,Question),
remove_square_brackets_from_string(Question,Q1_main_part),
generate_answer_vector_10_3_10(List1,List2,List3,Ans),
term_string(Ans,Temp_Ans),
generate_solution_vector_10_3_10(List1,List2,List3,Solution),
remove_square_brackets_from_string(Solution,Solution_steps),
term1(Result,Temp_Ans),
New_element_formed = term(Result,Temp_Ans),
Reasoning_list = [],
New_reasoning_list =[find_value_of_lambda(Result)],
Current_config = New_reasoning_list,
Concept = dot_product_of_perpendicular_vectors,
Main_predicate = New_element_formed,
string_concatenate(["answer_types([[string(),type(continuous(string(latex(\\\\lambda)  = ),input(string(),type(algebra_form)),string((Enter your answer in integer or radical form)),input(string(),type())))]])"],"",Answer_type_str),
Answer_type =  [Answer_type_str],
Answer = [3,[Ans]],
string_concatenate(["[string(If latex(\\\\\\\\overrightarrow{a}.\\\\\\\\overrightarrow{b} = 0), then latex(\\\\\\\\overrightarrow{a}) is perpendicular to latex(\\\\\\\\overrightarrow{b}) )]"],"",Hint_str),
Hint = [Hint_str],
%string_concatenate([],"",Solution_steps),
Steps = Solution_steps,

Q1_side_part = "",
Added_solution_step1 = "",
Ques_config_list = [[[Q1_side_part],[Q1_main_part],Added_solution_step1,qt1]],
get_qid_per_ques_representation(Ques_config_list, Qid_per_ques_representation),
complete_rule_by_adding_properties(New_element_formed,Reasoning_list,New_reasoning_list,Concept,
Main_predicate,Current_config,Answer_type,Answer,Steps,Qid_per_ques_representation,Hint,Ques_config_list,1).

/*****************************find_value_of_expression***************/
eof1, find_value_of_expression1(Result)==> check_fact_exists(term1(Result,_))|

%write(2),nl,read(_),
generate_magnitude(Mag),
generate_question_vector_10_3_13(Mag,Question),
remove_square_brackets_from_string(Question,Q1_main_part),
generate_answer_vector_10_3_13(Mag,Ans),
term_string(Ans,Temp_Ans),
generate_solution_vector_10_3_13(Mag,Solution),
remove_square_brackets_from_string(Solution,Solution_steps),
term1(Result,Temp_Ans),
New_element_formed = term(Result,Temp_Ans),
Reasoning_list = [],
New_reasoning_list =[find_value_of_expression(Result)],
Current_config = New_reasoning_list,
Concept = expand_expression,
Main_predicate = New_element_formed,
string_concatenate(["answer_types([[string(),type(continuous(string(Answer = ),input(string(),type(algebra_form)),string((Enter your answer in integer or radical form)),input(string(),type())))]])"],"",Answer_type_str),
Answer_type =  [Answer_type_str],
Answer = [3,[Ans]],
string_concatenate(["[string(latex(|\\\\overrightarrow{a}+\\\\overrightarrow{b}+\\\\overrightarrow{c}|^2 = (\\\\overrightarrow{a}+\\\\overrightarrow{b}+\\\\overrightarrow{c}).(\\\\overrightarrow{a}+\\\\overrightarrow{b}+\\\\overrightarrow{c})) )]"],"",Hint_str),
Hint = [Hint_str],
%string_concatenate([],"",Solution_steps),
Steps = Solution_steps,
Q1_side_part = "",
Added_solution_step1 = "",
Ques_config_list = [[[Q1_side_part],[Q1_main_part],Added_solution_step1,qt1]],
get_qid_per_ques_representation(Ques_config_list, Qid_per_ques_representation),
complete_rule_by_adding_properties(New_element_formed,Reasoning_list,New_reasoning_list,Concept,
Main_predicate,Current_config,Answer_type,Answer,Steps,Qid_per_ques_representation,Hint,Ques_config_list,1).

/*****************************find_angle_between_the_vectors_with_given_vertices_of_triangle***************/
eof1, find_angle_between_the_vectors_with_given_vertices_of_triangle1(Result)==> check_fact_exists(term1(Result,_))|

%write(2),nl,read(_),
generate_list(Point_list_1),
generate_list(Point_list_2),
generate_list(Point_list_3),

generate_question_vector_10_3_15(Point_list_1,Point_list_2,Point_list_3,Question),
remove_square_brackets_from_string(Question,Q1_main_part),
generate_answer_vector_10_3_15(Point_list_1,Point_list_2,Point_list_3,Ans),
term_string(Ans,Temp_Ans),

generate_solution_vector_10_3_15(Point_list_1,Point_list_2,Point_list_3,Solution),
remove_square_brackets_from_string(Solution,Solution_steps),
term1(Result,Temp_Ans),
New_element_formed = term(Result,Temp_Ans),
Reasoning_list = [],
New_reasoning_list =[find_angle_between_the_vectors_with_given_vertices_of_triangle(Result)],
Current_config = New_reasoning_list,
Concept = vector_and_trignometry, 
Main_predicate = New_element_formed,

string_concatenate(["answer_types([[string(Angle =),type(algebra_form),placeholder(string(Example of writing the answer: latex(sin^{-1}(\\\\frac{a}{b}))  )),unit()]])"],"",Answer_type_str),

Answer_type =  [Answer_type_str],
Answer = [3,[Ans]],
string_concatenate(["[string(latex(\\\\overrightarrow{BA}.\\\\overrightarrow{BC} = |\\\\overrightarrow{BA}| |\\\\overrightarrow{BC}| cos(\\\\angle{ABC})) )]"],"",Hint_str),
Hint = [Hint_str],
%string_concatenate([],"",Solution_steps),
Steps = Solution_steps,
Q1_side_part = "",
Added_solution_step1 = "",
Ques_config_list = [[[Q1_side_part],[Q1_main_part],Added_solution_step1,qt1]],
get_qid_per_ques_representation(Ques_config_list, Qid_per_ques_representation),
complete_rule_by_adding_properties(New_element_formed,Reasoning_list,New_reasoning_list,Concept,
Main_predicate,Current_config,Answer_type,Answer,Steps,Qid_per_ques_representation,Hint,Ques_config_list,1).

/*****************************find_magnitude_of_cross_product***************/
eof1, find_magnitude_of_cross_product1(Result)==> check_fact_exists(term1(Result,_))|

%write(2),nl,read(_),
generate_list(List1),
generate_list(List2),
generate_question_vector_ex_22(List1,List2,Question),
remove_square_brackets_from_string(Question,Q1_main_part),
generate_answer_vector_ex_22(List1,List2,Ans),
term_string(Ans,Temp_Ans),
generate_solution_vector_ex_22(List1,List2,Solution),
remove_square_brackets_from_string(Solution,Solution_steps),
term1(Result,Temp_Ans),
New_element_formed = term(Result,Temp_Ans),
Reasoning_list = [],
New_reasoning_list =[find_magnitude_of_cross_product(Result)],
Current_config = New_reasoning_list,
Concept = cross_product,
Main_predicate = New_element_formed,
string_concatenate(["answer_types([[string(),type(continuous(string(Answer = ),input(string(),type(algebra_form)),string((Enter your answer in integer or radical form)),input(string(),type())))]])"],"",Answer_type_str),
Answer_type =  [Answer_type_str],
Answer = [3,[Ans]],
string_concatenate(["[string(Find cross product latex(\\\\\\\\overrightarrow{a}) X latex(\\\\\\\\overrightarrow{b}))]"],"",Hint_str),
Hint = [Hint_str],
%string_concatenate([],"",Solution_steps),
Steps = Solution_steps,
Q1_side_part = "",
Added_solution_step1 = "",
Ques_config_list = [[[Q1_side_part],[Q1_main_part],Added_solution_step1,qt1]],
get_qid_per_ques_representation(Ques_config_list, Qid_per_ques_representation),
complete_rule_by_adding_properties(New_element_formed,Reasoning_list,New_reasoning_list,Concept,
Main_predicate,Current_config,Answer_type,Answer,Steps,Qid_per_ques_representation,Hint,Ques_config_list,1).

/*****************************find_unit_vector_perpendicular_to_each_of_the_vector***************/
eof1, find_unit_vector_perpendicular_to_each_of_the_vector1(Result)==> check_fact_exists(term1(Result,_))|

%write(2),nl,read(_),
generate_list(List1),
generate_list(List2),
generate_question_vector_ex_23(List1,List2,Question),
remove_square_brackets_from_string(Question,Q1_main_part),
generate_answer_vector_ex_23(List1,List2,Ans),
term_string(Ans,Temp_Ans),
generate_solution_vector_ex_23(List1,List2,Solution),
remove_square_brackets_from_string(Solution,Solution_steps),
term1(Result,Temp_Ans),
New_element_formed = term(Result,Temp_Ans),
Reasoning_list = [],
New_reasoning_list =[find_unit_vector_perpendicular_to_each_of_the_vector(Result)],
Current_config = New_reasoning_list,
Concept = perpendicular_unit_vector,
Main_predicate = New_element_formed,
string_concatenate(["answer_types([[string(latex(a\\\\hat{i} + b\\\\hat{j} + c\\\\hat{k}=)),type(algebra_form),placeholder(string(Enter the values of a,b,c in radical form and separated by comma)),unit()]])"],"",Answer_type_str),
Answer_type =  [Answer_type_str],
Answer = [3,[Ans]],
string_concatenate(["[string(Find cross product latex(\\\\\\\\overrightarrow{a}) X latex(\\\\\\\\overrightarrow{b}))]"],"",Hint_str),
Hint = [Hint_str],
%string_concatenate([],"",Solution_steps),
Steps = Solution_steps,
Q1_side_part = "",
Added_solution_step1 = "",
Ques_config_list = [[[Q1_side_part],[Q1_main_part],Added_solution_step1,qt1]],
get_qid_per_ques_representation(Ques_config_list, Qid_per_ques_representation),
complete_rule_by_adding_properties(New_element_formed,Reasoning_list,New_reasoning_list,Concept,
Main_predicate,Current_config,Answer_type,Answer,Steps,Qid_per_ques_representation,Hint,Ques_config_list,1).

/*****************************find_area_of_triangle_having_point_as_it_vertices***************/
eof1, find_area_of_triangle_having_point_as_it_vertices1(Result)==> check_fact_exists(term1(Result,_))|

%write(2),nl,read(_),
generate_list(Point_list_1),
generate_list(Point_list_2),
generate_list(Point_list_3),
generate_question_vector_ex_24(Point_list_1,Point_list_2,Point_list_3,Question),
remove_square_brackets_from_string(Question,Q1_main_part),
generate_answer_vector_ex_24(Point_list_1,Point_list_2,Point_list_3,Ans),
term_string(Ans,Temp_Ans),
generate_solution_vector_ex_24(Point_list_1,Point_list_2,Point_list_3,Solution),
remove_square_brackets_from_string(Solution,Solution_steps),
term1(Result,Temp_Ans),
New_element_formed = term(Result,Temp_Ans),
Reasoning_list = [],
New_reasoning_list =[find_area_of_triangle_having_point_as_it_vertices(Result)],
Current_config = New_reasoning_list,
Concept = cross_product_triangle_area,
Main_predicate = New_element_formed,
string_concatenate(["answer_types([[string(),type(continuous(string(Answer = ),input(string(),type(algebra_form)),string((Enter your answer in radical form)),input(string(),type())))]])"],"",Answer_type_str),
Answer_type =  [Answer_type_str],
Answer = [3,[Ans]],
string_concatenate(["[string(Find cross product latex(\\\\\\\\overrightarrow{a}) X latex(\\\\\\\\overrightarrow{b}))]"],"",Hint_str),
Hint = [Hint_str],
%string_concatenate([],"",Solution_steps),
Steps = Solution_steps,
Q1_side_part = "",
Added_solution_step1 = "",
Ques_config_list = [[[Q1_side_part],[Q1_main_part],Added_solution_step1,qt1]],
get_qid_per_ques_representation(Ques_config_list, Qid_per_ques_representation),
complete_rule_by_adding_properties(New_element_formed,Reasoning_list,New_reasoning_list,Concept,
Main_predicate,Current_config,Answer_type,Answer,Steps,Qid_per_ques_representation,Hint,Ques_config_list,1).

/*****************************find_lambda_and_mu_in_the_expression***************/
eof1, find_lambda_and_mu_in_the_expression1(Result)==> check_fact_exists(term1(Result,_))|

%write(2),nl,read(_),
generate_list(List1),
generate_magnitude(Mag),
generate_question_vector_10_4_5(List1,Mag,Question),
remove_square_brackets_from_string(Question,Q1_main_part),
generate_answer_vector_10_4_5(List1,Mag,Ans),
term_string(Ans,Temp_Ans),
generate_solution_vector_10_4_5(List1,Mag,Solution),
remove_square_brackets_from_string(Solution,Solution_steps),

term1(Result,Temp_Ans),
New_element_formed = term(Result,Temp_Ans),
Reasoning_list = [],
New_reasoning_list =[find_lambda_and_mu_in_the_expression(Result)],
Current_config = New_reasoning_list,
Concept = cross_product_to_find_variables,
Main_predicate = New_element_formed,
string_concatenate(["answer_types([[string(),type(continuous(string(latex(\\\\lambda,\\\\mu) = ),input(string(),type(algebra_form)),string((Enter the values separated by comma)),input(string(),type())))]])"],"",Answer_type_str),
Answer_type =  [Answer_type_str],
Answer = [3,[Ans]],
string_concatenate(["[string(Find cross product latex(\\\\\\\\overrightarrow{a}) X latex(\\\\\\\\overrightarrow{b}))]"],"",Hint_str),
Hint = [Hint_str],
%string_concatenate([],"",Solution_steps),
Steps = Solution_steps,
Q1_side_part = "",
Added_solution_step1 = "",
Ques_config_list = [[[Q1_side_part],[Q1_main_part],Added_solution_step1,qt1]],
get_qid_per_ques_representation(Ques_config_list, Qid_per_ques_representation),
complete_rule_by_adding_properties(New_element_formed,Reasoning_list,New_reasoning_list,Concept,
Main_predicate,Current_config,Answer_type,Answer,Steps,Qid_per_ques_representation,Hint,Ques_config_list,1).

/*****************************find_expression_value***************/
eof1, find_expression_value1(Result)==> check_fact_exists(term1(Result,_))|

%write(3),nl,read(_),
generate_magnitude(Mag1),
generate_magnitude(Mag2),
generate_magnitude(Mag3),
generate_question_vector_ex_28(Mag1,Mag2,Mag3,Question),
remove_square_brackets_from_string(Question,Q1_main_part),
generate_answer_vector_ex_28(Mag1,Mag2,Mag3,Ans),
term_string(Ans,Temp_Ans),
generate_solution_vector_ex_28(Mag1,Mag2,Mag3,Solution),
remove_square_brackets_from_string(Solution,Solution_steps),
term1(Result,Temp_Ans),
New_element_formed = term(Result,Temp_Ans),
Reasoning_list = [],
New_reasoning_list =[find_expression_value(Result)],
Current_config = New_reasoning_list,
Concept = perpendicular_condition_and_expand_expression,
Main_predicate = New_element_formed,
string_concatenate(["answer_types([[string(),type(continuous(string(Answer = ),input(string(),type(algebra_form)),string((Enter your answer in radical form)),input(string(),type())))]])"],"",Answer_type_str),
Answer_type =  [Answer_type_str],
Answer = [3,[Ans]],
string_concatenate(["[string(latex(|\\\\overrightarrow{a} + \\\\overrightarrow{b} + \\\\overrightarrow{c}|^2 = (\\\\overrightarrow{a} + \\\\overrightarrow{b} + \\\\overrightarrow{c})^2 = (\\\\overrightarrow{a} + \\\\overrightarrow{b} + \\\\overrightarrow{c}).(\\\\overrightarrow{a} + \\\\overrightarrow{b} + \\\\overrightarrow{c}) ) )]"],"",Hint_str),
Hint = [Hint_str],
%string_concatenate([],"",Solution_steps),
Steps = Solution_steps,
Q1_side_part = "",
Added_solution_step1 = "",
Ques_config_list = [[[Q1_side_part],[Q1_main_part],Added_solution_step1,qt1]],
get_qid_per_ques_representation(Ques_config_list, Qid_per_ques_representation),
complete_rule_by_adding_properties(New_element_formed,Reasoning_list,New_reasoning_list,Concept,
Main_predicate,Current_config,Answer_type,Answer,Steps,Qid_per_ques_representation,Hint,Ques_config_list,1).

/*****************************evaluate_the_quantity_mu***************/
eof1, evaluate_the_quantity_mu1(Result)==> check_fact_exists(term1(Result,_))|

%write(2),nl,read(_),
generate_magnitude(Mag1),
generate_magnitude(Mag2),
generate_magnitude(Mag3),
generate_question_vector_ex_29(Mag1,Mag2,Mag3,Question),
remove_square_brackets_from_string(Question,Q1_main_part),
generate_answer_vector_ex_29(Mag1,Mag2,Mag3,Ans),
term_string(Ans,Temp_ans),
generate_solution_vector_ex_29(Mag1,Mag2,Mag3,Solution),
remove_square_brackets_from_string(Solution,Solution_steps),
term1(Result,Temp_ans),
New_element_formed = term(Result,Temp_ans),
Reasoning_list = [],
New_reasoning_list =[evaluate_the_quantity_mu(Result)],
Current_config = New_reasoning_list,
Concept = evaluate_expression,
Main_predicate = New_element_formed,
string_concatenate(["answer_types([[string(Answer =),type(algebra_form),placeholder(string(Enter your answer in integer or radical form)),unit()]])"],"",Answer_type_str),
Answer_type =  [Answer_type_str],
Answer = [3,[Ans]],
string_concatenate(["[string(Multiply latex((\\\\\\\\overrightarrow{a} + \\\\\\\\overrightarrow{b} + \\\\\\\\overrightarrow{c}) = 0 ) with latex(\\\\\\\\overrightarrow{a}, \\\\\\\\overrightarrow{b}, \\\\\\\\overrightarrow{c}) respectively and then simplify. )]"],"",Hint_str),
Hint = [Hint_str],
%string_concatenate([],"",Solution_steps),
Steps = Solution_steps,
Q1_side_part = "",
Added_solution_step1 = "",
Ques_config_list = [[[Q1_side_part],[Q1_main_part],Added_solution_step1,qt1]],
get_qid_per_ques_representation(Ques_config_list, Qid_per_ques_representation),
complete_rule_by_adding_properties(New_element_formed,Reasoning_list,New_reasoning_list,Concept,
Main_predicate,Current_config,Answer_type,Answer,Steps,Qid_per_ques_representation,Hint,Ques_config_list,1).

/*****************************find_value_of_variable_x***************/
eof1, find_value_of_variable_x1(Result)==> check_fact_exists(term1(Result,_))|

%write(2),nl,read(_),
generate_list(List),
generate_question_vector_Mis_5(List,Question),
remove_square_brackets_from_string(Question,Q1_main_part),
generate_answer_vector_Mis_5(List,Ans),
term_string(Ans,Temp_ans),
generate_solution_vector_Mis_5(List,Solution),
remove_square_brackets_from_string(Solution,Solution_steps),
term1(Result,Temp_ans),
New_element_formed = term(Result,Temp_ans),
Reasoning_list = [],
New_reasoning_list =[find_value_of_variable_x(Result)],
Current_config = New_reasoning_list,
Concept = variable_in_a_unit_vector,
Main_predicate = New_element_formed,
string_concatenate(["answer_types([[string(Answer =),type(algebra_form),placeholder(string(Enter your answer in integer or radical form)),unit()]])"],"",Answer_type_str),
Answer_type =  [Answer_type_str],
Answer = [3,[Ans]],
string_concatenate(["[string(Magnitude of Unit Vector is 1)]"],"",Hint_str),
Hint = [Hint_str],
%string_concatenate([],"",Solution_steps),
Steps = Solution_steps,
Q1_side_part = "",
Added_solution_step1 = "",
Ques_config_list = [[[Q1_side_part],[Q1_main_part],Added_solution_step1,qt1]],
get_qid_per_ques_representation(Ques_config_list, Qid_per_ques_representation),
complete_rule_by_adding_properties(New_element_formed,Reasoning_list,New_reasoning_list,Concept,
Main_predicate,Current_config,Answer_type,Answer,Steps,Qid_per_ques_representation,Hint,Ques_config_list,1).

/*****************************find_a_vector_of_given_magnitude_and_parallel_to_resultant_of_vectors***************/
eof1,find_a_vector_of_given_magnitude_and_parallel_to_resultant_of_vectors1(Result)==> check_fact_exists(term1(Result,_))|

%write(2),nl,read(_),
generate_list(List1),
generate_list(List2),
generate_magnitude(Mag),
generate_question_vector_Mis_6(List1,List2,Mag,Question),
remove_square_brackets_from_string(Question,Q1_main_part),
generate_answer_vector_Mis_6(List1,List2,Mag,Ans),
term_string(Ans,Temp_ans),
generate_solution_vector_Mis_6(List1,List2,Mag,Solution),
remove_square_brackets_from_string(Solution,Solution_steps),
term1(Result,Temp_ans),
New_element_formed = term(Result,Temp_ans),
Reasoning_list = [],
New_reasoning_list =[find_a_vector_of_given_magnitude_and_parallel_to_resultant_of_vectors(Result)],
Current_config = New_reasoning_list,
Concept = parallel_vectors,
Main_predicate = New_element_formed,
string_concatenate(["answer_types([[string(latex(a\\\\hat{i} + b\\\\hat{j} + c\\\\hat{k}=)),type(algebra_form),placeholder(string(Enter the values of a,b,c in radical form and separated by comma)),unit()]])"],"",Answer_type_str),
Answer_type =  [Answer_type_str],
Answer = [3,[Ans]],
string_concatenate(["[string(Find Unit vector)]"],"",Hint_str),
Hint = [Hint_str],
%string_concatenate([],"",Solution_steps),
Steps = Solution_steps,
Q1_side_part = "",
Added_solution_step1 = "",
Ques_config_list = [[[Q1_side_part],[Q1_main_part],Added_solution_step1,qt1]],
get_qid_per_ques_representation(Ques_config_list, Qid_per_ques_representation),
complete_rule_by_adding_properties(New_element_formed,Reasoning_list,New_reasoning_list,Concept,
Main_predicate,Current_config,Answer_type,Answer,Steps,Qid_per_ques_representation,Hint,Ques_config_list,1).

/*****************************find_a_unit_vector_parallel_to_the_vector***************/
eof1,find_a_unit_vector_parallel_to_the_vector1(Result)==> check_fact_exists(term1(Result,_))|

%write(2),nl,read(_),
generate_list(List1),
generate_list(List2),
generate_list(List3),
generate_list(List4),
generate_question_vector_Mis_7(List1,List2,List3,List4,Question),
remove_square_brackets_from_string(Question,Q1_main_part),
generate_answer_vector_Mis_7(List1,List2,List3,List4,Ans),
term_string(Ans,Temp_ans),
generate_solution_vector_Mis_7(List1,List2,List3,List4,Solution),
remove_square_brackets_from_string(Solution,Solution_steps),
term1(Result,Temp_ans),
New_element_formed = term(Result,Temp_ans),
Reasoning_list = [],
New_reasoning_list =[find_a_unit_vector_parallel_to_the_vector(Result)],
Current_config = New_reasoning_list,
Concept = unit_parallel_vector,
Main_predicate = New_element_formed,
string_concatenate(["answer_types([[string(latex(a\\\\hat{i} + b\\\\hat{j} + c\\\\hat{k}=)),type(algebra_form),placeholder(string(Enter the values of a,b,c in radical form and separated by comma)),unit()]])"],"",Answer_type_str),
Answer_type =  [Answer_type_str],
Answer = [3,[Ans]],
string_concatenate(["[string(Substitute vector latex(\\\\\\\\overrightarrow{a},\\\\\\\\overrightarrow{b},\\\\\\\\overrightarrow{c}) to the vector.)]"],"",Hint_str),
Hint = [Hint_str],
%string_concatenate([],"",Solution_steps),
Steps = Solution_steps,
Q1_side_part = "",
Added_solution_step1 = "",
Ques_config_list = [[[Q1_side_part],[Q1_main_part],Added_solution_step1,qt1]],
get_qid_per_ques_representation(Ques_config_list, Qid_per_ques_representation),
complete_rule_by_adding_properties(New_element_formed,Reasoning_list,New_reasoning_list,Concept,
Main_predicate,Current_config,Answer_type,Answer,Steps,Qid_per_ques_representation,Hint,Ques_config_list,1).

/*****************************find_a_vector_perpendicular_to_given_vectors***************/
eof1,find_a_vector_perpendicular_to_given_vectors1(Result)==> check_fact_exists(term1(Result,_))|

%write(2),nl,read(_),
generate_list(List1),
generate_list(List2),
generate_list(List3),
generate_magnitude(Mag),
generate_question_vector_Mis_12(List1,List2,List3,Mag,Question),
remove_square_brackets_from_string(Question,Q1_main_part),
generate_answer_vector_Mis_12(List1,List2,List3,Mag,Ans),
term_string(Ans,Temp_ans),
generate_solution_vector_Mis_12(List1,List2,List3,Mag,Solution),
remove_square_brackets_from_string(Solution,Solution_steps),
term1(Result,Temp_ans),
New_element_formed = term(Result,Temp_ans),
Reasoning_list = [],
New_reasoning_list =[find_a_vector_perpendicular_to_given_vectors(Result)],
Current_config = New_reasoning_list,
Concept = linear_equation_with_three_variables ,
Main_predicate = New_element_formed,
string_concatenate(["answer_types([[string(latex(a\\\\hat{i} + b\\\\hat{j} + c\\\\hat{k}=)),type(algebra_form),placeholder(string(Enter the values of a,b,c in radical form and separated by comma)),unit()]])"],"",Answer_type_str),
Answer_type =  [Answer_type_str],
Answer = [3,[Ans]],
string_concatenate(["[string(Let latex(\\\\\\\\overrightarrow{d} = d_{1}\\\\hat{i} + d_{2}\\\\hat{j} + d_{3}\\\\hat{k}))]"],"",Hint_str),
Hint = [Hint_str],
%string_concatenate([],"",Solution_steps),
Steps = Solution_steps,
Q1_side_part = "",
Added_solution_step1 = "",
Ques_config_list = [[[Q1_side_part],[Q1_main_part],Added_solution_step1,qt1]],
get_qid_per_ques_representation(Ques_config_list, Qid_per_ques_representation),
complete_rule_by_adding_properties(New_element_formed,Reasoning_list,New_reasoning_list,Concept,
Main_predicate,Current_config,Answer_type,Answer,Steps,Qid_per_ques_representation,Hint,Ques_config_list,1).

