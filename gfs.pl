%:- include('../utils').
:- include('vector_algebra/vector_main').
:- include('vector_algebra/vector_main2').

eof2 ==> write('exercise'),nl,handle_include_for_cqt([vector_algebra]).


:- chr_constraint
    find_unit_vector1/1,
    check_whether_given_two_vectors_are_equal_or_not1/1,
    find_vector_in_direction_of_vector_that_has_magnitude1/1,
    calculate_direction_cosines1/1,
    find_the_vector_joining_two_points1/1,
    find_position_vector_of_a_point_which_divide_line_segment1/1,
    find_given_points_are_vertices_of_a_right_angle_triangle_or_not1/1,
    compute_magnitude_of_vector1/1,
    find_scalar_and_vector_components1/1,
    find_sum_of_vector1/1,
    find_unit_vector_in_diection_of_the_sum_of_vectors1/1,
    check_whether_two_vectors_are_collinear_or_not1/1,
    check_whether_vectors_is_equally_inclined_to_the_axes_or_not1/1,
    find_position_vector_of_the_mid_point_of_the_vector1/1,
    find_angle_between_two_vectors_with_given_magnitude_and_product1/1,
    find_angle_between_the_vectors1/1,
    check_whether_vectors_are_perpendicular_or_not1/1,
    find_projection_of_the_vector_on_another_vector1/1.
    

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

find_position_vector_of_a_point_which_divide_line_segment1(Result)==>check_fact_exists( find_position_vector_of_a_point_which_divide_line_segment1(Result))|
assert(is_fact_exists( find_position_vector_of_a_point_which_divide_line_segment1(Result))).

find_given_points_are_vertices_of_a_right_angle_triangle_or_not1(Result)==>check_fact_exists( find_given_points_are_vertices_of_a_right_angle_triangle_or_not1(Result))|
assert(is_fact_exists( find_given_points_are_vertices_of_a_right_angle_triangle_or_not1(Result))).

compute_magnitude_of_vector1(Result)==>check_fact_exists( compute_magnitude_of_vector1(Result))|
assert(is_fact_exists( compute_magnitude_of_vector1(Result))).

find_scalar_and_vector_components1(Result)==>check_fact_exists( find_scalar_and_vector_components1(Result))|
assert(is_fact_exists( find_scalar_and_vector_components1(Result))).

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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/*****************************check_whether_given_two_vectors_are_equal_or_not1***************/
eof1,check_whether_given_two_vectors_are_equal_or_not1(Result)==> check_fact_exists(term1(Result,_))|

%write(0),nl,read(_),

generate_list_of_two(List1),

generate_list_of_two(List2),
generate_question_vector_ex_5(List1,List2,Question),
remove_square_brackets_from_string(Question,Q1_main_part),

%generate_answer_vector_ex_5(List1,List2,Ans),

generate_solution_vector_ex_5(List1,List2,Solution),
remove_square_brackets_from_string(Solution,Solution_steps),
term1(Result,1),
New_element_formed = term(Result,1),
Reasoning_list = [],
New_reasoning_list =[check_whether_given_two_vectors_are_equal_or_not(Result)],
Current_config = New_reasoning_list,
Concept = equal_vector, 
Main_predicate = New_element_formed,
string_concatenate(["answer_types([[string(Term is = ),type(s_textbox)]])"],"",Answer_type_str),
Answer_type =  [Answer_type_str],
Answer = [[1]],
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

%generate_answer_vector_ex_6(List1,Ans),
generate_solution_vector_ex_6(List1,Solution),
remove_square_brackets_from_string(Solution,Solution_steps),

term1(Result,1),
New_element_formed = term(Result,1),
Reasoning_list = [],
New_reasoning_list =[find_unit_vector(Result)],
Current_config = New_reasoning_list,
Concept = unit_vector, 
Main_predicate = New_element_formed,
string_concatenate(["answer_types([[string(Unit Vector is = ),type(s_textbox)]])"],"",Answer_type_str),
Answer_type =  [Answer_type_str],
Answer = [[1]],
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
%generate_answer_vector_ex_7(List1,Magnitude,Ans),
generate_solution_vector_ex_7(List1,Magnitude,Solution),
remove_square_brackets_from_string(Solution,Solution_steps),

term1(Result,1),

New_element_formed = term(Result,1),
Reasoning_list = [],

New_reasoning_list =[find_vector_in_direction_of_vector_that_has_magnitude(Result)],
Current_config = New_reasoning_list,
Concept = magitude_product,
Main_predicate = New_element_formed,
string_concatenate(["answer_types([[string(Vector is = ),type(s_textbox)]])"],"",Answer_type_str),
Answer_type =  [Answer_type_str],
Answer = [[1]],
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

%generate_answer_vector_ex_9(List1,Ans),

generate_solution_vector_ex_9(List1,Solution),
remove_square_brackets_from_string(Solution,Solution_steps),

term1(Result,1),

New_element_formed = term(Result,1),
Reasoning_list = [],

New_reasoning_list =[calculate_direction_cosines(Result)],
Current_config = New_reasoning_list,
Concept = direction_cosines,
Main_predicate = New_element_formed,
string_concatenate(["answer_types([[string(Direction Ratio = ),type(textbox)]])"],"",Answer_type_str),
Answer_type =  [Answer_type_str],
Answer = [[1]],
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

%generate_answer_vector_ex_10(List1,List2,Ans),

generate_solution_vector_ex_10(List1,List2,Solution),
remove_square_brackets_from_string(Solution,Solution_steps),

term1(Result,1),

New_element_formed = term(Result,1),
Reasoning_list = [],

New_reasoning_list =[find_the_vector_joining_two_points(Result)],
Current_config = New_reasoning_list,
Concept = vector_joining_the_points,
Main_predicate = New_element_formed,
string_concatenate(["answer_types([[string(Vector is = ),type(textbox)]])"],"",Answer_type_str),
Answer_type =  [Answer_type_str],
Answer = [[1]],
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

/*****************************find_position_vector_of_a_point_which_divide_line_segment***************/
eof1,find_position_vector_of_a_point_which_divide_line_segment1(Result)==> check_fact_exists(term1(Result,_))|

%write(0),nl,read(_),
generate_list(Point_list_1),
generate_list(Point_list_2),

generate_magnitude(Ratio1),
generate_magnitude(Ratio2),
generate_question_vector_ex_11(Point_list_1,Point_list_2,Ratio1,Ratio2,Question),
remove_square_brackets_from_string(Question,Q1_main_part),

%generate_answer_vector_ex_11(Point_list_1,Point_list_2,Ratio1,Ratio2,Ans),

generate_solution_vector_ex_11(Point_list_1,Point_list_2,Ratio1,Ratio2,Solution),
remove_square_brackets_from_string(Solution,Solution_steps),

term1(Result,1),

New_element_formed = term(Result,1),
Reasoning_list = [],

New_reasoning_list =[find_position_vector_of_a_point_which_divide_line_segment(Result)],
Current_config = New_reasoning_list,
Concept = section_formula,
Main_predicate = New_element_formed,
string_concatenate(["answer_types([[string(Vector is = ),type(textbox)]])"],"",Answer_type_str),
Answer_type =  [Answer_type_str],
Answer = [[1]],
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
generate_list(Point_list_1),
generate_list(Point_list_2),
generate_list(Point_list_3),

generate_question_vector_ex_12(Point_list_1,Point_list_2,Point_list_3,Question),
remove_square_brackets_from_string(Question,Q1_main_part),

%generate_answer_vector_ex_12(Point_list_1,Point_list_2,Point_list_3,Ans),

generate_solution_vector_ex_12(Point_list_1,Point_list_2,Point_list_3,Solution),
remove_square_brackets_from_string(Solution,Solution_steps),

term1(Result,1),

New_element_formed = term(Result,1),
Reasoning_list = [],

New_reasoning_list =[find_given_points_are_vertices_of_a_right_angle_triangle_or_not(Result)],
Current_config = New_reasoning_list,
Concept = pythagoras_theorem,
Main_predicate = New_element_formed,
string_concatenate(["answer_types([[string(Right angle trianle or not (Yes/No) = ),type(textbox)]])"],"",Answer_type_str),
Answer_type =  [Answer_type_str],
Answer = [[1]],
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

%generate_answer_vector_q_1(Vector_list,Ans),

generate_solution_vector_q_1(Vector_list,Solution),
remove_square_brackets_from_string(Solution,Solution_steps),

term1(Result,Ans),

New_element_formed = term(Result,Ans),
Reasoning_list = [],

New_reasoning_list =[compute_magnitude_of_vector(Result)],
Current_config = New_reasoning_list,
Concept = vector_magnitude,
Main_predicate = New_element_formed,
string_concatenate(["answer_types([[string(Magnitude = ),type(textbox)]])"],"",Answer_type_str),
Answer_type =  [Answer_type_str],
Answer = [[Ans]],
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

/*****************************find_scalar_and_vector_components***************/
eof1,find_scalar_and_vector_components1(Result)==> check_fact_exists(term1(Result,_))|

%write(2),nl,read(_),
generate_list(Point_list_1),
generate_list(Point_list_2),
generate_question_vector_q_5(Point_list_1,Point_list_2,Question),
remove_square_brackets_from_string(Question,Q1_main_part),

%generate_answer_vector_q_5(Point_list_1,Point_list_2,Ans),

generate_solution_vector_q_5(Point_list_1,Point_list_2,Solution),
remove_square_brackets_from_string(Solution,Solution_steps),

term1(Result,1),

New_element_formed = term(Result,1),
Reasoning_list = [],

New_reasoning_list =[find_scalar_and_vector_components(Result)],
Current_config = New_reasoning_list,
Concept = scalar_vector_components,
Main_predicate = New_element_formed,
string_concatenate(["answer_types([[string(),type(continuous(string(Scalar Componenet :),input(string(),type(textbox)) ))]]),answer_types([[string(),type(continuous(string(Vector Componenet :),input(string(),type(textbox)) ))]])"],"",Answer_type_str),
Answer_type =  [Answer_type_str],
Answer = [[1]],
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

/*****************************find_sum_of_vector***************/
eof1,find_sum_of_vector1(Result)==> check_fact_exists(term1(Result,_))|

%write(2),nl,read(_),
generate_list(List1),
generate_list(List2),
generate_list(List3),
generate_question_vector_q_6(List1,List2,List3,Question),
remove_square_brackets_from_string(Question,Q1_main_part),

%generate_answer_vector_q_6(List1,List2,List3,Ans),

generate_solution_vector_q_6(List1,List2,List3,Solution),
remove_square_brackets_from_string(Solution,Solution_steps),

term1(Result,1),

New_element_formed = term(Result,1),
Reasoning_list = [],

New_reasoning_list =[find_sum_of_vector(Result)],
Current_config = New_reasoning_list,
Concept = sum_of_vector,
Main_predicate = New_element_formed,
string_concatenate(["answer_types([[string(Sum  = ),type(textbox)]])"],"",Answer_type_str),
Answer_type =  [Answer_type_str],
Answer = [[1]],
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

%generate_answer_vector_q_9(List1,List2,Ans),

generate_solution_vector_q_9(List1,List2,Solution),
remove_square_brackets_from_string(Solution,Solution_steps),

term1(Result,1),

New_element_formed = term(Result,1),
Reasoning_list = [],

New_reasoning_list =[find_unit_vector_in_diection_of_the_sum_of_vectors(Result)],
Current_config = New_reasoning_list,
Concept = unit_vector_in_diection_of_the_sum_of_vectors,
Main_predicate = New_element_formed,
string_concatenate(["answer_types([[string(Unit Vector  = ),type(textbox)]])"],"",Answer_type_str),
Answer_type =  [Answer_type_str],
Answer = [[1]],
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
generate_list(List1),
generate_list(List2),
generate_question_vector_q_11(List1,List2,Question),
remove_square_brackets_from_string(Question,Q1_main_part),

%generate_answer_vector_q_11(List1,List2,Ans),

generate_solution_vector_q_11(List1,List2,Solution),
remove_square_brackets_from_string(Solution,Solution_steps),

term1(Result,1),

New_element_formed = term(Result,1),
Reasoning_list = [],

New_reasoning_list =[check_whether_two_vectors_are_collinear_or_not(Result)],
Current_config = New_reasoning_list,
Concept = collinearity,
Main_predicate = New_element_formed,
string_concatenate(["answer_types([[string(Collinear or not (Yes/No) : ),type(textbox)]])"],"",Answer_type_str),
Answer_type =  [Answer_type_str],
Answer = [[1]],
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
generate_list(List1),
generate_question_vector_q_14(List1,Question),
remove_square_brackets_from_string(Question,Q1_main_part),

%generate_answer_vector_q_14(List1,Ans),

generate_solution_vector_q_14(List1,Solution),
remove_square_brackets_from_string(Solution,Solution_steps),

term1(Result,1),

New_element_formed = term(Result,1),
Reasoning_list = [],

New_reasoning_list =[check_whether_vectors_is_equally_inclined_to_the_axes_or_not(Result)],
Current_config = New_reasoning_list,
Concept = equally_inclined_vector,
Main_predicate = New_element_formed,
string_concatenate(["answer_types([[string(Equally Inclined or not (Yes/No) : ),type(textbox)]])"],"",Answer_type_str),
Answer_type =  [Answer_type_str],
Answer = [[1]],
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

%generate_answer_vector_q_16(Point_list_1,Point_list_2,Ans),

generate_solution_vector_q_16(Point_list_1,Point_list_2,Solution),
remove_square_brackets_from_string(Solution,Solution_steps),

term1(Result,1),

New_element_formed = term(Result,1),
Reasoning_list = [],

New_reasoning_list =[find_position_vector_of_the_mid_point_of_the_vector(Result)],
Current_config = New_reasoning_list,
Concept = mid_point_vector,
Main_predicate = New_element_formed,
string_concatenate(["answer_types([[string(Position vector : ),type(textbox)]])"],"",Answer_type_str),
Answer_type =  [Answer_type_str],
Answer = [[1]],
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

%generate_answer_vector_ex_13(Mag1,Mag2,Mag3,Ans),

generate_solution_vector_ex_13(Mag1,Mag2,Mag3,Solution),
remove_square_brackets_from_string(Solution,Solution_steps),

term1(Result,1),

New_element_formed = term(Result,1),
Reasoning_list = [],

New_reasoning_list =[find_angle_between_two_vectors_with_given_magnitude_and_product(Result)],
Current_config = New_reasoning_list,
Concept = basic_trignometry,
Main_predicate = New_element_formed,
string_concatenate(["answer_types([[string(Angle : ),type(fraction)]])"],"",Answer_type_str),
Answer_type =  [Answer_type_str],
Answer = [[1]],
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

%generate_answer_vector_ex_14(List1,List2,Ans),

generate_solution_vector_ex_14(List1,List2,Solution),
remove_square_brackets_from_string(Solution,Solution_steps),

term1(Result,1),

New_element_formed = term(Result,1),
Reasoning_list = [],

New_reasoning_list =[find_angle_between_the_vectors(Result)],
Current_config = New_reasoning_list,
Concept = trignometry_and_scalar_product,
Main_predicate = New_element_formed,
string_concatenate(["answer_types([[string(Angle : ),type(fraction)]])"],"",Answer_type_str),
Answer_type =  [Answer_type_str],
Answer = [[1]],
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
generate_list(List1),
generate_list(List2),
generate_question_vector_ex_15(List1,List2,Question),
remove_square_brackets_from_string(Question,Q1_main_part),
%generate_answer_vector_ex_15(List1,List2,Ans),

generate_solution_vector_ex_15(List1,List2,Solution),
remove_square_brackets_from_string(Solution,Solution_steps),
term1(Result,1),

New_element_formed = term(Result,1),
Reasoning_list = [],

New_reasoning_list =[check_whether_vectors_are_perpendicular_or_not(Result)],
Current_config = New_reasoning_list,
Concept = perpendicular_vectors,
Main_predicate = New_element_formed,
string_concatenate(["answer_types([[string(True/False : ),type(textbox)]])"],"",Answer_type_str),
Answer_type =  [Answer_type_str],
Answer = [[1]],
string_concatenate(["[string(If latex(\\\\\\\\overrightarrow{a}.\\\\\\\\overrightarrow{b} = 0), then latex(\\\\\\\\overrightarrow{a}) and latex(\\\\\\\\overrightarrow{b}) are perpendicular vectors. )]"],"",Hint_str),
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
%generate_answer_vector_ex_16(List1,List2,Ans),

generate_solution_vector_ex_16(List1,List2,Solution),
remove_square_brackets_from_string(Solution,Solution_steps),
term1(Result,1),

New_element_formed = term(Result,1),
Reasoning_list = [],

New_reasoning_list =[find_projection_of_the_vector_on_another_vector(Result)],
Current_config = New_reasoning_list,
Concept = projection_of_vetor,
Main_predicate = New_element_formed,
string_concatenate(["answer_types([[string(Projection of vector : ),type(fraction)]])"],"",Answer_type_str),
Answer_type =  [Answer_type_str],
Answer = [[1]],
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

