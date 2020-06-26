%:- include('../utils').
:- include('vector_algebra/vector_main').
eof2 ==> write('exercise'),nl,handle_include_for_cqt([vector_algebra]).


:- chr_constraint
    find_unit_vector1/1,
    check_whether_given_two_vectors_are_equal_or_not1/1,
    find_vector_in_direction_of_vector_that_has_magnitude1/1,
    calculate_direction_cosines1/1,
    find_the_vector_joining_two_points1/1.
    

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
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/*****************************check_whether_given_two_vectors_are_equal_or_not1***************/
eof1,check_whether_given_two_vectors_are_equal_or_not1(Result)==> check_fact_exists(term1(Result,_))|

%write(0),nl,read(_),
Var1=a,
generate_list_of_two(List1),
Var2=b,
generate_list_of_two(List2),
generate_question_vector_ex_5(Var1,Var2,List1,List2,Q1_main_part),

%generate_answer_vector_ex_5(Var1,Var2,List1,List2,Ans),

generate_solution_vector_ex_5(Var1,Var2,List1,List2,Solution_steps),
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
generate_question_vector_ex_6(Var1,List1,Q1_main_part),
%generate_answer_vector_ex_6(Var1,List1,Ans),
generate_solution_vector_ex_6(Var1,List1,Solution_steps),
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
generate_question_vector_ex_7(Var1,List1,Magnitude,Q1_main_part),

%generate_answer_vector_ex_7(List1,Magnitude,Ans),

generate_solution_vector_ex_7(Var1,List1,Magnitude,Solution_steps),
term1(Result,1),

New_element_formed = term(Result,1),
Reasoning_list = [],

New_reasoning_list =[find_vector_in_direction_of_vector_that_has_magnitude(Result)],
Current_config = New_reasoning_list,
Concept = magitude_product,
Main_predicate = New_element_formed,
string_concatenate(["answer_types([[string(Vector is = ),type(fraction),(latex(\\\\\\\\hat{i}),type(fraction),(latex(\\\\\\\\hat{j}),type(fraction),(latex(\\\\\\\\hat{k}))]])"],"",Answer_type_str),
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
generate_question_vector_ex_9(Var1,List1,Q1_main_part),

%generate_answer_vector_ex_9(List1,Ans),

generate_solution_vector_ex_9(List1,Solution_steps),
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
generate_question_vector_ex_10(List1,List2,Q1_main_part),

%generate_answer_vector_ex_10(List1,List2,Ans),

generate_solution_vector_ex_10(List1,List2,Solution_steps),
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

