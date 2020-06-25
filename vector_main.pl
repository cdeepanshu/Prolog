:- include('radicals/helper.pl').
:- include('vector_algebra/helper.pl').
:- include('application_of_integrals/gfs.pl').
:- include('trigno_with_val/helper.pl').
:- include('binomial_multinomials/helper_general.pl').

%--------------------------------------------------------------------Exercise 10.2-------------------------------------------------------------------------

%----------------------------------------------------------------------Example 4---------------------------------------------------------------------------------


%Question:
generate_question_vector_ex_4(Question):-
	Var1=a,
	Var2=b,
	generate_list(List1),
	generate_list(List2),
    generate_latex_vector_ijk(List1, [i,j,k],"",Latex_str1),
    generate_latex_vector_ijk(List2, [i,j,k],"",Latex_str2),

    generate_latex_vector_name(Var1,Latex_str_name1),
    generate_latex_vector_name(Var2,Latex_str_name2),

    string_concatenate(["[string(Find the values of x,y,z so that the vectors ",Latex_str_name1," = ",Latex_str1," and ",Latex_str_name2," = ",Latex_str2," are equal. )]"],"",Question).

%Answer
generate_answer_vector_ex_4(List1,List2,_):-
	write("[string("),
	check_vector(List1,List2).
	    %NOTe:  Add )] after the result in the parser to generate the solution.
 
%Solution


generate_solution(Var1,Var2):-
	generate_latex_vector_name(Var1,Latex_str_name1),
    generate_latex_vector_name(Var2,Latex_str_name2),

	string_concatenate(["[string(Note that two vectors are equal if and only if their corresponding components are equal.)" ],"",Sol_0),
	string_concatenate([",string(Thus, the given vectors",Latex_str_name1," and ",Latex_str_name2,"will be equal if and only if),string("],"",Sol_1),
	string_concatenate([Sol_0,Sol_1],"",Sol),
	write(Sol).

generate_solution_vector_ex_4(Var1,Var2,List1,List2,_):-

	generate_solution(Var1,Var2),
	check_vector(List1,List2).
    %NOTe:  Add )] after the result in the parser to generate the solution.



%---------------------------------------------------------------End of Example 4-----------------------------------------------------------------------------------------


%------------------------------------------------------------------Example 5---------------------------------------------------------------------------------
%Question

generate_question_vector_ex_5(Var1,Var2,List1,List2,Question):-
	%var(List1),var(List2),
	%random(0,1,R),

	%(R=:=0->
	%	generate_list_for_equal_magnitude(List1,List2);
	%	generate_list_for_non_equal_magnitude(List1,List2)
	%),

    generate_latex_vector_ijk(List1, [i,j],"",Latex_str1),
    generate_latex_vector_ijk(List2, [i,j],"",Latex_str2),
    generate_latex_vector_name(Var1,Latex_str_name1),
    generate_latex_vector_name(Var2,Latex_str_name2),
    string_concatenate([" ",Latex_str_name1," = ",Latex_str1," and ",Latex_str_name2," = ",Latex_str2," Is ",Latex_str_name1," = ",Latex_str_name2," ?. Are the vectors ",Latex_str_name1," and ",Latex_str_name2," equal ?."],"",Question).


%Answer
generate_answer_vector_ex_5(Var1,Var2,List1,List2,Answer):-
 	generate_latex_vector_name(Var1,Latex_str_name1),
    generate_latex_vector_name(Var2,Latex_str_name2),

    generate_magnitude(List1,0,List1_magnitude),
    generate_magnitude(List2,0,List2_magnitude),

    (List1_magnitude=:=List2_magnitude->
		(string_concatenate(["[string(|",Latex_str_name1,"| = |",Latex_str_name2,"| )"],"",Ans_0));

		(string_concatenate(["[string(|",Latex_str_name1,"| latex(\\\\\\\\neq) |",Latex_str_name2,"|.)"],"",Ans_0))

	),

	compare_vectors(List1,List2,Comp_result),


	(Comp_result=="Equal"->
		string_concatenate([",string(The vectors",Latex_str_name1," and ",Latex_str_name2,"are equal )]" ],"",Ans_1),
		string_concatenate([Ans_0,Ans_1],"",Answer)

		;

		string_concatenate([",string(The vectors",Latex_str_name1," and ",Latex_str_name2,"are not equal )]" ],"",Ans_1),
		string_concatenate([Ans_0,Ans_1],"",Answer)
	).
%Solution
generate_solution_vector_ex_5(Var1,Var2,List1,List2,Solution):-

    generate_latex_vector_name(Var1,Latex_str_name1),
    generate_latex_vector_name(Var2,Latex_str_name2),

    generate_magnitude(List1,0,List1_magnitude),
    generate_magnitude(List2,0,List2_magnitude),

    generate_latex_magnitude_expression_ijk(List1,"",List1_mag_exp),
    generate_latex_magnitude_expression_ijk(List2,"",List2_mag_exp),


    compare_vectors(List1,List2,Comp_result),

    (List1_magnitude=:=List2_magnitude->

	   	(string_concatenate(["string(We have |",Latex_str_name1,"| = ", List1_mag_exp,"= latex(\\\\\\\\sqrt{",List1_magnitude,"}) and |",Latex_str_name2,"| = ",List2_mag_exp,"= latex(\\\\\\\\sqrt{",List2_magnitude,"}) )" ],"",Sol_0),
		string_concatenate([",string(So, |",Latex_str_name1,"| = |",Latex_str_name2,"|)" ],"",Sol_1),
		string_concatenate([Sol_0,Sol_1],"",Sol_2))

		;

	   	(string_concatenate(["string(We have |",Latex_str_name1,"| = ", List1_mag_exp,"= latex(\\\\\\\\sqrt{",List1_magnitude,"}) and |",Latex_str_name2,"| = ",List2_mag_exp,"= latex(\\\\\\\\sqrt{",List2_magnitude,"}) )" ],"",Sol_0),
		string_concatenate([",string(So, |",Latex_str_name1,"| latex(\\\\\\\\neq) |",Latex_str_name2,"|)" ],"",Sol_1),
		string_concatenate([Sol_0,Sol_1],"",Sol_2))
	),
	(Comp_result=="Equal"->
		string_concatenate([",string(The Vectors are equal since there corresponding components are same. )" ],"",Sol_3),
		string_concatenate([Sol_2,Sol_3],"",Solution)

		;

		string_concatenate([",string(The Vectors are not equal since there corresponding components are distinct. )" ],"",Sol_3),
		string_concatenate([Sol_2,Sol_3],"",Solution)
	).
	
%---------------------------------------------------------------End of Example 5-----------------------------------------------------------------------------------------


%------------------------------------------------------------------Example 6---------------------------------------------------------------------------------

%Question:

generate_question_vector_ex_6(Var1,List1,Question):-
	%Var1=a,
	%generate_list(List1),
    generate_latex_vector_ijk(List1, [i,j,k],"",Latex_str1),

    generate_latex_vector_name(Var1,Latex_str_name1),

    string_concatenate(["Find the unit vector in the direction of vector ",Latex_str_name1," = ",Latex_str1," "],"",Question).

%Answer
generate_answer_vector_ex_6(Var1,List1,Answer):-
	generate_magnitude(List1,0,List1_magnitude),
	generate_latex_fraction_vector_ijk(List1, [i,j,k],List1_magnitude,"",Latex_frac_str1),

    string_concatenate(["[string(latex(\\\\hat{",Var1,"}) = ",Latex_frac_str1,")]"],"",Answer).

%Solution
generate_solution_vector_ex_6(Var1,List1,Solution):-

    generate_latex_vector_name(Var1,Latex_str_name1),

    generate_latex_vector_ijk(List1, [i,j,k],"",Latex_str1),

    generate_magnitude(List1,0,List1_magnitude),

	generate_latex_fraction_vector_ijk(List1, [i,j,k],List1_magnitude,"",Latex_frac_str1),


    generate_latex_magnitude_expression_ijk(List1,"",List1_mag_exp),

    string_concatenate(["string(The unit vector in the direction of vector ",Latex_str_name1," is given by latex(\\\\hat{",Var1,"}) = latex(\\\\frac{1}{\\\\overrightarrow{",Var1,"}})",Latex_str_name1,".)"],"",Sol_0),

    string_concatenate([",string(Now latex(\\\\overrightarrow{",Var1,"}) = ",List1_mag_exp," = latex(\\\\sqrt{",List1_magnitude,"}))"],"",Sol_1),

    string_concatenate([",string(Therefore latex(\\\\hat{",Var1,"}) = latex(\\\\frac{1}{\\\\sqrt{",List1_magnitude,"}})[",Latex_str1,"] = ",Latex_frac_str1,")"],"",Sol_2),

    string_concatenate([Sol_0,Sol_1,Sol_2],"",Solution).

%---------------------------------------------------------------End of Example 6-----------------------------------------------------------------------------------------

%------------------------------------------------------------------Example 7---------------------------------------------------------------------------------

%Question:

generate_question_vector_ex_7(Question):-
	Var1=a,
	generate_list(List1),
	generate_magnitude(Magnitude),

    generate_latex_vector_ijk(List1, [i,j,k],"",Latex_str1),

    generate_latex_vector_name(Var1,Latex_str_name1),

    string_concatenate(["[string(Find the vector in the direction of vector ",Latex_str_name1," = ",Latex_str1," that has magnitude ",Magnitude, " units.)]"],"",Question).


%Answer
generate_answer_vector_ex_7(List1,Magnitude,Answer):-
	generate_magnitude(List1,0,List1_magnitude),
	generate_latex_mag_fraction_vector_ijk(List1, [i,j,k],List1_magnitude,Magnitude,"",Latex_mag_frac_str1),

   string_concatenate(["[string(",Latex_mag_frac_str1,")]"],"",Answer).


%Solution
generate_solution_vector_ex_7(Var1,List1,Magnitude, Solution):-
	generate_latex_vector_name(Var1,Latex_str_name1),

    generate_latex_vector_ijk(List1, [i,j,k],"",Latex_str1),

    generate_magnitude(List1,0,List1_magnitude),

	generate_latex_fraction_vector_ijk(List1, [i,j,k],List1_magnitude,"",Latex_frac_str1),

	generate_latex_mag_fraction_vector_ijk(List1, [i,j,k],List1_magnitude,Magnitude,"",Latex_mag_frac_str1),


	string_concatenate(["[string(The unit vector int he direction of given vector ",Latex_str_name1," is)"],"",Sol_0),

	string_concatenate([",string(latex(\\\\hat{",Var1,"}) = latex(\\\\frac{1}{|\\\\overrightarrow{",Var1,"}|})",Latex_str_name1," = latex(\\\\frac{1}{\\\\sqrt{",List1_magnitude,"}})[",Latex_str1,"] = ",Latex_frac_str1,")"],"",Sol_1),

	string_concatenate([",string(Therefore, the vector having magnitude equal to ",Magnitude,"and in the direction of ",Latex_str_name1," is)"],"",Sol_2),

	string_concatenate([",string(",Magnitude,"latex(\\\\hat{",Var1,"}) = ",Magnitude,"[",Latex_frac_str1,"] = ",Latex_mag_frac_str1,"))]"],"",Sol_3),


    string_concatenate([Sol_0,Sol_1,Sol_2,Sol_3],"",Solution).


%---------------------------------------------------------------End of Example 7-----------------------------------------------------------------------------------------


%------------------------------------------------------------------Example 8---------------------------------------------------------------------------------

%Question:

generate_question_vector_ex_8(Question):-

	Var1=a,
	Var2=b,
	generate_list(List1),
	generate_list(List2),

    generate_latex_vector_ijk(List1, [i,j,k],"",Latex_str1),
    generate_latex_vector_ijk(List2, [i,j,k],"",Latex_str2),

    generate_latex_vector_name(Var1,Latex_str_name1),
    generate_latex_vector_name(Var2,Latex_str_name2),

    string_concatenate(["[string(Find the unit vector in the direction of the sum of the vectors, ",Latex_str_name1," = ",Latex_str1," and ",Latex_str_name2," = ",Latex_str2,".)]"],"",Question).


%Answer
generate_answer_vector_ex_8(_,_,List1,List2,Answer):-
	generate_sum_vector(List1,List2,Sum),

	generate_magnitude(Sum,0,Sum_magnitude),

	generate_latex_fraction_vector_ijk(Sum, [i,j,k],Sum_magnitude,"",Latex_frac_sum),


   string_concatenate(["[string(",Latex_frac_sum,")]"],"",Answer).

%Solution
generate_solution_vector_ex_8(Var1,Var2,List1,List2, Solution):-
	generate_latex_vector_name(Var1,Latex_str_name1),
    generate_latex_vector_name(Var2,Latex_str_name2),
    generate_latex_vector_name(c,Latex_str_var),

	generate_sum_vector(List1,List2,Sum),

    generate_latex_vector_ijk(Sum, [i,j,k],"",Latex_sum_var),

    generate_latex_magnitude_expression_ijk(Sum,"",Sum_mag_exp),
	
	generate_magnitude(Sum,0,Sum_magnitude),

	generate_latex_fraction_vector_ijk(Sum, [i,j,k],Sum_magnitude,"",Latex_frac_sum),

	string_concatenate(["[string(The Sum of the given vector is )"],"",Sol_0),
	string_concatenate([",string(",Latex_str_name1,"+",Latex_str_name2,"[=",Latex_str_var,",say]=",Latex_sum_var," )"],"",Sol_1),
	string_concatenate([",string(and  |",Latex_str_var,"| = ",Sum_mag_exp," = latex(\\\\sqrt{",Sum_magnitude,"}) )"],"",Sol_2),
	string_concatenate([",string(Thus the required unit vector is )"],"",Sol_3),
	string_concatenate([",string(latex(\\\\hat{",c,"}) = latex(\\\\frac{1}{|\\\\overrightarrow{",c,"}|})",Latex_str_var," = latex(\\\\frac{1}{\\\\sqrt{",Sum_magnitude,"}})[",Latex_sum_var,"] = ",Latex_frac_sum,")]"],"",Sol_4),

	string_concatenate([Sol_0,Sol_1,Sol_2,Sol_3,Sol_4],"",Solution).

%---------------------------------------------------------------End of Example 8-----------------------------------------------------------------------------------------

%------------------------------------------------------------------Example 9---------------------------------------------------------------------------------


%Question
generate_question_vector_ex_9(Question):-

	Var1=a,
	generate_list(List1),

    generate_latex_vector_ijk(List1, [i,j,k],"",Latex_str1),

    generate_latex_vector_name(Var1,Latex_str_name1),

    string_concatenate(["[string(Write the direction ratios of the vector ",Latex_str_name1," = ",Latex_str1," and hence calulate it's direction cosines.)]"],"",Question).


%Answer
generate_answer_vector_ex_9(List1,Answer):-
	generate_magnitude(List1,0,List1_magnitude),

	generate_direction_cosines(List1,List1_magnitude,"",Latex_direction_cosine),
   
   string_concatenate(["[string(Direction Ratios are: ",List1,")"],"",Ans_0),
   string_concatenate([",string(Direction Cosines are: ",Latex_direction_cosine,")]"],"",Ans_1),
   string_concatenate([Ans_0,Ans_1],"",Answer).


%generate_solution_vector_ex_9
generate_solution_vector_ex_9(List1,Solution):-
	generate_latex_var_vector_ijk([x,y,z], [i,j,k],"",Latex_vec),
    generate_latex_vector_name(r,Latex_str_vec),
    generate_magnitude(List1,0,List1_magnitude),
	generate_direction_cosines(List1,List1_magnitude,"",Latex_direction_cosine),

    string_concatenate(["[string(Note that he direction ratios a,b,c of a vector ",Latex_str_vec," = ",Latex_vec," are just the respective components x, y and z of the vector)"],"",Sol_0),
    string_concatenate([",string(So, for the given vector, we have [a,b,c] : ",List1,". Further if l, m and n are the direction cosines of the given vector, then)"],"",Sol_1),
    string_concatenate([",string(l = latex(\\\\frac{a}{|\\\\overrightarrow{",r,"}|}), m = latex(\\\\frac{b}{|\\\\overrightarrow{",r,"}|}), n = latex(\\\\frac{c}{|\\\\overrightarrow{",r,"}|}) and |",Latex_str_vec,"| = latex(\\\\sqrt{",List1_magnitude,"}))"],"",Sol_2),
    string_concatenate([",string(Thus, the direction cosines are [l,m,n] : [",Latex_direction_cosine,"])]"],"",Sol_3),

    string_concatenate([Sol_0,Sol_1,Sol_2,Sol_3],"",Solution).

%---------------------------------------------------------------End of Example 9-----------------------------------------------------------------------------------------


%------------------------------------------------------------------Example 10---------------------------------------------------------------------------------

%Question
generate_question_vector_ex_10(Question):-
	
	generate_list(Point1),
	generate_list(Point2),

    string_concatenate(["[string(Find the vector joining the points P(",Point1,") and Q(",Point2,") directed from P to Q.)]"],"",Question).


%Answer
generate_answer_vector_ex_10(Point1,Point2,Answer):-
	generate_diff_vector(Point2,Point1,Diff),
	generate_latex_vector_ijk(Diff, [i,j,k],"",Latex_Diff),

	string_concatenate(["[string(latex(\\\\overrightarrow{PQ}) = ",Latex_Diff,")]"],"",Answer).

%Solution
generate_solution_vector_ex_10(Point1,Point2,Solution):-
	generate_latex_diff_exp_ijk(Point2,Point1,[i,j,k],"",Latex_diff_exp),
	generate_diff_vector(Point2,Point1,Diff),
	generate_latex_vector_ijk(Diff, [i,j,k],"",Latex_Diff),

	string_concatenate(["[string(Since the vector is to be directed from P to Q, clearly P is the initial point and Q is the terminal point.)"],"",Sol_0),
	string_concatenate([",string(So, the required vector joining P and Q is the vector latex(\\\\overrightarrow{PQ}), given by)"],"",Sol_1),
    string_concatenate([",string(latex(\\\\overrightarrow{PQ}) = ",Latex_diff_exp,")"],"",Sol_2),
	string_concatenate([",string(latex(\\\\overrightarrow{PQ}) = ",Latex_Diff,")]"],"",Sol_3),

    string_concatenate([Sol_0,Sol_1,Sol_2,Sol_3],"",Solution).

%---------------------------------------------------------------End of Example 10-----------------------------------------------------------------------------------------


%------------------------------------------------------------------Example 11---------------------------------------------------------------------------------

%Question
generate_question_vector_ex_11(Question):-
	
	generate_list(Point1),
	generate_list(Point2),

	generate_magnitude(Ratio1),
	generate_magnitude(Ratio2),

	generate_latex_vector_ijk(Point1, [i,j,k],"",Latex_str1),
    generate_latex_vector_ijk(Point2, [i,j,k],"",Latex_str2),

	string_concatenate(["[string(Find the position vector of a point R which divides the lines joining two points P ans Q whose position vectrs are )"],"",Sol_0),
	string_concatenate([",string(",Latex_str1," and ",Latex_str2," respectively in the ratio ",Ratio1," : ",Ratio2,")"],"",Sol_1),
	string_concatenate([",string((i). internally)"],"",Sol_2),
	string_concatenate([",string((ii). externally)]"],"",Sol_3),

    string_concatenate([Sol_0,Sol_1,Sol_2,Sol_3],"",Question).


%Answer
generate_answer_vector_ex_11(Point1,Point2,Ratio1,Ratio2,Answer):-
	divide_internally(Point2,Point1,Ratio1,Ratio2,Div_internally),
	divide_externally(Point2,Point1,Ratio1,Ratio2,Div_externally),

	Internally is Ratio1+Ratio2,
	Externally is Ratio1-Ratio2,

	generate_latex_fraction_vector(Div_internally,[i,j,k],Internally,"",Latex_ratio_1),
	generate_latex_fraction_vector(Div_externally,[i,j,k],Externally,"",Latex_ratio_2),

	string_concatenate(["[string(Internally: ",Latex_ratio_1,")"],"",Ans_0),
	string_concatenate([",string(Externally: ",Latex_ratio_2,")]"],"",Ans_1),
    string_concatenate([Ans_0,Ans_1],"",Answer).


%Solution
generate_solution_vector_ex_11(Point1,Point2,Ratio1,Ratio2,Solution):-
	
	generate_vector_ijk(Point1, [i,j,k],"",Latex_str1),
    generate_vector_ijk(Point2, [i,j,k],"",Latex_str2),
	divide_internally(Point2,Point1,Ratio1,Ratio2,Div_internally),
	divide_externally(Point2,Point1,Ratio1,Ratio2,Div_externally),

	Internally is Ratio1+Ratio2,
	Externally is Ratio1-Ratio2,

	generate_latex_fraction_vector(Div_internally,[i,j,k],Internally,"",Latex_ratio_1),
	generate_latex_fraction_vector(Div_externally,[i,j,k],Externally,"",Latex_ratio_2),

	string_concatenate(["[string((i) The position vector of the point R dividing the join of P and Q intenally in the ratio",Ratio1," : ",Ratio2," is )"],"",Sol_0),
	string_concatenate([",string(latex(\\\\overrightarrow{OR}) = latex(\\\\frac{",Ratio1,"(",Latex_str2,") + ",Ratio2,"(",Latex_str1,")}{",Ratio1," + ",Ratio2,"}) = ",Latex_ratio_1,")]"],"",Sol_1),
	string_concatenate([",string((ii) The position vector of the point R dividing the join of P and Q extenally in the ratio",Ratio1," : ",Ratio2," is )"],"",Sol_2),
	string_concatenate([",string(latex(\\\\overrightarrow{OR}) = latex(\\\\frac{",Ratio1,"(",Latex_str2,") - ",Ratio2,"(",Latex_str1,")}{",Ratio1," - ",Ratio2,"}) = ",Latex_ratio_2,")]"],"",Sol_3),
    string_concatenate([Sol_0,Sol_1,Sol_2,Sol_3],"",Solution).

%---------------------------------------------------------------End of Example 11-----------------------------------------------------------------------------------------


%------------------------------------------------------------------Example 12---------------------------------------------------------------------------------

%Question
generate_question_vector_ex_12(Question):-
	
	generate_list(Point1),
	generate_list(Point2),
	generate_list(Point3),

    string_concatenate(["[string(Show that the points A(",Point1,"), B(",Point2,"), C(",Point3,") are vertices of right angle triangle.)]"],"",Question).


%Answer
generate_answer_vector_ex_12(Point1,Point2,Point3,Answer):-
	generate_diff_vector(Point2,Point1,Diff_1),
	generate_diff_vector(Point3,Point2,Diff_2),
	generate_diff_vector(Point1,Point3,Diff_3),

    generate_magnitude(Diff_1,0,Diff_1_magnitude),
    generate_magnitude(Diff_2,0,Diff_2_magnitude),
    generate_magnitude(Diff_3,0,Diff_3_magnitude),

    (Diff_3_magnitude=:=Diff_1_magnitude+Diff_2_magnitude->
    	    string_concatenate(["[string(The triangle formed is a right angled triangle)]"],"",Answer);
    	    (Diff_2_magnitude=:=Diff_3_magnitude+Diff_1_magnitude->
    	    	string_concatenate(["[string(The triangle formed is a right angled triangle)]"],"",Answer);
     			(Diff_1_magnitude=:=Diff_3_magnitude+Diff_2_magnitude->
     				string_concatenate(["[string(The triangle formed is a right angled triangle)]"],"",Answer);
					string_concatenate(["[string(The triangle formed is a not right angled triangle.)]"],"",Answer)
				)
			)
    ).

%Solution
generate_solution_vector_ex_12(Point1,Point2,Point3,Solution):-
	generate_latex_diff_exp_ijk(Point2,Point1,[i,j,k],"",Latex_diff_exp_1),
	generate_latex_diff_exp_ijk(Point3,Point2,[i,j,k],"",Latex_diff_exp_2),
	generate_latex_diff_exp_ijk(Point1,Point3,[i,j,k],"",Latex_diff_exp_3),

	generate_diff_vector(Point2,Point1,Diff_1),
	generate_diff_vector(Point3,Point2,Diff_2),
	generate_diff_vector(Point1,Point3,Diff_3),

    generate_magnitude(Diff_1,0,Diff_1_magnitude),
    generate_magnitude(Diff_2,0,Diff_2_magnitude),
    generate_magnitude(Diff_3,0,Diff_3_magnitude),

    generate_latex_vector_ijk(Diff_1, [i,j,k],"",Latex_Diff_1),
	generate_latex_vector_ijk(Diff_2, [i,j,k],"",Latex_Diff_2),
	generate_latex_vector_ijk(Diff_3, [i,j,k],"",Latex_Diff_3),


    string_concatenate(["[string(We have,)"],"",Sol_0),
	string_concatenate([",string(latex(\\\\overrightarrow{AB}) = ",Latex_diff_exp_1," = ",Latex_Diff_1,")"],"",Sol_1),
	string_concatenate([",string(latex(\\\\overrightarrow{BC}) = ",Latex_diff_exp_2," = ",Latex_Diff_2,")"],"",Sol_2),
	string_concatenate([",string(latex(\\\\overrightarrow{CA}) = ",Latex_diff_exp_3," = ",Latex_Diff_3,")"],"",Sol_3),
    string_concatenate([",string(Further note that,)"],"",Sol_4),

    (Diff_1_magnitude=:=Diff_3_magnitude+Diff_2_magnitude->
		string_concatenate([",string(latex(|\\\\overrightarrow{AB}|^2) = ",Diff_1_magnitude," latex(=) ",Diff_2_magnitude," + ",Diff_3_magnitude," = latex(|\\\\overrightarrow{BC}|^2) + latex(|\\\\overrightarrow{CA}|^2))"],"",Sol_5);
		string_concatenate([",string(latex(|\\\\overrightarrow{AB}|^2) = ",Diff_1_magnitude," latex(\\neq) ",Diff_2_magnitude," + ",Diff_3_magnitude," = latex(|\\\\overrightarrow{BC}|^2) + latex(|\\\\overrightarrow{CA}|^2))"],"",Sol_5)

	),

	(Diff_2_magnitude=:=Diff_1_magnitude+Diff_3_magnitude->
		string_concatenate([",string(latex(|\\\\overrightarrow{BC}|^2) = ",Diff_2_magnitude," latex(=) ",Diff_1_magnitude," + ",Diff_3_magnitude," = latex(|\\\\overrightarrow{AB}|^2) + latex(|\\\\overrightarrow{CA}|^2))"],"",Sol_6);
		string_concatenate([",string(latex(|\\\\overrightarrow{BC}|^2) = ",Diff_2_magnitude," latex(\\neq) ",Diff_1_magnitude," + ",Diff_3_magnitude," = latex(|\\\\overrightarrow{AB}|^2) + latex(|\\\\overrightarrow{CA}|^2))"],"",Sol_6)

	),

	(Diff_3_magnitude=:=Diff_1_magnitude+Diff_2_magnitude->
		string_concatenate([",string(latex(|\\\\overrightarrow{CA}|^2) = ",Diff_3_magnitude," latex(=) ",Diff_1_magnitude," + ",Diff_2_magnitude," = latex(|\\\\overrightarrow{AB}|^2) + latex(|\\\\overrightarrow{BC}|^2))"],"",Sol_7);
		string_concatenate([",string(latex(|\\\\overrightarrow{CA}|^2) = ",Diff_3_magnitude," latex(\\neq) ",Diff_1_magnitude," + ",Diff_2_magnitude," = latex(|\\\\overrightarrow{AB}|^2) + latex(|\\\\overrightarrow{BC}|^2))"],"",Sol_7)

	),

  	 (Diff_3_magnitude=:=Diff_1_magnitude+Diff_2_magnitude->
    	    string_concatenate([",string(Hence,The triangle formed is a right angled triangle)]"],"",Sol_8);
    	    (Diff_2_magnitude=:=Diff_3_magnitude+Diff_1_magnitude->
    	    	string_concatenate([",string(Hence,The triangle formed is a right angled triangle)]"],"",Sol_8);
     			(Diff_1_magnitude=:=Diff_3_magnitude+Diff_2_magnitude->
     				string_concatenate([",string(Hence,The triangle formed is a right angled triangle)]"],"",Sol_8);
					string_concatenate([",string(Hence,The triangle formed is a not right angled triangle.)]"],"",Sol_8)
				)
			)
    ),
	string_concatenate([Sol_0,Sol_1,Sol_2,Sol_3,Sol_4,Sol_5,Sol_6,Sol_7,Sol_8],"",Solution).

%---------------------------------------------------------------End of Example 12-----------------------------------------------------------------------------------------


%------------------------------------------------------------------Question 1---------------------------------------------------------------------------------

%Question
generate_question_vector_q_1(Question):-
	
	generate_list(Vector),
	Var1=a,
	generate_latex_vector_ijk(Vector, [i,j,k],"",Latex_str1),
    generate_latex_vector_name(Var1,Latex_str_name1),

    string_concatenate(["[string(Compute the magnitude of the following vector:)"],"",Q_0),
    string_concatenate([",string(",Latex_str_name1," = ",Latex_str1,")]"],"",Q_1),

	string_concatenate([Q_0,Q_1],"",Question).

%Answer
generate_answer_vector_q_1(Vector,Answer):-
	generate_magnitude(Vector,0,Mag),

	(Mag=:=1->
	string_concatenate(["[string(latex(|\\\\overrightarrow{a}|) = ",Mag,")]"],"",Answer);

	string_concatenate(["[string(latex(|\\\\overrightarrow{a}|) = latex(\\\\sqrt{",Mag,"}))]"],"",Answer)
	).

%Solution
generate_solution_vector_q_1(Vector,Solution):-
	generate_magnitude(Vector,0,Mag),
    generate_latex_magnitude_expression_ijk(Vector,"",List1_mag_exp),

	string_concatenate(["[string(latex(|\\\\overrightarrow{a}|) = ",List1_mag_exp," = latex(\\\\sqrt{",Mag,"}))]"],"",Solution).


%------------------------------------------------------------------End of Question 1---------------------------------------------------------------------------------


%------------------------------------------------------------------Question 4---------------------------------------------------------------------------------

%Question:
generate_question_vector_q_4(Question):-
	Var1=a,
	Var2=b,
	generate_list(List1),
	generate_list(List2),
    generate_latex_vector_ijk(List1, [i,j,k],"",Latex_str1),
    generate_latex_vector_ijk(List2, [i,j,k],"",Latex_str2),

    generate_latex_vector_name(Var1,Latex_str_name1),
    generate_latex_vector_name(Var2,Latex_str_name2),

    string_concatenate(["[string(Find the values of x,y so that the vectors ",Latex_str_name1," = ",Latex_str1," and ",Latex_str_name2," = ",Latex_str2," are equal. )]"],"",Question).

%Answer
generate_answer_vector_q_4(List1,List2,_):-
	write("[string("),
	check_vector(List1,List2).
	    %NOTe:  Add )] after the result in the parser to generate the solution.

%Solution


generate_solution_q(Var1,Var2):-
	generate_latex_vector_name(Var1,Latex_str_name1),
    generate_latex_vector_name(Var2,Latex_str_name2),

	string_concatenate(["[string(Note that two vectors are equal if and only if their corresponding components are equal.)" ],"",Sol_0),
	string_concatenate([",string(Thus, the given vectors",Latex_str_name1," and ",Latex_str_name2,"will be equal if and only if),string("],"",Sol_1),
	string_concatenate([Sol_0,Sol_1],"",Sol),
	write(Sol).

generate_solution_vector_q_4(Var1,Var2,List1,List2,_):-

	generate_solution_q(Var1,Var2),
	check_vector(List1,List2).
    %NOTe:  Add )] after the result in the parser to generate the solution.


%------------------------------------------------------------------End of Question 4---------------------------------------------------------------------------------

%------------------------------------------------------------------Question 5---------------------------------------------------------------------------------

%Question:
generate_question_vector_q_5(Question):-
	
	generate_list(Point1),
	generate_list(Point2),
  

    string_concatenate(["[string(Find the scalar and vector components of the vector with initial point ",Point1," and terminal point ",Point2,".)]"],"",Question).


%Answer
generate_answer_vector_q_5(Point1,Point2,Answer):-
	generate_diff_vector(Point2,Point1,Diff),
	generate_vector_component(Diff, [i,j,k],"",Latex_Diff),

	string_concatenate(["[string(Scalar components : ",Diff,")"],"",Ans_0),
	string_concatenate([",string(Vector components : ",Latex_Diff,")]"],"",Ans_1),

	string_concatenate([Ans_0,Ans_1],"",Answer).

%Solution
generate_solution_vector_q_5(Point1,Point2,Solution):-
	generate_latex_diff_exp_ijk(Point2,Point1,[i,j,k],"",Latex_diff_exp),
	generate_diff_vector(Point2,Point1,Diff),
	generate_latex_vector_ijk(Diff, [i,j,k],"",Latex_Diff),
	generate_vector_component(Diff, [i,j,k],"",Latex_vector_Diff),


	string_concatenate(["[string(The vector with the initial point P ",Point1," and terminal point Q ",Point2," is given by)"],"",Sol_0),
    string_concatenate([",string(latex(\\\\overrightarrow{PQ}) = ",Latex_diff_exp,")"],"",Sol_1),
	string_concatenate([",string(latex(\\\\overrightarrow{PQ}) = ",Latex_Diff,")"],"",Sol_2),
	string_concatenate([",string(Hence, the required scalar components are ",Diff," while the vector components are ",Latex_vector_Diff,".)]"],"",Sol_3),


    string_concatenate([Sol_0,Sol_1,Sol_2,Sol_3],"",Solution).

%------------------------------------------------------------------End of Question 5---------------------------------------------------------------------------------

%------------------------------------------------------------------Question 6---------------------------------------------------------------------------------

%Question:
generate_question_vector_q_6(Question):-
	Var1=a,
	Var2=b,
	Var3=c,
	generate_list(List1),
	generate_list(List2),
	generate_list(List3),
    generate_latex_vector_ijk(List1, [i,j,k],"",Latex_str1),
    generate_latex_vector_ijk(List2, [i,j,k],"",Latex_str2),
    generate_latex_vector_ijk(List3, [i,j,k],"",Latex_str3),

    generate_latex_vector_name(Var1,Latex_str_name1),
    generate_latex_vector_name(Var2,Latex_str_name2),
    generate_latex_vector_name(Var3,Latex_str_name3),


    string_concatenate(["[string(Find the sum of the vectors ",Latex_str_name1," = ",Latex_str1,", ",Latex_str_name2," = ",Latex_str2," and ",Latex_str_name3," = ",Latex_str3,".)]"],"",Question).

%Answer
generate_answer_vector_q_6(List1,List2,List3,Answer):-
	generate_sum_vector(List1,List2,Sum_1),
	generate_sum_vector(Sum_1,List3,Final_sum),
	generate_latex_vector_ijk(Final_sum, [i,j,k],"",Latex_sum),

	string_concatenate(["[string(",Latex_sum,")]"],"",Answer).


%Solution
generate_solution_vector_q_6(List1,List2,List3,Solution):-
	
	Var1=a,
	Var2=b,
	Var3=c,
 	generate_latex_vector_ijk(List1, [i,j,k],"",Latex_str1),
    generate_latex_vector_ijk(List2, [i,j,k],"",Latex_str2),
    generate_latex_vector_ijk(List3, [i,j,k],"",Latex_str3),

    generate_latex_vector_name(Var1,Latex_str_name1),
    generate_latex_vector_name(Var2,Latex_str_name2),
    generate_latex_vector_name(Var3,Latex_str_name3),

	generate_latex_sum_exp_ijk(List1,List2,List3,[i,j,k],"",Latex_sum_exp),

	generate_sum_vector(List1,List2,Sum_1),
	generate_sum_vector(Sum_1,List3,Final_sum),
	generate_latex_vector_ijk(Final_sum, [i,j,k],"",Latex_sum),
	
	string_concatenate(["[string(The given vetors are ",Latex_str_name1," = ",Latex_str1,", ",Latex_str_name2," = ",Latex_str2," and ",Latex_str_name3," = ",Latex_str3,")"],"",Sol_0),
	string_concatenate([",string(",Latex_str_name1," + ",Latex_str_name2," + ",Latex_str_name3," = ",Latex_sum_exp,")"],"",Sol_1),
	string_concatenate([",string(",Latex_str_name1," + ",Latex_str_name2," + ",Latex_str_name3," = ",Latex_sum,")]"],"",Sol_2),

    string_concatenate([Sol_0,Sol_1,Sol_2],"",Solution).


%------------------------------------------------------------------End of Question 6---------------------------------------------------------------------------------


%------------------------------------------------------------------Question 7---------------------------------------------------------------------------------

%Question:

generate_question_vector_q_7(Question):-
	Var1=a,
	generate_list(List1),
    generate_latex_vector_ijk(List1, [i,j,k],"",Latex_str1),

    generate_latex_vector_name(Var1,Latex_str_name1),

    string_concatenate(["[string(Find the unit vector in the direction of the vector ",Latex_str_name1," = ",Latex_str1," )]"],"",Question).

%Answer
generate_answer_vector_q_7(List1,Answer):-
	Var1=a,
	generate_magnitude(List1,0,List1_magnitude),
	generate_latex_fraction_vector_ijk(List1, [i,j,k],List1_magnitude,"",Latex_frac_str1),

    string_concatenate(["[string(latex(\\\\hat{",Var1,"}) = ",Latex_frac_str1,")]"],"",Answer).

%Solution
generate_solution_vector_q_7(List1,Solution):-
	Var1=a,
    generate_latex_vector_name(Var1,Latex_str_name1),

    generate_latex_vector_ijk(List1, [i,j,k],"",Latex_str1),

    generate_magnitude(List1,0,List1_magnitude),

	generate_latex_fraction_vector_ijk(List1, [i,j,k],List1_magnitude,"",Latex_frac_str1),


    generate_latex_magnitude_expression_ijk(List1,"",List1_mag_exp),

    string_concatenate(["[string(The unit vector in the direction of vector ",Latex_str_name1," is given by latex(\\\\hat{",Var1,"}) = latex(\\\\frac{1}{|\\\\overrightarrow{",Var1,"}|})",Latex_str_name1,".)"],"",Sol_0),

    string_concatenate([",string(Now |latex(\\\\overrightarrow{",Var1,"})| = ",List1_mag_exp," = latex(\\\\sqrt{",List1_magnitude,"}))"],"",Sol_1),

    string_concatenate([",string(Therefore latex(\\\\hat{",Var1,"}) = latex(\\\\frac{1}{\\\\sqrt{",List1_magnitude,"}})[",Latex_str1,"] = ",Latex_frac_str1,")]"],"",Sol_2),

    string_concatenate([Sol_0,Sol_1,Sol_2],"",Solution).

%------------------------------------------------------------------End of Question 7---------------------------------------------------------------------------------


%------------------------------------------------------------------Question 8---------------------------------------------------------------------------------

%Question:

generate_question_vector_q_8(Question):-
	generate_list(Point1),
	generate_list(Point2),

    string_concatenate(["[string(Find the unit vector in the directio of vector latex(\\\\overrightarrow{PQ}), where P and Q are the points ",Point1," and ",Point2,", respectively.)]"],"",Question).

%Answer
generate_answer_vector_q_8(Point1,Point2,Answer):-
	
	generate_diff_vector(Point2,Point1,Diff),
	generate_magnitude(Diff,0,Diff_magnitude),
	generate_latex_fraction_vector_ijk(Diff, [i,j,k],Diff_magnitude,"",Latex_frac_str1),

    string_concatenate(["[string(latex(\\\\hat{PQ}) = ",Latex_frac_str1,")]"],"",Answer).

%Solution
generate_solution_vector_q_8(Point1,Point2,Solution):-

    generate_latex_diff_exp_ijk(Point2,Point1,[i,j,k],"",Latex_diff_exp),
	generate_diff_vector(Point2,Point1,Diff),
	generate_latex_vector_ijk(Diff, [i,j,k],"",Latex_Diff),



    generate_magnitude(Diff,0,List1_magnitude),

	generate_latex_fraction_vector_ijk(Diff, [i,j,k],List1_magnitude,"",Latex_frac_str1),

    generate_latex_magnitude_expression_ijk(Diff,"",List1_mag_exp),


    string_concatenate(["[string(The given points are P",Point1," and Q",Point2,")"],"",Sol_0),
 	string_concatenate([",string(latex(\\\\overrightarrow{PQ}) = ",Latex_diff_exp,")"],"",Sol_1),
	string_concatenate([",string(latex(\\\\overrightarrow{PQ}) = ",Latex_Diff,")"],"",Sol_2),

    string_concatenate([",string(Now |latex(\\\\overrightarrow{PQ})| = ",List1_mag_exp," = latex(\\\\sqrt{",List1_magnitude,"}))"],"",Sol_3),

    string_concatenate([",string(Therefore latex(\\\\hat{PQ}) = latex(\\\\frac{1}{\\\\sqrt{",List1_magnitude,"}})[",Latex_Diff,"] = ",Latex_frac_str1,")]"],"",Sol_4),

    string_concatenate([Sol_0,Sol_1,Sol_2,Sol_3,Sol_4],"",Solution).

%------------------------------------------------------------------End of Question 8---------------------------------------------------------------------------------


%------------------------------------------------------------------Question 9---------------------------------------------------------------------------------

%Question:

generate_question_vector_q_9(Question):-
	Var1=a,
	Var2=b,
	generate_list(List1),
	generate_list(List2),

	generate_latex_vector_ijk(List1, [i,j,k],"",Latex_str1),
    generate_latex_vector_ijk(List2, [i,j,k],"",Latex_str2),

    generate_latex_vector_name(Var1,Latex_str_name1),
    generate_latex_vector_name(Var2,Latex_str_name2),
    string_concatenate(["[string(For the given vectors, ",Latex_str_name1," = ",Latex_str1," and ",Latex_str_name2," = ",Latex_str2," , find the unit vector in the direction of the vector ",Latex_str_name1," + ",Latex_str_name2,".)]"],"",Question).


%Answer
generate_answer_vector_q_9(List1,List2,Answer):-
	
	generate_sum_vector(List1,List2,Sum),
	generate_magnitude(Sum,0,Sum_magnitude),
	generate_latex_fraction_vector_ijk(Sum, [i,j,k],Sum_magnitude,"",Latex_frac_str1),

    string_concatenate(["[string(latex(\\\\hat{a+b}) = ",Latex_frac_str1,")]"],"",Answer).


%Solution
generate_solution_vector_q_9(List1,List2,Solution):-

	Var1=a,
	Var2=b,

	generate_latex_vector_ijk(List1, [i,j,k],"",Latex_str1),
    generate_latex_vector_ijk(List2, [i,j,k],"",Latex_str2),

    generate_latex_vector_name(Var1,Latex_str_name1),
    generate_latex_vector_name(Var2,Latex_str_name2),
  
	generate_sum_vector(List1,List2,Sum),
	generate_latex_vector_ijk(Sum, [i,j,k],"",Latex_Sum),
	generate_latex_sum_exp_ijk(List1,List2,[i,j,k],"",Latex_sum_exp),



    generate_magnitude(Sum,0,Sum_magnitude),

	generate_latex_fraction_vector_ijk(Sum, [i,j,k],Sum_magnitude,"",Latex_frac_str1),

    generate_latex_magnitude_expression_ijk(Sum,"",Sum_mag_exp),


    string_concatenate(["[string(The given points are ",Latex_str_name1," = ",Latex_str1," and ",Latex_str_name2," = ",Latex_str2,")"],"",Sol_0),
 	string_concatenate([",string(",Latex_str_name1,"+",Latex_str_name2," = ",Latex_sum_exp,")"],"",Sol_1),
	string_concatenate([",string(",Latex_str_name1,"+",Latex_str_name2," = ",Latex_Sum,")"],"",Sol_2),

    string_concatenate([",string(Now |",Latex_str_name1,"+",Latex_str_name2,"| = ",Sum_mag_exp," = latex(\\\\sqrt{",Sum_magnitude,"}))"],"",Sol_3),
    string_concatenate([",string(Hence, the unit vector in the direction of [ ",Latex_str_name1," + ",Latex_str_name2," ] is)"],"",Sol_4),

    string_concatenate([",string(Therefore latex(\\\\hat{a+b}) = latex(\\\\frac{1}{\\\\sqrt{",Sum_magnitude,"}})[",Latex_Sum,"] = ",Latex_frac_str1,")]"],"",Sol_5),

    string_concatenate([Sol_0,Sol_1,Sol_2,Sol_3,Sol_4,Sol_5],"",Solution).

%------------------------------------------------------------------End of Question 9---------------------------------------------------------------------------------


%------------------------------------------------------------------Question 10---------------------------------------------------------------------------------

%Question:

generate_question_vector_q_10(Question):-
	Var1=a,
	generate_list(List1),
	generate_magnitude(Magnitude),

    generate_latex_vector_ijk(List1, [i,j,k],"",Latex_str1),

    generate_latex_vector_name(Var1,Latex_str_name1),

    string_concatenate(["[string(Find the vector in the direction of vector ",Latex_str_name1," = ",Latex_str1," that has magnitude ",Magnitude, " units.)]"],"",Question).


%Answer
generate_answer_vector_q_10(List1,Magnitude,Answer):-
	generate_magnitude(List1,0,List1_magnitude),
	generate_latex_mag_fraction_vector_ijk(List1, [i,j,k],List1_magnitude,Magnitude,"",Latex_mag_frac_str1),

   string_concatenate(["[string(",Latex_mag_frac_str1,")]"],"",Answer).


%Solution
generate_solution_vector_q_10(List1,Magnitude, Solution):-

	Var1=a,
	generate_latex_vector_name(Var1,Latex_str_name1),

    generate_latex_vector_ijk(List1, [i,j,k],"",Latex_str1),

    generate_magnitude(List1,0,List1_magnitude),

	generate_latex_fraction_vector_ijk(List1, [i,j,k],List1_magnitude,"",Latex_frac_str1),

	generate_latex_mag_fraction_vector_ijk(List1, [i,j,k],List1_magnitude,Magnitude,"",Latex_mag_frac_str1),


	string_concatenate(["[string(The unit vector int he direction of given vector ",Latex_str_name1," is)"],"",Sol_0),

	string_concatenate([",string(latex(\\\\hat{",Var1,"}) = latex(\\\\frac{1}{|\\\\overrightarrow{",Var1,"}|})",Latex_str_name1," = latex(\\\\frac{1}{\\\\sqrt{",List1_magnitude,"}})[",Latex_str1,"] = ",Latex_frac_str1,")"],"",Sol_1),

	string_concatenate([",string(Therefore, the vector having magnitude equal to ",Magnitude,"and in the direction of ",Latex_str_name1," is)"],"",Sol_2),

	string_concatenate([",string(",Magnitude,"latex(\\\\hat{",Var1,"}) = ",Magnitude,"[",Latex_frac_str1,"] = ",Latex_mag_frac_str1,"))]"],"",Sol_3),


    string_concatenate([Sol_0,Sol_1,Sol_2,Sol_3],"",Solution).

%------------------------------------------------------------------End of Question 10---------------------------------------------------------------------------------


%------------------------------------------------------------------Question 11---------------------------------------------------------------------------------


%Question
generate_question_vector_q_11(Question):-

	generate_list(List1),
	generate_list(List2),

    generate_latex_vector_ijk(List1, [i,j,k],"",Latex_str1),
    generate_latex_vector_ijk(List2, [i,j,k],"",Latex_str2),

    string_concatenate(["[string(Show that the vectors ",Latex_str1," and ",Latex_str2," are collinear or not.)]"],"",Question).

generate_answer_vector_q_11(List1,List2,Answer):-
	check_collinear(List1,List2,Coll),
	check_list(Coll,Check_result),
   
	(Check_result=="Equal"->
		string_concatenate(["[string(The given vectors are collinear.)]" ],"",Answer)
		;
		string_concatenate(["[string(The given vectors are not collinear.)]" ],"",Answer)
	).

generate_solution_vector_q_11(List1,List2,Solution):-

    generate_latex_vector_ijk(List1, [i,j,k],"",Latex_str1),
    generate_latex_vector_ijk(List2, [i,j,k],"",Latex_str2),
	check_collinear(List1,List2,Coll),
	check_list(Coll,Check_result),
	find_lanbda(List1,List2,Lam),

	string_concatenate(["[string(Let latex(\\\\overrightarrow{a} = )",Latex_str1," and latex(\\\\overrightarrow{b} = )",Latex_str2,".)" ],"",Sol_0),
	string_concatenate([",string(For Vectors to be collinear latex(\\\\overrightarrow{b} = \\\\lambda.\\\\overrightarrow{a}))" ],"",Sol_1),
	(Check_result=="Equal"->
		string_concatenate([",string(It can be observed that ",Latex_str2," = ",Lam,".[",Latex_str1,"] where latex(\\\\lambda ) = ",Lam,".)" ],"",Sol_2),
		string_concatenate([",string(Hence, The given vectors are collinear.)]" ],"",Sol_3)
		;
		string_concatenate([",string(It can be observed that ",Latex_str2," latex(\\\\neq \\\\lambda.)[",Latex_str1,"].)" ],"",Sol_2),
		string_concatenate([",string(Hence, The given vectors are not collinear.)]" ],"",Sol_3)
	),
    string_concatenate([Sol_0,Sol_1,Sol_2,Sol_3],"",Solution).


%------------------------------------------------------------------End of Question 11---------------------------------------------------------------------------------


%------------------------------------------------------------------Question 12---------------------------------------------------------------------------------

%Question
generate_question_vector_q_12(Question):-

	Var1=a,
	generate_list(List1),

    generate_latex_vector_ijk(List1, [i,j,k],"",Latex_str1),

    generate_latex_vector_name(Var1,Latex_str_name1),

    string_concatenate(["[string(Find the direction cosines of the vector ",Latex_str_name1," = ",Latex_str1,".)]"],"",Question).


%Answer
generate_answer_vector_q_12(List1,Answer):-
	generate_magnitude(List1,0,List1_magnitude),

	generate_direction_cosines(List1,List1_magnitude,"",Latex_direction_cosine),
   
   string_concatenate(["[string(Direction Cosines are: ",Latex_direction_cosine,")]"],"",Answer).


%generate_solution_vector_ex_9
generate_solution_vector_q_12(List1,Solution):-
    generate_latex_vector_name(r,Latex_str_vec),
    generate_magnitude(List1,0,List1_magnitude),
	generate_direction_cosines(List1,List1_magnitude,"",Latex_direction_cosine),

    string_concatenate(["[string(If l, m and n are the direction cosines of the given vector, then)"],"",Sol_1),
    string_concatenate([",string(l = latex(\\\\frac{a}{|\\\\overrightarrow{",r,"}|}), m = latex(\\\\frac{b}{|\\\\overrightarrow{",r,"}|}), n = latex(\\\\frac{c}{|\\\\overrightarrow{",r,"}|}) and |",Latex_str_vec,"| = latex(\\\\sqrt{",List1_magnitude,"}))"],"",Sol_2),
    string_concatenate([",string(Thus, the direction cosines are [l,m,n] : [",Latex_direction_cosine,"])]"],"",Sol_3),

    string_concatenate([Sol_1,Sol_2,Sol_3],"",Solution).


%------------------------------------------------------------------End of Question 12---------------------------------------------------------------------------------

%------------------------------------------------------------------Question 13---------------------------------------------------------------------------------

%Question
generate_question_vector_q_13(Question):-

	generate_list(Point1),
	generate_list(Point2),

    string_concatenate(["[string(Find the direction cosines of the vector joining the points A ",Point1," and B ",Point2," , directed from A to B.)]"],"",Question).


%Answer
generate_answer_vector_q_13(Point1,Point2,Answer):-
	
	generate_diff_vector(Point2,Point1,Diff),
	generate_magnitude(Diff,0,Diff_magnitude),
	generate_direction_cosines(Diff,Diff_magnitude,"",Latex_direction_cosine),
   
   string_concatenate(["[string(Direction Cosines are: ",Latex_direction_cosine,")]"],"",Answer).


%Solution
generate_solution_vector_q_13(Point1,Point2,Solution):-


    generate_latex_diff_exp_ijk(Point2,Point1,[i,j,k],"",Latex_diff_exp),
	generate_diff_vector(Point2,Point1,Diff),
	generate_latex_vector_ijk(Diff, [i,j,k],"",Latex_Diff),

    generate_magnitude(Diff,0,List1_magnitude),
    generate_magnitude(Diff,0,Diff_magnitude),
    generate_latex_magnitude_expression_ijk(Diff,"",List1_mag_exp),
    generate_direction_cosines(Diff,Diff_magnitude,"",Latex_direction_cosine),



    string_concatenate(["[string(The given points are A",Point1," and B",Point2,")"],"",Sol_0),
 	string_concatenate([",string(latex(\\\\overrightarrow{AB}) = ",Latex_diff_exp,")"],"",Sol_1),
	string_concatenate([",string(latex(\\\\overrightarrow{AB}) = ",Latex_Diff,")"],"",Sol_2),
    string_concatenate([",string(Now |latex(\\\\overrightarrow{AB})| = ",List1_mag_exp," = latex(\\\\sqrt{",List1_magnitude,"}))"],"",Sol_3),
 	
 	string_concatenate([",string(If l, m and n are the direction cosines of the given vector, then)"],"",Sol_4),
    string_concatenate([",string(l = latex(\\\\frac{a}{|\\\\overrightarrow{AB}|}), m = latex(\\\\frac{b}{|\\\\overrightarrow{AB}|}), n = latex(\\\\frac{c}{|\\\\overrightarrow{AB}|}))"],"",Sol_5),
    string_concatenate([",string(Thus, the direction cosines are [l,m,n] : [",Latex_direction_cosine,"])]"],"",Sol_6),

    string_concatenate([Sol_0,Sol_1,Sol_2,Sol_3,Sol_4,Sol_5,Sol_6],"",Solution).

%------------------------------------------------------------------End of Question 13---------------------------------------------------------------------------------


%------------------------------------------------------------------Question 14---------------------------------------------------------------------------------
%Question
generate_question_vector_q_14(Question):-

	generate_list(List),
	generate_latex_vector_ijk(List, [i,j,k],"",Latex_str1),

    string_concatenate(["[string(Show that the vector ",Latex_str1," is equally inclined to the axes OX, OY and OZ.)]"],"",Question).


%Answer
generate_answer_vector_q_14(List,Answer):-
	
	check_list(List,Check_result),
   
	(Check_result=="Equal"->
		string_concatenate(["[string(The Vectors is equally inclined to OX,OY,OZ.)]" ],"",Answer);
		string_concatenate(["[string(The Vectors is not equally inclined to OX,OY,OZ.)]" ],"",Answer)
	).

%Solution
generate_solution_vector_q_14(List,Solution):-
	Var=a,
	generate_latex_vector_name(Var,Latex_str_name1),
	generate_latex_vector_ijk(List, [i,j,k],"",Latex_str1),
	generate_latex_magnitude_expression_ijk(List,"",List_mag_exp),
	generate_magnitude(List,0,List_magnitude),
	generate_direction_cosines(List,List_magnitude,"",Latex_direction_cosine),
    check_list(List,Check_result),

	string_concatenate(["[string(Let ",Latex_str_name1," = ",Latex_str1,")" ],"",Sol_1),
	string_concatenate([",string(and  |",Latex_str_name1,"| = ",List_mag_exp," = latex(\\\\sqrt{",List_magnitude,"}) )"],"",Sol_2),
    string_concatenate([",string(Therefore, the direction cosines of latex(\\\\overrightarrow{a}) are [",Latex_direction_cosine,"])"],"",Sol_3),

	(Check_result=="Equal"->
		string_concatenate([",string(Hence, the Vectors is equally inclined to OX,OY,OZ.)]" ],"",Sol_4)
		;
		string_concatenate([",string(Hence, the Vectors is not equally inclined to OX,OY,OZ.)]" ],"",Sol_4)
	),
    string_concatenate([Sol_1,Sol_2,Sol_3,Sol_4],"",Solution).


%------------------------------------------------------------------End of Question 14---------------------------------------------------------------------------------


%------------------------------------------------------------------Question 15---------------------------------------------------------------------------------

%Question
generate_question_vector_q_15(Question):-
	
	generate_list(Point1),
	generate_list(Point2),

	generate_magnitude(Ratio1),
	generate_magnitude(Ratio2),

	generate_latex_vector_ijk(Point1, [i,j,k],"",Latex_str1),
    generate_latex_vector_ijk(Point2, [i,j,k],"",Latex_str2),

	string_concatenate(["[string(Find the position vector of a point R which divides the lines joining two points P ans Q whose position vectrs are )"],"",Q_0),
	string_concatenate([",string(",Latex_str1," and ",Latex_str2," respectively in the ratio ",Ratio1," : ",Ratio2,")"],"",Q_1),
	string_concatenate([",string((i). internally)"],"",Q_2),
	string_concatenate([",string((ii). externally)]"],"",Q_3),

    string_concatenate([Q_0,Q_1,Q_2,Q_3],"",Question).


%Answer
generate_answer_vector_q_15(Point1,Point2,Ratio1,Ratio2,Answer):-
	divide_internally(Point2,Point1,Ratio1,Ratio2,Div_internally),
	divide_externally(Point2,Point1,Ratio1,Ratio2,Div_externally),

	Internally is Ratio1+Ratio2,
	Externally is Ratio1-Ratio2,

	generate_latex_fraction_vector(Div_internally,[i,j,k],Internally,"",Latex_ratio_1),
	generate_latex_fraction_vector(Div_externally,[i,j,k],Externally,"",Latex_ratio_2),

	string_concatenate(["[string(Internally: ",Latex_ratio_1,")"],"",Ans_0),
	string_concatenate([",string(Externally: ",Latex_ratio_2,")]"],"",Ans_1),
    string_concatenate([Ans_0,Ans_1],"",Answer).


%Solution
generate_solution_vector_q_15(Point1,Point2,Ratio1,Ratio2,Solution):-
	
	generate_vector_ijk(Point1, [i,j,k],"",Latex_str1),
    generate_vector_ijk(Point2, [i,j,k],"",Latex_str2),
	divide_internally(Point2,Point1,Ratio1,Ratio2,Div_internally),
	divide_externally(Point2,Point1,Ratio1,Ratio2,Div_externally),

	Internally is Ratio1+Ratio2,
	Externally is Ratio1-Ratio2,

	generate_latex_fraction_vector(Div_internally,[i,j,k],Internally,"",Latex_ratio_1),
	generate_latex_fraction_vector(Div_externally,[i,j,k],Externally,"",Latex_ratio_2),

	string_concatenate(["[string((i) The position vector of the point R dividing the join of P and Q intenally in the ratio",Ratio1," : ",Ratio2," is )"],"",Sol_0),
	string_concatenate([",string(latex(\\\\overrightarrow{OR}) = latex(\\\\frac{",Ratio1,"(",Latex_str2,") + ",Ratio2,"(",Latex_str1,")}{",Ratio1," + ",Ratio2,"}) = ",Latex_ratio_1,")]"],"",Sol_1),
	string_concatenate([",string((ii) The position vector of the point R dividing the join of P and Q extenally in the ratio",Ratio1," : ",Ratio2," is )"],"",Sol_2),
	string_concatenate([",string(latex(\\\\overrightarrow{OR}) = latex(\\\\frac{",Ratio1,"(",Latex_str2,") - ",Ratio2,"(",Latex_str1,")}{",Ratio1," - ",Ratio2,"}) = ",Latex_ratio_2,")]"],"",Sol_3),
    string_concatenate([Sol_0,Sol_1,Sol_2,Sol_3],"",Solution).



%------------------------------------------------------------------End of Question 15---------------------------------------------------------------------------------


%------------------------------------------------------------------Question 16---------------------------------------------------------------------------------

%Question
generate_question_vector_q_16(Question):-
	
	generate_list(Point1),
	generate_list(Point2),

    string_concatenate(["[string(Find the position vector of the mid point of the vector joining the points P",Point1," and Q",Point2,".)]"],"",Question).

%Answer
generate_answer_vector_q_16(Point1,Point2,Answer):-
	generate_sum_vector_mid(Point1,Point2,Sum_mid),
	generate_latex_vector_ijk(Sum_mid, [i,j,k],"",Sum_mid_str),
	string_concatenate(["[string(",Sum_mid_str,")]"],"",Answer).

%Solution
generate_solution_vector_q_16(Point1,Point2,Solution):-

	generate_sum_vector_mid(Point1,Point2,Sum_mid),
	generate_sum_vector(Point1,Point2,Sum),
	generate_sum_exp_ijk(Point1,Point2,[i,j,k],"",Latex_sum_exp),
	generate_vector_ijk(Point1, [i,j,k],"",Latex_str1),
	generate_vector_ijk(Point2, [i,j,k],"",Latex_str2),
	generate_latex_vector_ijk(Sum_mid, [i,j,k],"",Sum_mid_str),
	generate_vector_ijk(Sum, [i,j,k],"",Sum_str),

	string_concatenate(["[string(The position vector of the mid-point R of the vector joining points is given by,)"],"",Sol_0),
	string_concatenate([",string(latex(\\\\overrightarrow{OR}) = latex(\\\\frac{(",Latex_str1,") + (",Latex_str2,")}{2} = \\\\frac{",Latex_sum_exp,"}{2} ))"],"",Sol_1),
	string_concatenate([",string(latex(\\\\overrightarrow{OR}) = latex(\\\\frac{(",Sum_str,")}{2} = )",Sum_mid_str," )]"],"",Sol_2),

    string_concatenate([Sol_0,Sol_1,Sol_2],"",Solution).
%------------------------------------------------------------------End of Question 16---------------------------------------------------------------------------------


%------------------------------------------------------------------Question 17---------------------------------------------------------------------------------
%Question
generate_question_vector_q_17(Question):-
	
	Var1=a,
	Var2=b,
	Var3=c,
	generate_list(Point1),
	generate_list(Point2),
	generate_list(Point3),

	generate_latex_vector_ijk(Point1, [i,j,k],"",Latex_str1),
    generate_latex_vector_ijk(Point2, [i,j,k],"",Latex_str2),
    generate_latex_vector_ijk(Point3, [i,j,k],"",Latex_str3),

    generate_latex_vector_name(Var1,Latex_str_name1),
    generate_latex_vector_name(Var2,Latex_str_name2),
    generate_latex_vector_name(Var3,Latex_str_name3),


    string_concatenate(["[string(Show that the points A, B and C with position vectors, ",Latex_str_name1," = ",Latex_str1,", ",Latex_str_name2," = ",Latex_str2," and ",Latex_str_name3," = ",Latex_str3," ,)"],"",Q_0),
    string_concatenate([",string(respectively form the vertices of a right angled triangle. )]"],"",Q_1),
    string_concatenate([Q_0,Q_1],"",Question).
    

%Answer
generate_answer_vector_q_17(Point1,Point2,Point3,Answer):-
	generate_diff_vector(Point2,Point1,Diff_1),
	generate_diff_vector(Point3,Point2,Diff_2),
	generate_diff_vector(Point1,Point3,Diff_3),

    generate_magnitude(Diff_1,0,Diff_1_magnitude),
    generate_magnitude(Diff_2,0,Diff_2_magnitude),
    generate_magnitude(Diff_3,0,Diff_3_magnitude),

    (Diff_3_magnitude=:=Diff_1_magnitude+Diff_2_magnitude->
    	    string_concatenate(["[string(The triangle formed is a right angled triangle)]"],"",Answer);
    	    (Diff_2_magnitude=:=Diff_3_magnitude+Diff_1_magnitude->
    	    	string_concatenate(["[string(The triangle formed is a right angled triangle)]"],"",Answer);
     			(Diff_1_magnitude=:=Diff_3_magnitude+Diff_2_magnitude->
     				string_concatenate(["[string(The triangle formed is a right angled triangle)]"],"",Answer);
					string_concatenate(["[string(The triangle formed is a not right angled triangle.)]"],"",Answer)
				)
			)
    ).

%Solution
generate_solution_vector_q_17(Point1,Point2,Point3,Solution):-
	generate_latex_diff_exp_ijk(Point2,Point1,[i,j,k],"",Latex_diff_exp_1),
	generate_latex_diff_exp_ijk(Point3,Point2,[i,j,k],"",Latex_diff_exp_2),
	generate_latex_diff_exp_ijk(Point1,Point3,[i,j,k],"",Latex_diff_exp_3),

	generate_diff_vector(Point2,Point1,Diff_1),
	generate_diff_vector(Point3,Point2,Diff_2),
	generate_diff_vector(Point1,Point3,Diff_3),

    generate_magnitude(Diff_1,0,Diff_1_magnitude),
    generate_magnitude(Diff_2,0,Diff_2_magnitude),
    generate_magnitude(Diff_3,0,Diff_3_magnitude),

    generate_latex_vector_ijk(Diff_1, [i,j,k],"",Latex_Diff_1),
	generate_latex_vector_ijk(Diff_2, [i,j,k],"",Latex_Diff_2),
	generate_latex_vector_ijk(Diff_3, [i,j,k],"",Latex_Diff_3),


    string_concatenate(["[string(We have,)"],"",Sol_0),
	string_concatenate([",string(latex(\\\\overrightarrow{AB}) = ",Latex_diff_exp_1," = ",Latex_Diff_1,")"],"",Sol_1),
	string_concatenate([",string(latex(\\\\overrightarrow{BC}) = ",Latex_diff_exp_2," = ",Latex_Diff_2,")"],"",Sol_2),
	string_concatenate([",string(latex(\\\\overrightarrow{CA}) = ",Latex_diff_exp_3," = ",Latex_Diff_3,")"],"",Sol_3),
    string_concatenate([",string(Further note that,)"],"",Sol_4),

    (Diff_1_magnitude=:=Diff_3_magnitude+Diff_2_magnitude->
		string_concatenate([",string(latex(|\\\\overrightarrow{AB}|^2) = ",Diff_1_magnitude," latex(=) ",Diff_2_magnitude," + ",Diff_3_magnitude," = latex(|\\\\overrightarrow{BC}|^2) + latex(|\\\\overrightarrow{CA}|^2))"],"",Sol_5);
		string_concatenate([",string(latex(|\\\\overrightarrow{AB}|^2) = ",Diff_1_magnitude," latex(\\neq) ",Diff_2_magnitude," + ",Diff_3_magnitude," = latex(|\\\\overrightarrow{BC}|^2) + latex(|\\\\overrightarrow{CA}|^2))"],"",Sol_5)

	),

	(Diff_2_magnitude=:=Diff_1_magnitude+Diff_3_magnitude->
		string_concatenate([",string(latex(|\\\\overrightarrow{BC}|^2) = ",Diff_2_magnitude," latex(=) ",Diff_1_magnitude," + ",Diff_3_magnitude," = latex(|\\\\overrightarrow{AB}|^2) + latex(|\\\\overrightarrow{CA}|^2))"],"",Sol_6);
		string_concatenate([",string(latex(|\\\\overrightarrow{BC}|^2) = ",Diff_2_magnitude," latex(\\neq) ",Diff_1_magnitude," + ",Diff_3_magnitude," = latex(|\\\\overrightarrow{AB}|^2) + latex(|\\\\overrightarrow{CA}|^2))"],"",Sol_6)

	),

	(Diff_3_magnitude=:=Diff_1_magnitude+Diff_2_magnitude->
		string_concatenate([",string(latex(|\\\\overrightarrow{CA}|^2) = ",Diff_3_magnitude," latex(=) ",Diff_1_magnitude," + ",Diff_2_magnitude," = latex(|\\\\overrightarrow{AB}|^2) + latex(|\\\\overrightarrow{BC}|^2))"],"",Sol_7);
		string_concatenate([",string(latex(|\\\\overrightarrow{CA}|^2) = ",Diff_3_magnitude," latex(\\neq) ",Diff_1_magnitude," + ",Diff_2_magnitude," = latex(|\\\\overrightarrow{AB}|^2) + latex(|\\\\overrightarrow{BC}|^2))"],"",Sol_7)

	),

   (Diff_3_magnitude=:=Diff_1_magnitude+Diff_2_magnitude->
		string_concatenate([",string(latex(|\\\\overrightarrow{CA}|^2) = ",Diff_3_magnitude," latex(=) ",Diff_1_magnitude," + ",Diff_2_magnitude," = latex(|\\\\overrightarrow{AB}|^2) + latex(|\\\\overrightarrow{BC}|^2))"],"",Sol_7);
		string_concatenate([",string(latex(|\\\\overrightarrow{CA}|^2) = ",Diff_3_magnitude," latex(\\neq) ",Diff_1_magnitude," + ",Diff_2_magnitude," = latex(|\\\\overrightarrow{AB}|^2) + latex(|\\\\overrightarrow{BC}|^2))"],"",Sol_7)

	),

  	(Diff_3_magnitude=:=Diff_1_magnitude+Diff_2_magnitude->
    	    string_concatenate([",string(Hence,The triangle formed is a right angled triangle)]"],"",Sol_8);
    	    (Diff_2_magnitude=:=Diff_3_magnitude+Diff_1_magnitude->
    	    	string_concatenate([",string(Hence,The triangle formed is a right angled triangle)]"],"",Sol_8);
     			(Diff_1_magnitude=:=Diff_3_magnitude+Diff_2_magnitude->
     				string_concatenate([",string(Hence,The triangle formed is a right angled triangle)]"],"",Sol_8);
					string_concatenate([",string(Hence,The triangle formed is a not right angled triangle.)]"],"",Sol_8)
				)
			)
    ),
	string_concatenate([Sol_0,Sol_1,Sol_2,Sol_3,Sol_4,Sol_5,Sol_6,Sol_7,Sol_8],"",Solution).

%------------------------------------------------------------------End of Question 17---------------------------------------------------------------------------------


