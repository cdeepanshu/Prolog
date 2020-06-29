/*:- include('radicals/helper.pl').
:- include('vector_algebra/helper.pl').
:- include('application_of_integrals/gfs.pl').
:- include('trigno_with_val/helper.pl').
:- include('binomial_multinomials/helper_general.pl').*/
%--------------------------------------------------------------------Exercise 10.4-------------------------------------------------------------------------
%------------------------------------------------------------------Example 22---------------------------------------------------------------------------------
%Question
generate_question_vector_ex_22(List1,List2,Question):-
	Var1=a,
	Var2=b,
	/*generate_list(List1),
	generate_list(List2),*/

    generate_latex_vector_name(Var1,Latex_str_name1),
    generate_latex_vector_name(Var2,Latex_str_name2),

  	generate_latex_vector_ijk(List1, [i,j,k],"",Latex_str1),
    generate_latex_vector_ijk(List2, [i,j,k],"",Latex_str2),

    string_concatenate(["[string(Find latex(|)",Latex_str_name1," X ",Latex_str_name2,"latex(|), if ",Latex_str_name1," = ",Latex_str1," and ",Latex_str_name2," = ",Latex_str2,")]"],"",Question).
%Answer
generate_answer_vector_ex_22(List1,List2,Answer):-
	Var1=a,
	Var2=b,
    generate_latex_vector_name(Var1,Latex_str_name1),
    generate_latex_vector_name(Var2,Latex_str_name2),
    
    cross_product(List1,List2,Product),
    generate_magnitude(Product,0,Product_magnitude),
    
    get_updated_coefficient([[[1,1],[Product_magnitude,1]]],X,_),
	get_updated_coefficient_result(X,M,S),
	(M=:=1->
		(S=:=1->
			Num is S;
			string_concatenate(["\\\\sqrt{",S,"}"],"",Num)
		);
		(S=:=1->
			Num is M*S;
			string_concatenate(["",M,"\\\\sqrt{",S,"}"],"",Num)
		)
	),
    string_concatenate(["[string(latex(|)",Latex_str_name1," X ",Latex_str_name2,"latex(|) = latex(",Num,"))]"],"",Answer).
%Solution
generate_solution_vector_ex_22(List1,List2,Solution):-
	Var1=a,
	Var2=b,
    generate_latex_vector_name(Var1,Latex_str_name1),
    generate_latex_vector_name(Var2,Latex_str_name2),
	List1=[X1,Y1,Z1],
	List2=[X2,Y2,Z2],
    cross_product(List1,List2,Product),
    generate_latex_vector_ijk(Product, [i,j,k],"",Latex_product),

    generate_magnitude(Product,0,Product_magnitude),
    generate_latex_cross_product(List1,List2,Latex_cross_product),
    
    get_updated_coefficient([[[1,1],[Product_magnitude,1]]],X,_),
	get_updated_coefficient_result(X,M,S),
	(M=:=1->
		(S=:=1->
			Num is S;
			string_concatenate(["\\\\sqrt{",S,"}"],"",Num)
		);
		(S=:=1->
			Num is M*S;
			string_concatenate(["",M,"\\\\sqrt{",S,"}"],"",Num)
		)
	),
    string_concatenate(["[string(We have)"],"",Sol_0),
    string_concatenate([",string(",Latex_str_name1," X ",Latex_str_name2," = latex(\\\\begin{vmatrix} \\\\hat{i} & \\\\hat{j} & \\\\hat{j} \\\\\\\\\\\\ ",X1," & ",Y1," & ",Z1," \\\\\\\\\\\\ ",X2," & ",Y2," & ",Z2," \\\\end{vmatrix}))"],"",Sol_1),
    string_concatenate([",string(",Latex_str_name1," X ",Latex_str_name2," = latex(",Latex_cross_product," = )",Latex_product,"))"],"",Sol_2),
    string_concatenate([",string(Hence, latex(|)",Latex_str_name1," X ",Latex_str_name2,"latex(|) = latex(",Num,"))]"],"",Sol_3),
   
    string_concatenate([Sol_0,Sol_1,Sol_2,Sol_3],"",Solution).

%------------------------------------------------------------------End of Question 22--------------------------------------------------------------------------------

%------------------------------------------------------------------Example 23---------------------------------------------------------------------------------
%Question
generate_question_vector_ex_23(List1,List2,Question):-
	Var1=a,
	Var2=b,
	/*generate_list(List1),
	generate_list(List2),*/

    generate_vector_name(Var1,Latex_str_name1),
    generate_vector_name(Var2,Latex_str_name2),

  	generate_latex_vector_ijk(List1, [i,j,k],"",Latex_str1),
    generate_latex_vector_ijk(List2, [i,j,k],"",Latex_str2),

    string_concatenate(["[string(Find a unit vector perpendicular to each of the vectors latex((",Latex_str_name1," + ",Latex_str_name2,")) and latex((",Latex_str_name1," - ",Latex_str_name2,")) where latex(",Latex_str_name1,") = ",Latex_str1,", latex(",Latex_str_name2,") = ",Latex_str2,".)]"],"",Question).
%Answer
generate_answer_vector_ex_23(List1,List2,Answer):-
	generate_sum_vector(List1,List2,Sum),
	generate_diff_vector(List1,List2,Diff),

	cross_product(Sum,Diff,Product),
    generate_magnitude(Product,0,Product_magnitude),
    
    get_updated_coefficient([[[1,1],[Product_magnitude,1]]],X,_),
	get_updated_coefficient_result(X,M,S),
	(M=:=1->
		(S=:=1->
			Num is S;
			string_concatenate(["\\\\sqrt{",S,"}"],"",Num)
		);
		(S=:=1->
			Num is M*S;
			string_concatenate(["",M,"\\\\sqrt{",S,"}"],"",Num)
		)
	),
	Pro is (M)*(S),
	generate_latex_updated_fraction_vector_ijk(Product, [i,j,k],Num,Pro,"",Latex_frac_str),

    string_concatenate(["[string(",Latex_frac_str,")]"],"",Answer).
%Solution
generate_solution_vector_ex_23(List1,List2,Solution):-
	Var1=a,
	Var2=b,
	generate_latex_vector_name(Var1,Latex_str_name1),
    generate_latex_vector_name(Var2,Latex_str_name2),

    generate_latex_vector_ijk(List1, [i,j,k],"",Latex_str1),
    generate_latex_vector_ijk(List2, [i,j,k],"",Latex_str2),

	generate_sum_vector(List1,List2,Sum),
	generate_diff_vector(List1,List2,Diff),
    generate_latex_vector_ijk(Sum, [i,j,k],"",Latex_sum),
    generate_latex_vector_ijk(Diff, [i,j,k],"",Latex_diff),
	Sum=[X1,Y1,Z1],
	Diff=[X2,Y2,Z2],
	cross_product(Sum,Diff,Product),
    generate_magnitude(Product,0,Product_magnitude),
    generate_latex_vector_ijk(Product, [i,j,k],"",Latex_product),
    generate_latex_magnitude_expression_ijk(Product,"",Product_mag_exp),

    get_updated_coefficient([[[1,1],[Product_magnitude,1]]],X,_),
	get_updated_coefficient_result(X,M,S),
	(M=:=1->
		(S=:=1->
			Num is S;
			string_concatenate(["\\\\sqrt{",S,"}"],"",Num)
		);
		(S=:=1->
			Num is M*S;
			string_concatenate(["",M,"\\\\sqrt{",S,"}"],"",Num)
		)
	),
	Pro is (M)*(S),
	generate_latex_updated_fraction_vector_ijk(Product, [i,j,k],Num,Pro,"",Latex_frac_str),

    string_concatenate(["[string(We have ",Latex_str_name1," + ",Latex_str_name2," = ",Latex_sum," and ",Latex_str_name1," - ",Latex_str_name2," = ",Latex_diff,")"],"",Sol_0),
    string_concatenate([",string(A vector which is perpendicular to both ",Latex_str_name1," + ",Latex_str_name2," and ",Latex_str_name1," - ",Latex_str_name2," is given by )"],"",Sol_1),
    string_concatenate([",string([",Latex_str_name1," + ",Latex_str_name2,"] X [",Latex_str_name1," - ",Latex_str_name2,"] =  latex(\\\\begin{vmatrix} \\\\hat{i} & \\\\hat{j} & \\\\hat{j} \\\\\\\\\\\\ ",X1," & ",Y1," & ",Z1," \\\\\\\\\\\\ ",X2," & ",Y2," & ",Z2," \\\\end{vmatrix}) = ",Latex_product," = [latex(\\\\overrightarrow{c}, say)] )"],"",Sol_2),
    string_concatenate([",string(Now  latex(|\\\\overrightarrow{c}|) =  ",Product_mag_exp," = latex(\\\\sqrt{",Product_magnitude,"} = ",Num,"))"],"",Sol_3),
    string_concatenate([",string(Therefore, the required unit vector is )"],"",Sol_4),
    string_concatenate([",string( latex(\\\\frac{\\\\overrightarrow{c}}{|\\\\overrightarrow{c}|} = )",Latex_frac_str," )]"],"",Sol_5),
    string_concatenate([Sol_0,Sol_1,Sol_2,Sol_3,Sol_4,Sol_5],"",Solution).

%------------------------------------------------------------------End of Question 23--------------------------------------------------------------------------------

%------------------------------------------------------------------Example 24---------------------------------------------------------------------------------
%Question
generate_question_vector_ex_24(Point_list_1,Point_list_2,Point_list_3,Question):-
	/*generate_list(Point_list_1),
	generate_list(Point_list_2),
	generate_list(Point_list_3),*/
	convert_square_brackets_to_round_brackets(Point_list_1,Point_list_string_1),
    convert_square_brackets_to_round_brackets(Point_list_2,Point_list_string_2),
    convert_square_brackets_to_round_brackets(Point_list_3,Point_list_string_3),
    string_concatenate(["[string(Find the area of a triangle having the points A latex(",Point_list_string_1,"), B latex(",Point_list_string_2,") and C latex(",Point_list_string_1,") as its vertices.)]"],"",Question).
%Answer
generate_answer_vector_ex_24(List1,List2,List3,Answer):-
	generate_diff_vector(List2,List1,Diff_1),
	generate_diff_vector(List3,List2,Diff_2),
	cross_product(Diff_1,Diff_2,Product),
    generate_magnitude(Product,0,Product_magnitude),
    
    get_updated_coefficient([[[1,1],[Product_magnitude,1]]],X,_),
	get_updated_coefficient_result(X,M,S),
	(M=:=1->
		(S=:=1->
			Num is S;
			string_concatenate(["\\\\sqrt{",S,"}"],"",Num)
		);
		(S=:=1->
			Num is M*S;
			string_concatenate(["",M,"\\\\sqrt{",S,"}"],"",Num)
		)
	),
    string_concatenate(["[string(Area = latex(\\\\frac{1}{2}",Num,"))]"],"",Answer).
%Solution
generate_solution_vector_ex_24(List1,List2,List3,Solution):-
	generate_diff_vector(List2,List1,Diff_1),
	generate_diff_vector(List3,List2,Diff_2),
	generate_latex_vector_ijk(Diff_1, [i,j,k],"",Latex_Diff1),
    generate_latex_vector_ijk(Diff_2, [i,j,k],"",Latex_Diff2),
	Diff_1=[X1,Y1,Z1],
	Diff_2=[X2,Y2,Z2],
	cross_product(Diff_1,Diff_2,Product),
    generate_magnitude(Product,0,Product_magnitude),
    generate_latex_vector_ijk(Product, [i,j,k],"",Latex_product),
    generate_latex_magnitude_expression_ijk(Product,"",Product_mag_exp),

    get_updated_coefficient([[[1,1],[Product_magnitude,1]]],X,_),
	get_updated_coefficient_result(X,M,S),
	(M=:=1->
		(S=:=1->
			Num is S;
			string_concatenate(["\\\\sqrt{",S,"}"],"",Num)
		);
		(S=:=1->
			Num is M*S;
			string_concatenate(["",M,"\\\\sqrt{",S,"}"],"",Num)
		)
	),
    string_concatenate(["[string(We have latex(\\\\overrightarrow{AB} = )",Latex_Diff1," and latex(\\\\overrightarrow{AC} = )",Latex_Diff2,".)"],"",Sol_0),
    string_concatenate([",string(The area of the given triangle is latex(\\\\frac{1}{2}|\\\\overrightarrow{AB} X \\\\overrightarrow{AC}|).)"],"",Sol_1),
    string_concatenate([",string(Now latex(\\\\overrightarrow{AB} X \\\\overrightarrow{AC} = )  latex(\\\\begin{vmatrix} \\\\hat{i} & \\\\hat{j} & \\\\hat{j} \\\\\\\\\\\\ ",X1," & ",Y1," & ",Z1," \\\\\\\\\\\\ ",X2," & ",Y2," & ",Z2," \\\\end{vmatrix} = ) ",Latex_product," )"],"",Sol_2),
    string_concatenate([",string(Therefore  latex(|\\\\overrightarrow{AB} X \\\\overrightarrow{AC}|) =  ",Product_mag_exp," = latex(\\\\sqrt{",Product_magnitude,"} = ",Num,"))"],"",Sol_3),
    string_concatenate([",string(Thus, the required area is latex(\\\\frac{1}{2}",Num,"))]"],"",Sol_4),
    string_concatenate([Sol_0,Sol_1,Sol_2,Sol_3,Sol_4],"",Solution).
%------------------------------------------------------------------End of Question 24--------------------------------------------------------------------------------

%------------------------------------------------------------------Example 25---------------------------------------------------------------------------------
%Question
generate_question_vector_ex_25(List1,List2,Question):-
	Var1=a,
	Var2=b,
	/*generate_list(List1),
	generate_list(List2),*/

    generate_latex_vector_name(Var1,Latex_str_name1),
    generate_latex_vector_name(Var2,Latex_str_name2),

  	generate_latex_vector_ijk(List1, [i,j,k],"",Latex_str1),
    generate_latex_vector_ijk(List2, [i,j,k],"",Latex_str2),

    string_concatenate(["[string(Find the area of a parallelogram whose adjacent sides are given by the vectors ",Latex_str_name1," = ",Latex_str1," and ",Latex_str_name2," = ",Latex_str2,")]"],"",Question).
%Answer
generate_answer_vector_ex_25(List1,List2,Answer):-
	cross_product(List1,List2,Product),
    generate_magnitude(Product,0,Product_magnitude),
    
    get_updated_coefficient([[[1,1],[Product_magnitude,1]]],X,_),
	get_updated_coefficient_result(X,M,S),
	(M=:=1->
		(S=:=1->
			Num is S;
			string_concatenate(["\\\\sqrt{",S,"}"],"",Num)
		);
		(S=:=1->
			Num is M*S;
			string_concatenate(["",M,"\\\\sqrt{",S,"}"],"",Num)
		)
	),
    string_concatenate(["[string(Area of parallelogram = latex(",Num,"))]"],"",Answer).
%Solution
generate_solution_vector_ex_25(List1,List2,Solution):-
	Var1=a,
	Var2=b,
    generate_latex_vector_name(Var1,Latex_str_name1),
    generate_latex_vector_name(Var2,Latex_str_name2),
	List1=[X1,Y1,Z1],
	List2=[X2,Y2,Z2],
    cross_product(List1,List2,Product),
    generate_latex_vector_ijk(Product, [i,j,k],"",Latex_product),
    generate_latex_magnitude_expression_ijk(Product,"",Product_mag_exp),

    generate_magnitude(Product,0,Product_magnitude),
    generate_latex_cross_product(List1,List2,Latex_cross_product),
    
    get_updated_coefficient([[[1,1],[Product_magnitude,1]]],X,_),
	get_updated_coefficient_result(X,M,S),
	(M=:=1->
		(S=:=1->
			Num is S;
			string_concatenate(["\\\\sqrt{",S,"}"],"",Num)
		);
		(S=:=1->
			Num is M*S;
			string_concatenate(["",M,"\\\\sqrt{",S,"}"],"",Num)
		)
	),
    string_concatenate(["[string(The area of a parallelogram with ",Latex_str_name1," and ",Latex_str_name2," as its adjacent sides is given by |",Latex_str_name1," X ",Latex_str_name2,"|.)"],"",Sol_0),
    string_concatenate([",string(Now ",Latex_str_name1," X ",Latex_str_name2," = latex(\\\\begin{vmatrix} \\\\hat{i} & \\\\hat{j} & \\\\hat{j} \\\\\\\\\\\\ ",X1," & ",Y1," & ",Z1," \\\\\\\\\\\\ ",X2," & ",Y2," & ",Z2," \\\\end{vmatrix}))"],"",Sol_1),
    string_concatenate([",string(",Latex_str_name1," X ",Latex_str_name2," = latex(",Latex_cross_product," = )",Latex_product,"))"],"",Sol_2),
    string_concatenate([",string(Therefore   latex(|)",Latex_str_name1," X ",Latex_str_name2,"latex(|) = ",Product_mag_exp," = latex(",Num,"))"],"",Sol_3),
    string_concatenate([",string(and hence, the required area is latex(",Num,").)]"],"",Sol_4),

    string_concatenate([Sol_0,Sol_1,Sol_2,Sol_3,Sol_4],"",Solution).
%------------------------------------------------------------------End of Question 25--------------------------------------------------------------------------------

%------------------------------------------------------------------Question 1--------------------------------------------------------------------------------
%Question
generate_question_vector_10_4_1(Question):-
	Var1=a,
	Var2=b,
	generate_list(List1),
	generate_list(List2),

    generate_latex_vector_name(Var1,Latex_str_name1),
    generate_latex_vector_name(Var2,Latex_str_name2),

  	generate_latex_vector_ijk(List1, [i,j,k],"",Latex_str1),
    generate_latex_vector_ijk(List2, [i,j,k],"",Latex_str2),

    string_concatenate(["[string(Find |",Latex_str_name1," X ",Latex_str_name2,"|, if ",Latex_str_name1," = ",Latex_str1," and ",Latex_str_name2," = ",Latex_str2,")]"],"",Question).
%Answer
generate_answer_vector_10_4_1(List1,List2,Answer):-
	Var1=a,
	Var2=b,
    generate_latex_vector_name(Var1,Latex_str_name1),
    generate_latex_vector_name(Var2,Latex_str_name2),
    
    cross_product(List1,List2,Product),
    generate_magnitude(Product,0,Product_magnitude),
    
    get_updated_coefficient([[[1,1],[Product_magnitude,1]]],X,_),
	get_updated_coefficient_result(X,M,S),
	(M=:=1->
		(S=:=1->
			Num is S;
			string_concatenate(["\\\\sqrt{",S,"}"],"",Num)
		);
		(S=:=1->
			Num is M*S;
			string_concatenate(["",M,"\\\\sqrt{",S,"}"],"",Num)
		)
	),
    string_concatenate(["[string(|",Latex_str_name1," X ",Latex_str_name2,"| = latex(",Num,"))]"],"",Answer).
%Solution
generate_solution_vector_10_4_1(List1,List2,Solution):-
	Var1=a,
	Var2=b,
    generate_latex_vector_name(Var1,Latex_str_name1),
    generate_latex_vector_name(Var2,Latex_str_name2),
	List1=[X1,Y1,Z1],
	List2=[X2,Y2,Z2],
    cross_product(List1,List2,Product),
    generate_latex_vector_ijk(Product, [i,j,k],"",Latex_product),

    generate_magnitude(Product,0,Product_magnitude),
    generate_latex_cross_product(List1,List2,Latex_cross_product),
    
    get_updated_coefficient([[[1,1],[Product_magnitude,1]]],X,_),
	get_updated_coefficient_result(X,M,S),
	(M=:=1->
		(S=:=1->
			Num is S;
			string_concatenate(["\\\\sqrt{",S,"}"],"",Num)
		);
		(S=:=1->
			Num is M*S;
			string_concatenate(["",M,"\\\\sqrt{",S,"}"],"",Num)
		)
	),
    string_concatenate(["[string(We have)"],"",Sol_0),
    string_concatenate([",string(",Latex_str_name1," X ",Latex_str_name2," = latex(\\\\begin{vmatrix} \\\\hat{i} & \\\\hat{j} & \\\\hat{j} \\\\\\ ",X1," & ",Y1," & ",Z1," \\\\\\ ",X2," & ",Y2," & ",Z2," \\\\end{vmatrix}))"],"",Sol_1),
    string_concatenate([",string(",Latex_str_name1," X ",Latex_str_name2," = latex(",Latex_cross_product," = )",Latex_product,"))"],"",Sol_2),
    string_concatenate([",string(Hence, |",Latex_str_name1," X ",Latex_str_name2,"| = latex(",Num,"))]"],"",Sol_3),
   
    string_concatenate([Sol_0,Sol_1,Sol_2,Sol_3],"",Solution).

%------------------------------------------------------------------End of Question 1--------------------------------------------------------------------------------

%------------------------------------------------------------------Question 2--------------------------------------------------------------------------------
%Question
generate_question_vector_10_4_2(Question):-
	Var1=a,
	Var2=b,
	generate_list(List1),
	generate_list(List2),

    generate_latex_vector_name(Var1,Latex_str_name1),
    generate_latex_vector_name(Var2,Latex_str_name2),

  	generate_latex_vector_ijk(List1, [i,j,k],"",Latex_str1),
    generate_latex_vector_ijk(List2, [i,j,k],"",Latex_str2),

    string_concatenate(["[string(Find a unit vector perpendicular to each of the vectors [",Latex_str_name1," + ",Latex_str_name2,"] and [",Latex_str_name1," - ",Latex_str_name2,"] where ",Latex_str_name1," = ",Latex_str1,", ",Latex_str_name2," = ",Latex_str2,".)]"],"",Question).
%Answer
generate_answer_vector_10_4_2(List1,List2,Answer):-
	generate_sum_vector(List1,List2,Sum),
	generate_diff_vector(List1,List2,Diff),

	cross_product(Sum,Diff,Product),
    generate_magnitude(Product,0,Product_magnitude),
    
    get_updated_coefficient([[[1,1],[Product_magnitude,1]]],X,_),
	get_updated_coefficient_result(X,M,S),
	(M=:=1->
		(S=:=1->
			Num is S;
			string_concatenate(["\\\\sqrt{",S,"}"],"",Num)
		);
		(S=:=1->
			Num is M*S;
			string_concatenate(["",M,"\\\\sqrt{",S,"}"],"",Num)
		)
	),
	Pro is (M)*(S),
	generate_latex_updated_fraction_vector_ijk(Product, [i,j,k],Num,Pro,"",Latex_frac_str),

    string_concatenate(["[string(",Latex_frac_str,")]"],"",Answer).
%Solution
generate_solution_vector_10_4_2(List1,List2,Solution):-
	Var1=a,
	Var2=b,
	generate_latex_vector_name(Var1,Latex_str_name1),
    generate_latex_vector_name(Var2,Latex_str_name2),

    generate_latex_vector_ijk(List1, [i,j,k],"",Latex_str1),
    generate_latex_vector_ijk(List2, [i,j,k],"",Latex_str2),

	generate_sum_vector(List1,List2,Sum),
	generate_diff_vector(List1,List2,Diff),
    generate_latex_vector_ijk(Sum, [i,j,k],"",Latex_sum),
    generate_latex_vector_ijk(Diff, [i,j,k],"",Latex_diff),
	Sum=[X1,Y1,Z1],
	Diff=[X2,Y2,Z2],
	cross_product(Sum,Diff,Product),
    generate_magnitude(Product,0,Product_magnitude),
    generate_latex_vector_ijk(Product, [i,j,k],"",Latex_product),
    generate_latex_magnitude_expression_ijk(Product,"",Product_mag_exp),

    get_updated_coefficient([[[1,1],[Product_magnitude,1]]],X,_),
	get_updated_coefficient_result(X,M,S),
	(M=:=1->
		(S=:=1->
			Num is S;
			string_concatenate(["\\\\sqrt{",S,"}"],"",Num)
		);
		(S=:=1->
			Num is M*S;
			string_concatenate(["",M,"\\\\sqrt{",S,"}"],"",Num)
		)
	),
	Pro is (M)*(S),
	generate_latex_updated_fraction_vector_ijk(Product, [i,j,k],Num,Pro,"",Latex_frac_str),

    string_concatenate(["[string(We have ",Latex_str_name1," + ",Latex_str_name2," = ",Latex_sum," and ",Latex_str_name1," - ",Latex_str_name2," = ",Latex_diff,")"],"",Sol_0),
    string_concatenate([",string(A vector which is perpendicular to both ",Latex_str_name1," + ",Latex_str_name2," and ",Latex_str_name1," - ",Latex_str_name2," is given by )"],"",Sol_1),
    string_concatenate([",string([",Latex_str_name1," + ",Latex_str_name2,"] X [",Latex_str_name1," - ",Latex_str_name2,"] =  latex(\\\\begin{vmatrix} \\\\hat{i} & \\\\hat{j} & \\\\hat{j} \\\\\\ ",X1," & ",Y1," & ",Z1," \\\\\\ ",X2," & ",Y2," & ",Z2," \\\\end{vmatrix}) = ",Latex_product," = [latex(\\\\overrightarrow{c}, say)] )"],"",Sol_2),
    string_concatenate([",string(Now  latex(|\\\\overrightarrow{c}|) =  ",Product_mag_exp," = latex(\\\\sqrt{",Product_magnitude,"} = ",Num,"))"],"",Sol_3),
    string_concatenate([",string(Therefore, the required unit vector is )"],"",Sol_4),
    string_concatenate([",string( latex(\\\\frac{\\\\overrightarrow{c}}{|\\\\overrightarrow{c}|} = )",Latex_frac_str," )]"],"",Sol_5),
    string_concatenate([Sol_0,Sol_1,Sol_2,Sol_3,Sol_4,Sol_5],"",Solution).
%------------------------------------------------------------------End of Question 2--------------------------------------------------------------------------------

%------------------------------------------------------------------Question 5--------------------------------------------------------------------------------
%Question
generate_question_vector_10_4_5(List1,Mag,Question):-
	/*generate_list(List1),
	generate_magnitude(Mag),*/

  	generate_vector_ijk(List1, [i,j,k],"",Latex_str1),

    string_concatenate(["[string(Find latex(\\\\\\\\lambda) and latex(\\\\\\\\mu) if latex((",Latex_str1,")) X latex((",Mag,"\\\\\\\\hat{i} + \\\\\\\\lambda\\\\\\\\hat{j} + \\\\\\\\mu\\\\\\\\hat{k}) = 0))]"],"",Question).
%Answer
generate_answer_vector_10_4_5(List1,Mag,Answer):-
	List1=[A,B,C],
	Lambda is (Mag*B),
	Mu is (Mag*C),
	simplify(Lambda,A,N_L,D_L),
	simplify(Mu,A,N_M,D_M),
	(N_L=:=0->
		string_concatenate(["[string(latex(\\\\lambda = 0))"],"",Ans_0);
		(D_L=:=0->
			string_concatenate(["[string(latex(\\\\lambda = 0))"],"",Ans_0);
			(D_L=:=1->
				string_concatenate(["[string(latex(\\\\lambda = ",N_L,"))"],"",Ans_0);
				string_concatenate(["[string(latex(\\\\lambda = \\\\frac{",N_L,"}{",D_L,"}))"],"",Ans_0)
			)
		)
	),
	(N_M=:=0->
		string_concatenate([",string(latex(\\\\mu = 0))]"],"",Ans_1);
		(D_M=:=0->
			string_concatenate(["string(latex(\\\\mu = 0))]"],"",Ans_1);
			(D_M=:=1->
				string_concatenate([",string(latex(\\\\mu = ",N_M,"))]"],"",Ans_1);
				string_concatenate([",string(latex(\\\\mu = \\\\frac{",N_M,"}{",D_M,"}))]"],"",Ans_1)
			)
		)
	),
    string_concatenate([Ans_0,Ans_1],"",Answer).

%Solution
generate_solution_vector_10_4_5(List1,Mag,Solution):-
  	generate_vector_ijk(List1, [i,j,k],"",Latex_str1),
	List1=[A,B,C],
	Lambda is (Mag*B),
	Mu is (Mag*C),
	simplify(Lambda,A,N_L,D_L),
	simplify(Mu,A,N_M,D_M),
	(N_L=:=0->
		string_concatenate([",string(latex(\\\\lambda = 0))"],"",Sol_6);
		(D_L=:=0->
			string_concatenate([",string(latex(\\\\lambda = 0))"],"",Sol_6);

			(D_L=:=1->
				string_concatenate([",string(latex(\\\\lambda = ",N_L,"))"],"",Sol_6);
				string_concatenate([",string(latex(\\\\lambda = \\\\frac{",N_L,"}{",D_L,"}))"],"",Sol_6)
			)
		)
	),
	(N_M=:=0->
		string_concatenate([",string(latex(\\\\mu = 0))]"],"",Sol_7);
		(D_M=:=0->
			string_concatenate([",string(latex(\\\\mu = 0))]"],"",Sol_7);
			(D_M=:=1->
				string_concatenate([",string(latex(\\\\mu = ",N_M,"))]"],"",Sol_7);
				string_concatenate([",string(latex(\\\\mu = \\\\frac{",N_M,"}{",D_M,"}))]"],"",Sol_7)
			)
		)
	),
	Q2 is Mag*C,
	Q3 is Mag*B,
	string_concatenate(["[string(Given latex((",Latex_str1,")) X latex((",Mag,"\\\\hat{i} + \\\\lambda\\\\hat{j} + \\\\mu\\\\hat{k}) = 0))"],"",Sol_0),
    string_concatenate([",string(latex(\\\\begin{vmatrix} \\\\hat{i} & \\\\hat{j} & \\\\hat{j} \\\\\\\\\\\\ ",A," & ",B," & ",C," \\\\\\\\\\\\ ",Mag," & \\\\lambda & \\\\mu \\\\end{vmatrix} = 0\\\\hat{i} + 0\\\\hat{j} + 0\\\\hat{k}))"],"",Sol_1),
	string_concatenate([",string(latex([(",B,")\\\\mu - (",C,")\\\\lambda]\\\\hat{i} - [(",A,")\\\\mu - (",Q2,")]\\\\hat{j} + [(",A,")\\\\lambda - (",Q3,")]\\\\hat{k} = 0\\\\hat{i} + 0\\\\hat{j} + 0\\\\hat{k} ))"],"",Sol_2),
	string_concatenate([",string(latex((",B,")\\\\mu - (",C,")\\\\lambda = 0 ))"],"",Sol_3),
	string_concatenate([",string(latex((",A,")\\\\mu - (",Q2,") = 0 ))"],"",Sol_4),
	string_concatenate([",string(latex((",A,")\\\\lambda - (",Q3,") = 0 ))"],"",Sol_5),
    string_concatenate([Sol_0,Sol_1,Sol_2,Sol_3,Sol_4,Sol_5,Sol_6,Sol_7],"",Solution).

%------------------------------------------------------------------End of Question 5--------------------------------------------------------------------------------

%------------------------------------------------------------------Question 9--------------------------------------------------------------------------------
%Question
generate_question_vector_10_4_9(Question):-
	generate_list(List1),
	generate_list(List2),
	generate_list(List3),
    string_concatenate(["[string(Find the area of a triangle with vertices A",List1,", B",List2," and C",List3,".)]"],"",Question).
%Answer
generate_answer_vector_10_4_9(List1,List2,List3,Answer):-
	generate_diff_vector(List2,List1,Diff_1),
	generate_diff_vector(List3,List2,Diff_2),
	cross_product(Diff_1,Diff_2,Product),
    generate_magnitude(Product,0,Product_magnitude),
    
    get_updated_coefficient([[[1,1],[Product_magnitude,1]]],X,_),
	get_updated_coefficient_result(X,M,S),
	(M=:=1->
		(S=:=1->
			Num is S;
			string_concatenate(["\\\\sqrt{",S,"}"],"",Num)
		);
		(S=:=1->
			Num is M*S;
			string_concatenate(["",M,"\\\\sqrt{",S,"}"],"",Num)
		)
	),
	Pro is (M)*(S),
    string_concatenate(["[string(Area = latex(\\\\frac{1}{2}",Num,"))]"],"",Answer).
%Solution
generate_solution_vector_10_4_9(List1,List2,List3,Solution):-
	generate_diff_vector(List2,List1,Diff_1),
	generate_diff_vector(List3,List2,Diff_2),
	generate_latex_vector_ijk(Diff_1, [i,j,k],"",Latex_Diff1),
    generate_latex_vector_ijk(Diff_2, [i,j,k],"",Latex_Diff2),
	Diff_1=[X1,Y1,Z1],
	Diff_2=[X2,Y2,Z2],
	cross_product(Diff_1,Diff_2,Product),
    generate_magnitude(Product,0,Product_magnitude),
    generate_latex_vector_ijk(Product, [i,j,k],"",Latex_product),
    generate_latex_magnitude_expression_ijk(Product,"",Product_mag_exp),

    get_updated_coefficient([[[1,1],[Product_magnitude,1]]],X,_),
	get_updated_coefficient_result(X,M,S),
	(M=:=1->
		(S=:=1->
			Num is S;
			string_concatenate(["\\\\sqrt{",S,"}"],"",Num)
		);
		(S=:=1->
			Num is M*S;
			string_concatenate(["",M,"\\\\sqrt{",S,"}"],"",Num)
		)
	),
	Pro is (M)*(S),
    string_concatenate(["[string(We have latex(\\\\overrightarrow{AB} = )",Latex_Diff1," and latex(\\\\overrightarrow{AC} = )",Latex_Diff2,".)"],"",Sol_0),
    string_concatenate([",string(The area of the given triangle is latex(\\\\frac{1}{2}|\\\\overrightarrow{AB} X \\\\overrightarrow{AC}|).)"],"",Sol_1),
    string_concatenate([",string(Now latex(\\\\overrightarrow{AB} X \\\\overrightarrow{AC} = )  latex(\\\\begin{vmatrix} \\\\hat{i} & \\\\hat{j} & \\\\hat{j} \\\\\\ ",X1," & ",Y1," & ",Z1," \\\\\\ ",X2," & ",Y2," & ",Z2," \\\\end{vmatrix} = ) ",Latex_product," )"],"",Sol_2),
    string_concatenate([",string(Therefore  latex(|\\\\overrightarrow{AB} X \\\\overrightarrow{AC}|) =  ",Product_mag_exp," = latex(\\\\sqrt{",Product_magnitude,"} = ",Num,"))"],"",Sol_3),
    string_concatenate([",string(Thus, the required area is latex(\\\\frac{1}{2}",Num,"))]"],"",Sol_4),
    string_concatenate([Sol_0,Sol_1,Sol_2,Sol_3,Sol_4],"",Solution).
%------------------------------------------------------------------End of Question 9--------------------------------------------------------------------------------

%------------------------------------------------------------------Question 10--------------------------------------------------------------------------------
%Question
generate_question_vector_10_4_10(Question):-
	Var1=a,
	Var2=b,
	generate_list(List1),
	generate_list(List2),

    generate_latex_vector_name(Var1,Latex_str_name1),
    generate_latex_vector_name(Var2,Latex_str_name2),

  	generate_latex_vector_ijk(List1, [i,j,k],"",Latex_str1),
    generate_latex_vector_ijk(List2, [i,j,k],"",Latex_str2),

    string_concatenate(["[string(Find the area of a parallelogram whose adjacent sides are given by the vectors ",Latex_str_name1," = ",Latex_str1," and ",Latex_str_name2," = ",Latex_str2,")]"],"",Question).
%Answer
generate_answer_vector_10_4_10(List1,List2,Answer):-
	cross_product(List1,List2,Product),
    generate_magnitude(Product,0,Product_magnitude),
    
    get_updated_coefficient([[[1,1],[Product_magnitude,1]]],X,_),
	get_updated_coefficient_result(X,M,S),
	(M=:=1->
		(S=:=1->
			Num is S;
			string_concatenate(["\\\\sqrt{",S,"}"],"",Num)
		);
		(S=:=1->
			Num is M*S;
			string_concatenate(["",M,"\\\\sqrt{",S,"}"],"",Num)
		)
	),
    string_concatenate(["[string(Area of parallelogram = latex(",Num,"))]"],"",Answer).
%Solution
generate_solution_vector_10_4_10(List1,List2,Solution):-
	Var1=a,
	Var2=b,
    generate_latex_vector_name(Var1,Latex_str_name1),
    generate_latex_vector_name(Var2,Latex_str_name2),
	List1=[X1,Y1,Z1],
	List2=[X2,Y2,Z2],
    cross_product(List1,List2,Product),
    generate_latex_vector_ijk(Product, [i,j,k],"",Latex_product),
    generate_latex_magnitude_expression_ijk(Product,"",Product_mag_exp),

    generate_magnitude(Product,0,Product_magnitude),
    generate_latex_cross_product(List1,List2,Latex_cross_product),
    
    get_updated_coefficient([[[1,1],[Product_magnitude,1]]],X,_),
	get_updated_coefficient_result(X,M,S),
	(M=:=1->
		(S=:=1->
			Num is S;
			string_concatenate(["\\\\sqrt{",S,"}"],"",Num)
		);
		(S=:=1->
			Num is M*S;
			string_concatenate(["",M,"\\\\sqrt{",S,"}"],"",Num)
		)
	),
    string_concatenate(["[string(The area of a parallelogram with ",Latex_str_name1," and ",Latex_str_name2," as its adjacent sides is given by |",Latex_str_name1," X ",Latex_str_name2,"|.)"],"",Sol_0),
    string_concatenate([",string(Now ",Latex_str_name1," X ",Latex_str_name2," = latex(\\\\begin{vmatrix} \\\\hat{i} & \\\\hat{j} & \\\\hat{j} \\\\\\ ",X1," & ",Y1," & ",Z1," \\\\\\ ",X2," & ",Y2," & ",Z2," \\\\end{vmatrix}))"],"",Sol_1),
    string_concatenate([",string(",Latex_str_name1," X ",Latex_str_name2," = latex(",Latex_cross_product," = )",Latex_product,"))"],"",Sol_2),
    string_concatenate([",string(Therefore   |",Latex_str_name1," X ",Latex_str_name2,"| = ",Product_mag_exp," = latex(",Num,"))"],"",Sol_3),
    string_concatenate([",string(and hence, the required area is latex(",Num,").)]"],"",Sol_4),

    string_concatenate([Sol_0,Sol_1,Sol_2,Sol_3,Sol_4],"",Solution).

%------------------------------------------------------------------End of Question 10--------------------------------------------------------------------------------
