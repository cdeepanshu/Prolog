:- include('radicals/helper.pl').
:- include('vector_algebra/helper.pl').
:- include('application_of_integrals/gfs.pl').
:- include('trigno_with_val/helper.pl').
:- include('binomial_multinomials/helper_general.pl').


%--------------------------------------------------------------------Exercise 10.3-------------------------------------------------------------------------
%------------------------------------------------------------------Example 13---------------------------------------------------------------------------------

%Question
generate_question_vector_ex_13(Question):-
	Var1=a,
	Var2=b,
	generate_magnitude(Mag1),
	generate_magnitude(Mag2),
	generate_magnitude(Mag3),

    generate_latex_vector_name(Var1,Latex_str_name1),
    generate_latex_vector_name(Var2,Latex_str_name2),

    string_concatenate(["[string(Find the angle between two vectors ",Latex_str_name1," and ",Latex_str_name2," with magnitude ",Mag1," and ",Mag2," respectively and when ",Latex_str_name1,".",Latex_str_name2," = ",Mag3,".)]"],"",Question).

%Answer
generate_answer_vector_ex_13(Mag1,Mag2,Mag3,Answer):-
	Den is Mag1*Mag2,
	Num is Mag3,
	string_concatenate([Num,"/",Den],"",Term),
	trignometric_inverse_function(1,Term,Angle),
	string_concatenate(["[string(latex(",Angle,"))]"],"",Answer).
%Solution
generate_solution_vector_ex_13(Mag1,Mag2,Mag3,Solution):-
	Var1=a,
	Var2=b,

	generate_latex_vector_name(Var1,Latex_str_name1),
    generate_latex_vector_name(Var2,Latex_str_name2),

	Den is Mag1*Mag2,
	Num is Mag3,
	string_concatenate([Num,"/",Den],"",Term),
	trignometric_inverse_function(1,Term,Angle),
	string_concatenate(["[string(Given ",Latex_str_name1,".",Latex_str_name2," = ",Mag3,", |",Latex_str_name1,"| = ",Mag1," and ",Latex_str_name2,"| = ",Mag2,". We have)"],"",Sol_0),
	string_concatenate([",string(latex(\\\\theta = \\\\cos^{-1}(\\\\frac{\\\\overrightarrow{a}.\\\\overrightarrow{b}}{|\\\\overrightarrow{a}| |\\\\overrightarrow{b}|})))"],"",Sol_1),
	string_concatenate([",string(latex(\\\\cos^{-1}(\\\\frac{",Num,"}{",Den,"}) = ",Angle,"))]"],"",Sol_2),
	string_concatenate([Sol_0,Sol_1,Sol_2],"",Solution).

%------------------------------------------------------------------End of Question 13---------------------------------------------------------------------------------

%------------------------------------------------------------------Example 14---------------------------------------------------------------------------------
%Question
generate_question_vector_ex_14(Question):-
	Var1=a,
	Var2=b,
	generate_list(List1),
	generate_list(List2),

    generate_latex_vector_name(Var1,Latex_str_name1),
    generate_latex_vector_name(Var2,Latex_str_name2),

  	generate_latex_vector_ijk(List1, [i,j,k],"",Latex_str1),
    generate_latex_vector_ijk(List2, [i,j,k],"",Latex_str2),

    string_concatenate(["[string(Find angle 'latex(\\\\theta)' between the vector ",Latex_str_name1," = ",Latex_str1," and ",Latex_str_name2," = ",Latex_str2,".)]"],"",Question).

%Answer
generate_answer_vector_ex_14(List1,List2,Answer):-
	generate_mul_vector(List1,List2,Mul),
    generate_sum(Mul,0,Num),
    generate_magnitude(List1,0,List1_magnitude),
	generate_magnitude(List2,0,List2_magnitude),
	get_updated_coefficient([[[1,1],[List1_magnitude,1]]],X1,_),
	get_updated_coefficient_result(X1,M1,S1),
	
	get_updated_coefficient([[[1,1],[List2_magnitude,1]]],X2,_),
	get_updated_coefficient_result(X2,M2,S2), 
	(M1=:=1,M2=:=1->
		(S1=:=S2->
			Den is S1,
			simplify(Num,Den,R1,R2),
			string_concatenate([R1,"/",R2],"",Term),
			trignometric_inverse_function(1,Term,Angle),
			string_concatenate(["[string(latex(\\\\theta = cos^{-1}(\\\\frac{",Num,"}{",Den,"}) = ",Angle,"))]"],"",Answer);

			S is (S1)*(S2),
			string_concatenate(["\\\\sqrt{",S,"}"],"",Den),
			string_concatenate(["[string(latex(\\\\theta = cos^{-1}(\\\\frac{",Num,"}{",Den,"})))]"],"",Answer)

		);
		(S1=:=S2->
			Den is M1*M2*S1,
			simplify(Num,Den,R1,R2),
			string_concatenate([R1,"/",R2],"",Term),
			trignometric_inverse_function(1,Term,Angle),
			string_concatenate(["[string(latex(\\\\theta = cos^{-1}(\\\\frac{",Num,"}{",Den,"}) = ",Angle,"))]"],"",Answer);

			S is (S1)*(S2),
			M is (M1)*(M2),
			string_concatenate(["",M,"\\\\sqrt{",S,"}"],"",Den),
			string_concatenate(["[string(latex(\\\\theta = cos^{-1}(\\\\frac{",Num,"}{",Den,"})))]"],"",Answer)

		)
	).

%Solution
generate_solution_vector_ex_14(List1,List2,Solution):-
	Var1=a,
	Var2=b,


    generate_latex_vector_name(Var1,Latex_str_name1),
    generate_latex_vector_name(Var2,Latex_str_name2),

	generate_latex_vector_ijk(List1, [i,j,k],"",Latex_str1),
    generate_latex_vector_ijk(List2, [i,j,k],"",Latex_str2),

	generate_mul_vector(List1,List2,Mul),
    generate_sum(Mul,0,Num),
    generate_magnitude(List1,0,List1_magnitude),
	generate_magnitude(List2,0,List2_magnitude),

	get_updated_coefficient([[[1,1],[List1_magnitude,1]]],X1,_),
	get_updated_coefficient_result(X1,M1,S1),
	
	get_updated_coefficient([[[1,1],[List2_magnitude,1]]],X2,_),
	get_updated_coefficient_result(X2,M2,S2), 
	(M1=:=1,M2=:=1->
		(S1=:=S2->
			Den is S1,
			simplify(Num,Den,R1,R2),
			string_concatenate([R1,"/",R2],"",Term),
			trignometric_inverse_function(1,Term,Angle),
			string_concatenate([",string(Hence the required angle is latex(\\\\theta = cos^{-1}(\\\\frac{",Num,"}{",Den,"}) = ",Angle,"))]"],"",Sol_4);

			S is (S1)*(S2),
			string_concatenate(["\\\\sqrt{",S,"}"],"",Den),
			string_concatenate([",string(Hence the required angle is latex(\\\\theta = cos^{-1}(\\\\frac{",Num,"}{",Den,"})))]"],"",Sol_4)

		);
		(S1=:=S2->
			Den is M1*M2*S1,
			simplify(Num,Den,R1,R2),
			string_concatenate([R1,"/",R2],"",Term),
			trignometric_inverse_function(1,Term,Angle),
			string_concatenate([",string(Hence the required angle is latex(\\\\theta = cos^{-1}(\\\\frac{",Num,"}{",Den,"}) = ",Angle,"))]"],"",Sol_4);

			S is (S1)*(S2),
			M is (M1)*(M2),
			string_concatenate(["",M,"\\\\sqrt{",S,"}"],"",Den),
			string_concatenate([",string(Hence the required angle is latex(\\\\theta = cos^{-1}(\\\\frac{",Num,"}{",Den,"})))]"],"",Sol_4)

		)
	),

	string_concatenate(["[string(The angle latex(\\\\theta) between two vectors ",Latex_str_name1," and ",Latex_str_name2," is given by)"],"",Sol_0),
	string_concatenate([",string(latex(cos\\\\theta = \\\\frac{\\\\overrightarrow{a}.\\\\overrightarrow{b}}{|\\\\overrightarrow{a}|.|\\\\overrightarrow{b}|}) )"],"",Sol_1),
	string_concatenate([",string(Now ",Latex_str_name1,".",Latex_str_name2," = [",Latex_str1,"].[",Latex_str2,"] = ",Num,".)"],"",Sol_2),
	string_concatenate([",string(Therefore, we have latex(cos\\\\theta = \\\\frac{",Num,"}{",Den,"}))"],"",Sol_3),
	string_concatenate([Sol_0,Sol_1,Sol_2,Sol_3,Sol_4],"",Solution).


%------------------------------------------------------------------End of Question 14---------------------------------------------------------------------------------

%------------------------------------------------------------------Example 15---------------------------------------------------------------------------------
%Question
generate_question_vector_ex_15(Question):-
	Var1=a,
	Var2=b,
	generate_list(List1),
	generate_list(List2),

	generate_latex_vector_ijk(List1, [i,j,k],"",Latex_str1),
    generate_latex_vector_ijk(List2, [i,j,k],"",Latex_str2),

    generate_latex_vector_name(Var1,Latex_str_name1),
    generate_latex_vector_name(Var2,Latex_str_name2),

    string_concatenate(["[string(If ",Latex_str_name1," = ",Latex_str1," and ",Latex_str_name2," = ",Latex_str2," , then show that vectors",Latex_str_name1," + ",Latex_str_name2," and ",Latex_str_name1," - ",Latex_str_name2," are perpendicular.)]"],"",Question).
%Answer
generate_answer_vector_ex_15(List1,List2,Answer):-
	Var1=a,
	Var2=b,

	generate_latex_vector_name(Var1,Latex_str_name1),
    generate_latex_vector_name(Var2,Latex_str_name2),

    generate_sum_vector(List1,List2,Sum),
    generate_diff_vector(List1,List2,Diff),
    generate_mul_vector(Sum,Diff,Mul),
    generate_sum(Mul,0,Result),

    (Result=:=0->
    	string_concatenate(["[string(",Latex_str_name1," + ",Latex_str_name2," and ",Latex_str_name1," - ",Latex_str_name2," are perpendicular.)]"],"",Answer);
    	string_concatenate(["[string(",Latex_str_name1," + ",Latex_str_name2," and ",Latex_str_name1," - ",Latex_str_name2," are not perpendicular.)]"],"",Answer)

    ).
%Solution
generate_solution_vector_ex_15(List1,List2,Solution):-
	Var1=a,
	Var2=b,

	generate_latex_vector_name(Var1,Latex_str_name1),
    generate_latex_vector_name(Var2,Latex_str_name2),

    generate_sum_vector(List1,List2,Sum),
    generate_diff_vector(List1,List2,Diff),
    generate_mul_vector(Sum,Diff,Mul),
    generate_sum(Mul,0,Result),

    generate_latex_vector_ijk(List1, [i,j,k],"",Latex_str1),
    generate_latex_vector_ijk(List2, [i,j,k],"",Latex_str2),
    generate_latex_vector_ijk(Sum, [i,j,k],"",Latex_sum),
    generate_latex_vector_ijk(Diff, [i,j,k],"",Latex_diff),

    string_concatenate(["[string(We know that two nonzero vectors are perpendicular if there scalar product is zero.)"],"",Sol_0),
    string_concatenate([",string(Hence",Latex_str_name1," + ",Latex_str_name2," = [",Latex_str1,"] + [",Latex_str2,"] = ",Latex_sum,")"],"",Sol_1),
    string_concatenate([",string(and",Latex_str_name1," - ",Latex_str_name2," = [",Latex_str1,"] - [",Latex_str2,"] = ",Latex_diff,")"],"",Sol_2),
    string_concatenate([",string(So [",Latex_str_name1," + ",Latex_str_name2,"].[",Latex_str_name1," - ",Latex_str_name2,"] = [",Latex_sum,"] . [",Latex_diff,"] = ",Result,")"],"",Sol_3),
    (Result=:=0->
    	string_concatenate([",string(Hence, ",Latex_str_name1," + ",Latex_str_name2," and ",Latex_str_name1," - ",Latex_str_name2," are perpendicular.)]"],"",Sol_4);
    	string_concatenate([",string(Hence, ",Latex_str_name1," + ",Latex_str_name2," and ",Latex_str_name1," - ",Latex_str_name2," are not perpendicular.)]"],"",Sol_4)

    ),
	string_concatenate([Sol_0,Sol_1,Sol_2,Sol_3,Sol_4],"",Solution).

%------------------------------------------------------------------End of Example 15---------------------------------------------------------------------------------

%------------------------------------------------------------------Example 16---------------------------------------------------------------------------------
%Question
generate_question_vector_ex_16(Question):-
	Var1=a,
	Var2=b,
	generate_list(List1),
	generate_list(List2),

	generate_latex_vector_ijk(List1, [i,j,k],"",Latex_str1),
    generate_latex_vector_ijk(List2, [i,j,k],"",Latex_str2),

    generate_latex_vector_name(Var1,Latex_str_name1),
    generate_latex_vector_name(Var2,Latex_str_name2),

    string_concatenate(["[string(Find the projection of the vector ",Latex_str_name1," = ",Latex_str1," on the vector ",Latex_str_name2," = ",Latex_str2,".)]"],"",Question).

%Answer
generate_answer_vector_ex_16(List1,List2,Answer):-
	generate_mul_vector(List1,List2,Mul),
    generate_sum(Mul,0,Result),
	generate_magnitude(List2,0,List2_magnitude),
	get_updated_coefficient([[[1,1],[List2_magnitude,1]]],X1,_),
	get_updated_coefficient_result(X1,M1,S1),
	(Result=:=0->
		string_concatenate(["[string(0)]"],"",Answer);
		(M1=:=1->
			string_concatenate(["[string(latex(\\\\frac{",Result,"}{\\\\sqrt{",S1,"}}))]"],"",Answer);
			string_concatenate(["[string(latex(\\\\frac{",Result,"}{",M1,"\\\\sqrt{",S1,"}}))]"],"",Answer)
		)
	).
%Solution
generate_solution_vector_ex_16(List1,List2,Solution):-
Var1=a,
	Var2=b,
	generate_latex_vector_name(Var1,Latex_str_name1),
    generate_latex_vector_name(Var2,Latex_str_name2),
	generate_vector_ijk(List1, [i,j,k],"",Latex_str1),
    generate_vector_ijk(List2, [i,j,k],"",Latex_str2),
	generate_mul_vector(List1,List2,Mul),
    generate_sum(Mul,0,Result),
	generate_magnitude(List2,0,List2_magnitude),
	generate_magnitude_expression_ijk(List2,"",List2_mag_exp),
	get_updated_coefficient([[[1,1],[List2_magnitude,1]]],X1,_),
	get_updated_coefficient_result(X1,M1,S1),

    string_concatenate(["[string(The projection of vector ",Latex_str_name1," on the vector ",Latex_str_name2," is given by )"],"",Sol_0),
    (Result=:=0->
	    string_concatenate([",string(latex(\\\\frac{1}{|\\\\overrightarrow{b}|}(\\\\overrightarrow{a}).(\\\\overrightarrow{b}) = \\\\frac{[",Latex_str1,"].[",Latex_str2,"]}{",List2_mag_exp,"} = \\\\frac{",Result,"}{\\\\sqrt{",S1,"}}) = 0 )]"],"",Sol_1);

	    (M1=:=1->
	    	string_concatenate([",string(latex(\\\\frac{1}{|\\\\overrightarrow{b}|}(\\\\overrightarrow{a}).(\\\\overrightarrow{b}) = \\\\frac{[",Latex_str1,"].[",Latex_str2,"]}{",List2_mag_exp,"} = \\\\frac{",Result,"}{\\\\sqrt{",S1,"}}) )]"],"",Sol_1);
	    	string_concatenate([",string(latex(\\\\frac{1}{|\\\\overrightarrow{b}|}(\\\\overrightarrow{a}).(\\\\overrightarrow{b}) = \\\\frac{[",Latex_str1,"].[",Latex_str2,"]}{",List2_mag_exp,"} = \\\\frac{",Result,"}{",M1,"\\\\sqrt{",S1,"}}) )]"],"",Sol_1)
	    )
    ),
	string_concatenate([Sol_0,Sol_1],"",Solution).

%------------------------------------------------------------------End of Example 16---------------------------------------------------------------------------------

%------------------------------------------------------------------Example 17---------------------------------------------------------------------------------
%Question
generate_question_vector_ex_17(Question):-
	Var1=a,
	Var2=b,
	generate_magnitude(Mag1),
	generate_magnitude(Mag2),
	generate_magnitude(Mag3),

    generate_latex_vector_name(Var1,Latex_str_name1),
    generate_latex_vector_name(Var2,Latex_str_name2),

    string_concatenate(["[string(Find |",Latex_str_name1," - ",Latex_str_name2,"|, if two vectors ",Latex_str_name1," and ",Latex_str_name2," are such that |",Latex_str_name1,"|=",Mag1,", |",Latex_str_name2,"|=",Mag2," and ",Latex_str_name1,".",Latex_str_name2," = ",Mag3,".)]"],"",Question).
%Answer
generate_answer_vector_ex_17(Mag1,Mag2,Mag3,Answer):-
	Var1=a,
	Var2=b,
	generate_latex_vector_name(Var1,Latex_str_name1),
    generate_latex_vector_name(Var2,Latex_str_name2),
	Result is (Mag1**2+Mag2**2-2*Mag3),
	string_concatenate(["[string(|",Latex_str_name1," - ",Latex_str_name2,"| = latex(\\\\sqrt{",Result,"}))]"],"",Answer).
%Solution
generate_solution_vector_ex_17(Mag1,Mag2,Mag3,Solution):-
	Var1=a,
	Var2=b,
	generate_latex_vector_name(Var1,Latex_str_name1),
    generate_latex_vector_name(Var2,Latex_str_name2),
    Result is (Mag1**2+Mag2**2-2*Mag3),
    
    string_concatenate(["[string(We have)"],"",Sol_0),
    string_concatenate([",string(latex(|\\\\overrightarrow{a}-\\\\overrightarrow{b}|^2 = (\\\\overrightarrow{a}-\\\\overrightarrow{b}).(\\\\overrightarrow{a}-\\\\overrightarrow{b}) ))"],"",Sol_1),
    string_concatenate([",string(=",Latex_str_name1,".",Latex_str_name1,"-",Latex_str_name1,".",Latex_str_name2,"-",Latex_str_name2,".",Latex_str_name1,"+",Latex_str_name2,".",Latex_str_name2,")"],"",Sol_2),
    string_concatenate([",string(= latex(|\\\\overrightarrow{a}|^2 - 2(\\\\overrightarrow{a}.\\\\overrightarrow{b}) + |\\\\overrightarrow{b}|^2) )"],"",Sol_3),
    string_concatenate([",string(= latex( (",Mag1,")^2 - 2.(",Mag3,") + (",Mag2,")^2 ))"],"",Sol_4),
	string_concatenate([",string(Therefore |",Latex_str_name1," - ",Latex_str_name2,"| = latex(\\\\sqrt{",Result,"}))]"],"",Sol_5),

	string_concatenate([Sol_0,Sol_1,Sol_2,Sol_3,Sol_4,Sol_5],"",Solution).

%------------------------------------------------------------------End of Example 17---------------------------------------------------------------------------------

%------------------------------------------------------------------Example 18---------------------------------------------------------------------------------

%Question
generate_question_vector_ex_18(Question):-
	Var1=x,
	Var2=a,
	generate_magnitude(Mag),
	
    generate_latex_vector_name(Var1,Latex_str_name1),
    generate_latex_vector_name(Var2,Latex_str_name2),

    string_concatenate(["[string(If ",Latex_str_name2," is a unit vector and latex((\\\\overrightarrow{x}-\\\\overrightarrow{a}).(\\\\overrightarrow{x}+\\\\overrightarrow{a})=",Mag,"), then find |",Latex_str_name1,"|.)]"],"",Question).

%Answer
generate_answer_vector_ex_18(Mag,Answer):-
	Var1=x,
	generate_latex_vector_name(Var1,Latex_str_name1),
	Result is Mag+1,
	get_updated_coefficient([[[1,1],[Result,1]]],X,_),
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
	string_concatenate(["[string(|",Latex_str_name1,"| = latex(",Num,"))]"],"",Answer).
%Solution
generate_solution_vector_ex_18(Mag,Solution):-
	Var1=x,
	Var2=a,
	
    generate_latex_vector_name(Var1,Latex_str_name1),
    generate_latex_vector_name(Var2,Latex_str_name2),
	Result is Mag+1,

    string_concatenate(["[string(Since ",Latex_str_name2," is a unit vector, |",Latex_str_name2,"| = 1. Also,)"],"",Sol_0),
    string_concatenate([",string(latex((\\\\overrightarrow{x}-\\\\overrightarrow{a}).(\\\\overrightarrow{x}+\\\\overrightarrow{a})=",Mag,") )"],"",Sol_1),
    string_concatenate([",string(or  ",Latex_str_name1,".",Latex_str_name1,"+",Latex_str_name1,".",Latex_str_name2,"-",Latex_str_name2,".",Latex_str_name1,"-",Latex_str_name2,".",Latex_str_name2," = ",Mag,")"],"",Sol_2),
    string_concatenate([",string(or  latex(|\\\\overrightarrow{x}|^2 - 1 = ",Mag,") i.e. latex(|\\\\overrightarrow{x}|^2) = ",Result,")"],"",Sol_3),
	get_updated_coefficient([[[1,1],[Result,1]]],X,_),
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
	string_concatenate([",string(|",Latex_str_name1,"| = latex(",Num,") (as magnitude of a vector is non negative))]"],"",Sol_4),
	string_concatenate([Sol_0,Sol_1,Sol_2,Sol_3,Sol_4],"",Solution).

%------------------------------------------------------------------End of Example 18---------------------------------------------------------------------------------

%------------------------------------------------------------------Example 21---------------------------------------------------------------------------------
%Question
generate_question_vector_ex_21(Question):-
	generate_list(Point1),
	generate_list(Point2),
	generate_list(Point3),

	generate_latex_vector_ijk(Point1, [i,j,k],"",Latex_str1),
    generate_latex_vector_ijk(Point2, [i,j,k],"",Latex_str2),
    generate_latex_vector_ijk(Point3, [i,j,k],"",Latex_str3),

    string_concatenate(["[string(Show that the points A[",Latex_str1,"], B[",Latex_str2,"] and C[",Latex_str3,"] are collinear or not.)]"],"",Question).

%Answer
generate_answer_vector_ex_21(Point1,Point2,Point3,Answer):-
	generate_diff_vector(Point2,Point1,Diff_1),
	generate_diff_vector(Point3,Point2,Diff_2),
	generate_diff_vector(Point1,Point3,Diff_3),

    generate_magnitude(Diff_1,0,Diff_1_magnitude),
    generate_magnitude(Diff_2,0,Diff_2_magnitude),
    generate_magnitude(Diff_3,0,Diff_3_magnitude),

    get_updated_coefficient([[[1,1],[Diff_1_magnitude,1]]],X1,_),
	get_updated_coefficient_result(X1,M1,S1),
	
	get_updated_coefficient([[[1,1],[Diff_2_magnitude,1]]],X2,_),
	get_updated_coefficient_result(X2,M2,S2), 
	
	get_updated_coefficient([[[1,1],[Diff_3_magnitude,1]]],X3,_),
	get_updated_coefficient_result(X3,M3,S3),

	(S1=\=S2->
    	string_concatenate(["[string(The points A, B and C are not collinear.)]"],"",Answer);
    	(S2=\=S3->
    		string_concatenate(["[string(The points A, B and C are not collinear.)]"],"",Answer);
			(M3=:=M1+M2->
    			string_concatenate(["[string(The points A, B and C are collinear.)]"],"",Answer);
	    	    (M2=:=M3+M1->
    				string_concatenate(["[string(The points A, B and C are collinear.)]"],"",Answer);
	     			(M1=:=M3+M2->
    					string_concatenate(["[string(The points A, B and C are collinear.)]"],"",Answer);
    					string_concatenate(["[string(The points A, B and C are not collinear.)]"],"",Answer)
					)
				)
    		)
    	)
	).
%Solution
generate_solution_vector_ex_21(Point1,Point2,Point3,Solution):-
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

	get_updated_coefficient([[[1,1],[Diff_1_magnitude,1]]],X1,_),
	get_updated_coefficient_result(X1,M1,S1),
	
	get_updated_coefficient([[[1,1],[Diff_2_magnitude,1]]],X2,_),
	get_updated_coefficient_result(X2,M2,S2), 
	
	get_updated_coefficient([[[1,1],[Diff_3_magnitude,1]]],X3,_),
	get_updated_coefficient_result(X3,M3,S3),


    string_concatenate(["[string(We have,)"],"",Sol_0),
	string_concatenate([",string(latex(\\\\overrightarrow{AB}) = ",Latex_diff_exp_1," = ",Latex_Diff_1,")"],"",Sol_1),
	string_concatenate([",string(latex(\\\\overrightarrow{BC}) = ",Latex_diff_exp_2," = ",Latex_Diff_2,")"],"",Sol_2),
	string_concatenate([",string(latex(\\\\overrightarrow{CA}) = ",Latex_diff_exp_3," = ",Latex_Diff_3,")"],"",Sol_3),

	(M1=:=1->
		string_concatenate([",string(latex(|\\\\overrightarrow{AB}| = \\\\sqrt{",S1,"}) )"],"",Sol_4);
		(S1=:=1->
			string_concatenate([",string(latex(|\\\\overrightarrow{AB}| = ",M1,") )"],"",Sol_4);
			string_concatenate([",string(latex(|\\\\overrightarrow{AB}| = ",M1,"\\\\sqrt{",S1,"}) )"],"",Sol_4)
		)
	),


	(M2=:=1->
		string_concatenate([",string(latex(|\\\\overrightarrow{BC}| = \\\\sqrt{",S2,"}) )"],"",Sol_5);
		(S2=:=1->
			string_concatenate([",string(latex(|\\\\overrightarrow{BC}| = ",M2,") )"],"",Sol_5);
			string_concatenate([",string(latex(|\\\\overrightarrow{BC}| = ",M2,"\\\\sqrt{",S2,"}) )"],"",Sol_5)
		)
	),

	(M3=:=1->
		string_concatenate([",string(latex(|\\\\overrightarrow{AC}| = \\\\sqrt{",S3,"}) )"],"",Sol_6);
		(S3=:=1->
			string_concatenate([",string(latex(|\\\\overrightarrow{AC}| = ",M3,") )"],"",Sol_6);
			string_concatenate([",string(latex(|\\\\overrightarrow{AC}| = ",M3,"\\\\sqrt{",S3,"}) )"],"",Sol_6)
		)
	),

	(S1=\=S2->
    	string_concatenate([",string(Hence The points A, B and C are not collinear.)]"],"",Sol_7),
    	string_concatenate([Sol_0,Sol_1,Sol_2,Sol_3,Sol_4,Sol_5,Sol_6,Sol_7],"",Solution);
    	(S2=\=S3->
    		string_concatenate([",string(Hence The points A, B and C are not collinear.)]"],"",Sol_7),
    	    string_concatenate([Sol_0,Sol_1,Sol_2,Sol_3,Sol_4,Sol_5,Sol_6,Sol_7],"",Solution);
			(M3=:=M1+M2->
				string_concatenate([",string(latex(|\\\\overrightarrow{AC}|) = latex(|\\\\overrightarrow{AB}|) + latex(|\\\\overrightarrow{BC}|))"],"",Sol_7),
    			string_concatenate([",string(Hence The points A, B and C are collinear.)]"],"",Sol_8),
    	    	string_concatenate([Sol_0,Sol_1,Sol_2,Sol_3,Sol_4,Sol_5,Sol_6,Sol_7,Sol_8],"",Solution);

	    	    (M2=:=M3+M1->
					string_concatenate([",string(latex(|\\\\overrightarrow{BC}|) = latex(|\\\\overrightarrow{AB}|) + latex(|\\\\overrightarrow{AC}|))"],"",Sol_7),
	    			string_concatenate([",string(Hence The points A, B and C are collinear.)]"],"",Sol_8),
	    	    	string_concatenate([Sol_0,Sol_1,Sol_2,Sol_3,Sol_4,Sol_5,Sol_6,Sol_7,Sol_8],"",Solution);	     			
    	    		(M1=:=M3+M2->
						string_concatenate([",string(latex(|\\\\overrightarrow{AB}|) = latex(|\\\\overrightarrow{BC}|) + latex(|\\\\overrightarrow{AC}|))"],"",Sol_7),
		    			string_concatenate([",string(Hence The points A, B and C are collinear.)]"],"",Sol_8),
		    	    	string_concatenate([Sol_0,Sol_1,Sol_2,Sol_3,Sol_4,Sol_5,Sol_6,Sol_7,Sol_8],"",Solution); 

		    	    	string_concatenate(["[string(Hence The points A, B and C are not collinear.)]"],"",Sol_7),
		    			string_concatenate([Sol_0,Sol_1,Sol_2,Sol_3,Sol_4,Sol_5,Sol_6,Sol_7],"",Solution)
					)
				)
    		)
    	)
	).

%------------------------------------------------------------------End of Example 21---------------------------------------------------------------------------------

%------------------------------------------------------------------Question 1---------------------------------------------------------------------------------
%Question
generate_question_vector_10_3_1(Question):-
	Var1=a,
	Var2=b,
	generate_magnitude(Mag1),
	generate_magnitude(Mag2),
	generate_magnitude(Mag3),

    generate_latex_vector_name(Var1,Latex_str_name1),
    generate_latex_vector_name(Var2,Latex_str_name2),

    string_concatenate(["[string(Find the angle between two vectors ",Latex_str_name1," and ",Latex_str_name2," with magnitude ",Mag1," and ",Mag2," respectively having ",Latex_str_name1,".",Latex_str_name2," = ",Mag3,".)]"],"",Question).

%Answer
generate_answer_vector_10_3_1(Mag1,Mag2,Mag3,Answer):-
	Den is Mag1*Mag2,
	Num is Mag3,
	string_concatenate([Num,"/",Den],"",Term),
	trignometric_inverse_function(1,Term,Angle),
	string_concatenate(["[string(latex(",Angle,"))]"],"",Answer).
%Solution
generate_solution_vector_10_3_1(Mag1,Mag2,Mag3,Solution):-
	Var1=a,
	Var2=b,

	generate_latex_vector_name(Var1,Latex_str_name1),
    generate_latex_vector_name(Var2,Latex_str_name2),

	Den is Mag1*Mag2,
	Num is Mag3,
	string_concatenate([Num,"/",Den],"",Term),
	trignometric_inverse_function(1,Term,Angle),
	string_concatenate(["[string(Given ",Latex_str_name1,".",Latex_str_name2," = ",Mag3,", |",Latex_str_name1,"| = ",Mag1," and ",Latex_str_name2,"| = ",Mag2,". We have)"],"",Sol_0),
	string_concatenate([",string(latex(\\\\theta = \\\\cos^{-1}(\\\\frac{\\\\overrightarrow{a}.\\\\overrightarrow{b}}{|\\\\overrightarrow{a}| |\\\\overrightarrow{b}|})))"],"",Sol_1),
	string_concatenate([",string(latex(\\\\cos^{-1}(\\\\frac{",Num,"}{",Den,"}) = ",Angle,"))]"],"",Sol_2),
	string_concatenate([Sol_0,Sol_1,Sol_2],"",Solution).
%------------------------------------------------------------------End of Question 1---------------------------------------------------------------------------------

%------------------------------------------------------------------Question 2---------------------------------------------------------------------------------
%Question
generate_question_vector_10_3_2(Question):-
	generate_list(List1),
	generate_list(List2),

  	generate_latex_vector_ijk(List1, [i,j,k],"",Latex_str1),
    generate_latex_vector_ijk(List2, [i,j,k],"",Latex_str2),

    string_concatenate(["[string(Find angle between the vector ",Latex_str1," and ",Latex_str2,".)]"],"",Question).

%Answer
generate_answer_vector_10_3_2(List1,List2,Answer):-
	generate_mul_vector(List1,List2,Mul),
    generate_sum(Mul,0,Num),
    generate_magnitude(List1,0,List1_magnitude),
	generate_magnitude(List2,0,List2_magnitude),
	get_updated_coefficient([[[1,1],[List1_magnitude,1]]],X1,_),
	get_updated_coefficient_result(X1,M1,S1),
	
	get_updated_coefficient([[[1,1],[List2_magnitude,1]]],X2,_),
	get_updated_coefficient_result(X2,M2,S2), 
	(M1=:=1,M2=:=1->
		(S1=:=S2->
			Den is S1,
			simplify(Num,Den,R1,R2),
			string_concatenate([R1,"/",R2],"",Term),
			trignometric_inverse_function(1,Term,Angle),
			string_concatenate(["[string(latex(\\\\theta = cos^{-1}(\\\\frac{",Num,"}{",Den,"}) = ",Angle,"))]"],"",Answer);

			S is (S1)*(S2),
			string_concatenate(["\\\\sqrt{",S,"}"],"",Den),
			string_concatenate(["[string(latex(\\\\theta = cos^{-1}(\\\\frac{",Num,"}{",Den,"})))]"],"",Answer)

		);
		(S1=:=S2->
			Den is M1*M2*S1,
			simplify(Num,Den,R1,R2),
			string_concatenate([R1,"/",R2],"",Term),
			trignometric_inverse_function(1,Term,Angle),
			string_concatenate(["[string(latex(\\\\theta = cos^{-1}(\\\\frac{",Num,"}{",Den,"}) = ",Angle,"))]"],"",Answer);

			S is (S1)*(S2),
			M is (M1)*(M2),
			string_concatenate(["",M,"\\\\sqrt{",S,"}"],"",Den),
			string_concatenate(["[string(latex(\\\\theta = cos^{-1}(\\\\frac{",Num,"}{",Den,"})))]"],"",Answer)

		)
	).

%Solution
generate_solution_vector_10_3_2(List1,List2,Solution):-
	Var1=a,
	Var2=b,

    generate_latex_vector_name(Var1,Latex_str_name1),
    generate_latex_vector_name(Var2,Latex_str_name2),

	generate_latex_vector_ijk(List1, [i,j,k],"",Latex_str1),
    generate_latex_vector_ijk(List2, [i,j,k],"",Latex_str2),

	generate_mul_vector(List1,List2,Mul),
    generate_sum(Mul,0,Num),
    generate_magnitude(List1,0,List1_magnitude),
	generate_magnitude(List2,0,List2_magnitude),

	get_updated_coefficient([[[1,1],[List1_magnitude,1]]],X1,_),
	get_updated_coefficient_result(X1,M1,S1),
	
	get_updated_coefficient([[[1,1],[List2_magnitude,1]]],X2,_),
	get_updated_coefficient_result(X2,M2,S2), 

	(M1=:=1,M2=:=1->
		(S1=:=S2->
			Den is S1,
			simplify(Num,Den,R1,R2),
			string_concatenate([R1,"/",R2],"",Term),
			trignometric_inverse_function(1,Term,Angle),
			string_concatenate([",string(Hence the required angle is latex(\\\\theta = cos^{-1}(\\\\frac{",Num,"}{",Den,"}) = ",Angle,"))]"],"",Sol_5);

			S is (S1)*(S2),
			string_concatenate(["\\\\sqrt{",S,"}"],"",Den),
			string_concatenate([",string(Hence the required angle is latex(\\\\theta = cos^{-1}(\\\\frac{",Num,"}{",Den,"})))]"],"",Sol_5)

		);
		(S1=:=S2->
			Den is M1*M2*S1,
			simplify(Num,Den,R1,R2),
			string_concatenate([R1,"/",R2],"",Term),
			trignometric_inverse_function(1,Term,Angle),
			string_concatenate([",string(Hence the required angle is latex(\\\\theta = cos^{-1}(\\\\frac{",Num,"}{",Den,"}) = ",Angle,"))]"],"",Sol_5);

			S is (S1)*(S2),
			M is (M1)*(M2),
			string_concatenate(["",M,"\\\\sqrt{",S,"}"],"",Den),
			string_concatenate([",string(Hence the required angle is latex(\\\\theta = cos^{-1}(\\\\frac{",Num,"}{",Den,"})))]"],"",Sol_5)

		)
	),
	string_concatenate(["[string(Let ",Latex_str_name1," = ",Latex_str1," and ",Latex_str_name2," = ",Latex_str2,")"],"",Sol_0),
	string_concatenate([",string(The angle latex(\\\\theta) between two vectors ",Latex_str_name1," and ",Latex_str_name2," is given by)"],"",Sol_1),
	string_concatenate([",string(latex(cos\\\\theta = \\\\frac{\\\\overrightarrow{a}.\\\\overrightarrow{b}}{|\\\\overrightarrow{a}|.|\\\\overrightarrow{b}|}) )"],"",Sol_2),
	string_concatenate([",string(Now ",Latex_str_name1,".",Latex_str_name2," = [",Latex_str1,"].[",Latex_str2,"] = ",Num,".)"],"",Sol_3),
	string_concatenate([",string(Therefore, we have latex(cos\\\\theta = \\\\frac{",Num,"}{",Den,"}))"],"",Sol_4),
	string_concatenate([Sol_0,Sol_1,Sol_2,Sol_3,Sol_4,Sol_5],"",Solution).

%------------------------------------------------------------------End of Question 2---------------------------------------------------------------------------------

%------------------------------------------------------------------Question 3 and 4---------------------------------------------------------------------------------
%Question
generate_question_vector_10_3_3(Question):-
	Var1=a,
	Var2=b,
	generate_list(List1),
	generate_list(List2),

	generate_latex_vector_ijk(List1, [i,j,k],"",Latex_str1),
    generate_latex_vector_ijk(List2, [i,j,k],"",Latex_str2),

    generate_latex_vector_name(Var1,Latex_str_name1),
    generate_latex_vector_name(Var2,Latex_str_name2),

    string_concatenate(["[string(Find the projection of the vector ",Latex_str_name1," = ",Latex_str1," on the vector ",Latex_str_name2," = ",Latex_str2,".)]"],"",Question).

%Answer
generate_answer_vector_10_3_3(List1,List2,Answer):-
	generate_mul_vector(List1,List2,Mul),
    generate_sum(Mul,0,Result),
	generate_magnitude(List2,0,List2_magnitude),
	get_updated_coefficient([[[1,1],[List2_magnitude,1]]],X1,_),
	get_updated_coefficient_result(X1,M1,S1),
	(Result=:=0->
		string_concatenate(["[string(0)]"],"",Answer);
		(M1=:=1->
			string_concatenate(["[string(latex(\\\\frac{",Result,"}{\\\\sqrt{",S1,"}}))]"],"",Answer);
			string_concatenate(["[string(latex(\\\\frac{",Result,"}{",M1,"\\\\sqrt{",S1,"}}))]"],"",Answer)
		)
	).
%Solution
generate_solution_vector_10_3_3(List1,List2,Solution):-
	Var1=a,
	Var2=b,
	generate_latex_vector_name(Var1,Latex_str_name1),
    generate_latex_vector_name(Var2,Latex_str_name2),
	generate_vector_ijk(List1, [i,j,k],"",Latex_str1),
    generate_vector_ijk(List2, [i,j,k],"",Latex_str2),
	generate_mul_vector(List1,List2,Mul),
    generate_sum(Mul,0,Result),
	generate_magnitude(List2,0,List2_magnitude),
	generate_magnitude_expression_ijk(List2,"",List2_mag_exp),
	get_updated_coefficient([[[1,1],[List2_magnitude,1]]],X1,_),
	get_updated_coefficient_result(X1,M1,S1),

    string_concatenate(["[string(The projection of vector ",Latex_str_name1," on the vector ",Latex_str_name2," is given by )"],"",Sol_0),
    (Result=:=0->
	    string_concatenate([",string(latex(\\\\frac{1}{|\\\\overrightarrow{b}|}(\\\\overrightarrow{a}).(\\\\overrightarrow{b}) = \\\\frac{[",Latex_str1,"].[",Latex_str2,"]}{",List2_mag_exp,"} = \\\\frac{",Result,"}{\\\\sqrt{",S1,"}}) = 0 )]"],"",Sol_1);

	    (M1=:=1->
	    	string_concatenate([",string(latex(\\\\frac{1}{|\\\\overrightarrow{b}|}(\\\\overrightarrow{a}).(\\\\overrightarrow{b}) = \\\\frac{[",Latex_str1,"].[",Latex_str2,"]}{",List2_mag_exp,"} = \\\\frac{",Result,"}{\\\\sqrt{",S1,"}}) )]"],"",Sol_1);
	    	string_concatenate([",string(latex(\\\\frac{1}{|\\\\overrightarrow{b}|}(\\\\overrightarrow{a}).(\\\\overrightarrow{b}) = \\\\frac{[",Latex_str1,"].[",Latex_str2,"]}{",List2_mag_exp,"} = \\\\frac{",Result,"}{",M1,"\\\\sqrt{",S1,"}}) )]"],"",Sol_1)
	    )
    ),
	string_concatenate([Sol_0,Sol_1],"",Solution).
%------------------------------------------------------------------End of Question 3 and 4---------------------------------------------------------------------------------

%------------------------------------------------------------------Question 5---------------------------------------------------------------------------------
%Question
generate_question_vector_10_3_5(Question):-
	generate_magnitude(Mag1),
	generate_magnitude(Mag2),
	generate_magnitude(Mag3),

	generate_list(List1),
	generate_list(List2),
	generate_list(List3),

	generate_latex_vector_ijk(List1, [i,j,k],"",Latex_str1),
    generate_latex_vector_ijk(List2, [i,j,k],"",Latex_str2),
    generate_latex_vector_ijk(List3, [i,j,k],"",Latex_str3),
    string_concatenate(["[string(Show that each of the three given vectors are unit vector or not: )"],"",Sol_0),
    string_concatenate([",string(latex(\\\\frac{1}{",Mag1,"})[",Latex_str1,"], latex(\\\\frac{1}{",Mag2,"})[",Latex_str2,"], latex(\\\\frac{1}{",Mag3,"})[",Latex_str3,"] )"],"",Sol_1),
    string_concatenate([",string(Also check whether they are mutually perpendicular to each other or not)]"],"",Sol_2),
	string_concatenate([Sol_0,Sol_1,Sol_2],"",Question).

%Answer
generate_answer_vector_10_3_5(Mag1,Mag2,Mag3,List1,List2,List3,Answer):-
	list_power_sum(List1,2,0,Pow_sum1),
	list_power_sum(List2,2,0,Pow_sum2),
	list_power_sum(List3,2,0,Pow_sum3),

	Pow_mag1 is (Mag1**2),
	Pow_mag2 is (Mag2**2),
	Pow_mag3 is (Mag3**2),

	(Pow_sum1=:=Pow_mag1->
		(Pow_sum2=:=Pow_mag2->
			(Pow_sum3=:=Pow_mag3->
				string_concatenate(["[string(Given three vectors are unit vectors.)"],"",Ans_0);
				string_concatenate(["[string(Given three vectors are not unit vectors.)"],"",Ans_0)
			);
			string_concatenate(["[string(Given three vectors are not unit vectors.)"],"",Ans_0)
		);
		string_concatenate(["[string(Given three vectors are not unit vectors.)"],"",Ans_0)
	),
	generate_mul_vector(List1,List2,Mul1),
    generate_sum(Mul1,0,Result1),

    generate_mul_vector(List2,List3,Mul2),
    generate_sum(Mul2,0,Result2),

    generate_mul_vector(List1,List3,Mul3),
    generate_sum(Mul3,0,Result3),
	
	(Result1=:=0->
		(Result2=:=0->
			(Result3=:=0->
				string_concatenate([",string(Given three vectors are mutually perpendicular to each other.)]"],"",Ans_1);
				string_concatenate([",string(Given three vectors are not mutually perpendicular to each other.)]"],"",Ans_1)
			);
				string_concatenate([",string(Given three vectors are not mutually perpendicular to each other.)]"],"",Ans_1)
		);
				string_concatenate([",string(Given three vectors are not mutually perpendicular to each other.)]"],"",Ans_1)
	),
	string_concatenate([Ans_0,Ans_1],"",Answer).

%Solution
generate_solution_vector_10_3_5(Mag1,Mag2,Mag3,List1,List2,List3,Solution):-
	Var1=a,
	Var2=b,
	Var3=c,
	generate_latex_vector_name(Var1,Latex_str_name1),
    generate_latex_vector_name(Var2,Latex_str_name2),
    generate_latex_vector_name(Var3,Latex_str_name3),

	generate_latex_vector_ijk(List1, [i,j,k],"",Latex_str1),
    generate_latex_vector_ijk(List2, [i,j,k],"",Latex_str2),
    generate_latex_vector_ijk(List3, [i,j,k],"",Latex_str3),

    generate_magnitude_den_expression_ijk(List1,Mag1,"",Mag_den_expression_1),
    generate_magnitude_den_expression_ijk(List2,Mag2,"",Mag_den_expression_2),
    generate_magnitude_den_expression_ijk(List3,Mag3,"",Mag_den_expression_3),

    list_power_sum(List1,2,0,Pow_sum1),
	list_power_sum(List2,2,0,Pow_sum2),
	list_power_sum(List3,2,0,Pow_sum3),

	Pow_mag1 is (Mag1**2),
	Pow_mag2 is (Mag2**2),
	Pow_mag3 is (Mag3**2),

	Unit_result_1 is (Pow_sum1/Pow_mag1),
	Unit_result_2 is (Pow_sum2/Pow_mag2),
	Unit_result_3 is (Pow_sum3/Pow_mag3),

	generate_mul_vector(List1,List2,Mul1),
    generate_sum(Mul1,0,Result1),

    generate_mul_vector(List2,List3,Mul2),
    generate_sum(Mul2,0,Result2),

    generate_mul_vector(List1,List3,Mul3),
    generate_sum(Mul3,0,Result3),

	string_concatenate(["[string(",Latex_str_name1," = latex(\\\\frac{1}{",Mag1,"})[",Latex_str1,"])"],"",Sol_0),
	string_concatenate([",string(",Latex_str_name2," = latex(\\\\frac{1}{",Mag2,"})[",Latex_str2,"])"],"",Sol_1),
	string_concatenate([",string(",Latex_str_name3," = latex(\\\\frac{1}{",Mag3,"})[",Latex_str3,"])"],"",Sol_2),
	string_concatenate([",string(latex(|\\\\overrightarrow{a}| = ",Mag_den_expression_1," = ",Unit_result_1,"))"],"",Sol_3),
	string_concatenate([",string(latex(|\\\\overrightarrow{b}| = ",Mag_den_expression_2," = ",Unit_result_2,"))"],"",Sol_4),
	string_concatenate([",string(latex(|\\\\overrightarrow{c}| = ",Mag_den_expression_3," = ",Unit_result_3,"))"],"",Sol_5),
	(Pow_sum1=:=Pow_mag1->
		(Pow_sum2=:=Pow_mag2->
			(Pow_sum3=:=Pow_mag3->
				string_concatenate([",string(So,Given three vectors are unit vectors.)"],"",Sol_6);
				string_concatenate([",string(So,Given three vectors are not unit vectors.)"],"",Sol_6)
			);
			string_concatenate([",string(So,Given three vectors are not unit vectors.)"],"",Sol_6)
		);
		string_concatenate([",string(So,Given three vectors are not unit vectors.)"],"",Sol_6)
	),
	string_concatenate([",string(latex(\\\\overrightarrow{a}.\\\\overrightarrow{b} = \\\\frac{1}{",Mag1,"})[",Latex_str1,"].latex(\\\\frac{1}{",Mag2,"})[",Latex_str2,"] = ",Result1,")"],"",Sol_7),
	string_concatenate([",string(latex(\\\\overrightarrow{b}.\\\\overrightarrow{c} = \\\\frac{1}{",Mag2,"})[",Latex_str2,"].latex(\\\\frac{1}{",Mag3,"})[",Latex_str3,"] = ",Result2,")"],"",Sol_8),
	string_concatenate([",string(latex(\\\\overrightarrow{a}.\\\\overrightarrow{c} = \\\\frac{1}{",Mag1,"})[",Latex_str1,"].latex(\\\\frac{1}{",Mag3,"})[",Latex_str3,"] = ",Result3,")"],"",Sol_9),
	(Result1=:=0->
		(Result2=:=0->
			(Result3=:=0->
				string_concatenate([",string(So,Given three vectors are mutually perpendicular to each other.)]"],"",Sol_10);
				string_concatenate([",string(So,Given three vectors are not mutually perpendicular to each other.)]"],"",Sol_10)
			);
				string_concatenate([",string(So,Given three vectors are not mutually perpendicular to each other.)]"],"",Sol_10)
		);
				string_concatenate([",string(So,Given three vectors are not mutually perpendicular to each other.)]"],"",Sol_10)
	),
	string_concatenate([Sol_0,Sol_1,Sol_2,Sol_3,Sol_4,Sol_5,Sol_6,Sol_7,Sol_8,Sol_9,Sol_10],"",Solution).

%------------------------------------------------------------------End of Question 5---------------------------------------------------------------------------------

%------------------------------------------------------------------Question 6---------------------------------------------------------------------------------
%Question
generate_question_vector_10_3_6(Question):-
	Var1=a,
	Var2=b,
	generate_magnitude(Mag1),
	generate_magnitude(Mag2),
	generate_latex_vector_name(Var1,Latex_str_name1),
    generate_latex_vector_name(Var2,Latex_str_name2),
    string_concatenate(["[string(Find |",Latex_str_name1,"| and |",Latex_str_name2,"|, if [",Latex_str_name1," + ",Latex_str_name2,"].[",Latex_str_name1," - ",Latex_str_name2,"] = ",Mag1," and |",Latex_str_name1,"| = ",Mag2,"|",Latex_str_name2,"|.)]"],"",Question).
%Answer
generate_answer_vector_10_3_6(Mag1,Mag2,Answer):-
	Var1=a,
	Var2=b,
	generate_latex_vector_name(Var1,Latex_str_name1),
    generate_latex_vector_name(Var2,Latex_str_name2),
	Mul is (Mag2**2-1),
	get_updated_coefficient([[[1,1],[Mag1,1]]],X1,_),
	get_updated_coefficient_result(X1,M1,S1),
	get_updated_coefficient([[[1,1],[Mul,1]]],X2,_),
	get_updated_coefficient_result(X2,M2,S2),

	(M1=:=1->
		(S1=:=1->
			Num is S1;
			string_concatenate(["\\\\sqrt{",S1,"}"],"",Num)
		);
		(S1=:=1->
			Num is M1*S1;
			string_concatenate(["",M1,"\\\\sqrt{",S1,"}"],"",Num)
		)
	),

	(M2=:=1->
		(S2=:=1->
			Den is S2;
			string_concatenate(["\\\\sqrt{",S2,"}"],"",Den)
		);
		(S2=:=1->
			Den is M2*S2;
			string_concatenate(["",M2,"\\\\sqrt{",S2,"}"],"",Den)
		)
	),
	string_concatenate(["[string(|",Latex_str_name2,"| = latex(\\\\frac{",Num,"}{",Den,"}))"],"",Ans_0),
	(Mag2=:=1->
		(M1=:=1->
			(S1=:=1->
				Num1 is S1;
				string_concatenate(["\\\\sqrt{",S1,"}"],"",Num1)
			);
			(S1=:=1->
				Num1 is M1*S1;
				string_concatenate(["",M1,"\\\\sqrt{",S1,"}"],"",Num1)
			)
		);
		(S1=:=1->
				Num1 is Mag2*M1;
				M is Mag2*M1,
				string_concatenate(["",M,"\\\\sqrt{",S1,"}"],"",Num1)
		)
	),
	string_concatenate([",string(|",Latex_str_name1,"| = latex(\\\\frac{",Num1,"}{",Den,"}))]"],"",Ans_1),
	string_concatenate([Ans_0,Ans_1],"",Answer).
%Solution
generate_solution_vector_10_3_6(Mag1,Mag2,Solution):-
	Var1=a,
	Var2=b,
	generate_latex_vector_name(Var1,Latex_str_name1),
    generate_latex_vector_name(Var2,Latex_str_name2),
	
	Mul is (Mag2**2-1),
	Mul1 is (Mag2**2),
	get_updated_coefficient([[[1,1],[Mag1,1]]],X1,_),
	get_updated_coefficient_result(X1,M1,S1),
	get_updated_coefficient([[[1,1],[Mul,1]]],X2,_),
	get_updated_coefficient_result(X2,M2,S2),

    string_concatenate(["[string(Given,[",Latex_str_name1," + ",Latex_str_name2,"].[",Latex_str_name1," - ",Latex_str_name2,"] = ",Mag1,")"],"",Sol_0),
    string_concatenate([",string(",Latex_str_name1,".",Latex_str_name1,"-",Latex_str_name1,".",Latex_str_name2,"+",Latex_str_name2,".",Latex_str_name1,"-",Latex_str_name2,".",Latex_str_name2," = ",Mag1,")"],"",Sol_1),
    string_concatenate([",string(latex(|\\\\overrightarrow{a}|^2 - |\\\\overrightarrow{b}|^2 = ",Mag1,"))"],"",Sol_2),
    string_concatenate([",string(latex((",Mag2,"|\\\\overrightarrow{b}|)^2 - |\\\\overrightarrow{b}|^2 = ",Mag1,"))"],"",Sol_3),
    string_concatenate([",string(",Mul1,"latex(|\\\\overrightarrow{b}|^2 - |\\\\overrightarrow{b}|^2 = ",Mag1,"))"],"",Sol_4),
   	string_concatenate([",string(",Mul,"latex(|\\\\overrightarrow{b}|^2 = ",Mag1,"))"],"",Sol_5),
   	string_concatenate([",string(latex(|\\\\overrightarrow{b}|^2 = \\\\frac{",Mag1,"}{",Mul,"}))"],"",Sol_6),
	(M1=:=1->
		(S1=:=1->
			Num is S1;
			string_concatenate(["\\\\sqrt{",S1,"}"],"",Num)
		);
		(S1=:=1->
			Num is M1*S1;
			string_concatenate(["",M1,"\\\\sqrt{",S1,"}"],"",Num)
		)
	),

	(M2=:=1->
		(S2=:=1->
			Den is S2;
			string_concatenate(["\\\\sqrt{",S2,"}"],"",Den)
		);
		(S2=:=1->
			Den is M2*S2;
			string_concatenate(["",M2,"\\\\sqrt{",S2,"}"],"",Den)
		)
	),
	string_concatenate([",string(|",Latex_str_name2,"| = latex(\\\\frac{",Num,"}{",Den,"}))"],"",Sol_7),
	string_concatenate([",string(|",Latex_str_name1,"| = ",Mag2,"|",Latex_str_name2,"|)"],"",Sol_8),
	(Mag2=:=1->
		(M1=:=1->
			(S1=:=1->
				Num1 is S1;
				string_concatenate(["\\\\sqrt{",S1,"}"],"",Num1)
			);
			(S1=:=1->
				Num1 is M1*S1;
				string_concatenate(["",M1,"\\\\sqrt{",S1,"}"],"",Num1)
			)
		);
		(S1=:=1->
				Num1 is Mag2*M1;
				M is Mag2*M1,
				string_concatenate(["",M,"\\\\sqrt{",S1,"}"],"",Num1)
		)
	),
	string_concatenate([",string(|",Latex_str_name1,"| = latex(\\\\frac{",Num1,"}{",Den,"}))]"],"",Sol_9),
	string_concatenate([Sol_0,Sol_1,Sol_2,Sol_3,Sol_4,Sol_5,Sol_6,Sol_7,Sol_8,Sol_9],"",Solution).


%------------------------------------------------------------------End of Question 6--------------------------------------------------------------------------------

%----------------------------------------------------------------------Question 7---------------------------------------------------------------------------------

%Question
generate_question_vector_10_3_7(Question):-
	generate_list_of_two(List1),
	generate_list_of_two(List2),
	generate_latex_abc(List1,[a,b,c],"",Latex_str1),
	generate_latex_abc(List2,[a,b,c],"",Latex_str2),

	string_concatenate(["[string(Evaluate the product [",Latex_str1,"].[",Latex_str2,"])]"],"",Question).
%Answer
generate_answer_vector_10_3_7(List1,List2,Answer):-
	product_scalar_vector(List1,List2,Product),
	generate_latex_product(Product,"",Product_exp),
	string_concatenate(["[string(latex(",Product_exp,"))]"],"",Answer).
%Answer
generate_solution_vector_10_3_7(List1,List2,Solution):-
	generate_latex_abc(List1,[a,b,c],"",Latex_str1),
	generate_latex_abc(List2,[a,b,c],"",Latex_str2),
	
	product_scalar_vector(List1,List2,Product),
	generate_latex_product(Product,"",Product_exp),
	
	string_concatenate(["[string(Given [",Latex_str1,"].[",Latex_str2,"])"],"",Sol_0),
	string_concatenate([",string(Therefore, latex(",Product_exp,"))]"],"",Sol_1),
	string_concatenate([Sol_0,Sol_1],"",Solution).

%------------------------------------------------------------------End of Question 7---------------------------------------------------------------------------------

%----------------------------------------------------------------------Question 8---------------------------------------------------------------------------------
%Question
generate_question_vector_10_3_8(Question):-
	Var1=a,
	Var2=b,
	generate_latex_vector_name(Var1,Latex_str_name1),
    generate_latex_vector_name(Var2,Latex_str_name2),
    generate_magnitude(Scalar_product),
    generate_angle(Angle),

    string_concatenate(["[string(Find the magnitude of two vectors ",Latex_str_name1," and ",Latex_str_name2,", having the same magnitude and such that the)"],"",Q_0),
    string_concatenate([",string(angle between them is latex(\\\\mathring{",Angle,"}) and their scalar product is ",Scalar_product,".)]"],"",Q_1),
	string_concatenate([Q_0,Q_1],"",Question).
%Answer
generate_answer_vector_10_3_8(Angle,Scalar_product,Answer):-
	Var1=a,
	Var2=b,
	generate_latex_vector_name(Var1,Latex_str_name1),
    generate_latex_vector_name(Var2,Latex_str_name2),
	find_trig_val(1,Angle,A,B,_,_),
	term_string(T,B),
	Ans is truncate((Scalar_product)/(T)),
	get_updated_coefficient([[[1,1],[Ans,1]]],X,_),
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
    string_concatenate(["[string(|",Latex_str_name1,"| = |",Latex_str_name2,"| = latex(",Num,").)]"],"",Answer).
%Solution
generate_solution_vector_10_3_8(Angle,Scalar_product,Solution):-
	Var1=a,
	Var2=b,
	generate_latex_vector_name(Var1,Latex_str_name1),
    generate_latex_vector_name(Var2,Latex_str_name2),
	find_trig_val(1,Angle,A,B,_,_),
	term_string(T,B),
	term_string(Scalar_product,Scalarproduct),
	Ans is truncate((Scalar_product)/(T)),
	get_updated_coefficient([[[1,1],[Ans,1]]],X,_),
	get_updated_coefficient_result(X,M,S),                
	string_concatenate(["[string(Let latex(\\\\theta) be the angle between ",Latex_str_name1," and ",Latex_str_name2,")"],"",Sol_0),
	string_concatenate([",string(|",Latex_str_name1,"| = |",Latex_str_name2,"|, ",Latex_str_name1,".",Latex_str_name2," = ",Scalarproduct," and latex(\\\\theta) = ",Angle,")"],"",Sol_1),
	string_concatenate([",string(",Scalarproduct," = |",Latex_str_name1,"|.|",Latex_str_name2,"| cos",Angle,")"],"",Sol_2),
	string_concatenate([",string(",Scalarproduct," = latex(|\\\\overrightarrow{a}|^2).",B,")"],"",Sol_3),
	string_concatenate([",string(latex(|\\\\overrightarrow{a}|^2) = ",Ans,")"],"",Sol_4),
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
    string_concatenate([",string(|",Latex_str_name1,"| = |",Latex_str_name2,"| = latex(",Num,").)]"],"",Sol_5),
	string_concatenate([Sol_0,Sol_1,Sol_2,Sol_3,Sol_4,Sol_5],"",Solution).
%------------------------------------------------------------------End of Question 8---------------------------------------------------------------------------------

%----------------------------------------------------------------------Question 9---------------------------------------------------------------------------------
%Question
generate_question_vector_10_3_9(Question):-
	Var1=x,
	Var2=a,
	generate_magnitude(Mag),
	
    generate_latex_vector_name(Var1,Latex_str_name1),
    generate_latex_vector_name(Var2,Latex_str_name2),

    string_concatenate(["[string(Find |",Latex_str_name1,"|, if for a unit vector ",Latex_str_name2,", latex((\\\\overrightarrow{x}-\\\\overrightarrow{a}).(\\\\overrightarrow{x}+\\\\overrightarrow{a})=",Mag,").)]"],"",Question).
%Answer
generate_answer_vector_10_3_9(Mag,Answer):-
	Var1=x,
	generate_latex_vector_name(Var1,Latex_str_name1),
	Result is Mag+1,
	get_updated_coefficient([[[1,1],[Result,1]]],X,_),
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
	string_concatenate(["[string(|",Latex_str_name1,"| = latex(",Num,"))]"],"",Answer).
%Solution
generate_solution_vector_10_3_9(Mag,Solution):-
	Var1=x,
	Var2=a,
	
    generate_latex_vector_name(Var1,Latex_str_name1),
    generate_latex_vector_name(Var2,Latex_str_name2),
	Result is Mag+1,

    string_concatenate(["[string(Since ",Latex_str_name2," is a unit vector, |",Latex_str_name2,"| = 1. Also,)"],"",Sol_0),
    string_concatenate([",string(latex((\\\\overrightarrow{x}-\\\\overrightarrow{a}).(\\\\overrightarrow{x}+\\\\overrightarrow{a})=",Mag,") )"],"",Sol_1),
    string_concatenate([",string(or  ",Latex_str_name1,".",Latex_str_name1,"+",Latex_str_name1,".",Latex_str_name2,"-",Latex_str_name2,".",Latex_str_name1,"-",Latex_str_name2,".",Latex_str_name2," = ",Mag,")"],"",Sol_2),
    string_concatenate([",string(or  latex(|\\\\overrightarrow{x}|^2 - 1 = ",Mag,") i.e. latex(|\\\\overrightarrow{x}|^2) = ",Result,")"],"",Sol_3),
	get_updated_coefficient([[[1,1],[Result,1]]],X,_),
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
	string_concatenate([",string(|",Latex_str_name1,"| = latex(",Num,") (as magnitude of a vector is non negative))]"],"",Sol_4),
	string_concatenate([Sol_0,Sol_1,Sol_2,Sol_3,Sol_4],"",Solution).
%------------------------------------------------------------------End of Question 9---------------------------------------------------------------------------------
%----------------------------------------------------------------------Question 10---------------------------------------------------------------------------------

%Question
generate_question_vector_10_3_10(Question):-
	generate_list(List1),
	generate_list(List2),	
    generate_mul_vector(List1,List2,Mul),
    generate_sum(Mul,0,Result),
    (Result=\=0->
    	generate_question_vector_10_3_10(Question);
    	(generate_list(List3),
		Var1=a,
		Var2=b,
		Var3=c,
		generate_latex_vector_ijk(List1, [i,j,k],"",Latex_str1),
    	generate_latex_vector_ijk(List2, [i,j,k],"",Latex_str2),
    	generate_latex_vector_ijk(List3, [i,j,k],"",Latex_str3),
		generate_latex_vector_name(Var1,Latex_str_name1),
	    generate_latex_vector_name(Var2,Latex_str_name2),
	    generate_latex_vector_name(Var3,Latex_str_name3),
	    string_concatenate(["[string(If ",Latex_str_name1," = ",Latex_str1,", ",Latex_str_name2," = ",Latex_str2," and ",Latex_str_name3," = ",Latex_str1," are such that ",Latex_str_name1," + latex(\\\\lambda)",Latex_str_name2," is perpendicular to ",Latex_str_name3,", then find the value of latex(\\\\lambda).)]"],"",Question))
    ).
%Answer
generate_answer_vector_10_3_10(List1,List2,List3,Answer):-
	generate_mul_vector(List1,List3,Mul1),
    generate_sum(Mul1,0,Result1),

    generate_mul_vector(List2,List3,Mul2),
    generate_sum(Mul2,0,Result2),

    Ans is -((Result1)/(Result2)),
   	string_concatenate(["[string(latex(\\\\lambda = )",Ans,")]"],"",Answer).
%Solution
generate_solution_vector_10_3_10(List1,List2,List3,Solution):-	
	List1=[L1,L2,L3],
	List2=[M1,M2,M3],
	List3=[N1,N2,N3],
	Var1=a,
	Var2=b,
	Var3=c,
	generate_latex_vector_name(Var1,Latex_str_name1),
    generate_latex_vector_name(Var2,Latex_str_name2),
    generate_latex_vector_name(Var3,Latex_str_name3),
    generate_latex_vector_ijk(List1, [i,j,k],"",Latex_str1),
	generate_latex_vector_ijk(List2, [i,j,k],"",Latex_str2),
	generate_latex_vector_ijk(List3, [i,j,k],"",Latex_str3),
	generate_mul_vector(List1,List3,Mul1),
	Mul1=[X1,Y1,Z1],
    generate_sum(Mul1,0,Result1),

    generate_mul_vector(List2,List3,Mul2),
    Mul2=[X2,Y2,Z2],
    generate_sum(Mul2,0,Result2),

    Ans is -((Result1)/(Result2)),

	string_concatenate(["[string(We have,)"],"",Sol_0),
	string_concatenate([",string(",Latex_str_name1," = ",Latex_str1,", ",Latex_str_name2," = ",Latex_str2," and ",Latex_str_name3," = ",Latex_str3,")"],"",Sol_1),
	string_concatenate([",string(Also given that ",Latex_str_name1," + latex(\\\\lambda)",Latex_str_name2," is perpendicular to ",Latex_str_name3,")"],"",Sol_2),
	string_concatenate([",string(i.e [",Latex_str_name1," + latex(\\\\lambda)",Latex_str_name2,"].",Latex_str_name3," = 0)"],"",Sol_3),
	string_concatenate([",string( [",Latex_str1," + latex(\\\\lambda)[",Latex_str2,"]].",Latex_str3," = 0)"],"",Sol_4),
	string_concatenate([",string([[(",L1,") + latex(\\\\lambda) (",M1,")]latex(\\\\hat{i}) + [(",L2,") + latex(\\\\lambda) (",M2,")]latex(\\\\hat{j}) + [(",L3,") + latex(\\\\lambda) (",M3,")]latex(\\\\hat{k})].",List3," = 0  )"],"",Sol_5),
	string_concatenate([",string(",N1,"[(",L1,") + latex(\\\\lambda) (",M1,")] + ",N2,"[(",L2,") + latex(\\\\lambda) (",M2,")] + ",N3,"[(",L3,") + latex(\\\\lambda) (",M3,")] = 0  )"],"",Sol_6),
	string_concatenate([",string([(",N1,").(",L1,") + (",N2,").(",L2,") + (",N3,").(",L3,")] + latex(\\\\lambda)[(",N1,").(",M1,") + (",N2,").(",M2,") + (",N3,").(",M3,")] = 0 )"],"",Sol_7),
	string_concatenate([",string([(",X1,") + (",Y1,") + (",Z1,")] + latex(\\\\lambda)[(",X2,") + (",Y2,") + (",Z2,")] = 0 )"],"",Sol_8),
	string_concatenate([",string(latex(\\\\lambda = -(\\\\frac{",Result1,"}{",Result2,"})))"],"",Sol_9),
   	string_concatenate([",string(latex(\\\\lambda = ",Ans,"))]"],"",Sol_10),
	string_concatenate([Sol_0,Sol_1,Sol_2,Sol_3,Sol_4,Sol_5,Sol_6,Sol_7,Sol_8,Sol_9,Sol_10],"",Solution).

%------------------------------------------------------------------End of Question 10-------+--------------------------------------------------------------------------


%----------------------------------------------------------------------Question 13---------------------------------------------------------------------------------
%Question
generate_question_vector_10_3_13(Question):-
	Var1=a,
	Var2=b,
	Var3=c,
	generate_magnitude(Mag),
	generate_latex_vector_name(Var1,Latex_str_name1),
    generate_latex_vector_name(Var2,Latex_str_name2),
    generate_latex_vector_name(Var3,Latex_str_name3),
    string_concatenate(["[string(If ",Latex_str_name1,", ",Latex_str_name2,", ",Latex_str_name3," are unit vectors such that ",Latex_str_name1," + ",Latex_str_name2," + ",Latex_str_name3," = ",Mag,", find the value of ",Latex_str_name1,".",Latex_str_name2," + ",Latex_str_name2,".",Latex_str_name3," + ",Latex_str_name3,".",Latex_str_name1,".)]"],"",Question).
%Answer
generate_answer_vector_10_3_13(Mag,Answer):-
	Var1=a,
	Var2=b,
	Var3=c,
	generate_latex_vector_name(Var1,Latex_str_name1),
    generate_latex_vector_name(Var2,Latex_str_name2),
    generate_latex_vector_name(Var3,Latex_str_name3),
	Ans0 is (Mag**2),
	Ans is (Ans0-3),
	string_concatenate(["[string(",Latex_str_name1,".",Latex_str_name2," + ",Latex_str_name2,".",Latex_str_name3," + ",Latex_str_name3,".",Latex_str_name1," = latex(\\\\frac{",Ans,"}{2}))]"],"",Answer).
%Solution
generate_solution_vector_10_3_13(Mag,Solution):-
	Ans0 is (Mag**2),
	Ans is (Ans0-3),
	Var1=a,
	Var2=b,
	Var3=c,
	generate_latex_vector_name(Var1,Latex_str_name1),
    generate_latex_vector_name(Var2,Latex_str_name2),
    generate_latex_vector_name(Var3,Latex_str_name3),
	string_concatenate(["[string(latex(|\\\\overrightarrow{a}+\\\\overrightarrow{b}+\\\\overrightarrow{c}|^2 = (\\\\overrightarrow{a}+\\\\overrightarrow{b}+\\\\overrightarrow{c}).(\\\\overrightarrow{a}+\\\\overrightarrow{b}+\\\\overrightarrow{c}) = |",Mag,"|^2))"],"",Sol_0),
	string_concatenate([",string(latex(|\\\\overrightarrow{a}|^2+|\\\\overrightarrow{b}|^2+|\\\\overrightarrow{c}|^2+2)[",Latex_str_name1,".",Latex_str_name2," + ",Latex_str_name2,".",Latex_str_name3," + ",Latex_str_name3,".",Latex_str_name1,"] = ",Ans0,")"],"",Sol_1),
	string_concatenate([",string(1 + 1 + 1 + 2[",Latex_str_name1,".",Latex_str_name2," + ",Latex_str_name2,".",Latex_str_name3," + ",Latex_str_name3,".",Latex_str_name1,"] = ",Ans0,")"],"",Sol_2),
	string_concatenate([",string(",Latex_str_name1,".",Latex_str_name2," + ",Latex_str_name2,".",Latex_str_name3," + ",Latex_str_name3,".",Latex_str_name1," = latex(\\\\frac{",Ans,"}{2}))]"],"",Sol_3),
	string_concatenate([Sol_0,Sol_1,Sol_2,Sol_3],"",Solution).

%------------------------------------------------------------------End of Question 13---------------------------------------------------------------------------------

%----------------------------------------------------------------------Question 15---------------------------------------------------------------------------------
%Question
generate_question_vector_10_3_15(Question):-
	generate_list(Point1),
	generate_list(Point2),
	generate_list(Point3),
	string_concatenate(["[string(If the vertices A, B, C of a triangle ABC are ",Point1,", ",Point2,", ",Point3,", recpectively, then find latex(\\\\angle{ABC}). [latex(\\\\angle{ABC}) is the angle between the vectors latex(\\\\overrightarrow{BA}) and latex(\\\\overrightarrow{BC})].)]"],"",Question).
%Answer
generate_answer_vector_10_3_15(Point1,Point2,Point3,Answer):-
	generate_diff_vector(Point1,Point2,Diff_1),
	generate_diff_vector(Point3,Point2,Diff_2),

    generate_magnitude(Diff_1,0,Diff_1_magnitude),
    generate_magnitude(Diff_2,0,Diff_2_magnitude),

    generate_mul_vector(Diff_1,Diff_2,Mul),
    generate_sum(Mul,0,Result),
    
    get_updated_coefficient([[[1,1],[Diff_1_magnitude,1]]],X1,_),
	get_updated_coefficient_result(X1,M1,S1),
	
	get_updated_coefficient([[[1,1],[Diff_2_magnitude,1]]],X2,_),
	get_updated_coefficient_result(X2,M2,S2), 
	(M1=:=1,M2=:=1->
		(S1=:=S2->
			Den is S1;
			S is (S1)*(S2),
			string_concatenate(["\\\\sqrt{",S,"}"],"",Den)
		);
		(S1=:=S2->
			Den is M1*M2*S1;
			S is (S1)*(S2),
			M is (M1)*(M2),
			string_concatenate(["",M,"\\\\sqrt{",S,"}"],"",Den)
		)
	),
	string_concatenate(["[string(latex(\\\\angle{ABC} = cos^{-1}(\\\\frac{",Result,"}{",Den,"})))]"],"",Answer).
%Solution
generate_solution_vector_10_3_15(Point1,Point2,Point3,Solution):-
	generate_diff_vector(Point1,Point2,Diff_1),
	generate_diff_vector(Point3,Point2,Diff_2),

    generate_magnitude(Diff_1,0,Diff_1_magnitude),
    generate_magnitude(Diff_2,0,Diff_2_magnitude),

    generate_latex_diff_exp_ijk(Point1,Point2,[i,j,k],"",Latex_diff_exp_1),
	generate_latex_diff_exp_ijk(Point3,Point2,[i,j,k],"",Latex_diff_exp_2),

	generate_latex_vector_ijk(Diff_1, [i,j,k],"",Latex_Diff_1),
	generate_latex_vector_ijk(Diff_2, [i,j,k],"",Latex_Diff_2),

	generate_latex_magnitude_expression_ijk(Diff_1,"",Diff_1_mag_exp), 
	generate_latex_magnitude_expression_ijk(Diff_2,"",Diff_2_mag_exp),

    generate_mul_vector(Diff_1,Diff_2,Mul),
    generate_sum(Mul,0,Result),
    
    get_updated_coefficient([[[1,1],[Diff_1_magnitude,1]]],X1,_),
	get_updated_coefficient_result(X1,M1,S1),
	
	get_updated_coefficient([[[1,1],[Diff_2_magnitude,1]]],X2,_),
	get_updated_coefficient_result(X2,M2,S2),
	
	string_concatenate(["[string(latex(\\\\overrightarrow{BA}) = ",Latex_diff_exp_1," = ",Latex_Diff_1,")"],"",Sol_0),
	string_concatenate([",string(latex(\\\\overrightarrow{BC}) = ",Latex_diff_exp_2," = ",Latex_Diff_2,")"],"",Sol_1),
	string_concatenate([",string(latex(\\\\overrightarrow{BA}.\\\\overrightarrow{BC}) = [",Latex_Diff_1,"].[",Latex_Diff_2,"])"],"",Sol_2),
	string_concatenate([",string(latex(|\\\\overrightarrow{BA}|) = ",Diff_1_mag_exp," = latex(\\\\sqrt{",Diff_1_magnitude,"}))"],"",Sol_3),
	string_concatenate([",string(latex(|\\\\overrightarrow{BC}|) = ",Diff_2_mag_exp," = latex(\\\\sqrt{",Diff_2_magnitude,"}))"],"",Sol_4),
	string_concatenate([",string(latex(\\\\overrightarrow{BA}.\\\\overrightarrow{BC} = |\\\\overrightarrow{BA}| |\\\\overrightarrow{BC}| cos(\\\\angle{ABC})))"],"",Sol_5),
	string_concatenate([",string(",Result," = latex(\\\\sqrt{",Diff_1_magnitude,"}.\\\\sqrt{",Diff_2_magnitude,"} cos(\\\\angle{ABC})))"],"",Sol_6),
	string_concatenate([",string(latex(cos(\\\\angle{ABC}) = \\\\frac{",Result,"}{\\\\sqrt{",Diff_1_magnitude,"}.\\\\sqrt{",Diff_2_magnitude,"}}))"],"",Sol_7),
	(M1=:=1,M2=:=1->
		(S1=:=S2->
			Den is S1;
			S is (S1)*(S2),
			string_concatenate(["\\\\sqrt{",S,"}"],"",Den)
		);
		(S1=:=S2->
			Den is M1*M2*S1;
			S is (S1)*(S2),
			M is (M1)*(M2),
			string_concatenate(["",M,"\\\\sqrt{",S,"}"],"",Den)
		)
	),
	string_concatenate([",string(latex(\\\\angle{ABC} = cos^{-1}(\\\\frac{",Result,"}{",Den,"})))]"],"",Sol_8),
	string_concatenate([Sol_0,Sol_1,Sol_2,Sol_3,Sol_4,Sol_5,Sol_6,Sol_7,Sol_8],"",Solution).

%------------------------------------------------------------------End of Question 15---------------------------------------------------------------------------------

%----------------------------------------------------------------------Question 16---------------------------------------------------------------------------------
%Question
generate_question_vector_10_3_16(Question):-
	generate_list(Point1),
	generate_list(Point2),
	generate_list(Point3),

    string_concatenate(["[string(Show that the points A",Point1,", B",Point2," and C",Point3," are collinear or not.)]"],"",Question).

%Answer
generate_answer_vector_10_3_16(Point1,Point2,Point3,Answer):-
	generate_diff_vector(Point2,Point1,Diff_1),
	generate_diff_vector(Point3,Point2,Diff_2),
	generate_diff_vector(Point1,Point3,Diff_3),

    generate_magnitude(Diff_1,0,Diff_1_magnitude),
    generate_magnitude(Diff_2,0,Diff_2_magnitude),
    generate_magnitude(Diff_3,0,Diff_3_magnitude),

    get_updated_coefficient([[[1,1],[Diff_1_magnitude,1]]],X1,_),
	get_updated_coefficient_result(X1,M1,S1),
	
	get_updated_coefficient([[[1,1],[Diff_2_magnitude,1]]],X2,_),
	get_updated_coefficient_result(X2,M2,S2), 
	
	get_updated_coefficient([[[1,1],[Diff_3_magnitude,1]]],X3,_),
	get_updated_coefficient_result(X3,M3,S3),

	(S1=\=S2->
    	string_concatenate(["[string(The points A, B and C are not collinear.)]"],"",Answer);
    	(S2=\=S3->
    		string_concatenate(["[string(The points A, B and C are not collinear.)]"],"",Answer);
			(M3=:=M1+M2->
    			string_concatenate(["[string(The points A, B and C are collinear.)]"],"",Answer);
	    	    (M2=:=M3+M1->
    				string_concatenate(["[string(The points A, B and C are collinear.)]"],"",Answer);
	     			(M1=:=M3+M2->
    					string_concatenate(["[string(The points A, B and C are collinear.)]"],"",Answer);
    					string_concatenate(["[string(The points A, B and C are not collinear.)]"],"",Answer)
					)
				)
    		)
    	)
	).
%Solution
generate_solution_vector_10_3_16(Point1,Point2,Point3,Solution):-
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

	get_updated_coefficient([[[1,1],[Diff_1_magnitude,1]]],X1,_),
	get_updated_coefficient_result(X1,M1,S1),
	
	get_updated_coefficient([[[1,1],[Diff_2_magnitude,1]]],X2,_),
	get_updated_coefficient_result(X2,M2,S2), 
	
	get_updated_coefficient([[[1,1],[Diff_3_magnitude,1]]],X3,_),
	get_updated_coefficient_result(X3,M3,S3),


    string_concatenate(["[string(We have,)"],"",Sol_0),
	string_concatenate([",string(latex(\\\\overrightarrow{AB}) = ",Latex_diff_exp_1," = ",Latex_Diff_1,")"],"",Sol_1),
	string_concatenate([",string(latex(\\\\overrightarrow{BC}) = ",Latex_diff_exp_2," = ",Latex_Diff_2,")"],"",Sol_2),
	string_concatenate([",string(latex(\\\\overrightarrow{CA}) = ",Latex_diff_exp_3," = ",Latex_Diff_3,")"],"",Sol_3),

	(M1=:=1->
		string_concatenate([",string(latex(|\\\\overrightarrow{AB}| = \\\\sqrt{",S1,"}) )"],"",Sol_4);
		(S1=:=1->
			string_concatenate([",string(latex(|\\\\overrightarrow{AB}| = ",M1,") )"],"",Sol_4);
			string_concatenate([",string(latex(|\\\\overrightarrow{AB}| = ",M1,"\\\\sqrt{",S1,"}) )"],"",Sol_4)
		)
	),


	(M2=:=1->
		string_concatenate([",string(latex(|\\\\overrightarrow{BC}| = \\\\sqrt{",S2,"}) )"],"",Sol_5);
		(S2=:=1->
			string_concatenate([",string(latex(|\\\\overrightarrow{BC}| = ",M2,") )"],"",Sol_5);
			string_concatenate([",string(latex(|\\\\overrightarrow{BC}| = ",M2,"\\\\sqrt{",S2,"}) )"],"",Sol_5)
		)
	),

	(M3=:=1->
		string_concatenate([",string(latex(|\\\\overrightarrow{AC}| = \\\\sqrt{",S3,"}) )"],"",Sol_6);
		(S3=:=1->
			string_concatenate([",string(latex(|\\\\overrightarrow{AC}| = ",M3,") )"],"",Sol_6);
			string_concatenate([",string(latex(|\\\\overrightarrow{AC}| = ",M3,"\\\\sqrt{",S3,"}) )"],"",Sol_6)
		)
	),

	(S1=\=S2->
    	string_concatenate([",string(Hence The points A, B and C are not collinear.)]"],"",Sol_7),
    	string_concatenate([Sol_0,Sol_1,Sol_2,Sol_3,Sol_4,Sol_5,Sol_6,Sol_7],"",Solution);
    	(S2=\=S3->
    		string_concatenate([",string(Hence The points A, B and C are not collinear.)]"],"",Sol_7),
    	    string_concatenate([Sol_0,Sol_1,Sol_2,Sol_3,Sol_4,Sol_5,Sol_6,Sol_7],"",Solution);
			(M3=:=M1+M2->
				string_concatenate([",string(latex(|\\\\overrightarrow{AC}|) = latex(|\\\\overrightarrow{AB}|) + latex(|\\\\overrightarrow{BC}|))"],"",Sol_7),
    			string_concatenate([",string(Hence The points A, B and C are collinear.)]"],"",Sol_8),
    	    	string_concatenate([Sol_0,Sol_1,Sol_2,Sol_3,Sol_4,Sol_5,Sol_6,Sol_7,Sol_8],"",Solution);

	    	    (M2=:=M3+M1->
					string_concatenate([",string(latex(|\\\\overrightarrow{BC}|) = latex(|\\\\overrightarrow{AB}|) + latex(|\\\\overrightarrow{AC}|))"],"",Sol_7),
	    			string_concatenate([",string(Hence The points A, B and C are collinear.)]"],"",Sol_8),
	    	    	string_concatenate([Sol_0,Sol_1,Sol_2,Sol_3,Sol_4,Sol_5,Sol_6,Sol_7,Sol_8],"",Solution);	     			
    	    		(M1=:=M3+M2->
						string_concatenate([",string(latex(|\\\\overrightarrow{AB}|) = latex(|\\\\overrightarrow{BC}|) + latex(|\\\\overrightarrow{AC}|))"],"",Sol_7),
		    			string_concatenate([",string(Hence The points A, B and C are collinear.)]"],"",Sol_8),
		    	    	string_concatenate([Sol_0,Sol_1,Sol_2,Sol_3,Sol_4,Sol_5,Sol_6,Sol_7,Sol_8],"",Solution); 

		    	    	string_concatenate(["[string(Hence The points A, B and C are not collinear.)]"],"",Sol_7),
		    			string_concatenate([Sol_0,Sol_1,Sol_2,Sol_3,Sol_4,Sol_5,Sol_6,Sol_7],"",Solution)
					)
				)
    		)
    	)
	).

%------------------------------------------------------------------End of Question 16---------------------------------------------------------------------------------

%------------------------------------------------------------------Question 17---------------------------------------------------------------------------------
%Question
generate_question_vector_10_3_17(Question):-
	
	generate_list(Point1),
	generate_list(Point2),
	generate_list(Point3),

	generate_latex_vector_ijk(Point1, [i,j,k],"",Latex_str1),
    generate_latex_vector_ijk(Point2, [i,j,k],"",Latex_str2),
    generate_latex_vector_ijk(Point3, [i,j,k],"",Latex_str3),

    string_concatenate(["[string(Show that the vectors, ",Latex_str1," ,",Latex_str2," and ",Latex_str3," form the vertices of a right angled triangle or not.)]"],"",Question).
   
%Answer
generate_answer_vector_10_3_17(Point1,Point2,Point3,Answer):-
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
generate_solution_vector_10_3_17(Point1,Point2,Point3,Solution):-
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