/*:- include('radicals/helper.pl').
:- include('vector_algebra/helper.pl').
:- include('application_of_integrals/gfs.pl').
:- include('trigno_with_val/helper.pl').
:- include('binomial_multinomials/helper_general.pl').*/

%--------------------------------------------------------------------Miscellaneous-------------------------------------------------------------------------
%------------------------------------------------------------------Example 27---------------------------------------------------------------------------------
%Question
generate_question_vector_ex_27(Question):-
	generate_list(List1),
	generate_list(List2),
	generate_list(List3),
	generate_list(List4),

  	generate_latex_vector_ijk(List1, [i,j,k],"",Latex_str1),
    generate_latex_vector_ijk(List2, [i,j,k],"",Latex_str2),
    generate_latex_vector_ijk(List3, [i,j,k],"",Latex_str3),
    generate_latex_vector_ijk(List4, [i,j,k],"",Latex_str4),

    string_concatenate(["[string(If ",Latex_str1,", ",Latex_str2,", ",Latex_str3," and ",Latex_str4," are the position vectors of the points A,B,C and D respectively, then find the angle between latex(\\\\overrightarrow{AB}) and latex(\\\\overrightarrow{CD}).)]"],"",Question).
%Answer
generate_answer_vector_ex_27(List1,List2,List3,List4,Answer):-
	generate_diff_vector(List2,List1,Diff1),
	generate_diff_vector(List4,List3,Diff2),

	generate_mul_vector(Diff1,Diff2,Mul),
    generate_sum(Mul,0,Num),
    generate_magnitude(Diff1,0,Diff1_magnitude),
	generate_magnitude(Diff2,0,Diff2_magnitude),
	get_updated_coefficient([[[1,1],[Diff1_magnitude,1]]],X1,_),
	get_updated_coefficient_result(X1,M1,S1),
	
	get_updated_coefficient([[[1,1],[Diff2_magnitude,1]]],X2,_),
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
generate_solution_vector_ex_27(List1,List2,List3,List4,Solution):-
	generate_latex_vector_ijk(List1, [i,j,k],"",Latex_str1),
    generate_latex_vector_ijk(List2, [i,j,k],"",Latex_str2),
    generate_latex_vector_ijk(List3, [i,j,k],"",Latex_str3),
    generate_latex_vector_ijk(List4, [i,j,k],"",Latex_str4),
    generate_diff_vector(List2,List1,Diff1),
	generate_diff_vector(List4,List3,Diff2),
	
    generate_vector_ijk(Diff1, [i,j,k],"",Latex_Diff1),
    generate_vector_ijk(Diff2, [i,j,k],"",Latex_Diff2),
    generate_magnitude(Diff1,0,Diff1_magnitude),
	generate_magnitude(Diff2,0,Diff2_magnitude),

	generate_mul_vector(Diff1,Diff2,Mul),
    generate_sum(Mul,0,Num),

    get_updated_coefficient([[[1,1],[Diff1_magnitude,1]]],X1,_),
	get_updated_coefficient_result(X1,M1,S1),

	(M1=:=1->
		(S1=:=1->
			Mag1 is S1;
			string_concatenate(["\\\\sqrt{",S1,"}"],"",Mag1)
		);
		(S1=:=1->
			Mag1 is M1*S1;
			string_concatenate(["",M1,"\\\\sqrt{",S1,"}"],"",Mag1)
		)
	),
	
	get_updated_coefficient([[[1,1],[Diff2_magnitude,1]]],X2,_),
	get_updated_coefficient_result(X2,M2,S2), 

	(M2=:=1->
		(S2=:=1->
			Mag2 is S2;
			string_concatenate(["\\\\sqrt{",S2,"}"],"",Mag2)
		);
		(S2=:=1->
			Mag2 is M2*S2;
			string_concatenate(["",M2,"\\\\sqrt{",S2,"}"],"",Mag2)
		)
	),

	(M1=:=1,M2=:=1->
		(S1=:=S2->
			Den is S1,
			simplify(Num,Den,R1,R2),
			string_concatenate([R1,"/",R2],"",Term),
			trignometric_inverse_function(1,Term,Angle),
			string_concatenate([",string(latex(\\\\theta = cos^{-1}(\\\\frac{",Num,"}{",Den,"}) = ",Angle,"))]"],"",Sol_7);

			S is (S1)*(S2),
			string_concatenate(["\\\\sqrt{",S,"}"],"",Den),
			string_concatenate([",string(latex(\\\\theta = cos^{-1}(\\\\frac{",Num,"}{",Den,"})))]"],"",Sol_7)

		);
		(S1=:=S2->
			Den is M1*M2*S1,
			simplify(Num,Den,R1,R2),
			string_concatenate([R1,"/",R2],"",Term),
			trignometric_inverse_function(1,Term,Angle),
			string_concatenate([",string(latex(\\\\theta = cos^{-1}(\\\\frac{",Num,"}{",Den,"}) = ",Angle,"))]"],"",Sol_7);

			S is (S1)*(S2),
			M is (M1)*(M2),
			string_concatenate(["",M,"\\\\sqrt{",S,"}"],"",Den),
			string_concatenate([",string(latex(\\\\theta = cos^{-1}(\\\\frac{",Num,"}{",Den,"})))]"],"",Sol_7)

		)
	),

	string_concatenate(["[string(Note that if latex(\\\\theta) is the angle between AB and CD, then latex(\\\\theta) is also the angle between latex(\\\\overrightarrow{AB}) and latex(\\\\overrightarrow{CD}))"],"",Sol_0),
	string_concatenate([",string(Now latex(\\\\overrightarrow{AB}) = Position vector of B - Position vector of A)"],"",Sol_1),
	string_concatenate([",string( latex(\\\\overrightarrow{AB}) = [",Latex_str2,"] - [",Latex_str1,"] = latex(",Latex_Diff1,").)"],"",Sol_2),
	string_concatenate([",string(Therefore latex(|\\\\overrightarrow{AB}| = \\\\sqrt{",Diff1_magnitude,"} = ",Mag1,"))"],"",Sol_3),
	string_concatenate([",string(Similarly latex(\\\\overrightarrow{AB} = ",Latex_Diff2,") and latex(|\\\\overrightarrow{CD}| = \\\\sqrt{",Diff2_magnitude,"} = ",Mag2,"))"],"",Sol_4),
	string_concatenate([",string(Thus latex(cos\\\\theta = \\\\frac{\\\\overrightarrow{AB}.\\\\overrightarrow{CD}}{|\\\\overrightarrow{AB}|.|\\\\overrightarrow{CD}|}))"],"",Sol_5),
	string_concatenate([",string( latex(cos\\\\theta = \\\\frac{[",Latex_Diff1,"].[",Latex_Diff2,"]}{(",Mag1,")(",Mag2,")}))"],"",Sol_6),
    string_concatenate([Sol_0,Sol_1,Sol_2,Sol_3,Sol_4,Sol_5,Sol_6,Sol_7],"",Solution).

%------------------------------------------------------------------End of Example 27--------------------------------------------------------------------------------

%------------------------------------------------------------------Example 28---------------------------------------------------------------------------------
%Question
generate_question_vector_ex_28(Mag1,Mag2,Mag3,Question):-
	Var1=a,
	Var2=b,
	Var3=c,
	/*generate_magnitude(Mag1),
	generate_magnitude(Mag2),
	generate_magnitude(Mag3),*/
    generate_latex_vector_name(Var1,Latex_str_name1),
    generate_latex_vector_name(Var2,Latex_str_name2),
    generate_latex_vector_name(Var3,Latex_str_name3),

    string_concatenate(["[string(Let ",Latex_str_name1,", ",Latex_str_name2," and ",Latex_str_name3," be three vectors such that latex(|)",Latex_str_name1,"latex(|) = ",Mag1,", latex(|)",Latex_str_name2,"latex(|) = ",Mag2,", latex(|)",Latex_str_name3,"latex(|) = ",Mag3," and each of then is perpendicular to the sum of other two, find latex(|)",Latex_str_name1," + ",Latex_str_name2," + ",Latex_str_name3,"latex(|).)]"],"",Question).
%Answer
generate_answer_vector_ex_28(Mag1,Mag2,Mag3,Answer):-
	/*Var1=a,
	Var2=b,
	Var3=c,
	generate_latex_vector_name(Var1,Latex_str_name1),
    generate_latex_vector_name(Var2,Latex_str_name2),
    generate_latex_vector_name(Var3,Latex_str_name3),*/
	S1 is (Mag1**2),
	S2 is (Mag2**2),
	S3 is (Mag3**2),
	Sum is (S1 + S2 + S3),
	get_updated_coefficient([[[1,1],[Sum,1]]],X,_),
	get_updated_coefficient_result(X,M,S),

	(M=:=1->
		(S=:=1->
			Ans is S;
			string_concatenate(["\\\\sqrt{",S,"}"],"",Ans)


		);
		(S=:=1->
			Ans is M*S;
			string_concatenate(["",M,"\\\\sqrt{",S,"}"],"",Ans)
		)
	),

	string_concatenate(["",Ans,""],"",Answer).
%Solution
generate_solution_vector_ex_28(Mag1,Mag2,Mag3,Solution):-
	Var1=a,
	Var2=b,
	Var3=c,
	generate_vector_name(Var1,Latex_str_name1),
    generate_vector_name(Var2,Latex_str_name2),
    generate_vector_name(Var3,Latex_str_name3),
    S1 is (Mag1**2),
	S2 is (Mag2**2),
	S3 is (Mag3**2),
	Sum is (S1 + S2 + S3),
	get_updated_coefficient([[[1,1],[Sum,1]]],X,_),
	get_updated_coefficient_result(X,M,S),
	string_concatenate(["[string(Given latex(",Latex_str_name1,".(",Latex_str_name2," + ",Latex_str_name3,") = 0, ",Latex_str_name2,".(",Latex_str_name3," + ",Latex_str_name1,") = 0, ",Latex_str_name3,".(",Latex_str_name1," + ",Latex_str_name2,") = 0.))"],"",Sol_0),
	string_concatenate([",string(Now latex(|\\\\overrightarrow{a} + \\\\overrightarrow{b} + \\\\overrightarrow{c}|^2 = (\\\\overrightarrow{a} + \\\\overrightarrow{b} + \\\\overrightarrow{c})^2 = (\\\\overrightarrow{a} + \\\\overrightarrow{b} + \\\\overrightarrow{c}).(\\\\overrightarrow{a} + \\\\overrightarrow{b} + \\\\overrightarrow{c}) ))"],"",Sol_1),
	string_concatenate([",string(    latex(|\\\\overrightarrow{a} + \\\\overrightarrow{b} + \\\\overrightarrow{c}|^2 = ) latex(",Latex_str_name1,".",Latex_str_name1," +",Latex_str_name1,".(",Latex_str_name2," + ",Latex_str_name3,") + ",Latex_str_name2,".",Latex_str_name2," +",Latex_str_name2,".(",Latex_str_name1," + ",Latex_str_name3,") + ,",Latex_str_name3,".",Latex_str_name3," +",Latex_str_name3,".(",Latex_str_name1," + ",Latex_str_name2,")))"],"",Sol_2),
	string_concatenate([",string(    latex(|\\\\overrightarrow{a} + \\\\overrightarrow{b} + \\\\overrightarrow{c}|^2 = |\\\\overrightarrow{a}|^2 + |\\\\overrightarrow{b}|^2 + |\\\\overrightarrow{c}|^2))"],"",Sol_3),
	string_concatenate([",string(    latex(|\\\\overrightarrow{a} + \\\\overrightarrow{b} + \\\\overrightarrow{c}|^2 = ",S1," + ",S2," + ",S3," = ",Sum,"))"],"",Sol_4),
	(M=:=1->
		(S=:=1->
			Ans is S;
			string_concatenate(["\\\\sqrt{",S,"}"],"",Ans)
		);
		(S=:=1->
			Ans is M*S;
			string_concatenate(["",M,"\\\\sqrt{",S,"}"],"",Ans)
		)
	),
	string_concatenate([",string(Therefore latex(|",Latex_str_name1," + ",Latex_str_name2," + ",Latex_str_name3,"|) = latex(\\\\\\\\sqrt{",Sum,"}) = latex(",Ans,").)]"],"",Sol_5),
    string_concatenate([Sol_0,Sol_1,Sol_2,Sol_3,Sol_4,Sol_5],"",Solution).

%------------------------------------------------------------------End of Example 28--------------------------------------------------------------------------------
%------------------------------------------------------------------Example 29---------------------------------------------------------------------------------
%Question
generate_question_vector_ex_29(Mag1,Mag2,Mag3,Question):-
	Var1=a,
	Var2=b,
	Var3=c,
	/*generate_magnitude(Mag1),
	generate_magnitude(Mag2),
	generate_magnitude(Mag3),*/
    generate_latex_vector_name(Var1,Latex_str_name1),
    generate_latex_vector_name(Var2,Latex_str_name2),
    generate_latex_vector_name(Var3,Latex_str_name3),
    string_concatenate(["[string(Three vectors ",Latex_str_name1,", ",Latex_str_name2," and ",Latex_str_name3," satisfy the condition ",Latex_str_name1," + ",Latex_str_name2," + ",Latex_str_name3," = 0. Evaluate the quantity latex(\\\\\\\\mu = ) ",Latex_str_name1,".",Latex_str_name2," + ",Latex_str_name2,".",Latex_str_name3," + ",Latex_str_name3,".",Latex_str_name1," if latex(|)",Latex_str_name1,"latex(|) = ",Mag1,", latex(|)",Latex_str_name2,"latex(|) = ",Mag2," and latex(|)",Latex_str_name3,"latex(|) = ",Mag3,".)]"],"",Question).
%Answer
generate_answer_vector_ex_29(Mag1,Mag2,Mag3,Answer):-
	S1 is (Mag1**2),
	S2 is (Mag2**2),
	S3 is (Mag3**2),
	Sum is -(S1 + S2 + S3),
	simplify(Sum,2,Num,Den),
	(Den=:=1->
		string_concatenate(["",Num,""],"",Answer);
		string_concatenate(["\\\\\\\\frac{",Num,"}{",Den,"}"],"",Answer)
	).		

%Solution
generate_solution_vector_ex_29(Mag1,Mag2,Mag3,Solution):-
	Var1=a,
	Var2=b,
	Var3=c,
	generate_latex_vector_name(Var1,Latex_str_name1),
    generate_latex_vector_name(Var2,Latex_str_name2),
    generate_latex_vector_name(Var3,Latex_str_name3),
    S1 is (Mag1**2),
	S2 is (Mag2**2),
	S3 is (Mag3**2),
	Sum is -(S1 + S2 + S3),
	string_concatenate(["[string(Since ",Latex_str_name1," + ",Latex_str_name2," + ",Latex_str_name3," = 0)"],"",Sol_0),
	string_concatenate([",string(",Latex_str_name1,".latex((\\\\\\\\overrightarrow{a} + \\\\\\\\overrightarrow{b} + \\\\\\\\overrightarrow{c})) = 0)"],"",Sol_1),
	string_concatenate([",string(or ",Latex_str_name1,".",Latex_str_name1," + ",Latex_str_name1,".",Latex_str_name2," + ",Latex_str_name1,".",Latex_str_name3," = 0)"],"",Sol_2),
	string_concatenate([",string(Therefore ",Latex_str_name1,".",Latex_str_name2," + ",Latex_str_name1,".",Latex_str_name3," = latex(-|\\\\overrightarrow{a}|^2 = -",S1,")           .....(1))"],"",Sol_3),
	string_concatenate([",string(Again ",Latex_str_name2,".latex((\\\\\\\\overrightarrow{a} + \\\\\\\\overrightarrow{b} + \\\\\\\\overrightarrow{c})) = 0)"],"",Sol_4),
	string_concatenate([",string(or ",Latex_str_name1,".",Latex_str_name2," + ",Latex_str_name2,".",Latex_str_name3," = latex(-|\\\\overrightarrow{b}|^2 = -",S2,")           		 .....(2))"],"",Sol_5),
	string_concatenate([",string(Similarly ",Latex_str_name1,".",Latex_str_name3," + ",Latex_str_name2,".",Latex_str_name3," = latex(-|\\\\overrightarrow{c}|^2 = -",S3,")           .....(3))"],"",Sol_6),
	string_concatenate([",string(Adding (1),(2) and (3) we have)"],"",Sol_7),
	string_concatenate([",string(2 latex((\\\\\\\\overrightarrow{a}.\\\\\\\\overrightarrow{b} + \\\\\\\\overrightarrow{b}.\\\\\\\\overrightarrow{c} + \\\\\\\\overrightarrow{c}.\\\\\\\\overrightarrow{a}) = ",Sum,"))"],"",Sol_8),
	string_concatenate([",string(or latex(2\\\\mu = ",Sum,"))"],"",Sol_9),
	simplify(Sum,2,Num,Den),
	(Den=:=1->
		string_concatenate([",string(latex(\\\\mu = ",Num,").)]"],"",Sol_10);
		string_concatenate([",string(latex(\\\\mu = \\\\\\\\frac{",Num,"}{",Den,"}).)]"],"",Sol_10)
	),    
	string_concatenate([Sol_0,Sol_1,Sol_2,Sol_3,Sol_4,Sol_5,Sol_6,Sol_7,Sol_8,Sol_9,Sol_10],"",Solution).

%------------------------------------------------------------------End of Example 29--------------------------------------------------------------------------------
%------------------------------------------------------------------Question 5---------------------------------------------------------------------------------
%Question
generate_question_vector_Mis_5(List,Question):-
	%generate_list(List),
	generate_vector_ijk(List, [i,j,k],"",Latex_str),
    string_concatenate(["[string(Find the value of latex(x) for which latex(x(",Latex_str,"))is a unit vector.)]"],"",Question).

%Answer
generate_answer_vector_Mis_5(List,Answer):-
	generate_magnitude(List,0,Mag),
	get_updated_coefficient([[[1,1],[Mag,1]]],X,_),
	get_updated_coefficient_result(X,M,S),

	(M=:=1->
		(S=:=1->
			Ans is S;
			string_concatenate(["\\\\sqrt{",S,"}"],"",Ans)
		);
		(S=:=1->
			Ans is M*S;
			string_concatenate(["",M,"\\\\sqrt{",S,"}"],"",Ans)
		)
	),
	string_concatenate(["\\\\pm\\\\frac{1}{",Ans,"}"],"",Answer).
%Solution
generate_solution_vector_Mis_5(List,Solution):-
	generate_vector_ijk(List, [i,j,k],"",Latex_str),
	generate_magnitude_expression_ijk(List,"",Latex_mag),
	generate_magnitude(List,0,Mag),
	get_updated_coefficient([[[1,1],[Mag,1]]],X,_),
	get_updated_coefficient_result(X,M,S),
	(M=:=1->
		(S=:=1->
			Ans is S;
			string_concatenate(["\\\\sqrt{",S,"}"],"",Ans)
		);
		(S=:=1->
			Ans is M*S;
			string_concatenate(["",M,"\\\\sqrt{",S,"}"],"",Ans)
		)
	),
	string_concatenate(["[string(latex(|x(",Latex_str,")| = 1))"],"",Sol_0),
	string_concatenate([",string(latex(x(",Latex_mag,") = 1))"],"",Sol_1),
	string_concatenate([",string(latex(x(\\\\sqrt{",Mag,"}) = 1))"],"",Sol_2),
	string_concatenate([",string(latex(x = \\\\pm\\\\frac{1}{",Ans,"}).)]"],"",Sol_3),
    string_concatenate([Sol_0,Sol_1,Sol_2,Sol_3],"",Solution).
%------------------------------------------------------------------End of Question 5--------------------------------------------------------------------------------

%------------------------------------------------------------------Question 6---------------------------------------------------------------------------------
%Question
generate_question_vector_Mis_6(List1,List2,Mag,Question):-
	Var1=a,
	Var2=b,
	generate_latex_vector_name(Var1,Latex_str_name1),
    generate_latex_vector_name(Var2,Latex_str_name2),
	/*generate_list(List1),
	generate_list(List2),
	generate_magnitude(Mag),*/
	generate_latex_vector_ijk(List1, [i,j,k],"",Latex_str1),
    generate_latex_vector_ijk(List2, [i,j,k],"",Latex_str2),
    string_concatenate(["[string(Find a vector of magnitude ",Mag," units, and parallel to the resultant of the vectors ",Latex_str_name1," = ",Latex_str1," and ",Latex_str_name2," = ",Latex_str2,").]"],"",Question).

%Answer
generate_answer_vector_Mis_6(List1,List2,Mag,Answer):-
	generate_sum_vector(List1,List2,Sum),
	generate_magnitude(Sum,0,Sum_mag),
	get_updated_coefficient([[[1,1],[Sum_mag,1]]],X,_),
	get_updated_coefficient_result(X,M,S),
	(M=:=1->
		(S=:=1->
			Ans is S;
			string_concatenate(["\\\\sqrt{",S,"}"],"",Ans)
		);
		(S=:=1->
			Ans is M*S;
			string_concatenate(["",M,"\\\\sqrt{",S,"}"],"",Ans)
		)
	),
	Pro is (M*S),
	generate_latex_mag_updated_fraction_vector(Sum,[i,j,k],Ans,Mag,Pro,"",Latex_frac_sum),
	string_concatenate(["",Latex_frac_sum,""],"",Answer).
%Solution
generate_solution_vector_Mis_6(List1,List2,Mag,Solution):-
	Var1=a,
	Var2=b,
	Var3=c,
	generate_latex_vector_name(Var1,Latex_str_name1),
    generate_latex_vector_name(Var2,Latex_str_name2),
    generate_latex_vector_name(Var3,Latex_str_name3),

	generate_latex_vector_ijk(List1, [i,j,k],"",Latex_str1),
    generate_latex_vector_ijk(List2, [i,j,k],"",Latex_str2),

    generate_latex_sum_exp_ijk(List1,List2,[i,j,k],"",Latex_sum_exp),
    generate_sum_vector(List1,List2,Sum),
    generate_vector_ijk(Sum, [i,j,k],"",Latex_sum),
	generate_magnitude(Sum,0,Sum_mag),
	generate_latex_magnitude_expression_ijk(Sum,"",Latex_mag_exp),

	get_updated_coefficient([[[1,1],[Sum_mag,1]]],X,_),
	get_updated_coefficient_result(X,M,S),
	(M=:=1->
		(S=:=1->
			Ans is S;
			string_concatenate(["\\\\sqrt{",S,"}"],"",Ans)
		);
		(S=:=1->
			Ans is M*S;
			string_concatenate(["",M,"\\\\sqrt{",S,"}"],"",Ans)
		)
	),
	Pro is (M*S),
	generate_latex_mag_updated_fraction_vector_ijk(Sum,[i,j,k],Ans,Mag,Pro,"",Latex_frac_sum),

	string_concatenate(["[string(Given ",Latex_str_name1," = ",Latex_str1," and ",Latex_str_name2," = ",Latex_str2,")"],"",Sol_0),
	string_concatenate([",string(",Latex_str_name3," = ",Latex_str_name1," + ",Latex_str_name2," = ",Latex_sum_exp," = latex(",Latex_sum,"))"],"",Sol_1),
	string_concatenate([",string(|",Latex_str_name3,"| = ",Latex_mag_exp," = latex(\\\\sqrt{",Sum_mag,"} = ",Ans,"))"],"",Sol_2),
	string_concatenate([",string(latex(\\\\hat{c} = \\\\frac{\\\\overrightarrow{c}}{|\\\\overrightarrow{c}|} = \\\\frac{",Latex_sum,"}{",Ans,"}))"],"",Sol_3),
	string_concatenate([",string(So the vector of Magnitude ",Mag," and parallel to the resultant of ",Latex_str_name1," and ",Latex_str_name2," is)"],"",Sol_4),
	string_concatenate([",string(",Mag,"latex((\\\\hat{c}) = )" ,Latex_frac_sum,")]"],"",Sol_5),
    string_concatenate([Sol_0,Sol_1,Sol_2,Sol_3,Sol_4,Sol_5],"",Solution).
%------------------------------------------------------------------End of Question 6--------------------------------------------------------------------------------

%------------------------------------------------------------------Question 7---------------------------------------------------------------------------------
%Question
generate_question_vector_Mis_7(List1,List2,List3,List4,Question):-
	Var1=a,
	Var2=b,
	Var3=c,
	generate_latex_vector_name(Var1,Latex_str_name1),
    generate_latex_vector_name(Var2,Latex_str_name2),
    generate_latex_vector_name(Var3,Latex_str_name3),
    /*generate_list(List1),
	generate_list(List2),
	generate_list(List3),
	generate_list(List4),*/
	generate_latex_vector_ijk(List1, [i,j,k],"",Latex_str1),
    generate_latex_vector_ijk(List2, [i,j,k],"",Latex_str2),
    generate_latex_vector_ijk(List3, [i,j,k],"",Latex_str3),
    generate_abc(List4, [a,b,c],"",Latex_str4),
    string_concatenate(["[string(If ",Latex_str_name1," = ",Latex_str1,", ",Latex_str_name2," = ",Latex_str2," and ",Latex_str_name3," = ",Latex_str3," find a unit vector parallel to the vector latex(",Latex_str4,"))]"],"",Question).
%Answer
generate_answer_vector_Mis_7(List1,List2,List3,List4,Answer):-
	List4=[A,B,C],
	multiply_list(List1,A,New_list1),
	multiply_list(List2,B,New_list2),
	multiply_list(List3,C,New_list3),
	generate_sum_vector(New_list1,New_list2,Sum1),
	generate_sum_vector(Sum1,New_list3,Sum),

	generate_magnitude(Sum,0,Sum_mag),
	get_updated_coefficient([[[1,1],[Sum_mag,1]]],X,_),
	get_updated_coefficient_result(X,M,S),
	(M=:=1->
		(S=:=1->
			Ans is S;
			string_concatenate(["\\\\sqrt{",S,"}"],"",Ans)
		);
		(S=:=1->
			Ans is M*S;
			string_concatenate(["",M,"\\\\sqrt{",S,"}"],"",Ans)
		)
	),
	Pro is (M*S),
	generate_latex_updated_fraction_vector(Sum,[i,j,k],Ans,Pro,"",Latex_frac_sum),
	string_concatenate(["",Latex_frac_sum,""],"",Answer).
%Solution
generate_solution_vector_Mis_7(List1,List2,List3,List4,Solution):-
	Var1=a,
	Var2=b,
	Var3=c,
	generate_latex_vector_name(Var1,Latex_str_name1),
    generate_latex_vector_name(Var2,Latex_str_name2),
    generate_latex_vector_name(Var3,Latex_str_name3),
	List4=[A,B,C],
	generate_latex_vector_ijk(List1, [i,j,k],"",Latex_str1),
    generate_latex_vector_ijk(List2, [i,j,k],"",Latex_str2),
    generate_latex_vector_ijk(List3, [i,j,k],"",Latex_str3),
    generate_abc(List4, [a,b,c],"",Latex_str4),
    multiply_list(List1,A,New_list1),
	multiply_list(List2,B,New_list2),
	multiply_list(List3,C,New_list3),

	generate_sum_vector(New_list1,New_list2,Sum1),
	generate_sum_vector(Sum1,New_list3,Sum),

	generate_latex_vector_ijk(New_list1, [i,j,k],"",Latex_new_str1),
    generate_latex_vector_ijk(New_list2, [i,j,k],"",Latex_new_str2),
    generate_latex_vector_ijk(New_list3, [i,j,k],"",Latex_new_str3),
    generate_vector_ijk(Sum, [i,j,k],"",Latex_sum),
    generate_latex_magnitude_expression_ijk(Sum,"",Latex_mag_exp),
	generate_magnitude(Sum,0,Sum_mag),
	get_updated_coefficient([[[1,1],[Sum_mag,1]]],X,_),
	get_updated_coefficient_result(X,M,S),
	(M=:=1->
		(S=:=1->
			Ans is S;
			string_concatenate(["\\\\sqrt{",S,"}"],"",Ans)
		);
		(S=:=1->
			Ans is M*S;
			string_concatenate(["",M,"\\\\sqrt{",S,"}"],"",Ans)
		)
	),
	Pro is (M*S),
	generate_updated_fraction_vector_ijk(Sum,[i,j,k],Ans,Pro,"",Latex_frac_sum),


	string_concatenate(["[string(Given ",Latex_str_name1," = ",Latex_str1,", ",Latex_str_name2," = ",Latex_str2," and ",Latex_str_name3," = ",Latex_str3,")"],"",Sol_0),
	string_concatenate([",string(latex(",Latex_str4,") = (",A,")[",Latex_str1,"] + (",B,")[",Latex_str2,"] + (",C,")[",Latex_str3,"])"],"",Sol_1),
	string_concatenate([",string(latex(",Latex_str4,") = [",Latex_new_str1,"] + [",Latex_new_str2,"] + [",Latex_new_str3,"])"],"",Sol_2),
	string_concatenate([",string(latex(",Latex_str4,") = latex(",Latex_sum,"))"],"",Sol_3),
	string_concatenate([",string(latex(|",Latex_str4,"|) = ",Latex_mag_exp," = latex(",Ans,"))"],"",Sol_4),
	string_concatenate([",string(So the required vector is)"],"",Sol_5),
	string_concatenate([",string(latex(\\\\frac{",Latex_sum,"}{",Ans,"} = ) latex(",Latex_frac_sum,"))]"],"",Sol_6),
    string_concatenate([Sol_0,Sol_1,Sol_2,Sol_3,Sol_4,Sol_5,Sol_6],"",Solution).
%------------------------------------------------------------------End of Question 7--------------------------------------------------------------------------------

%------------------------------------------------------------------Question 8---------------------------------------------------------------------------------
%Question
generate_question_vector_Mis_8(Question):-
	generate_list(Point1),
	generate_list(Point2),
	generate_list(Point3),

    string_concatenate(["[string(Show that the points A",Point1,", B",Point2," and C",Point3," are collinear or not, and find the rato in which B divides AC)]"],"",Question).

%Answer
generate_answer_vector_Mis_8(Point1,Point2,Point3,Answer):-
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

	Diff_1=[A,B,C],
	Diff_2=[A1,B1,C1],
	simplify(A,A1,R1,R2),


	(S1=\=S2->
    	string_concatenate(["[string(The points A, B and C are not collinear.)]"],"",Answer);
    	(S2=\=S3->
    		string_concatenate(["[string(The points A, B and C are not collinear.)]"],"",Answer);
			(M3=:=M1+M2->
    			string_concatenate(["[string(The points A, B and C are collinear.)"],"",Ans_0),
    			string_concatenate([",string(B divides AC int the ratio of ",R1,":",R2,".)]"],"",Ans_1),
    			string_concatenate([Ans_0,Ans_1],"",Answer);

	    	    (M2=:=M3+M1->
    					string_concatenate(["[string(The points A, B and C are collinear.)"],"",Ans_0),
		    			string_concatenate([",string(B divides AC int the ratio of ",R1,":",R2,".)]"],"",Ans_1),
		    			string_concatenate([Ans_0,Ans_1],"",Answer);
	     			(M1=:=M3+M2->
    						string_concatenate(["[string(The points A, B and C are collinear.)"],"",Ans_0),
			    			string_concatenate([",string(B divides AC int the ratio of ",R1,":",R2,".)]"],"",Ans_1),
			    			string_concatenate([Ans_0,Ans_1],"",Answer);
    						
    						string_concatenate(["[string(The points A, B and C are not collinear.)]"],"",Answer)
					)
				)
    		)
    	)
	).
%Solution
generate_solution_vector_Mis_8(Point1,Point2,Point3,Solution):-
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

	generate_vector_ijk(Point1, [i,j,k],"",Latex_str1),
	generate_vector_ijk(Point2, [i,j,k],"",Latex_str2),
	generate_vector_ijk(Point3, [i,j,k],"",Latex_str3),

	get_updated_coefficient([[[1,1],[Diff_1_magnitude,1]]],X1,_),
	get_updated_coefficient_result(X1,M1,S1),
	
	get_updated_coefficient([[[1,1],[Diff_2_magnitude,1]]],X2,_),
	get_updated_coefficient_result(X2,M2,S2), 
	
	get_updated_coefficient([[[1,1],[Diff_3_magnitude,1]]],X3,_),
	get_updated_coefficient_result(X3,M3,S3),


	Diff_1=[A,B,C],
	Diff_2=[A0,B0,C0],
	simplify(A,A0,R1,R2),
	Point1=[A1,A2,A3],
	Point1=[B1,B2,B3],
	Point1=[C1,C2,C3],


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


    string_concatenate([",string(Let B divide AC in latex(\\\\lambda : 1))"],"",Sol_9),
    string_concatenate([",string(latex(\\\\overrightarrow{OB} = \\\\frac{\\\\lambda(\\\\overrightarrow{OC}) + \\\\overrightarrow{OA}}{\\\\lambda + 1}))"],"",Sol_10),
    string_concatenate([",string(latex(",Latex_str2," = \\\\frac{\\\\lambda(",Latex_str3,") + ",Latex_str1,"}{\\\\lambda + 1}))"],"",Sol_11),
    string_concatenate([",string(latex((\\\\lambda + 1)(",Latex_str2,") = (",C1,"\\\\lambda)\\\\hat{i} + (",C2,"\\\\lambda)\\\\hat{j} + (",C3,"\\\\lambda)\\\\hat{k} + ",Latex_str1,"))"],"",Sol_12),
    string_concatenate([",string(latex((\\\\lambda + 1)(",Latex_str2,") = ((",C1,"\\\\lambda) + (",A1,"))\\\\hat{i} + ((",C2,"\\\\lambda) + (",A2,"))\\\\hat{j} + ((",C3,"\\\\lambda) + (",A3,"))\\\\hat{k} ))"],"",Sol_13),
    string_concatenate([",string(latex(\\\\lambda = \\\\frac{",R1,"}{",R2,"} ))"],"",Sol_14),
    string_concatenate([",string(So, B divides AC int the ratio of ",R1,":",R2,".)]"],"",Sol_15),

	(S1=\=S2->
    	string_concatenate([",string(Hence The points A, B and C are not collinear.)]"],"",Sol_7),
    	string_concatenate([Sol_0,Sol_1,Sol_2,Sol_3,Sol_4,Sol_5,Sol_6,Sol_7],"",Solution);
    	(S2=\=S3->
    		string_concatenate([",string(Hence The points A, B and C are not collinear.)]"],"",Sol_7),
    	    string_concatenate([Sol_0,Sol_1,Sol_2,Sol_3,Sol_4,Sol_5,Sol_6,Sol_7],"",Solution);
			(M3=:=M1+M2->
				string_concatenate([",string(latex(|\\\\overrightarrow{AC}|) = latex(|\\\\overrightarrow{AB}|) + latex(|\\\\overrightarrow{BC}|))"],"",Sol_7),
    			string_concatenate([",string(Hence The points A, B and C are collinear.)"],"",Sol_8),
    	    	string_concatenate([Sol_0,Sol_1,Sol_2,Sol_3,Sol_4,Sol_5,Sol_6,Sol_7,Sol_8,Sol_9,Sol_10,Sol_11,Sol_12,Sol_13,Sol_14,Sol_15],"",Solution);

	    	    (M2=:=M3+M1->
					string_concatenate([",string(latex(|\\\\overrightarrow{BC}|) = latex(|\\\\overrightarrow{AB}|) + latex(|\\\\overrightarrow{AC}|))"],"",Sol_7),
	    			string_concatenate([",string(Hence The points A, B and C are collinear.)"],"",Sol_8),
    	    		string_concatenate([Sol_0,Sol_1,Sol_2,Sol_3,Sol_4,Sol_5,Sol_6,Sol_7,Sol_8,Sol_9,Sol_10,Sol_11,Sol_12,Sol_13,Sol_14,Sol_15],"",Solution);
    	    		(M1=:=M3+M2->
						string_concatenate([",string(latex(|\\\\overrightarrow{AB}|) = latex(|\\\\overrightarrow{BC}|) + latex(|\\\\overrightarrow{AC}|))"],"",Sol_7),
		    			string_concatenate([",string(Hence The points A, B and C are collinear.)"],"",Sol_8),
    	    			string_concatenate([Sol_0,Sol_1,Sol_2,Sol_3,Sol_4,Sol_5,Sol_6,Sol_7,Sol_8,Sol_9,Sol_10,Sol_11,Sol_12,Sol_13,Sol_14,Sol_15],"",Solution);

		    	    	string_concatenate(["[string(Hence The points A, B and C are not collinear.)]"],"",Sol_7),
		    			string_concatenate([Sol_0,Sol_1,Sol_2,Sol_3,Sol_4,Sol_5,Sol_6,Sol_7],"",Solution)
					)
				)
    		)
    	)
	).

%------------------------------------------------------------------End of Question 8--------------------------------------------------------------------------------

%------------------------------------------------------------------Question 9---------------------------------------------------------------------------------
%Question
generate_question_vector_Mis_9(Question):-
	
	generate_list(Point1),
	generate_list(Point2),

	generate_magnitude(Ratio1),
	generate_magnitude(Ratio2),

	generate_latex_vector_ijk(Point1, [i,j,k],"",Latex_str1),
    generate_latex_vector_ijk(Point2, [i,j,k],"",Latex_str2),

	string_concatenate(["[string(Find the position vector of a point R which divides the lines joining two points P ans Q whose position vectrs are )"],"",Q_0),
	string_concatenate([",string(",Latex_str1," and ",Latex_str2," externally in the ratio ",Ratio1," : ",Ratio2,".)]"],"",Q_1),
    string_concatenate([Q_0,Q_1],"",Question).


%Answer
generate_answer_vector_Mis_9(Point1,Point2,Ratio1,Ratio2,Answer):-
	divide_externally(Point2,Point1,Ratio1,Ratio2,Div_externally),
	Externally is Ratio1-Ratio2,

	generate_latex_fraction_vector(Div_externally,[i,j,k],Externally,"",Latex_ratio_2),

	string_concatenate(["[string(Externally: ",Latex_ratio_2,")]"],"",Answer).


%Solution
generate_solution_vector_Mis_9(Point1,Point2,Ratio1,Ratio2,Solution):-
	generate_vector_ijk(Point1, [i,j,k],"",Latex_str1),
    generate_vector_ijk(Point2, [i,j,k],"",Latex_str2),

	divide_externally(Point2,Point1,Ratio1,Ratio2,Div_externally),
	Externally is Ratio1-Ratio2,
	generate_latex_fraction_vector(Div_externally,[i,j,k],Externally,"",Latex_ratio_2),

	string_concatenate(["[string(The position vector of the point R dividing the join of P and Q extenally in the ratio",Ratio1," : ",Ratio2," is )"],"",Sol_0),
	string_concatenate([",string(latex(\\\\overrightarrow{OR}) = latex(\\\\frac{",Ratio1,"(",Latex_str2,") - ",Ratio2,"(",Latex_str1,")}{",Ratio1," - ",Ratio2,"}) = ",Latex_ratio_2,")]"],"",Sol_1),
    string_concatenate([Sol_0,Sol_1],"",Solution).
%------------------------------------------------------------------End of Question 9--------------------------------------------------------------------------------
%------------------------------------------------------------------Question 10---------------------------------------------------------------------------------
%Question
generate_question_vector_Mis_10(Question):-
	generate_list(List1),
	generate_list(List2),
	generate_latex_vector_ijk(List1, [i,j,k],"",Latex_str1),
    generate_latex_vector_ijk(List2, [i,j,k],"",Latex_str2),

	string_concatenate(["[string(The two adjacent sides of a parallelogram are ",Latex_str1," and ",Latex_str2,". )"],"",Q_0),
	string_concatenate([",string(Find the unit vector parallel to it's diagonal.Also, find it's area.)]"],"",Q_1),
    string_concatenate([Q_0,Q_1],"",Question).
%Answer
generate_answer_vector_Mis_10(List1,List2,Answer):-   
	generate_sum_vector(List1,List2,Sum),
	generate_magnitude(Sum,0,Sum_mag),
	get_updated_coefficient([[[1,1],[Sum_mag,1]]],X,_),
	get_updated_coefficient_result(X,M,S),
	(M=:=1->
		(S=:=1->
			Ans is S;
			string_concatenate(["\\\\sqrt{",S,"}"],"",Ans)
		);
		(S=:=1->
			Ans is M*S;
			string_concatenate(["",M,"\\\\sqrt{",S,"}"],"",Ans)
		)
	),
	Pro is (M*S),
	generate_latex_updated_fraction_vector_ijk(Sum,[i,j,k],Ans,Pro,"",Latex_frac_sum),

	cross_product(List1,List2,Product),
    generate_magnitude(Product,0,Product_magnitude),
    
    get_updated_coefficient([[[1,1],[Product_magnitude,1]]],X1,_),
	get_updated_coefficient_result(X1,M1,S1),
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
	string_concatenate(["[string(Unit Vector :",Latex_frac_sum,")"],"",Ans_0),
	string_concatenate([",string(Area of parallelogram = latex(",Num,"))]"],"",Ans_1),
    string_concatenate([Ans_0,Ans_1],"",Answer).

%Solution
generate_solution_vector_Mis_10(List1,List2,Solution):-
	Var1=a,
	Var2=b,
	generate_latex_vector_name(Var1,Latex_str_name1),
    generate_latex_vector_name(Var2,Latex_str_name2),

	generate_latex_vector_ijk(List1, [i,j,k],"",Latex_str1),
    generate_latex_vector_ijk(List2, [i,j,k],"",Latex_str2),

    generate_latex_sum_exp_ijk(List1,List2,[i,j,k],"",Latex_sum_exp),
    generate_sum_vector(List1,List2,Sum),
    generate_vector_ijk(Sum, [i,j,k],"",Latex_sum),
	generate_magnitude(Sum,0,Sum_mag),
	generate_latex_magnitude_expression_ijk(Sum,"",Latex_mag_exp),

	get_updated_coefficient([[[1,1],[Sum_mag,1]]],X,_),
	get_updated_coefficient_result(X,M,S),
	(M=:=1->
		(S=:=1->
			Ans is S;
			string_concatenate(["\\\\sqrt{",S,"}"],"",Ans)
		);
		(S=:=1->
			Ans is M*S;
			string_concatenate(["",M,"\\\\sqrt{",S,"}"],"",Ans)
		)
	),
	Pro is (M*S),
	generate_latex_updated_fraction_vector_ijk(Sum,[i,j,k],Ans,Pro,"",Latex_frac_sum),


	List1=[X1,Y1,Z1],
	List2=[X2,Y2,Z2],
    cross_product(List1,List2,Product),
    generate_latex_vector_ijk(Product, [i,j,k],"",Latex_product),
    generate_latex_magnitude_expression_ijk(Product,"",Product_mag_exp),

    generate_magnitude(Product,0,Product_magnitude),
    generate_latex_cross_product(List1,List2,Latex_cross_product),
    
  	get_updated_coefficient([[[1,1],[Product_magnitude,1]]],X3,_),
	get_updated_coefficient_result(X3,M1,S1),
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
	
	string_concatenate(["[string(Let ",Latex_str_name1," = ",Latex_str1," and ",Latex_str_name2," = ",Latex_str2,")"],"",Sol_0),
	string_concatenate([",string(Diagonal of a parallelogram is ",Latex_str_name1," + ",Latex_str_name2,")"],"",Sol_1),
	string_concatenate([",string(",Latex_str_name1," + ",Latex_str_name2," = ",Latex_sum_exp," = latex(",Latex_sum,"))"],"",Sol_2),
	string_concatenate([",string(|",Latex_str_name1," + ",Latex_str_name2,"| = ",Latex_mag_exp," = latex(\\\\sqrt{",Sum_mag,"} = ",Ans,"))"],"",Sol_3),
	string_concatenate([",string(So the unit vector parallel to diagonal is)"],"",Sol_4),
	string_concatenate([",string(latex(\\\\hat{c} = \\\\frac{\\\\overrightarrow{a} + \\\\overrightarrow{b}}{|\\\\overrightarrow{a} + \\\\overrightarrow{b}|} = \\\\frac{",Latex_sum,"}{",Ans,"}))"],"",Sol_5),	
	string_concatenate([",string(latex(\\\\hat{c} = )",Latex_frac_sum,")"],"",Sol_6),

 	string_concatenate([",string(The area of a parallelogram with ",Latex_str_name1," and ",Latex_str_name2," as its adjacent sides is given by |",Latex_str_name1," X ",Latex_str_name2,"|.)"],"",Sol_7),
    string_concatenate([",string(Now ",Latex_str_name1," X ",Latex_str_name2," = latex(\\\\begin{vmatrix} \\\\hat{i} & \\\\hat{j} & \\\\hat{j} \\\\\\ ",X1," & ",Y1," & ",Z1," \\\\\\ ",X2," & ",Y2," & ",Z2," \\\\end{vmatrix}))"],"",Sol_8),
    string_concatenate([",string(",Latex_str_name1," X ",Latex_str_name2," = latex(",Latex_cross_product," = )",Latex_product,"))"],"",Sol_9),
    string_concatenate([",string(Therefore   |",Latex_str_name1," X ",Latex_str_name2,"| = ",Product_mag_exp," = latex(",Num,"))"],"",Sol_10),
    string_concatenate([",string(and hence, the required area is latex(",Num,").)]"],"",Sol_11),
    string_concatenate([Sol_0,Sol_1,Sol_2,Sol_3,Sol_4,Sol_5,Sol_6,Sol_7,Sol_8,Sol_9,Sol_10,Sol_11],"",Solution).

%------------------------------------------------------------------End of Question 10--------------------------------------------------------------------------------

%------------------------------------------------------------------Question 12---------------------------------------------------------------------------------
%Question
generate_question_vector_Mis_12(List1,List2,List3,Mag,Question):-
	Var1=a,
	Var2=b,
	Var3=c,
	Var4=d,
	generate_latex_vector_name(Var1,Latex_str_name1),
    generate_latex_vector_name(Var2,Latex_str_name2),
    generate_latex_vector_name(Var3,Latex_str_name3),
    generate_latex_vector_name(Var4,Latex_str_name4),
	/*generate_list(List1),
	generate_list(List2),
	generate_list(List3),
	generate_magnitude(Mag),*/
	generate_latex_vector_ijk(List1, [i,j,k],"",Latex_str1),
    generate_latex_vector_ijk(List2, [i,j,k],"",Latex_str2),
    generate_latex_vector_ijk(List3, [i,j,k],"",Latex_str3),
   
	string_concatenate(["[string(Let ",Latex_str_name1," = ",Latex_str1,", ",Latex_str_name2," = ",Latex_str2," and ",Latex_str_name3," = ",Latex_str3," Find a vector which is perpendicular )"],"",Q_0),
	string_concatenate([",string(to both ",Latex_str_name1," and ",Latex_str_name2 ," and ",Latex_str_name3,".",Latex_str_name4," = ",Mag,".)]"],"",Q_1),
    string_concatenate([Q_0,Q_1],"",Question). 
%Answer
generate_answer_vector_Mis_12(List1,List2,List3,Mag,Answer):- 
	Var1=d,
	generate_latex_vector_name(Var1,Latex_str_name1),
	List1=[A1,A2,A3],
	List2=[B1,B2,B3],
	List3=[C1,C2,C3],
	D1 is 0,
	D2 is 0,
	D3 is Mag,
	Y1 is (A1*B2 - B1*A2),
	Y2 is (A1*B3 - B1*A3),
	Constant_Y is (A1*D2 - B1*D1),
	Z1 is (A1*C2 - C1*A2),
	Z2 is (A1*C3 - C1*A3),
	Constant_Z is (A1*D3 - C1*D1),
	get_one_var_in_form_of_other(Y1,Y2,Constant_Y,Z1,Z2,Constant_Z,Y_value,Z_value,N_Y,D_Y,N_Z,D_Z),
	X_value is ((D1 - A2*Y_value - A3*Z_value)/A1),
	Num_X is (D1*D_Y*D_Z - A2*N_Y*D_Z - A3*N_Z*D_Y),
	Den_X is A1*D_Y*D_Z,
	simplify(Num_X,Den_X,N_X,D_X),
	
	generate_fraction_vector([N_X],[i],D_X,"",Latex_X),
	generate_fraction_vector([N_Y],[j],D_Y,"",Latex_Y),
	generate_fraction_vector([N_Z],[k],D_Z,"",Latex_Z),

	string_concatenate(["",Latex_X,",",Latex_Y,",",Latex_Z,""],"",Answer).
%Answer
generate_solution_vector_Mis_12(List1,List2,List3,Mag,Solution):- 	
	Var1=a,
	Var2=b,
	Var3=c,
	Var4=d,
	generate_latex_vector_name(Var1,Latex_str_name1),
    generate_latex_vector_name(Var2,Latex_str_name2),
    generate_latex_vector_name(Var3,Latex_str_name3),
    generate_latex_vector_name(Var4,Latex_str_name4),
	List1=[A1,A2,A3],
	List2=[B1,B2,B3],
	List3=[C1,C2,C3],
	D1 is 0,
	D2 is 0,
	D3 is Mag,
	Y1 is (A1*B2 - B1*A2),
	Y2 is (A1*B3 - B1*A3),
	Constant_Y is (A1*D2 - B1*D1),
	Z1 is (A1*C2 - C1*A2),
	Z2 is (A1*C3 - C1*A3),
	Constant_Z is (A1*D3 - C1*D1),
	get_one_var_in_form_of_other(Y1,Y2,Constant_Y,Z1,Z2,Constant_Z,Y_value,Z_value,N_Y,D_Y,N_Z,D_Z),
	X_value is ((D1 - A2*Y_value - A3*Z_value)/A1),
	Num_X is (D1*D_Y*D_Z - A2*N_Y*D_Z - A3*N_Z*D_Y),
	Den_X is A1*D_Y*D_Z,
	simplify(Num_X,Den_X,N_X,D_X),
	
	generate_latex_fraction_vector([N_X],[i],D_X,"",Latex_X),
	generate_latex_fraction_vector([N_Y],[j],D_Y,"",Latex_Y),
	generate_latex_fraction_vector([N_Z],[k],D_Z,"",Latex_Z),
	string_concatenate(["[string(Let ",Latex_str_name4," = latex(d_{1}\\\\hat{i} + d_{2}\\\\hat{j} + d_{3}\\\\hat{k}))"],"",Sol_0),
	string_concatenate([",string(",Latex_str_name4,".",Latex_str_name1," = 0 latex( = (",A1,")d_{1} + (",A2,")d_{2} + (",A3,")d_{3} = 0))"],"",Sol_1),
	string_concatenate([",string(",Latex_str_name4,".",Latex_str_name2," = 0 latex( = (",B1,")d_{1} + (",B2,")d_{2} + (",B3,")d_{3} = 0))"],"",Sol_2),
	string_concatenate([",string(",Latex_str_name4,".",Latex_str_name3," = ",Mag," latex( =  (",C1,")d_{1} + (",C2,")d_{2} + (",C3,")d_{3} = ",Mag,"))"],"",Sol_3),
	string_concatenate([",string(Solving these equations, we get)"],"",Sol_4),
	string_concatenate([",string(latex(d_{1} = \\\\frac{",N_X,"}{",D_X,"}, d_{2} = \\\\frac{",N_Y,"}{",D_Y,"}, d_{3} = \\\\frac{",N_Z,"}{",D_Z,"}))"],"",Sol_5),
	string_concatenate([",string(",Latex_str_name4," = ",Latex_X,Latex_Y,Latex_Z,")]"],"",Sol_6),
    string_concatenate([Sol_0,Sol_1,Sol_2,Sol_3,Sol_4,Sol_5,Sol_6],"",Solution).


%------------------------------------------------------------------End of Question 12--------------------------------------------------------------------------------
