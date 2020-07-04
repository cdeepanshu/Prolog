:- include('binomial_multinomials/helper_general.pl').


%String Concatenation Function 
string_concatenate([], Final_temp_string, Final_string) :-
    Final_string = Final_temp_string.
string_concatenate([H|T], Init_string, Final_string):-
    (is_list(H) -> term_string(H, H_new) ; H_new = H), 
    string_concat(Init_string, H_new, Temp_string),
    string_concatenate(T, Temp_string, Final_string).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%fraction of type 1/sqrt(5)+1 i/sqrt(3) j vector latex 
generate_latex_fraction_vector_ijk([],_,_,Init_str,Latex_str):-
    Latex_str = Init_str.
generate_latex_fraction_vector_ijk([H|T],[H1|T1],Mag,Init_str,Latex_str):-
    (H>0 ->
    	(Mag=:=1 ->
		    (Init_str == "" ->
		    string_concatenate(["latex(\\\\\\\\hat{",H1,"})"],"",Str1);
		    string_concatenate(["+latex(\\\\\\\\hat{",H1,"})"],"",Str1)
		    );
		    (Init_str == "" ->
		    string_concatenate(["latex(\\\\\\\\frac{",H,"}{\\\\\\\\sqrt{",Mag,"}}\\\\\\\\hat{",H1,"})"],"",Str1);
		    string_concatenate(["+latex(\\\\\\\\frac{",H,"}{\\\\\\\\sqrt{",Mag,"}}\\\\\\\\hat{",H1,"})"],"",Str1)
		    )
		);

		(H=:=0  ->
		    Str1=""; 
		    H2 is abs(H),
		    string_concatenate(["-latex(\\\\\\\\frac{",H2,"}{\\\\\\\\sqrt{",Mag,"}}\\\\\\\\hat{",H1,"})"],"",Str1)
		)
	),

    string_concat(Init_str, Str1,Temp_str),
    generate_latex_fraction_vector_ijk(T,T1,Mag,Temp_str,Latex_str).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%fraction of type 1/sqrt(5)+1 i/sqrt(3) j vector latex 
generate_latex_updated_fraction_vector_ijk([],_,_,_,Init_str,Latex_str):-
    Latex_str = Init_str.
generate_latex_updated_fraction_vector_ijk([H|T],[H1|T1],Mag,Pro,Init_str,Latex_str):-
    (H>0 ->
    	(Pro=:=1 ->
		    (Init_str == "" ->
		    string_concatenate(["latex(\\\\\\\\hat{",H1,"})"],"",Str1);

		    string_concatenate(["latex(+\\\\\\\\hat{",H1,"})"],"",Str1)

		    );
		    (Init_str == "" ->
		    string_concatenate(["latex(\\\\\\\\frac{",H,"}{",Mag,"}\\\\\\\\hat{",H1,"})"],"",Str1);

		    string_concatenate(["latex(+\\\\\\\\frac{",H,"}{",Mag,"}\\\\\\\\hat{",H1,"})"],"",Str1)
		    )
		);

		(H=:=0  ->
		    Str1=""; 
		    H2 is abs(H),
		    string_concatenate(["latex(-\\\\\\\\frac{",H2,"}{",Mag,"}\\\\\\\\hat{",H1,"})"],"",Str1)

		)
	),

    string_concat(Init_str, Str1,Temp_str),
    generate_latex_updated_fraction_vector_ijk(T,T1,Mag,Pro,Temp_str,Latex_str).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%fraction of type 1/sqrt(5)+1 i/sqrt(3) j vector latex 
generate_updated_fraction_vector_ijk([],_,_,_,Init_str,Latex_str):-
    Latex_str = Init_str.
generate_updated_fraction_vector_ijk([H|T],[H1|T1],Mag,Pro,Init_str,Latex_str):-
    (H>0 ->
    	(Pro=:=1 ->
		    (Init_str == "" ->
		    string_concatenate(["\\\\\\\\hat{",H1,"}"],"",Str1);

		    string_concatenate(["+\\\\\\\\hat{",H1,"}"],"",Str1)

		    );
		    (Init_str == "" ->
		    string_concatenate(["\\\\\\\\frac{",H,"}{",Mag,"}\\\\\\\\hat{",H1,"}"],"",Str1);

		    string_concatenate(["+\\\\\\\\frac{",H,"}{",Mag,"}\\\\\\\\hat{",H1,"}"],"",Str1)
		    )
		);

		(H=:=0  ->
		    Str1=""; 
		    H2 is abs(H),
		    string_concatenate(["-\\\\\\\\frac{",H2,"}{",Mag,"}\\\\\\\\hat{",H1,"}"],"",Str1)

		)
	),

    string_concat(Init_str, Str1,Temp_str),
    generate_updated_fraction_vector_ijk(T,T1,Mag,Pro,Temp_str,Latex_str).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%fraction of type 1/sqrt(5)+1 i/sqrt(3) j vector latex 
generate_latex_updated_fraction_vector([],_,_,_,Init_str,Frac_str):-
    Frac_str = Init_str.
generate_latex_updated_fraction_vector([H|T],[H1|T1],Mag,Pro,Init_str,Frac_str):-
    (H>0 ->
    	(Pro=:=1 ->
		    (Init_str == "" ->
		    string_concatenate(["1"],"",Str);

		    string_concatenate([",1"],"",Str)

		    );
		    (Init_str == "" ->
		    string_concatenate(["\\\\\\\\frac{",H,"}{",Mag,"}"],"",Str);

		    string_concatenate([",\\\\\\\\frac{",H,"}{",Mag,"}"],"",Str)
		    )
		);

		(H=:=0  ->
		    Str=""; 
		    H2 is abs(H),
		    (Init_str == "" ->
		    	string_concatenate(["-\\\\\\\\frac{",H2,"}{",Mag,"}"],"",Str);
		    	string_concatenate([",-\\\\\\\\frac{",H2,"}{",Mag,"}"],"",Str)
		    )


		)
	),

    string_concat(Init_str, Str,Temp_str),
    generate_latex_updated_fraction_vector(T,T1,Mag,Pro,Temp_str,Frac_str).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%latex Vector Generation Function for variable likr xi+yj+zk type
generate_latex_var_vector_ijk([],_,Init_str,Latex_str):-
    Latex_str = Init_str.
generate_latex_var_vector_ijk([H|T],[H1|T1],Init_str,Latex_str):-
 
    (Init_str == "" ->
	    string_concatenate(["latex(",H,"\\\\\\\\hat{",H1,"})"],"",Str1);
	    string_concatenate(["+latex(",H,"\\\\\\\\hat{",H1,"})"],"",Str1)
	),

    string_concat(Init_str, Str1,Temp_str),
    generate_latex_var_vector_ijk(T,T1,Temp_str,Latex_str).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%latex Vector Generation Function
generate_latex_vector_ijk([],_,Init_str,Latex_str):-
    Latex_str = Init_str.
generate_latex_vector_ijk([H|T],[H1|T1],Init_str,Latex_str):-
    (H>0 ->
    	(H=:=1 ->
		    (Init_str == "" ->
		    string_concatenate(["latex(\\\\\\\\hat{",H1,"})"],"",Str1);
		    string_concatenate(["latex(+\\\\\\\\hat{",H1,"})"],"",Str1)
		    );
		    (Init_str == "" ->
		    string_concatenate(["latex(",H,"\\\\\\\\hat{",H1,"})"],"",Str1);
		    string_concatenate(["latex(+",H,"\\\\\\\\hat{",H1,"})"],"",Str1)
		    )
		);

		(H=:=0 ->
		    Str1="";
		    H2 is abs(H),

			(H2=:=1 ->
			    (Init_str == "" ->
			    string_concatenate(["latex(-\\\\\\\\hat{",H1,"})"],"",Str1);
			    string_concatenate(["latex(-\\\\\\\\hat{",H1,"})"],"",Str1)
			    );
			    string_concatenate(["latex(-",H2,"\\\\\\\\hat{",H1,"})"],"",Str1)
			    
			)		
		)
	),

    string_concat(Init_str, Str1,Temp_str),
    generate_latex_vector_ijk(T,T1,Temp_str,Latex_str).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%latex Generation Function
generate_abc([],_,Init_str,Latex_str):-
    Latex_str = Init_str.
generate_abc([H|T],[H1|T1],Init_str,Latex_str):-
    (H>0 ->
    	(H=:=1 ->
		    (Init_str == "" ->
		    string_concatenate(["\\\\\\\\overrightarrow{",H1,"}"],"",Str1);
		    string_concatenate(["+\\\\\\\\overrightarrow{",H1,"}"],"",Str1)
		    );
		    (Init_str == "" ->
		    string_concatenate(["",H,"\\\\\\\\overrightarrow{",H1,"}"],"",Str1);
		    string_concatenate(["+",H,"\\\\\\\\overrightarrow{",H1,"}"],"",Str1)
		    )
		);

		(H=:=0 ->
		    Str1="";
		    H2 is abs(H),

			(H2=:=1 ->
			    (Init_str == "" ->
			    string_concatenate(["-\\\\\\\\overrightarrow{",H1,"}"],"",Str1);
			    string_concatenate(["-\\\\\\\\overrightarrow{",H1,"}"],"",Str1)
			    );
			    string_concatenate(["-",H2,"\\\\\\\\overrightarrow{",H1,"}"],"",Str1)
			    
			)		
		)
	),

    string_concat(Init_str, Str1,Temp_str),
    generate_abc(T,T1,Temp_str,Latex_str).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Vector Generation Function
generate_vector_ijk([],_,Init_str,Latex_str):-
    Latex_str = Init_str.
generate_vector_ijk([H|T],[H1|T1],Init_str,Latex_str):-
    (H>0 ->
    	(H=:=1 ->
		    (Init_str == "" ->
		    string_concatenate(["\\\\\\\\hat{",H1,"}"],"",Str1);
		    string_concatenate(["+\\\\\\\\hat{",H1,"}"],"",Str1)
		    );
		    (Init_str == "" ->
		    string_concatenate(["",H,"\\\\\\\\hat{",H1,"}"],"",Str1);
		    string_concatenate(["+",H,"\\\\\\\\hat{",H1,"}"],"",Str1)
		    )
		);

		(H=:=0 ->
		    Str1="";
		    H2 is abs(H),

			(H2=:=1 ->
			    (Init_str == "" ->
			    string_concatenate(["-\\\\\\\\hat{",H1,"}"],"",Str1);
			    string_concatenate(["-\\\\\\\\hat{",H1,"}"],"",Str1)
			    );
			    string_concatenate(["-",H2,"\\\\\\\\hat{",H1,"}"],"",Str1)
			    
			)		
		)
	),

    string_concat(Init_str, Str1,Temp_str),
    generate_vector_ijk(T,T1,Temp_str,Latex_str).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Vector Generation Function
generate_vector([],_,Init_str,Str):-
    Str = Init_str.
generate_vector([H|T],[H1|T1],Init_str,Str):-
    (H>0 ->
    	(H=:=1 ->
		    (Init_str == "" ->
		    string_concatenate(["1"],"",Str1);
		    string_concatenate([",1"],"",Str1)
		    );
		    (Init_str == "" ->
		    string_concatenate(["",H,""],"",Str1);
		    string_concatenate([",",H,""],"",Str1)
		    )
		);

		(H=:=0 ->
		    Str1="";
		    H2 is abs(H),

			(H2=:=1 ->
			    (Init_str == "" ->
			    string_concatenate(["-1"],"",Str1);
			    string_concatenate(["-1"],"",Str1)
			    );
			    (Init_str == "" ->
			    string_concatenate(["-",H2,""],"",Str1);
			    string_concatenate([",-",H2,""],"",Str1)
				)
			    
			)		
		)
	),
    string_concat(Init_str, Str1,Temp_str),
    generate_vector(T,T1,Temp_str,Str).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%(1+2)i+(4+5)j+(7+8)k;  //for 2 vectors

generate_sum_exp_ijk([],[],_,Init_str,Latex_str):-
    Latex_str = Init_str.
generate_sum_exp_ijk([H|T],[H1|T1],[H2|T2],Init_str,Latex_str):-
 
    (Init_str == "" ->
	    string_concatenate(["[(",H,")+(",H1,")]\\\\hat{",H2,"}"],"",Str1);
	    string_concatenate(["+[(",H,")+(",H1,")]\\\\hat{",H2,"}"],"",Str1)
	),

    string_concat(Init_str, Str1,Temp_str),
    generate_sum_exp_ijk(T,T1,T2,Temp_str,Latex_str).
 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%Latex Vector name Function
generate_latex_vector_name(Var,Latex_str_name):-
    string_concatenate(["latex(\\\\\\\\overrightarrow{",Var,"})"],"",Latex_str_name).

%Latex name Function
generate_vector_name(Var,Latex_str_name):-
    string_concatenate(["\\\\\\\\overrightarrow{",Var,"}"],"",Latex_str_name).

%Find magnitude
generate_magnitude([],Init_magnitude,Final_magnitude):-
	Final_magnitude is Init_magnitude.
generate_magnitude([H|T],Init_magnitude,Final_magnitude):-
	Temp is Init_magnitude+(H*H),
	generate_magnitude(T,Temp,Final_magnitude).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% magnitude expression
generate_magnitude_expression_ijk([],Init_mag_exp_str,Latex_mag_exp_str):-
	
	string_concat(Init_mag_exp_str,"}",Latex_mag_exp_str).
generate_magnitude_expression_ijk([H|T],Init_mag_exp_str,Latex_mag_exp_str):-
	(Init_mag_exp_str == "" ->
	    string_concatenate(["\\\\sqrt{(",H,")^2"],"",Str1);
	    string_concatenate(["+(",H,")^2"],"",Str1)
    ),
    string_concat(Init_mag_exp_str, Str1,Temp_str),
    generate_magnitude_expression_ijk(T,Temp_str,Latex_mag_exp_str).

% magnitude expression
generate_magnitude_den_expression_ijk([],_,Init_mag_exp_str,Latex_mag_exp_str):-
	
	string_concat(Init_mag_exp_str,"}",Latex_mag_exp_str).
generate_magnitude_den_expression_ijk([H|T],Mag,Init_mag_exp_str,Latex_mag_exp_str):-
	(Init_mag_exp_str == "" ->
	    string_concatenate(["\\\\\\\\sqrt{(\\\\\\\\frac{",H,"}{",Mag,"})^2"],"",Str1);
	    string_concatenate(["+(\\\\\\\\frac{",H,"}{",Mag,"})^2"],"",Str1)
    ),
    string_concat(Init_mag_exp_str, Str1,Temp_str),
    generate_magnitude_den_expression_ijk(T,Mag,Temp_str,Latex_mag_exp_str).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%latex magnitude expression
generate_latex_magnitude_expression_ijk([],Init_mag_exp_str,Latex_mag_exp_str):-
	
	string_concat(Init_mag_exp_str,"})",Latex_mag_exp_str).
generate_latex_magnitude_expression_ijk([H|T],Init_mag_exp_str,Latex_mag_exp_str):-
	(Init_mag_exp_str == "" ->
	    string_concatenate(["latex(\\\\\\\\sqrt{(",H,")^2"],"",Str1);
	    string_concatenate(["+(",H,")^2"],"",Str1)
    ),
    string_concat(Init_mag_exp_str, Str1,Temp_str),
    generate_latex_magnitude_expression_ijk(T,Temp_str,Latex_mag_exp_str).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%equal List
check_list([H,T],Final_comp):-
	(H=:=T->
		Final_comp = "Equal";
		Final_comp = "Not Equal"
	).
check_list([],Final_comp):-
	Final_comp = "Equal".
check_list([H,H1|T],Final_comp):-
	(H=:=H1->
		check_list([H1|T],Final_comp);
		Final_comp = "Not Equal"
	).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%compare vector

compare_vectors([],[],Final_comp):-
	Final_comp = "Equal".
compare_vectors([H|T],[H1|T1],Final_comp):-
	(H=:=H1->
		compare_vectors(T,T1,Final_comp);
		Final_comp = "Not Equal"
	).

%sum of list
generate_sum([],Init_sum,Final_sum):-
	Final_sum = Init_sum.
generate_sum([H|T],Init_sum,Final_sum):-
	Temp is Init_sum+(H),
	generate_sum(T,Temp,Final_sum).

%sum of vectors
generate_sum_vector([],[],[]).
generate_sum_vector([H|T],[H1|T1],[Sum_H|Sum_T]):-
	Sum_H is (H)+(H1),
	generate_sum_vector(T,T1,Sum_T).

%difference of vectors
generate_diff_vector([],[],[]).
generate_diff_vector([H|T],[H1|T1],[Diff_H|Diff_T]):-
	Diff_H is (H)-(H1),
	generate_diff_vector(T,T1,Diff_T).

%sum of vectors  mid(/2)
generate_sum_vector_mid([],[],[]).
generate_sum_vector_mid([H|T],[H1|T1],[Sum_H|Sum_T]):-
	Temp is (H)+(H1),
	Sum_H is Temp/2,
	generate_sum_vector_mid(T,T1,Sum_T).

%multiplication of vectors
generate_mul_vector([],[],[]).
generate_mul_vector([H|T],[H1|T1],[Diff_H|Diff_T]):-
	Diff_H is (H)*(H1),
	generate_mul_vector(T,T1,Diff_T).

generate_list_of_two(List1):-
	random_between(-10,10,X1),
	random_between(-10,10,X2),
	List1 = [X1,X2].

generate_list(List1):-
	random_between(-10,10,X1),
	random_between(-10,10,X2),
	random_between(-10,10,X3),
	List1 = [X1,X2,X3].

generate_magnitude(Magnitude):-
	random_between(2,10,M),	
	Magnitude = M.

generate_angle(Angle):-
	List=[0,30,45,60,90],
	random_member(A,List),	
	Angle = A.

%vector check
check_vector([],[]).
check_vector([H|T],[H1|T1]):-
	H=H1,

	check_vector(T,T1).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%fraction of type 7*1/sqrt(5)i+(7*1)/sqrt(3) j vector latex 
generate_latex_mag_updated_fraction_vector_ijk([],_,_,_,_,Init_str,Latex_str):-
    Latex_str = Init_str.
generate_latex_mag_updated_fraction_vector_ijk([H|T],[H1|T1],Mag,Given_mag,Pro,Init_str,Latex_str):-
    (H>0 ->
    	(Pro=:=1 ->
    		(Given_mag=:=1 ->
			    (Init_str == "" ->
			    string_concatenate(["latex(\\\\\\\\hat{",H1,"})"],"",Str1);
			    string_concatenate(["latex(+\\\\\\\\hat{",H1,"})"],"",Str1)
			    );

			    (Init_str == "" ->
			    string_concatenate(["latex(\\\\\\\\frac{",Given_mag,"}{",Mag,"}\\\\\\\\hat{",H1,"})"],"",Str1);
		    	string_concatenate(["latex(+\\\\\\\\frac{",Given_mag,"}{",Mag,"}\\\\\\\\hat{",H1,"})"],"",Str1)
			    )
			);
		    (Init_str == "" ->
		    H3 is Given_mag*H,
		    string_concatenate(["latex(\\\\\\\\frac{",H3,"}{",Mag,"}\\\\\\\\hat{",H1,"})"],"",Str1);
		    H4 is Given_mag*H,
		    string_concatenate(["latex(+\\\\\\\\frac{",H4,"}{",Mag,"}\\\\\\\\hat{",H1,"})"],"",Str1)
		    )
		);

		(H=:=0  ->
		    Str1=""; 
		    H2 is abs(H),
		    H5 is Given_mag*H2,
		    string_concatenate(["latex(-\\\\\\\\frac{",H5,"}{",Mag,"}\\\\\\\\hat{",H1,"})"],"",Str1)
		)
	),

    string_concat(Init_str, Str1,Temp_str),
    generate_latex_mag_updated_fraction_vector_ijk(T,T1,Mag,Given_mag,Pro,Temp_str,Latex_str).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%fraction of type 7*1/sqrt(5)i+(7*1)/sqrt(3) j vector latex 
generate_latex_mag_updated_fraction_vector([],_,_,_,_,Init_str,Latex_str):-
    Latex_str = Init_str.
generate_latex_mag_updated_fraction_vector([H|T],[H1|T1],Mag,Given_mag,Pro,Init_str,Latex_str):-
    (H>0 ->
    	(Pro=:=1 ->
    		(Given_mag=:=1 ->
			    (Init_str == "" ->
			    string_concatenate(["1"],"",Str1);
			    string_concatenate([",1"],"",Str1)
			    );

			    (Init_str == "" ->
			    string_concatenate(["\\\\\\\\frac{",Given_mag,"}{",Mag,"}"],"",Str1);
		    	string_concatenate([",\\\\\\\\frac{",Given_mag,"}{",Mag,"}"],"",Str1)
			    )
			);
		    (Init_str == "" ->
		    H3 is Given_mag*H,
		    string_concatenate(["\\\\\\\\frac{",H3,"}{",Mag,"}"],"",Str1);
		    H4 is Given_mag*H,
		    string_concatenate([",\\\\\\\\frac{",H4,"}{",Mag,"}"],"",Str1)
		    )
		);

		(H=:=0  ->
		    Str1=""; 
		    H2 is abs(H),
		    H5 is Given_mag*H2,
			(Init_str == "" ->
		    	string_concatenate(["-\\\\\\\\frac{",H5,"}{",Mag,"}"],"",Str1);
		    	string_concatenate([",-\\\\\\\\frac{",H5,"}{",Mag,"}"],"",Str1)
		    )

		)
	),

    string_concat(Init_str, Str1,Temp_str),
    generate_latex_mag_updated_fraction_vector(T,T1,Mag,Given_mag,Pro,Temp_str,Latex_str).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%fraction of type 7*1/sqrt(5)i+(7*1)/sqrt(3) j vector latex 
generate_latex_mag_fraction_vector_ijk([],_,_,_,Init_str,Latex_str):-
    Latex_str = Init_str.
generate_latex_mag_fraction_vector_ijk([H|T],[H1|T1],Mag,Given_mag,Init_str,Latex_str):-
    (H>0 ->
    	(Mag=:=1 ->
    		(Given_mag=:=1 ->
			    (Init_str == "" ->
			    string_concatenate(["latex(\\\\hat{",H1,"})"],"",Str1);
			    string_concatenate(["+latex(\\\\hat{",H1,"})"],"",Str1)
			    );

			    (Init_str == "" ->
			    string_concatenate(["latex(\\frac{",Given_mag,"}{\\sqrt{",Mag,"}}\\\\hat{",H1,"})"],"",Str1);
		    	string_concatenate(["+latex(\\frac{",Given_mag,"}{\\sqrt{",Mag,"}}\\\\hat{",H1,"})"],"",Str1)
			    )
			);
		    (Init_str == "" ->
		    H3 is Given_mag*H,
		    string_concatenate(["latex(\\frac{",H3,"}{\\sqrt{",Mag,"}}\\\\hat{",H1,"})"],"",Str1);
		    H4 is Given_mag*H,
		    string_concatenate(["+latex(\\frac{",H4,"}{\\sqrt{",Mag,"}}\\\\hat{",H1,"})"],"",Str1)
		    )
		);

		(H=:=0  ->
		    Str1=""; 
		    H2 is abs(H),
		    H5 is Given_mag*H2,
		    string_concatenate(["-latex(\\frac{",H5,"}{\\sqrt{",Mag,"}}\\\\hat{",H1,"})"],"",Str1)
		)
	),

    string_concat(Init_str, Str1,Temp_str),
    generate_latex_mag_fraction_vector_ijk(T,T1,Mag,Given_mag,Temp_str,Latex_str).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%(-1-2)i+(2-3)j+(3-2)k   //for 2 vectors
generate_latex_diff_exp_ijk([],[],_,Init_str,Latex_str):-
    Latex_str = Init_str.
generate_latex_diff_exp_ijk([H|T],[H1|T1],[H2|T2],Init_str,Latex_str):-
 
    (Init_str == "" ->
	    string_concatenate(["latex([(",H,")-(",H1,")]\\\\\\\\hat{",H2,"})"],"",Str1);
	    string_concatenate(["+latex([(",H,")-(",H1,")]\\\\\\\\hat{",H2,"})"],"",Str1)
	),

    string_concat(Init_str, Str1,Temp_str),
    generate_latex_diff_exp_ijk(T,T1,T2,Temp_str,Latex_str).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%(1+2)i+(4+5)j+(7+8)k;  //for 2 vectors

generate_latex_sum_exp_ijk([],[],_,Init_str,Latex_str):-
    Latex_str = Init_str.
generate_latex_sum_exp_ijk([H|T],[H1|T1],[H2|T2],Init_str,Latex_str):-
 
    (Init_str == "" ->
	    string_concatenate(["latex([(",H,")+(",H1,")]\\\\\\\\hat{",H2,"})"],"",Str1);
	    string_concatenate(["latex(+[(",H,")+(",H1,")]\\\\\\\\hat{",H2,"})"],"",Str1)
	),

    string_concat(Init_str, Str1,Temp_str),
    generate_latex_sum_exp_ijk(T,T1,T2,Temp_str,Latex_str).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%(1+2+3)i+(4+5+6)j+(7+8+9)k;  //for 3 vectors
generate_latex_sum_exp_ijk([],[],[],_,Init_str,Latex_str):-
    Latex_str = Init_str.
generate_latex_sum_exp_ijk([H|T],[H1|T1],[H2|T2],[H3|T3],Init_str,Latex_str):-
 
    (Init_str == "" ->
	    string_concatenate(["latex([(",H,")+(",H1,")+(",H2,")]\\\\\\\\hat{",H3,"})"],"",Str1);
	    string_concatenate(["latex(+[(",H,")+(",H1,")+(",H2,")]\\\\\\\\hat{",H3,"})"],"",Str1)
	),

    string_concat(Init_str, Str1,Temp_str),
    generate_latex_sum_exp_ijk(T,T1,T2,T3,Temp_str,Latex_str).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%function for direction cosines 
generate_direction_cosines([],_,Init_str,Latex_str):-
    Latex_str = Init_str.
generate_direction_cosines([H|T],Mag,Init_str,Latex_str):-
    (H>0 ->

		    (Init_str == "" ->
		    string_concatenate(["\\\\\\\\frac{",H,"}{",Mag,"}"],"",Str1);
		    string_concatenate([",\\\\\\\\frac{",H,"}{",Mag,"}"],"",Str1)
		    )
		;

		(H=:=0  ->
		    Str1=",0"; 
		    H2 is abs(H),
			(Init_str == "" ->
		    string_concatenate(["-\\\\\\\\frac{",H2,"}{",Mag,"}"],"",Str1);
		    string_concatenate([",-\\\\\\\\frac{",H2,"}{",Mag,"}"],"",Str1)
		    )		
		)
	),

    string_concat(Init_str, Str1,Temp_str),
    generate_direction_cosines(T,Mag,Temp_str,Latex_str).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%Funtion to divide line segment internally
divide_internally([],[],_,_,[]).
divide_internally([H|T],[H1|T1],Ratio1,Ratio2,[R_H|R_T]):-
	R_H is Ratio1*H+Ratio2*H1,
	divide_internally(T,T1,Ratio1,Ratio2,R_T).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%Funtion to divide line segment externally

divide_externally([],[],_,_,[]).
divide_externally([H|T],[H1|T1],Ratio1,Ratio2,[R_H|R_T]):-
	R_H is Ratio1*H-Ratio2*H1,
	divide_externally(T,T1,Ratio1,Ratio2,R_T).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%fraction of type 1/5+1 i/3 j vector latex 
generate_latex_fraction_vector([],_,_,Init_str,Latex_str):-
    Latex_str = Init_str.
generate_latex_fraction_vector([H|T],[H1|T1],Mag,Init_str,Latex_str):-
    (H>0 ->
    	(Mag>0 ->
	    	(Mag=:=1 ->
	    		(H=:=1->
				    (Init_str == "" ->
				    string_concatenate(["latex(\\\\\\\\hat{",H1,"})"],"",Str1);
				    string_concatenate(["latex(+\\\\\\\\hat{",H1,"})"],"",Str1)
				    );
				    (Init_str == "" ->
				    string_concatenate(["latex(",H,"\\\\\\\\hat{",H1,"})"],"",Str1);
				    string_concatenate(["latex(+",H,"\\\\\\\\hat{",H1,"})"],"",Str1)
				    )
				);

			    (Init_str == "" ->
			    string_concatenate(["latex(\\\\\\\\frac{",H,"}{",Mag,"}\\\\\\\\hat{",H1,"})"],"",Str1);
			    string_concatenate(["latex(+\\\\\\\\frac{",H,"}{",Mag,"}\\\\\\\\hat{",H1,"})"],"",Str1)
			    )
			);
			Mag1 is abs(Mag),
			(Mag1=:=1 ->
	    		(H=:=1->
				    (Init_str == "" ->
				    string_concatenate(["latex(-\\\\\\\\hat{",H1,"})"],"",Str1);
				    string_concatenate(["latex(-\\\\\\\\hat{",H1,"})"],"",Str1)
				    );
				    (Init_str == "" ->
				    string_concatenate(["latex(-",H,"\\\\\\\\hat{",H1,"})"],"",Str1);
				    string_concatenate(["latex(-",H,"\\\\\\\\hat{",H1,"})"],"",Str1)
				    )
				);

			    (Init_str == "" ->
			    string_concatenate(["latex(-\\\\\\\\frac{",H,"}{",Mag1,"}\\\\\\\\hat{",H1,"})"],"",Str1);
			    string_concatenate(["latex(-\\\\\\\\frac{",H,"}{",Mag1,"}\\\\\\\\hat{",H1,"})"],"",Str1)
			    )

			)
		);

		(H=:=0  ->
		    Str1=""; 
		    H2 is abs(H),
		    (Mag>0 ->
			    (Mag=:=1 ->
		    		(H=:=1->
					    (Init_str == "" ->
					    string_concatenate(["latex(-\\\\\\\\hat{",H1,"})"],"",Str1);
					    string_concatenate(["latex(-\\\\\\\\hat{",H1,"})"],"",Str1)
					    );
					    (Init_str == "" ->
					    string_concatenate(["latex(-",H2,"\\\\\\\\hat{",H1,"})"],"",Str1);
					    string_concatenate(["latex(-",H2,"\\\\\\\\hat{",H1,"})"],"",Str1)
					    )
					);

				    (Init_str == "" ->
				    string_concatenate(["latex(-\\\\\\\\frac{",H2,"}{",Mag,"}\\\\\\\\hat{",H1,"})"],"",Str1);
				    string_concatenate(["latex(-\\\\\\\\frac{",H2,"}{",Mag,"}\\\\\\\\hat{",H1,"})"],"",Str1)
				    )
				);
				Mag1 is abs(Mag),
				(Mag1=:=1 ->
		    		(H=:=1->
					    (Init_str == "" ->
					    string_concatenate(["latex(\\\\\\\\hat{",H1,"})"],"",Str1);
					    string_concatenate(["latex(+\\\\\\\\hat{",H1,"})"],"",Str1)
					    );
					    (Init_str == "" ->
					    string_concatenate(["latex(",H2,"\\\\\\\\hat{",H1,"})"],"",Str1);
					    string_concatenate(["+latex(+",H2,"\\\\\\\\hat{",H1,"})"],"",Str1)
					    )
					);

				    (Init_str == "" ->
				    string_concatenate(["latex(\\\\\\\\frac{",H2,"}{",Mag1,"}\\\\\\\\hat{",H1,"})"],"",Str1);
				    string_concatenate(["latex(+\\\\\\\\frac{",H2,"}{",Mag1,"}\\\\\\\\hat{",H1,"})"],"",Str1)
				    )

				)
			)

		)
	),

    string_concat(Init_str, Str1,Temp_str),
    generate_latex_fraction_vector(T,T1,Mag,Temp_str,Latex_str).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%fraction of type 1/5,1/5,6/8 
generate_fraction_vector([],_,_,Init_str,Latex_str):-
    Latex_str = Init_str.
generate_fraction_vector([H|T],[H1|T1],Mag,Init_str,Latex_str):-
    (H>0 ->
    	(Mag>0 ->
	    	(Mag=:=1 ->
	    		(H=:=1->
				    (Init_str == "" ->
				    string_concatenate(["1"],"",Str1);
				    string_concatenate([",1"],"",Str1)
				    );
				    (Init_str == "" ->
				    string_concatenate(["",H,""],"",Str1);
				    string_concatenate([",",H,""],"",Str1)
				    )
				);

			    (Init_str == "" ->
			    string_concatenate(["\\\\\\\\frac{",H,"}{",Mag,"}"],"",Str1);
			    string_concatenate([",\\\\\\\\frac{",H,"}{",Mag,"}"],"",Str1)
			    )
			);
			Mag1 is abs(Mag),
			(Mag1=:=1 ->
	    		(H=:=1->
				    (Init_str == "" ->
				    string_concatenate(["-1"],"",Str1);
				    string_concatenate([",-1"],"",Str1)
				    );
				    (Init_str == "" ->
				    string_concatenate(["-",H,""],"",Str1);
				    string_concatenate([",-",H,""],"",Str1)
				    )
				);

			    (Init_str == "" ->
			    string_concatenate(["-\\\\\\\\frac{",H,"}{",Mag1,"}"],"",Str1);
			    string_concatenate([",-\\\\\\\\frac{",H,"}{",Mag1,"}"],"",Str1)
			    )

			)
		);

		(H=:=0  ->
		    Str1=""; 
		    H2 is abs(H),
		    (Mag>0 ->
			    (Mag=:=1 ->
		    		(H=:=1->
					    (Init_str == "" ->
					    string_concatenate(["-1"],"",Str1);
					    string_concatenate([",-1"],"",Str1)
					    );
					    (Init_str == "" ->
					    string_concatenate(["-",H2,""],"",Str1);
					    string_concatenate([",-",H2,""],"",Str1)
					    )
					);

				    (Init_str == "" ->
				    string_concatenate(["-\\\\\\\\frac{",H2,"}{",Mag,"}"],"",Str1);
				    string_concatenate([",-\\\\\\\\frac{",H2,"}{",Mag,"}"],"",Str1)
				    )
				);
				Mag1 is abs(Mag),
				(Mag1=:=1 ->
		    		(H=:=1->
					    (Init_str == "" ->
					    string_concatenate(["1"],"",Str1);
					    string_concatenate([",1"],"",Str1)
					    );
					    (Init_str == "" ->
					    string_concatenate(["",H2,""],"",Str1);
					    string_concatenate([",",H2,""],"",Str1)
					    )
					);

				    (Init_str == "" ->
				    string_concatenate(["\\\\\\\\frac{",H2,"}{",Mag1,"}"],"",Str1);
				    string_concatenate([",\\\\\\\\frac{",H2,"}{",Mag1,"}"],"",Str1)
				    )

				)
			)

		)
	),

    string_concat(Init_str, Str1,Temp_str),
    generate_fraction_vector(T,T1,Mag,Temp_str,Latex_str).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%check for collinear
check_collinear([],[],[]).
check_collinear([H|T],[H1|T1],[C_H|C_T]):-
	(abs(H)>abs(H1)->
		(H1=:=0->
			C_H is 0;
			C_H is (H)/(H1)
		);
		(H=:=0->
			C_H is 0;
		 	C_H is (H1)/(H)
		)
	),
	check_collinear(T,T1,C_T).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%latex Vector Generation Function
generate_latex_vector_component([],_,Init_str,Latex_str):-
    Latex_str = Init_str.
generate_latex_vector_component([H|T],[H1|T1],Init_str,Latex_str):-
    (H>0 ->
    	(H=:=1 ->
		    (Init_str == "" ->
		    string_concatenate(["\\\\\\\\hat{",H1,"}"],"",Str1);
		    string_concatenate([",\\\\\\\\hat{",H1,"}"],"",Str1)
		    );
		    (Init_str == "" ->
		    string_concatenate(["",H,"\\\\\\\\hat{",H1,"}"],"",Str1);
		    string_concatenate([",",H,"\\\\\\\\hat{",H1,"}"],"",Str1)
		    )
		);
		(H=:=0 ->
		    Str1=""; 
			(Init_str == "" ->
			string_concatenate(["",H,"\\\\\\\\hat{",H1,"}"],"",Str1);
			string_concatenate([",",H,"\\\\\\\\hat{",H1,"}"],"",Str1)
			)
		)
	),

    string_concat(Init_str, Str1,Temp_str),
   generate_latex_vector_component(T,T1,Temp_str,Latex_str).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%latex Vector Generation Function
generate_vector_component([],_,Init_str,Latex_str):-
    Latex_str = Init_str.
generate_vector_component([H|T],[H1|T1],Init_str,Latex_str):-
    (H>0 ->
    	(H=:=1 ->
		    (Init_str == "" ->
		    string_concatenate(["1"],"",Str1);
		    string_concatenate([",1"],"",Str1)
		    );
		    (Init_str == "" ->
		    string_concatenate(["",H,""],"",Str1);
		    string_concatenate([",",H,""],"",Str1)
		    )
		);
		(H=:=0 ->
		    Str1=""; 
			(Init_str == "" ->
			string_concatenate(["",H,""],"",Str1);
			string_concatenate([",",H,""],"",Str1)
			)
		)
	),

    string_concat(Init_str, Str1,Temp_str),
   generate_vector_component(T,T1,Temp_str,Latex_str).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

   find_lanbda([H|_],[H1|_],Lam):-
	(abs(H)>abs(H1)->
		(H1=:=0->
			Lam is 0;
			Lam is (H)/(H1)
		);
		(H=:=0->
			Lam is 0;
			Lam is (H1)/(H)
		)
	).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Ffunction to simplify squareroots like sqrt(9)=3
%Add 	
% get_updated_coefficient([[[1,1],[Mag,1]]],X,_),

get_updated_coefficient_result([H|_],M,S):-
	get_updated_coefficient_result1(H,M,S).

get_updated_coefficient_result1([H|T],M,S):-
	get_updated_coefficient_result_M(H,M),	
	get_updated_coefficient_result_S(T,S).

get_updated_coefficient_result_M([H|_],M):-
	M is H.

get_updated_coefficient_result_S([H|_],S):-
	get_updated_coefficient_result_S1(H,S).

get_updated_coefficient_result_S1([H|_],S):-
	S is H.	

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%(2^2)+(3^2)+(6^2)=Final_sum here power =2
list_power_sum([],_,Init_sum,Final_sum):-
	Final_sum is Init_sum.

list_power_sum([H|T],Power,Init_sum,Final_sum):-
	Temp is (H**Power)+Init_sum,
	list_power_sum(T,Power,Temp,Final_sum).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%compute [3,-5].[2,7]=[10,11,-35]
product_scalar_vector1(H,[T1|_],F_1):-
	F_1 is (H)*(T1).
product_scalar_vector2([T|_],H1,F_2):-
	F_2 is (T)*(H1).
product_scalar_vector3([T|_],[T1|_],F_3):-
	F_3 is (T)*(T1).
product_scalar_vector([H|T],[H1|T1],List):-
	F_0 is (H)*(H1),
	product_scalar_vector1(H,T1,F_1),
	product_scalar_vector2(T,H1,F_2),
	product_scalar_vector3(T,T1,F_3),
	F_4 is (F_1)+(F_2),
	List = [F_0,F_4,F_3].
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%6|a|^2+11|a||b|-35|b|^2
generate_latex_product2([H|_],Init_str,Str3):-
	(H>0 ->
    	(H=:=1 ->
		    (Init_str == "" ->
		    string_concatenate(["|\\\\overrightarrow{b}|^2"],"",Str3);
		    string_concatenate(["+|\\\\overrightarrow{b}|^2"],"",Str3)
		    );
		    (Init_str == "" ->
		    string_concatenate(["",H,"|\\\\overrightarrow{b}|^2"],"",Str3);
		    string_concatenate(["+",H,"|\\\\overrightarrow{b}|^2"],"",Str3)
		    )
		);

		(H=:=0 ->
		    Str3=""; 
		    string_concatenate(["",H,"|\\\\overrightarrow{b}|^2"],"",Str3)
		)
	).

generate_latex_product1([H|T],Init_str,Str2):-
	(H>0 ->
    	(H=:=1 ->
		    (Init_str == "" ->
		    string_concatenate(["|\\\\overrightarrow{a}|.|\\\\overrightarrow{b}|"],"",Str);
		    string_concatenate(["+|\\\\overrightarrow{a}|.|\\\\overrightarrow{b}|"],"",Str)
		    );
		    (Init_str == "" ->
		    string_concatenate(["",H,"|\\\\overrightarrow{a}|.|\\\\overrightarrow{b}|"],"",Str);
		    string_concatenate(["+",H,"|\\\\overrightarrow{a}|.|\\\\overrightarrow{b}|"],"",Str)
		    )
		);

		(H=:=0 ->
		    Str=""; 
		    string_concatenate(["",H,"|\\\\overrightarrow{a}|.|\\\\overrightarrow{b}|"],"",Str)
		)
	),
	generate_latex_product2(T,Str,Str3),
	string_concatenate([Str,Str3],"",Str2).


generate_latex_product([H|T],Init_str,Final_str):-
	(H>0 ->
    	(H=:=1 ->
		    (Init_str == "" ->
		    string_concatenate(["|\\\\overrightarrow{a}|^2"],"",Str1);
		    string_concatenate(["+|\\\\overrightarrow{a}|^2"],"",Str1)
		    );
		    (Init_str == "" ->
		    string_concatenate(["",H,"|\\\\overrightarrow{a}|^2"],"",Str1);
		    string_concatenate(["+",H,"|\\\\overrightarrow{a}|^2"],"",Str1)
		    )
		);

		(H=:=0 ->
		    Str1=""; 
		    string_concatenate(["",H,"|\\\\overrightarrow{a}|^2"],"",Str1)
		)
	),
	generate_latex_product1(T,Str1,Str2),
	string_concatenate([Str1,Str2],"",Final_str).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
cross_product(List1,List2,Product):-
	List1=[X1,Y1,Z1],
	List2=[X2,Y2,Z2],
	P1 is (((Y1)*(Z2))-((Z1)*(Y2))),
	P2 is -(((X1)*(Z2))-((Z1)*(X2))),
	P3 is (((X1)*(Y2))-((Y1)*(X2))),
	Product = [P1,P2,P3].
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

generate_latex_cross_product(List1,List2,Latex_cross_product):-
	List1=[X1,Y1,Z1],
	List2=[X2,Y2,Z2],
	P1 is ((Y1)*(Z2)),
	Q1 is ((Z1)*(Y2)),
	P2 is ((X1)*(Z2)),
	Q2 is ((Z1)*(X2)),
	P3 is ((X1)*(Y2)),
	Q3 is ((Y1)*(X2)),
	string_concatenate(["[(",P1,")-(",Q1,")]\\\\hat{i} - [(",P2,")-(",Q2,")]\\\\hat{j} + [(",P3,")-(",Q3,")]\\\\hat{k}"],"",Latex_cross_product).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
multiply_list([],_,[]).
multiply_list([H|T],Mag,[F_H|F_T]):-
	F_H is (Mag)*(H),
	multiply_list(T,Mag,F_T).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_one_var_in_form_of_other(Coeff_x1,Coeff_y1,Constant1,Coeff_x2,Coeff_y2,Constant2,X_value,Y_value,N_X,D_X,N_Y,D_Y):-
   Coeff_y11 is -1*Coeff_y1,

   Constant_new1 is Coeff_x2*Constant1,
   Coeff_y_new1 is Coeff_x2*Coeff_y11,
   Coeff_y_new2 is Coeff_x1*Coeff_y2,
   Constant_new2 is Coeff_x1*Constant2,
   Constant_new is Constant_new2-Constant_new1,
   Coeff_y_new is Coeff_y_new1 + Coeff_y_new2,

   Y_value is Constant_new/Coeff_y_new,
   X_value is (Constant1 +Coeff_y11*Y_value)/Coeff_x1,
   simplify(Constant_new,Coeff_y_new,N_Y,D_Y),
   Num_X_value is (Constant1*D_Y +Coeff_y11*N_Y),
   Den_X_value is (Coeff_x1*D_Y),
   simplify(Num_X_value,Den_X_value,N_X,D_X).
 

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
convert_square_brackets_to_round_brackets(List,List_round_brackets):-
	term_string(List,List_string),
	sub_string(List_string, _,_,Z,"["),
	New is Z-1,
	sub_string(List_string, 1,New,_,List_brackets_remove),
	string_concatenate(["(",List_brackets_remove,")"],"",List_round_brackets).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
remove_square_brackets_from_string(String,String_without_brackets):-
	sub_string(String, _,_,Z,"["),
	New is Z-1,
	sub_string(String, 1,New,_,String_without_brackets).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
/*List 1= a1+a2+a3,
List 2= b1+b2+b3,
a1.b1+a2.b2+a3.b3=0.*/
generate_list_with_condition(List1,List2,List3):-
	generate_list(L1),
	generate_list(L2),	
    generate_mul_vector(L1,L2,Mul),
    generate_sum(Mul,0,Result),
    (Result=\=0->
		generate_list_with_condition(List1,List2,List3);    	
		(generate_list(L3),
		List1 = L1,
		List2 = L2,
		List3 = L3)
	).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%Multiple choice questtion

get_option_from_list_of_options(List,Ans,Answer_type_str):-

List=[Option,Option1,Option2,Option3],
Final_result is Ans,
string_concatenate(["objective_answer_types(["],"",Answer_type1),
(Final_result == 0 ->
string_concatenate([Answer_type1,"[string(A),type(continuous(",Option,"))],[string(B),type(continuous(",Option1,"))],[sting(C),type(continuous(",Option2,"))],[string(D),type(continuous(",Option3,"))]])"],"",Answer_type_str)
;
(Final_result == 1 ->
string_concatenate([Answer_type1,"[string(A),type(continuous(",Option1,"))],[string(B),type(continuous(",Option,"))],[string(C),type(continuous(",Option2,"))],[string(D),type(continuous(",Option3,"))]])"],"",Answer_type_str)
;
(Final_result == 2 ->
string_concatenate([Answer_type1,"[string(A),type(continuous(",Option1,"))],[string(B),type(continuous(",Option2,"))],[string(C),type(continuous(",Option,"))],[string(D),type(continuous(",Option3,"))]])"],"",Answer_type_str)
;
(Final_result == 3 ->
string_concatenate([Answer_type1,"[string(A),type(continuous(",Option1,"))],[string(B),type(continuous(",Option3,"))],[string(C),type(continuous(",Option2,"))],[string(D),type(continuous(",Option,"))]])"],"",Answer_type_str)
;
write("wrong answer in gfs"),nl)))).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
generate_two_different_number(List):-
	random_between(-10,10,X1),
	random_between(-10,10,X2),
    (X1=:=X2->
    	generate_two_different_number(List);
    	List = [X1,X2]
    ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%Function to genertate one equal list
generate_equal_list(List):-
	generate_list(L1),
	check_list(L1,Check_result),
	(Check_result=="Not Equal"->
		generate_equal_list(List);
		List = L1
	).

generate_random_list_equal_or_unequal(List):-
	random_between(0,1,X),
	(X=:=0->
		generate_equal_list(List);
		generate_list(List)
	).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%Function to genertate collinear vector list

generate_collinear_list(List1,List2):-
	generate_list(L1),
	generate_list(L2),
	check_collinear(L1,L2,Coll),
	check_list(Coll,Check_result),
	
	(Check_result=="Not Equal"->
		generate_collinear_list(List1,List2);
		List1 = L1,
		List2 = L2
	).
generate_random_list_collinear_or_non_collinear(List1,List2):-
	random_between(0,1,X),
	(X=:=0->
		generate_collinear_list(List1,List2);
		generate_list(List1),
		generate_list(List2)
	).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%Function to genertate two equal list
generate_two_equal_list(List1,List2):-
	generate_list(L1),
	generate_list(L2),
	compare_vectors(L1,L2,Comp_result),
	(Comp_result=="Not Equal"->
		generate_two_equal_list(List1,List2);
		List1 = L1,
		List2 = L2
	).
generate_random_list_equal_or_unequal_vectors(List1,List2):-
	random_between(0,1,X),
	(X=:=0->
		generate_two_equal_list(List1,List2);
		generate_list(List1),
		generate_list(List2)
	).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%Funtion for unit vector

generate_unit_vector_list(List,Mag):-
	generate_magnitude(M1),
	generate_list(L1),
	list_power_sum(L1,2,0,Pow_sum),
	Pow_mag is (M1**2),
	(Pow_sum=\=Pow_mag->
		generate_unit_vector_list(List,Mag);
		List=L1,
		Mag=M1
	).
generate_random_list_magnitude_for_unit_vector(List,Mag):-
	random_between(0,1,X),
	(X=:=0->
		generate_unit_vector_list(List,Mag);
		generate_list(List),
		generate_magnitude(Mag)
	).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%Funtion for mutually perpendicular vector

generate_mutually_perpendicular_vector_list(List1,List2):-
	generate_list(L1),
	generate_list(L2),
	generate_mul_vector(L1,L2,Mul),
    generate_sum(Mul,0,Result),
	(Result=\=0->
		generate_mutually_perpendicular_vector_list(List1,List2);
		List1=L1,
		List2=L2
	).

generate_random_list_for_mutually_perpendicular_vector(List1,List2):-
	random_between(0,1,X),
	(X=:=0->
		generate_mutually_perpendicular_vector_list(List1,List2);
		generate_list(List1),
		generate_list(List2)
	).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%Funtion for three points to be collinear or not

generate_collinear_point_list(List1,List2,List3):-

	generate_list(L1),
	generate_list(L2),
	generate_list(L3),

	generate_diff_vector(L2,L1,Diff_1),
	generate_diff_vector(L3,L2,Diff_2),
	generate_diff_vector(L1,L3,Diff_3),

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
    	generate_collinear_point_list(List1,List2,List3);
    	(S2=\=S3->
    		generate_collinear_point_list(List1,List2,List3);
			(M3=:=M1+M2->
    			List1=L1,
    			List2=L2,
    			List3=L3;
	    	    (M2=:=M3+M1->
	    			List1=L1,
	    			List2=L2,
	    			List3=L3;	     			
					(M1=:=M3+M2->
						List1=L1,
		    			List2=L2,
		    			List3=L3;
    					generate_collinear_point_list(List1,List2,List3)
					)
				)
    		)
    	)
	).

	


generate_random_list_for_collinear_point_list(List1,List2,List3):-

	random_between(0,1,X),
	(X=:=0->
		generate_collinear_point_list(List1,List2,List3);
		generate_list(List1),
		generate_list(List2),
		generate_list(List3)
	).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%Funtion for three points to be vertices of right angle triangle

generate_right_angle_triangle_point_list(List1,List2,List3):-

	generate_list(L1),
	generate_list(L2),
	generate_list(L3),

	generate_diff_vector(L2,L1,Diff_1),
	generate_diff_vector(L3,L2,Diff_2),
	generate_diff_vector(L1,L3,Diff_3),

    generate_magnitude(Diff_1,0,Diff_1_magnitude),
    generate_magnitude(Diff_2,0,Diff_2_magnitude),
    generate_magnitude(Diff_3,0,Diff_3_magnitude),

    (Diff_3_magnitude=:=Diff_1_magnitude+Diff_2_magnitude->
    		List1=L1,
			List2=L2,
			List3=L3;
    	    (Diff_2_magnitude=:=Diff_3_magnitude+Diff_1_magnitude->
				List1=L1,
				List2=L2,
				List3=L3;
     			(Diff_1_magnitude=:=Diff_3_magnitude+Diff_2_magnitude->
		    		List1=L1,
					List2=L2,
					List3=L3;
					generate_right_angle_triangle_point_list(List1,List2,List3)				)
			)
    ).

	


generate_random_list_for_right_angle_triangle_point_list(List1,List2,List3):-

	random_between(0,1,X),
	(X=:=0->
		generate_right_angle_triangle_point_list(List1,List2,List3);
		generate_list(List1),
		generate_list(List2),
		generate_list(List3)
	).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%Funtion for perpendicular vector (a+b).(a-b)=0

generate_perpendicular_vector_list(List1,List2):-
	generate_list(L1),
	generate_list(L2),

    generate_sum_vector(L1,L2,Sum),
    generate_diff_vector(L1,L2,Diff),
    generate_mul_vector(Sum,Diff,Mul),
    generate_sum(Mul,0,Result),
	(Result=\=0->
		generate_perpendicular_vector_list(List1,List2);
		List1=L1,
		List2=L2
	).

generate_random_list_for_perpendicular_vector(List1,List2):-
	random_between(0,1,X),
	(X=:=0->
		generate_perpendicular_vector_list(List1,List2);
		generate_list(List1),
		generate_list(List2)
	).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%