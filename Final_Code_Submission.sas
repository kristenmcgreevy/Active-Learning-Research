*********************************************************************
*  TITLE :      Active Learning Code For Submission                                
*  JOB NAME:    Final_Code_Submission                                                                
*                                                                   
*  NAME:        Kristen McGreevy  
*  COAUTHOR:	Frank Church                            
*  DATE:        12/28/2018                                                                       
*                                                                   
********************************************************************;

libname act "/folders/myfolders/Active_Learning";
libname data "/folders/myfolders/Active_Learning/Datasets";
libname data2 "/folders/myfolders/Active_Learning/Data_ActiveLabel";
libname fin "/folders/myfolders/Active_Learning/Final_Comparison_Data";	



***********************************************************;
********************** SURVEY CODE ************************;
***********************************************************;
*** Data.survey_3factors is the final version used ***;


*** DESCRIPTIVE STATISTICS ON SURVEY ***;
PROC MEANS DATA=data.Survey_3factors N MIN MEDIAN MAX MEAN STD SKEW KURT MAXDEC=3;
	VAR score year;
	class factor;
RUN;


*** MANN-KENDALL TIME TREND ANALYSIS ON THREE SURVEY FACTORS ***;
proc sort data=data.Survey_3factors out=work.class;
   by factor;
run;
proc corr data=work.class kendall ;
   var score;
   with year;
   by factor;
   ods output Kendallcorr= newdata;
run;


*** PLOT SURVEY TIME TREND DATA AND LOESS FIT CURVE ***;
data survey_data2;
	set data.survey_3factors;
	length course $ 11 ;
	if year=2012.5 then course="Fall 2012";
	if year=2013.5 then course="Fall 2013";
	if year=2014.5 then course="Fall 2014";
	if year=2015.5 then course="Fall 2015";
	if year=2016.0 then course="Spring 2016";
	if year=2017.0 then course="Spring 2017";
	if year=2017.5 then course="Fall 2017";
	if year=2018.0 then course="Spring 2018";
	
	if factor = "Indiv_Utility" then Factor1="Perceived Individual Utility";
	if factor = "Theor_Utility" then Factor1="Perceived General Utility";
	if factor = "Team_Situation" then Factor1="Team Situation";
run;
title "Scatter Plot of Individual Utility with Time Trend line";
Proc sgplot data= survey_Data2;
	where factor="Indiv_Utility";
	scatter x=year y=score / group=factor1;
	loess x=year y=score / group=factor1 lineattrs=(color=darkblue);
	format year 4.0;
	xaxis label="Course Year" VALUES=(2012 TO 2018 BY 1);
	yaxis label="Survey Scores by Question";
run;



***********************************************************;
********** INTRA-EXAM ACTIVE AND LECTURE SCORE ************;
***********************************************************;

*** MACRO WILL TAKE IN LABEL1 WHICH IS PART OF DATASET NAME, QNUM1-3 WHICH IS THE NUMBER OF
QUESTIONS FOR EXAM 1,2,& 3. THIS MACRO DETERMINES A STUDENT'S SCORE FOR ACTIVE LEARNING AND 
LECTURE LEARNING QUESTIONS ***;

%MACRO Active_Count(label1, qnum1, qnum2, qnum3);
data &label1._active_count;
	set data2.&label1._merged;
	retain  Exam1_active_score Exam2_active_score Exam3_active_score 
			Exam1_NOTact_score Exam2_NOTact_score Exam3_NOTact_score 0;
	
	array act{*} act_Q001-act_Q064;
	array sub{*} sub_Q001-sub_Q064;
	
	array ex1{*} A_1_001-A_1_0&qnum1.;
	array ex2{*} A_2_001-A_2_0&qnum2.;
	array ex3{*} A_3_001-A_3_0&qnum3.;
	
	Exam1_active_score=0; Exam1_NOTact_score=0;
	if unit=1 then
		do i=1 to &qnum1.;
		if act{i}="Active" then Exam1_active_score= Exam1_active_score + SUM(of ex1{i});
		else Exam1_NOTact_score= Exam1_NOTact_score + SUM(of ex1{i});
		end;

	Exam2_active_score=0; Exam2_NOTact_score=0;		
	if unit=2 then
		do i=1 to &qnum2.;
		if act{i}="Active" then Exam2_active_score= Exam2_active_score + SUM(of ex2{i});
		else Exam2_NOTact_score= Exam2_NOTact_score + SUM(of ex2{i});
		end;	
		
	Exam3_active_score=0; Exam3_NOTact_score=0;		
	if unit=3 then
		do i=1 to &qnum3.;
		if act{i}="Active" then Exam3_active_score= Exam3_active_score + SUM(of ex3{i});
		else Exam3_NOTact_score= Exam3_NOTact_score + SUM(of ex3{i});
		end;		
run;
proc print data= &label1._active_count (obs=15);
	var idnumber act_Q010 Exam1_active_score Exam1_NOTact_score Exam2_active_score Exam2_NOTact_score Exam3_active_score;
run;
proc print data=&label1._active_count (obs=10);
	var A_3_001-A_3_0&qnum3.;
run;	
data &label1._active_count_2;
	set &label1._active_count;	
	retain  Exam1_active_count Exam2_active_count Exam3_active_count 
			Exam1_NOT_count Exam2_NOT_count Exam3_NOT_count 0; 
	array act{*} act_Q001-act_Q064;
	array ex1{*} A_1_001-A_1_0&qnum1.;
	array ex2{*} A_2_001-A_2_0&qnum2.;
	array ex3{*} A_3_001-A_3_0&qnum3.;
	
	Exam1_active_count=0; Exam1_NOT_count=0; exam1_missing_active=0; exam1_missing_NOT=0;
	if unit=1 then 
	do i=1 to &qnum1.;
		if act{i}="Active" and ex1{i}^=. then Exam1_active_count = Exam1_active_count + 1;
		else if act{i}^="Active" and ex1{i}^=. then Exam1_NOT_count = Exam1_NOT_count +1;
		else if act{i}="Active" then Exam1_missing_active= exam1_missing_active +1;
		else if act{i}^="Active" then Exam1_missing_NOT = exam1_missing_NOT + 1;
	end;
	Exam1_active_count=Exam1_active_count-exam1_missing_active;
	Exam1_NOT_count=Exam1_NOT_count-exam1_missing_NOT;
	
	Exam2_active_count=0; Exam2_NOT_count=0; exam2_missing_active=0; exam2_missing_NOT=0;
	if unit=2 then 
	do i=1 to &qnum2.;
		if act{i}="Active" and ex2{i}^=. then Exam2_active_count = Exam2_active_count + 1;
		else if act{i}^="Active" and ex2{i}^=. then Exam2_NOT_count = Exam2_NOT_count +1;
		else if act{i}="Active" then Exam2_missing_active= exam2_missing_active +1;
		else if act{i}^="Active" then Exam2_missing_NOT = exam2_missing_NOT + 1;
	end;
	Exam2_active_count=Exam2_active_count-exam2_missing_active;
	Exam2_NOT_count=Exam2_NOT_count-exam2_missing_NOT;
	
	Exam3_active_count=0; Exam3_NOT_count=0; exam3_missing_active=0; exam3_missing_NOT=0;
	if unit=3 then 
	do i=1 to &qnum3.;
		if act{i}="Active" and ex3{i}^=. then Exam3_active_count = Exam3_active_count + 1;
		else if act{i}^="Active" and ex3{i}^=. then Exam3_NOT_count = Exam3_NOT_count +1;
		else if act{i}="Active" then Exam3_missing_active= exam3_missing_active +1;
		else if act{i}^="Active" then Exam3_missing_NOT = exam3_missing_NOT + 1;
	end;
	Exam3_active_count=Exam3_active_count-exam3_missing_active;
	Exam3_NOT_count=Exam3_NOT_count-exam3_missing_NOT;

	 exam1tot= &qnum1.; exam2tot= &qnum2.; exam3tot= &qnum3.;
run;
proc print data=&label1._active_count_2 (obs=10);
	var Exam1_active_score Exam1_NOTact_score Exam1_active_count Exam1_NOT_count exam1tot;
run;	

*** Proportion of each type of question (ACTIVE OR NOT) correct ***;
*** This is already corrected for missing responses in the count of active learning ***;

data fin.&label1._active;
	set &label1._active_count_2;
	
	Exam1_Active_Final = DIVIDE(Exam1_active_score, Exam1_active_count);
	Exam2_Active_Final = DIVIDE(Exam2_active_score, Exam2_active_count);
	Exam3_Active_Final = DIVIDE(Exam3_active_score, Exam3_active_count);
	
	Exam1_NOT_Final = DIVIDE(Exam1_NOTact_score, Exam1_NOT_count);
	Exam2_NOT_Final = DIVIDE(Exam2_NOTact_score, Exam2_NOT_count);
	Exam3_NOT_Final = DIVIDE(Exam3_NOTact_score, Exam3_NOT_count);
	
run;	
proc print data=fin.&label1._active (obs=30);
	var Exam1_active_Final Exam1_NOT_Final Exam1_Active_score Exam1_active_count 
		Exam2_active_Final Exam2_NOT_Final Exam2_Active_score Exam2_active_count
		Exam3_active_Final Exam3_NOT_Final Exam3_Active_score Exam3_active_count;
run;	
%MEND;

*** CALLING THE MACRO FOR ALL DATASETS BY YEAR ***;
%Active_Count(F2012, 56, 60, 52);
%Active_Count(F2013, 55, 55, 64);	
%Active_Count(F2014, 52, 50, 57);	
%Active_Count(F2015, 50, 52, 57);
%Active_Count(S2015, 52, 55, 59);
%Active_Count(F2016, 53, 52, 57);
%Active_Count(S2016, 47, 50, 64);
%Active_Count(F2017, 53, 54, 64);
%Active_Count(S2017, 47, 50, 64);
%Active_Count(S2018, 52, 54, 64);

*** DOUBLE CHECK THAT NOBODY HAS A SCORE THAT IS ABOVE 100% ***;
proc print data= fin.S2017_active;
	where 	Exam1_active_Final>1 or Exam1_NOT_Final>1 
			or Exam2_active_Final>1 or Exam2_NOT_Final>1
			or Exam3_active_Final>1 or Exam3_NOT_Final>1;
run;
*** PREP FOR EACH DATASET WHERE ACTIVE SCORE AND LECTURE SCORE ARE LABELLED ***;
data f2012_active_2;
	set fin.f2012_active;
	
	if Exam1_active_FINAL^=. then Active_score= Exam1_active_FINAL;
	if Exam2_active_FINAL^=. then Active_score= Exam2_active_FINAL;
	if Exam3_active_FINAL^=. then Active_score= Exam3_active_FINAL;
	
	if Exam1_Not_FINAL^=. then NOTActive_score= Exam1_NOT_FINAL;
	if Exam2_Not_FINAL^=. then NOTActive_score= Exam2_NOT_FINAL;
	if Exam3_Not_FINAL^=. then NOTActive_score= Exam3_NOT_FINAL;
	
	honors=0;
	
	keep idnumber Unit honors Active_score NOTActive_score 
		 Exam1_active_FINAL Exam2_active_final Exam3_active_final
		 Exam1_NOT_Final Exam2_NOT_final Exam3_NOT_final;
run;
*** REPEAT ABOVE FOR ALL DATASETS AND THEN MERGE THEM ***;
data both2_3_4_5_6;
	set f2012_active_22 f2013_active_22 f2014_active_2 f2015_active_2 f2016_active_2 f2017_active_2
		s2015_active_22 s2016_active_2 s2017_active_2 s2018_active_2;
run;
Data Fin.All_Years_Active;
	set both2_3_4_5_6;
run;
Data Fin.All_Years_Active_DEIDENT ;
	set Fin.All_Years_Active;
	DROP IDNUMBER;
run;
proc sort data=Fin.All_Years_Active_DEIDENT;
	by honors;
run;
proc means data=Fin.All_Years_Active_DEIDENT n mean median min max;
	var active_score notActive_score;
	class honors;
run;

***********************;


**************************************************************;
***************** INTRA-EXAM SUBTYPE SCORE *******************;
**************************************************************;
*** MACRO WILL TAKE IN LABEL1 WHICH IS PART OF DATASET NAME, QNUM1-3 WHICH IS THE NUMBER OF
QUESTIONS FOR EXAM 1,2,& 3. THIS MACRO DETERMINES A STUDENT'S SCORE FOR ACTIVE LEARNING SUBTYPES ***;


%MACRO Active_Subtype(label1, qnum1, qnum2, qnum3);
data &label1._subtype_count;
	set data2.&label1._merged;
	retain  Exam1_sub_C_score Exam1_sub_O_score Exam1_sub_A_score Exam1_sub_R_score Exam1_sub_E_score
			Exam2_sub_C_score Exam2_sub_O_score Exam2_sub_A_score Exam2_sub_R_score Exam2_sub_E_score
			Exam3_sub_C_score Exam3_sub_O_score Exam3_sub_A_score Exam3_sub_R_score Exam3_sub_E_score 
			
			Exam1_sub_C_count Exam1_sub_O_count Exam1_sub_A_count Exam1_sub_R_count Exam1_sub_E_count
			Exam2_sub_C_count Exam2_sub_O_count Exam2_sub_A_count Exam2_sub_R_count Exam2_sub_E_count
			Exam3_sub_C_count Exam3_sub_O_count Exam3_sub_A_count Exam3_sub_R_count Exam3_sub_E_count 0;	
	
	array sub{*} sub_Q001-sub_Q064;
	
	array ex1{*} A_1_001-A_1_0&qnum1.;
	array ex2{*} A_2_001-A_2_0&qnum2.;
	array ex3{*} A_3_001-A_3_0&qnum3.;
	
	Exam1_sub_C_score=0; Exam1_sub_O_score=0; Exam1_sub_A_score=0; Exam1_sub_R_score=0; Exam1_sub_E_score=0; 
 	Exam1_sub_C_count=0; Exam1_sub_O_count=0; Exam1_sub_A_count=0; Exam1_sub_R_count=0; Exam1_sub_E_count=0;
	if unit=1 then 
		do i=1 to &qnum1.;
		if sub{i} = "C" then Exam1_sub_C_score= Exam1_sub_C_score + SUM(of ex1{i});
		else if sub{i} = "O" then Exam1_sub_O_score= Exam1_sub_O_score + SUM(of ex1{i});
		else if sub{i} = "A" then Exam1_sub_A_score= Exam1_sub_A_score + SUM(of ex1{i});
		else if sub{i} = "R" then Exam1_sub_R_score= Exam1_sub_R_score + SUM(of ex1{i});
		else if sub{i} = "E" then Exam1_sub_E_score= Exam1_sub_E_score + SUM(of ex1{i});

		if sub{i}="C" and ex1{i}^=. then Exam1_sub_C_count = Exam1_sub_C_count + 1;
		else if sub{i}="O" and ex1{i}^=. then Exam1_sub_O_count = Exam1_sub_O_count + 1;
		else if sub{i}="A" and ex1{i}^=. then Exam1_sub_A_count = Exam1_sub_A_count + 1;
		else if sub{i}="R" and ex1{i}^=. then Exam1_sub_R_count = Exam1_sub_R_count + 1;
		else if sub{i}="E" and ex1{i}^=. then Exam1_sub_E_count = Exam1_sub_E_count + 1;
		end;

	Exam2_sub_C_score=0; Exam2_sub_O_score=0; Exam2_sub_A_score=0; Exam2_sub_R_score=0; Exam2_sub_E_score=0; 
	Exam2_sub_C_count=0; Exam2_sub_O_count=0; Exam2_sub_A_count=0; Exam2_sub_R_count=0; Exam2_sub_E_count=0;
	if unit=2 then 
		do i=1 to &qnum2.;
		if sub{i} = "C" then Exam2_sub_C_score= Exam2_sub_C_score + SUM(of ex2{i});
		else if sub{i} = "O" then Exam2_sub_O_score= Exam2_sub_O_score + SUM(of ex2{i});
		else if sub{i} = "A" then Exam2_sub_A_score= Exam2_sub_A_score + SUM(of ex2{i});
		else if sub{i} = "R" then Exam2_sub_R_score= Exam2_sub_R_score + SUM(of ex2{i});
		else if sub{i} = "E" then Exam2_sub_E_score= Exam2_sub_E_score + SUM(of ex2{i});
		
		if sub{i}="C" and ex2{i}^=. then Exam2_sub_C_count = Exam2_sub_C_count + 1;
		else if sub{i}="O" and ex2{i}^=. then Exam2_sub_O_count = Exam2_sub_O_count + 1;
		else if sub{i}="A" and ex2{i}^=. then Exam2_sub_A_count = Exam2_sub_A_count + 1;
		else if sub{i}="R" and ex2{i}^=. then Exam2_sub_R_count = Exam2_sub_R_count + 1;
		else if sub{i}="E" and ex2{i}^=. then Exam2_sub_E_count = Exam2_sub_E_count + 1;
	
		end;	
	
	Exam3_sub_C_score=0; Exam3_sub_O_score=0; Exam3_sub_A_score=0; Exam3_sub_R_score=0; Exam3_sub_E_score=0; 
	Exam3_sub_C_count=0; Exam3_sub_O_count=0; Exam3_sub_A_count=0; Exam3_sub_R_count=0; Exam3_sub_E_count=0;
	if unit=3 then 
		do i=1 to &qnum3.;
		if sub{i} = "C" then Exam3_sub_C_score= Exam3_sub_C_score + SUM(of ex3{i});
		else if sub{i} = "O" then Exam3_sub_O_score= Exam3_sub_O_score + SUM(of ex3{i});
		else if sub{i} = "A" then Exam3_sub_A_score= Exam3_sub_A_score + SUM(of ex3{i});
		else if sub{i} = "R" then Exam3_sub_R_score= Exam3_sub_R_score + SUM(of ex3{i});
		else if sub{i} = "E" then Exam3_sub_E_score= Exam3_sub_E_score + SUM(of ex3{i});
		
		if sub{i}="C" and ex3{i}^=. then Exam3_sub_C_count = Exam3_sub_C_count + 1;
		else if sub{i}="O" and ex3{i}^=. then Exam3_sub_O_count = Exam3_sub_O_count + 1;
		else if sub{i}="A" and ex3{i}^=. then Exam3_sub_A_count = Exam3_sub_A_count + 1;
		else if sub{i}="R" and ex3{i}^=. then Exam3_sub_R_count = Exam3_sub_R_count + 1;
		else if sub{i}="E" and ex3{i}^=. then Exam3_sub_E_count = Exam3_sub_E_count + 1;
		end;	
run;
proc print data=&label1._subtype_count (obs=10);
	var Exam1_sub_C_score Exam1_sub_C_count Exam2_sub_C_score 
		Exam2_sub_C_count Exam3_sub_C_score Exam3_sub_C_count;
run;
*** calculating percentage for each subtype ***;

data fin.&label1._subtype_count_2;
	set &label1._subtype_count;
	
	Exam1_Sub_Final_C = DIVIDE(Exam1_sub_C_score, Exam1_sub_C_count);
	Exam2_Sub_Final_C = DIVIDE(Exam2_sub_C_score, Exam2_sub_C_count);
	Exam3_Sub_Final_C = DIVIDE(Exam3_sub_C_score, Exam3_sub_C_count);
	
	Exam1_Sub_Final_O = DIVIDE(Exam1_sub_O_score, Exam1_sub_O_count);
	Exam2_Sub_Final_O = DIVIDE(Exam2_sub_O_score, Exam2_sub_O_count);
	Exam3_Sub_Final_O = DIVIDE(Exam3_sub_O_score, Exam3_sub_O_count);
	
	Exam1_Sub_Final_A = DIVIDE(Exam1_sub_A_score, Exam1_sub_A_count);
	Exam2_Sub_Final_A = DIVIDE(Exam2_sub_A_score, Exam2_sub_A_count);
	Exam3_Sub_Final_A = DIVIDE(Exam3_sub_A_score, Exam3_sub_A_count);
	
	Exam1_Sub_Final_R = DIVIDE(Exam1_sub_R_score, Exam1_sub_R_count);
	Exam2_Sub_Final_R = DIVIDE(Exam2_sub_R_score, Exam2_sub_R_count);
	Exam3_Sub_Final_R = DIVIDE(Exam3_sub_R_score, Exam3_sub_R_count);
	
	Exam1_Sub_Final_E = DIVIDE(Exam1_sub_E_score, Exam1_sub_E_count);
	Exam2_Sub_Final_E = DIVIDE(Exam2_sub_E_score, Exam2_sub_E_count);
	Exam3_Sub_Final_E = DIVIDE(Exam3_sub_E_score, Exam3_sub_E_count);
	
run;
proc print data= fin.&label1._subtype_count_2 (obs=10);
	var Exam1_Sub_Final_C Exam2_Sub_Final_C Exam3_Sub_Final_C Exam1_Sub_Final_R Exam2_Sub_Final_R Exam3_Sub_Final_R ;
run;
%MEND;	


*** CALLING THE MACRO FOR SUBTYPES ***;
%Active_Subtype(F2012, 56, 60, 52);	
%Active_Subtype(F2013, 55, 55, 64);	
%Active_Subtype(F2014, 52, 50, 57);	
%Active_Subtype(F2015, 50, 52, 57);
%Active_Subtype(S2015, 52, 55, 59);
%Active_Subtype(F2016, 53, 52, 57);
%Active_Subtype(S2016, 47, 50, 64);
%Active_Subtype(F2017, 53, 54, 64);
%Active_Subtype(S2017, 47, 50, 64);
%Active_Subtype(S2018, 52, 54, 64);


*** PREP FOR EACH DATASET WHERE ACTIVE SUBTYPES ARE LABELLED ***;
data f2012_subtype_2;
	set fin.f2012_subtype_count_2;
	
	if Exam1_Sub_Final_C^=. then Subtype_C= Exam1_Sub_Final_C;
	if Exam2_Sub_Final_C^=. then Subtype_C= Exam2_Sub_Final_C;
	if Exam3_Sub_Final_C^=. then Subtype_C= Exam3_Sub_Final_C;
	
	if Exam1_Sub_Final_O^=. then Subtype_O= Exam1_Sub_Final_O;
	if Exam2_Sub_Final_O^=. then Subtype_O= Exam2_Sub_Final_O;
	if Exam3_Sub_Final_O^=. then Subtype_O= Exam3_Sub_Final_O;
	
	if Exam1_Sub_Final_A^=. then Subtype_A= Exam1_Sub_Final_A;
	if Exam2_Sub_Final_A^=. then Subtype_A= Exam2_Sub_Final_A;
	if Exam3_Sub_Final_A^=. then Subtype_A= Exam3_Sub_Final_A;
	
	if Exam1_Sub_Final_R^=. then Subtype_R= Exam1_Sub_Final_R;
	if Exam2_Sub_Final_R^=. then Subtype_R= Exam2_Sub_Final_R;
	if Exam3_Sub_Final_R^=. then Subtype_R= Exam3_Sub_Final_R;
	
	if Exam1_Sub_Final_E^=. then Subtype_E= Exam1_Sub_Final_E;
	if Exam2_Sub_Final_E^=. then Subtype_E= Exam2_Sub_Final_E;
	if Exam3_Sub_Final_E^=. then Subtype_E= Exam3_Sub_Final_E;	
	
	honors=0;
	keep idnumber Unit honors Subtype_C Subtype_O Subtype_A Subtype_R Subtype_E;
run;
*** REPEAT ABOVE FOR ALL DATASETS AND THEN MERGE THEM ***;

data both2_6;
	set f2012_subtype_22 f2013_subtype_22 f2014_subtype_2 f2015_subtype_2 f2016_subtype_2 f2017_subtype_2
		s2015_subtype_22 s2016_subtype_2 s2017_subtype_2 s2018_subtype_2;
run;
data subtype_again;
	set both2_6;

	subtype_percent = .;
	if Subtype_C^=. then subtype_percent = Subtype_C; Subtype="C"; output;
	subtype_percent = .;
	if Subtype_O^=. then subtype_percent = Subtype_O; Subtype="O"; output;
	subtype_percent = .;
	if Subtype_A^=. then subtype_percent = Subtype_A; Subtype="A"; output;
	subtype_percent = .;
	if Subtype_R^=. then subtype_percent = Subtype_R; Subtype="R"; output;
	subtype_percent = .;
	if Subtype_E^=. then subtype_percent = Subtype_E; Subtype="E"; output;
run;

Data Fin.All_Years_Subtype;
	set subtype_again;
run;
Data Fin.All_Years_Subtype_DEIDENT;
	set Fin.All_Years_Subtype;
	DROP IDNUMBER;
run;	
******************************;




**************************************************************;
************* INTRA-EXAM PAIRED TTEST ANALYSIS ***************;
**************************************************************;

proc ttest data=Fin.All_Years_Active_DEIDENT;
	by honors;
	paired Active_Score*NotActive_Score;
run;

**************************************************************;
*************** INTRA-EXAM SUBTYPE ANALYSIS ******************;
**************************************************************;
*** ANOVA And GLM assume normal distribution. Since our data cluster around 90% and are not normal, 
I perform a nonparametric rank test instead, which is wilcoxon/ kruskal wallis ***;

*** DESCRIPTIVE STATISTICS ***;
proc means data= Fin.All_Years_Subtype_DEIDENT n min median mean max;
	var subtype_percent;
	class subtype;
run;
proc sort data=Fin.All_Years_Subtype_DEIDENT;
	by subtype;
run;
	
*** INTRA-EXAM SUBTYPE WILCOXON RANK SUM TEST ***;
proc npar1way data=Fin.All_Years_Subtype_DEIDENT wilcoxon;
	class subtype;
	var subtype_percent;
run;	

*** Overall Wilcoxon is significant, so proceed with pairwise tests.
Bonferroni is too conservative but the Dwass Steel Crichtlow Flinger has 
family wise error rate protection built into it. ***;

*** POST-HOC INTRA-EXAM SUBTYPE WILCOXON RANK SUM TEST WITH DSCF CORRECTION ***;
proc npar1way data=Fin.All_Years_Subtype_DEIDENT wilcoxon DSCF;
	class subtype;
	var subtype_percent;
run;	

*** BOXPLOT OF SUBTYPE SCORES ***;
proc sgplot data=Fin.All_Years_Subtype_DEIDENT;
	vbox subtype_percent /category=subtype ;
	yaxis label="Percent Correct" ;
	xaxis label="Active Learning Subtype";
run;




**************************************************************;
*************** ALL FIGURES AND TABLE OUTPUT *****************;
**************************************************************;
proc template;
   	define style Styles.NewStyle1;
    parent=Styles.journal;
	style fonts / 	"cellfont"=("Times", 12pt)
					"headingfont"=("Times",12pt)
					"titlefont"=("Times", 12pt);
	style systemtitle / font=fonts("titlefont");
	style header / font=fonts("headingfont");
	style cellcontents / font=fonts("cellfont");
	end;
run;

ods pdf file = "/folders/myfolders/Active_Learning/Active_Learning_Output.pdf" style=newstyle1 startpage=no;
ods noproctitle;

proc sort data=Fin.All_Years_Active_DEIDENT;
	by honors;
run;

title "Overall Descriptive Statistics of Exam Scores";
proc means data=Fin.All_Years_Active_DEIDENT n mean median min max;
	var active_score notActive_score;
run;	

title "Descriptive Statistics of Exam Scores by Honors";
proc means data=Fin.All_Years_Active_DEIDENT n mean median min max;
	var active_score notActive_score;
	class honors;
run;

title "Descriptive Statistics of Subtype Exam Scores";
proc means data= Fin.All_Years_Subtype_DEIDENT n min median mean max;
	var subtype_percent;
	class subtype;
run;

proc sort data=Fin.All_Years_Subtype_DEIDENT;
	by subtype;
run;
	
*** histogram of different subtypes to show distributions ***;
Title "Histogram of Score Distributions by Subtype";
data fin.all_years_subtype_2;
	set Fin.All_Years_Subtype_DEIDENT;
	subtype_percent = subtype_percent*100;
run;

proc univariate data=Fin.All_Years_Subtype_DEIDENT ;
  class subtype;
  var subtype_percent;      /* computes descriptive statisitcs */
  histogram subtype_percent / vaxislabel= "Proportion of Students"/* nrows=3 odstitle="PROC UNIVARIATE with CLASS statement"*/;
  ods select histogram ; /* display on the histograms */
  label subtype_percent="Percent Correct";
run;

proc univariate data=fin.all_years_subtype_2 ;
  class subtype;
  var subtype_percent;      /* computes descriptive statisitcs */
  histogram subtype_percent / vaxislabel= "Proportion of Students"/* nrows=3 odstitle="PROC UNIVARIATE with CLASS statement"*/;
  ods select histogram ; /* display on the histograms */
  label subtype_percent="Percent Correct";
run;

title "Box Plot of Subtype Score Distribution with Lecture Learning Reference";
proc sgplot data=Fin.All_Years_Subtype_DEIDENT;
	vbox subtype_percent /category=subtype ;
	yaxis label="Percent Correct" ;
	xaxis label="Active Learning Subtype";
	refline .92 /axis=y label="Lecture Learning Median" lineattrs=(color=darkblue);
run;

title1;
proc sort data=Fin.All_Years_Active_DEIDENT;
	by honors;
run;
data fin.all_years_active2;
	set Fin.All_Years_Active_DEIDENT;
	Active_Score=Active_Score*100;
	NotActive_Score=NotActive_Score*100;
run;	

proc ttest data=Fin.All_Years_Active_DEIDENT plots(only)=summary;
	by honors;
	paired Active_Score*NotActive_Score;
	ods output SummaryPanel;
run;

proc ttest data=fin.all_years_active2 plots(only)=summary;
	by honors;
	paired Active_Score*NotActive_Score;
	ods output SummaryPanel;
run;

proc sort data=data.Survey_3FACTORS out=work.class;
   by factor;
run;
proc corr data=work.class kendall ;
   var score;
   with year;
   by factor;
   ods output Kendallcorr= newdata;
run;


title "Scatter Plot of Individual Utility with Time Trend line";
Proc sgplot data= Survey_3FACTORS;
	scatter x=year y=score / group=factor1;
	loess x=year y=score / group=factor1 lineattrs=(color=darkblue);
	format year 4.0;
	xaxis label="Course Year" VALUES=(2012 TO 2018 BY 1);
	yaxis label="Survey Scores by Question";
run;
ods pdf close;



**************************************************************;
*************** FINAL DATASETS FOR SUBMISSION ****************;
**************************************************************;
PROC CONTENTS DATA=DATA.SURVEY_3FACTORS;
RUN;
*** .5 MEANS FALL COURSE .0 MEANS SPRING COURSE ***;
*** SCORE IS THE MEAN SCORE FROM ALL RESPONSES ***;

PROC CONTENTS DATA=Fin.All_Years_Active_DEIDENT;
RUN;
*** EACH LINE IS DATA FOR ONE EXAM FOR ONE PERSON - IE IN LONG FORMAT (3 ROWS PER STUDENT)
*** UNIT IS THE EXAM-EITHER 1,2, OR 3.
*** EXAM#_ACTIVE_FINAL IS THE EXAM SCORE FOR ACTIVE LEARNING FOR THE CORRESPONDING EXAM
*** EXAM#_NOT_FINAL IS THE EXAM SCORE FOR LECTURE LEARNING FOR THE CORRESPONDING EXAM
*** ACTIVE_SCORE AND NOTACTIVE_SCORE ARE THE AVERAGE SCORES FOR THE STUDENT FOR THE EXAM
*** HONORS 0 MEANS NONHONORS COURSE AND 1 MEANS HONORS SECTION;

PROC CONTENTS DATA=Fin.All_Years_SUBTYPE_DEIDENT;
RUN;
*** EACH LINE IS DATA FOR ONE EXAM FOR ONE PERSON - IE IN LONG FORMAT (15 ROWS PER STUDENT)
*** UNIT IS THE EXAM-EITHER 1,2, OR 3.
*** SUBTYPE_letter IS THE EXAM SCORE FOR THE CORRESPONDING ACTIVE LEARNING SUBTYPE AND EXAM
*** SUBTYPE_PERCENT IS MEAN SCORE FOR THE SUBTYPE OF INTEREST INDICATED BY VARIABLE SUBTYPE FOR THE STUDENT
*** HONORS 0 MEANS NONHONORS COURSE AND 1 MEANS HONORS SECTION;





