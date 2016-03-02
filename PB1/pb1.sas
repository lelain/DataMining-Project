/*Importation des donnees*/
/*on supprime la ligne des qu'on a une valeur manquante*/
data donnees;
  	infile "\\univ-lyon1\enseignement\homes\l\p1513678\
		Mes documents\Doc\Data mining\PB1\mammographic_masses.data" 
		dlm=',' missover; 
  	input assessment age shape margin density severity ;
  	IF NMISS(OF _NUMERIC_)>0 THEN DELETE ;   
run;

/*on enleve ou change les donnees aberantes*/
data donnees;
	set donnees;
	if assessment =0 then delete;
	if assessment =6 then delete;
	if assessment =55 then assessment=5;
run;


/*discretisation de age*/
proc rank data=donnees groups=11 out=quantiles;
	var age;
	ranks dAge;
run;

proc means data=quantiles min max;
	class dAge;
	var Age;
run;

proc freq data = quantiles;
	tables dAge * severity /nocol ;
run;


/*definition des facteurs*/
proc format;
	value AGE
		0-35 = '<=35 ans'
		36-51 = '36-51 ans'
		52-66 = '52-66 ans'
		66-high = '>=66 ans';
	value SHAPE
		1 = 'round'
		2 = 'oval'
		3 = 'lobular'
		4 = 'irregular';
	value MARGIN
		1 = 'circumscribed'
		2 = 'microlobulated'
		3 = 'obscured'
		4 = 'ill-defined'
		5 = 'spiculated';
	value DENSITY
		1 = 'high'
		2 = 'iso'
		3 = 'low'
		4 = 'fat-containing';
	value SEVERITY
		0 = 'benign'
		1 = 'malignant' ;
run;

/*croisement de chaque variable avec severity*/
proc freq data=donnees;
	tables assessment * severity /nocol;
	format severity SEVERITY. ; 
run;

proc freq data=donnees;
	tables shape * severity /nocol;
	format shape SHAPE. severity SEVERITY. ; 
run;

proc freq data=donnees;
	tables margin * severity /nocol;
	format margin MARGIN. severity SEVERITY. ; 
run;

proc freq data=donnees;
	tables density * severity /nocol;
	format density DENSITY. severity SEVERITY. ; 
run;

/*Calcul du V de Cramer de chaque variable avec severity 
ODS OUTPUT ChiSq = ChiSq;
proc freq data = donnees;
	tables (assessment age shape margin density) * severity 
		/ chisq nocol noprint;
	format age AGE. shape SHAPE. margin MARGIN. density 
		DENSITY. severity SEVERITY.;
run;

data ChiSq;
	set ChiSq;
	where Statistic CONTAINS "Cramer" ;
	abs_V_Cramer = ABS(Value);
	Variable = SCAN (Table,2);
	KEEP Variable Value abs_V_Cramer ;
run;

proc sort data = ChiSq ;
	by descending abs_V_Cramer ;
run;

proc print data = ChiSq ; run ;


