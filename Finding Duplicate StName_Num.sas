************************RICHMOND************************;
PROC IMPORT OUT=Work.Lines
            DATAFILE= "C:\Users\mmarti24\Documents\Segregation Project\Richmond\Lines.csv" 
            DBMS=CSV REPLACE;
     GETNAMES=YES;
RUN;

data lines1; set lines;
fleft=dbf_L_f_add;
tleft=dbf_L_t_add;
fright=dbf_R_f_add;
tright=dbf_R_t_add;
name=dbf_name;
streetstd=dbf_street_std;
leftr=fleft||tleft;
rightr=fright||tright;
run;

proc sort data=lines1; by streetstd leftr;
data lines2; set lines1; count + 1; by streetstd leftr; 
if first.streetstd or first.leftr then count=1; run;
proc means data=lines2 noprint; var count; by streetstd leftr; output out=lines3 n=count; run;
proc sort data=lines3; by streetstd leftr;run;
data lines4; merge lines2 lines3;
drop _type_ _freq_; by streetstd leftr;  run;

data dup; set lines4;
if count > 1; if streetstd ne "NA"; run;

data nodup; set lines4;
if count = 1; if streetstd ne "NA"; run;


***************ATLANTA********************;
	*True for 423 out of 1924 street segments.
	This is after deleting segments without a streetname.
	Before this deletion there were 2119 street segments.;
PROC IMPORT OUT=Work.Lines
            DATAFILE= "C:\Users\mmarti24\Documents\Segregation Project\Lines.csv" 
            DBMS=CSV REPLACE;
     GETNAMES=YES;
RUN;

data lines1; set lines;
fleft=dbf_L_f_add;
tleft=dbf_L_t_add;
fright=dbf_R_f_add;
tright=dbf_R_t_add;
name=dbf_name;
streetstd=dbf_Full_name;
leftr=fleft||tleft;
rightr=fright||tright;
run;

proc sort data=lines1; by streetstd leftr;
data lines2; set lines1; count + 1; by streetstd leftr; 
if first.streetstd or first.leftr then count=1; run;
proc means data=lines2 noprint; var count; by streetstd leftr; output out=lines3 n=count; run;
proc sort data=lines3; by streetstd leftr;run;
data lines4; merge lines2 lines3;
drop _type_ _freq_; by streetstd leftr;  run;

data dup; set lines4;
if count > 1; if streetstd ne "NA"; run;

data nodup; set lines4;
if count = 1; if streetstd ne "NA"; run;

***************WASHINGTON DC********************;
	*True for 423 out of 1924 street segments.
	This is after deleting segments without a streetname.
	Before this deletion there were 2119 street segments.;
PROC IMPORT OUT=Work.Lines
            DATAFILE= "C:\Users\mmarti24\Documents\Segregation Project\Lines.csv" 
            DBMS=CSV REPLACE;
     GETNAMES=YES;
RUN;

data lines1; set lines;
fleft=dbf_L_f_add;
tleft=dbf_L_t_add;
fright=dbf_R_f_add;
tright=dbf_R_t_add;
name=dbf_name;
streetstd=dbf_Full_name;
leftr=fleft||tleft;
rightr=fright||tright;
run;

proc sort data=lines1; by streetstd leftr;
data lines2; set lines1; count + 1; by streetstd leftr; 
if first.streetstd or first.leftr then count=1; run;
proc means data=lines2 noprint; var count; by streetstd leftr; output out=lines3 n=count; run;
proc sort data=lines3; by streetstd leftr;run;
data lines4; merge lines2 lines3;
drop _type_ _freq_; by streetstd leftr;  run;

data dup; set lines4;
if count > 1;  if streetstd ne "NA"; run;

data nodup; set lines4;
if count = 1; if streetstd ne "NA"; run;
