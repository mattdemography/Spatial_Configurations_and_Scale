*Import Combined City Data;

PROC IMPORT OUT= WORK.City 
            DATAFILE= "Z:\Users\Matt\Segregation Project_Irish and Germans\Combined Households_AllCities.csv" 
            DBMS=CSV REPLACE;
     GETNAMES=YES;
     DATAROW=2; 
RUN;

data city1; set city;
if german_new>1 then german_new=.;
if irish_new>1 then irish_new=.;
if black_new>1 then black_new=.;
if other_new>1 then other_new=.;
if native_white_new>1 then native_white_new=.;
*Create Unique ED/City Identifier;
c_ed=city||enumdist;
if german_new ne .;
run;

proc freq data=city1; tables black_new other_new native_white_new german_new irish_new; run;

proc glimmix data=city1 noitprint;
class City;
model multi(desc)=black_new other_new native_white_new german_new city_g_per city_i_per city_w_per/ dist=binary  link=logit solution;
random intercept/ subject=City;
run;

proc glimmix data=city1 noitprint;
class c_ed city;
model multi(desc)=black_new other_new native_white_new german_new ed_g_per ed_i_per ed_w_per hb_max/ dist=binary  link=logit solution;
random intercept/ subject=c_ed;
run;

proc freq data=city1; tables city; run;

data albany; set city1;
if city="Albany";
run;

proc glimmix data=albany method=quad noitprint;
class c_ed;
model multi(desc)=irish_new ed_g_per ed_i_per ed_w_per irish_new*ed_g_per irish_new*ed_i_per irish_new*ed_w_per/ dist=binary  link=logit solution;
random intercept/ subject=c_ed;
run;
