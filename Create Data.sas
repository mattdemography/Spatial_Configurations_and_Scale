libname seg "C:\Users\mmarti24\Documents\Segregation Project\Richmond"; run;
proc contents data=seg.richmond; run;


data rich; set seg.richmond;

*Create Variable to merge with ARCGIS file;
serial1=put(serial, z10.);

*Create Race Variable;
whiten=0;
if race=100 then whiten=1;
if race=999 then whiten=.;

blackn=0;
if (race=200 OR race=210) then blackn=1;
if race=999 then blackn=.;

AmNative=0;
if race=300 then AmNative=1;
if race=999 then AmNative=.;

chinese=0;
if race=400 then chinese=1;
if race=999 then chinese=.;

keep serial serial1 addr streetname addrnum countyus statfpus
 race AmNative chinese blackn whiten white black
enumdist citypop; 

/*label
	whiten="White"
	blackn="Black"
	AmNative="American Indian/Alaska Native"
	chinese="Chinese"
	enumdist="Enumeration District"
	citypop="City Population";
*/
run;

data rich; set rich;
obsid=_n_; run;

*Get City Totals;
proc means data=rich nway noprint; var blackn whiten AmNative chinese; 
output out=rich2(rename=(blackn=blackct whiten=whitect AmNative=AmNativeCT chinese=chinesect)) sum=blackn whiten AmNative chinese; run;

data richmonduse; if _n_=1 then set rich2; set rich;
drop _type_ _freq_;
pwhitec=(whitect/citypop);
pblackc=(blackct/citypop);
pamnativec=(AmNativeCT/citypop);
pchinesec=(chinesect/citypop);
/*label 
	blackct="Black City Total"
	whitect="White City Total"
	AmNativeCT="American Native City Total"
	chinesect="Chinese City Total"
	pwhitec="Proportion White City"
	pblackc="Proportion Black City"
	pamnativec="Proportion American Native City"
	pchinesec= "Proportion Chinese City";
*/
run;

*Get Counts of Race by Household Unit;
proc sort data=richmonduse; by serial1; run;
proc means data=richmonduse nway noprint; var blackn whiten AmNative chinese; 
output out=richmonduse2(rename=(blackn=blackhht whiten=whitehht AmNative=AmNativehht chinese=chinesehht)) sum=blackn whiten AmNative chinese; by serial1; run;

proc sort data=richmonduse2; by serial1;
data seg.richmondperson; merge richmonduse richmonduse2; 
hhtot=_freq_;
drop _type_ _freq_;
by serial1;
run;

*Create a Household Dataset by droppping identical household unit numbers;
proc sort data=seg.richmondperson nodupkey out=richhouse; by serial1; run;

data seg.richhouse; set richhouse;
keep blackct whitect amnativect chinesect serial1 citypop 
race whiten blackn amnative chinese obsid pwhitec pblackc pamnativec pchinesec
blackhht whitehht amnativehht chinesehht hhtot;
run;

PROC EXPORT DATA= SEG.RICHMONDUSE 
            OUTFILE= "C:\Users\mmarti24\Documents\Segregation Project\Richmonduse.xlsx" 
            DBMS=xlsx LABEL REPLACE;
RUN;
