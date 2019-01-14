PROC IMPORT OUT= WORK.USA
            DATAFILE= "S:\Private\jogerhartz\Finance\tech_company_data1.xls"
            DBMS=XLS REPLACE;
            GETNAMES=YES;
    
Data Rate;
set WORK.USA;
/*variables */
firmagesq = firmage*firmage;
IPOsq=IPO5*IPO5;

/*time dummies (2009 base) */ 
if year=2010 then twenty10=1; else twenty10=0;
if year=2011 then twenty11=1; else twenty11=0;
if year=2012 then twenty12=1; else twenty12=0;
if year=2013 then twenty13=1; else twenty13=0;
if year=2014 then twenty14=1; else twenty14=0;
if year=2015 then twenty15=1; else twenty15=0;
if year=2016 then twenty16=1; else twenty16=0;
if year=2017 then twenty17=1; else twenty17=0;

/*CS dummies (Alphabet base) */
if company="Facebook" then FB=1; else FB=0;
if company="Twitter" then TW=1; else TW=0;
if company="Ebay" then EB=1; else EB=0;
if company="Netflix" then NF=1; else NF=0;
if company="Salesforce" then SF=1; else SF=0;

/* IPO Age Dummies */
if new_old="new" then new=1; else new=0;
if new_old="old" then old=1; else old=0;

/*interaction variable*/
newage=firmage*new;
newagesq=firmagesq*new;



proc sort;
by company year;
proc means;
title 'means proc';

/* proc corr to check correlation between x variables */
Proc corr;
var D_E Firmagesq Firmage Market gdp Margin t_rate tax_rate twenty10 twenty11 twenty12 twenty13 twenty14 twenty15 FB TW EB SF NF;
run;

proc reg; 
id Company Year;
model D_E = Firmagesq Firmage Market Margin t_rate tax_rate twenty10 twenty11 twenty12 twenty13 twenty14 twenty15 FB TW EB SF /vif;
title 'Panel output 1';
Output out=Diagpanel R=Residp;

proc reg; 
id Company Year;
model D_E = new firmage firmagesq newage newagesq Market t_rate tax_rate twenty10 twenty11 twenty12 twenty13 twenty14 twenty15 FB TW EB SF/vif;
title 'Panel output 2 new';
Output out=Diagpanel R=Residp;

proc reg; 
id Company Year;
model D_E = old Market t_rate twenty10 twenty11 twenty12 twenty13 twenty14 twenty15 FB TW EB SF NF/vif;
title 'Panel output 3 old';
Output out=Diagpanel R=Residp;


proc reg; 
id Company Year;
model D_E = IPOsq IPO5 Market t_rate twenty10 twenty11 twenty12 twenty13 twenty14 twenty15 FB TW EB SF NF/vif;
title 'Panel output 4 IPO';
Output out=Diagpanel R=Residp;

proc reg; 
id Company Year;
model margin = IPOsq IPO5 Market t_rate /vif;
title 'Panel output 5 margin';
Output out=Diagpanel R=Residp;


/*test for hetero
_________________________________________________________*/

/* plot of resids to check for hetero pattern */
Proc Plot;
Plot residp * Age;
plot residp * Tax_rate;
plot residp * Market;
plot residp * GDP;
plot residp * t_rate;
title 'resid plots for hetero';

data Tests;
set diagpanel;

/* logged variables for park test */
logage=log(age+1);
logtax_rate=log(tax_rate+6.5);
logmarket=log(market);
logGDP=log (GDP);
logt_rate=log(t_rate);

/*resid variables*/
residplag=lag1(residp);
ressq=residp**2;
logressq=log(ressq);

/* park test reg for logage*/
proc reg;
model logressq=logage;
title 'park test logage';


/*park test reg for log tax rate */
proc reg;
model logressq=logtax_rate;
title 'park test logtax_rate';

/* park test reg for log market */
proc reg;
model logressq=logmarket;
title 'park test logmarket';

/* park test reg for log GDP */
proc reg;
model logressq=logGDP;
title 'park test logGDP';

/* park test reg for log GDP */
proc reg;
model logressq=logt_rate;
title 'park test t rate';

/* whites tests */
Proc Reg data=usa;
model D_E = Age Tax_rate Market GDP t_rate / white;
title 'whites test';
run;

/*____________________________________________________________*/


/* Test for Serial Corr */
Proc Plot;
Plot logressq * age;
plot logressq * tax_rate;
plot logressq * market;
plot logressq * GDP;
plot logressq * t_rate;
title 'resid tests for serial corr';


Proc Autoreg data=usa;
model D_E = Age Tax_rate Market GDP t_rate/NLAG=1;
title 'Autoreg to correct serial corr';
run;

/* model 2*/
Data new;
set rate;

Proc panel;
id Company Year;
model Margin = D_E Age Tax_rate Market GDP t_rate/fixtwo;
title 'Optimal Debt to Equity Ratio for Profit Margin';
Output out=Diagpanel2 R=Residp2;


Data tests2;
set diagpanel2;

/* logged variables */

logD_E=log(D_E);
logAge=log(Age+1);
logtax_rate=log(tax_rate+6.5);
logmarket=log(market);
logGDP=log(GDP);
logt_rate=log(t_rate+1);

/*resid variables*/
residplag2=lag1(residp2);
ressq2=residp2**2;
logressq2=log(ressq2);


Proc Plot; 
Plot residp2 * D_E;
Plot residp2 * Age;
Plot residp2 * tax_rate;
Plot residp2 * market;
Plot residp2 * GDP;
Plot residp2 * t_rate;
title 'resid plots for hetero model 2';

/*park test reg for log D_E */
proc reg;
model logressq2=logD_E;
title 'park test logD_E';

/* park test reg for log Age */
proc reg;
model logressq2=logAge;
title 'park test logAge';

/*park test reg for tax rate */
proc reg;
model logressq2=logtax_rate;
title 'park test logtaxrate';

/*park test reg for log market */
proc reg;
model logressq2=logmarket;
title 'park test logmarket';

/*park test reg for log GDP */
proc reg;
model logressq2=logGDP;
title 'park test logGDP';

/*park test reg for log t_rate */
proc reg;
model logressq2=logt_rate;
title 'park test log t_rate';

/* plot tests for serial corr */
Proc Plot;
plot logressq2 * D_E;
plot logressq2 * Age;
Plot logressq2 * tax_rate;
Plot logressq2 * market;
Plot logressq2 * GDP;
Plot losressq2 * t_rate;

title 'resid tests for serial corr';

Proc Autoreg data=usa;
model Margin = D_E Age Tax_rate Market GDP t_rate/NLAG=1;
title 'Autoreg to correct serial corr';
run;

