* replicating io analysis 

use iorep.DTA, clear
log using iorep.log, replace


set more off
#delimit ;
* replicating the first equation;

probit bremdisp smorg_31 deml demh depl lrgddep3 growthl ally1r geo1 caprat
 , robust cluster(dyad);
* replicating the second equation;
regress sumorg_3 depl deml gdppcl ally1r distab ldi, robust cluster(dyad);




