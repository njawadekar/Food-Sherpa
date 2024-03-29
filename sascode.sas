/* Program to Import Food Inspection Data */
/* njawadekar */
/* Established: 01/28/2015 */



libname a 'C:\users\njawadekar';
  
  
/* Importing the csv */
data a.fidata;
  
  infile 'C:\users\njawadekar\FI.csv'  dlm=',' dsd missover firstobs=2;


  attrib
    BusinessName	  Length = $70      Format = $70.      Informat = $70.
    VIOLDTTM        Length = $30      Format = $30.      Informat = $30.	
    ISSDTTM	        Length = $30      Format = $30.      Informat = $30.                                                 
    EXPDTTM	        Length = $30      Format = $30.      Informat = $30.
    ViolLevel	      Length = $30      Format = $30.      Informat = $30.
    ViolStatus	    Length = $30      Format = $30.      Informat = $30.
    RESULT	        Length = $30      Format = $30.      Informat = $30.
    Violation	      Length = $30      Format = $30.      Informat = $30.
    ViolDesc	      Length = $50      Format = $50.      Informat = $50.
    LICENSENO	      Length = $30      Format = $30.      Informat = $30.
    Comments	      Length = $400     Format = $400.     Informat = $400.
    Address	        Length = $40      Format = $40.      Informat = $40.
    City	          Length = $30      Format = $30.      Informat = $30.
    State	          Length = $30      Format = $30.      Informat = $30.                                                
    Zip	            Length = $30      Format = $30.      Informat = $30.
    LICENSECAT	    Length = $30      Format = $30.      Informat = $30.
    StatusDate	    Length = $30      Format = $30.      Informat = $30.
    LICSTATUS       Length = $30      Format = $30.      Informat = $30.
    ;



  input
    BusinessName	      $
    VIOLDTTM	          $
    ISSDTTM	            $
    EXPDTTM	            $
    ViolLevel           $	
    ViolStatus          $	
    RESULT         	    $
    Violation	          $
    ViolDesc            $
    LICENSENO	          $
    Comments            $
    Address	            $
    City                $
    State	              $
    Zip	                $
    LICENSECAT          $	
    StatusDate          $	
    LICSTATUS           $
   ;


run;



/* Only taking the inspections that failed */
/* Also, creating a ViolDate variable in mmddyy10. format */
data a.fidata2;
attrib
ViolDate   length = 8   format = mmddyy10.   
;
set a.fidata;
where strip(ViolStatus) = 'Fail';
ViolDate = input(scan(strip(ViolDTTM), 1, ' '), mmddyy10.) ;
run;



/* Only taking failed inspections since March 3, 2010 */
data a.fidata3; 
set a.fidata2;
where ViolDate >= '03mar2010'd;
drop
StatusDate;
run;




/* Sorting the dataset by BusinessName and Address */
proc sort data = a.fidata3 out = a.fidata4;
by BusinessName Address;
run;



/* Creating id variable, based on BusinessName and Address */
data a.fidata5;
set a.fidata4;
by BusinessName Address;
retain ID 0;
if first.Address then ID = ID + 1;
run;


/* Violations.txt dataset to send to Dylan */

data a.fidata6;
retain ID BusinessName VIOLDTTM ViolDate ViolLevel ViolStatus Violation ViolDesc RESULT LICSTATUS  EXPDTTM ISSDTTM  LICENSENO LICENSECAT Comments Address City State ZIP;
set a.fidata5;
run;

proc sort data = a.fidata6 out = a.fidata7;
by ID ViolDate;
run;





/* Keeping counter variables for each of the 3 types of violations. 
  This logic will maintain a count of each type of violation level (*, **, and ***) for each restaurant */
  
data a.fidata8;
retain ID BusinessName count_3     count_2     count_1 ;
set a.fidata7;
by ID;

if first.ID then 
 DO;
   count_1 = 0;
   count_2 = 0;
   count_3 = 0;
  end;

/* Counting number of each violation level */
if strip(ViolLevel) = '*' then count_1 + 1;
if strip(ViolLevel) = '**' then count_2 + 1;
if strip(ViolLevel) = '***' then count_3 + 1;

if last.ID then output;
keep BusinessName ID count_3 count_2 count_1 LICSTATUS LICENSENO EXPDTTM Address City State Zip;
run;



/* Identifying the percentile values, to enable me to create a grading rubric */

proc univariate data = a.fidata8;
	var count_1;
	output out = percentiles1 pctlpts =  0  10 20 25 30 35 40 45 50 55 60 65 70 75 80 85 90 100    pctlpre = P;
histogram / normal (color = red)
            ctext = blue;
run;


proc univariate data = a.fidata8;
	var count_2;
	output out = percentiles2 pctlpts = 0  10 20 25 30 35 40 45 50 55 60 65 70 75 80 85 90 100   pctlpre = P;
histogram / normal (color = red)
            ctext = blue;
run;


proc univariate data = a.fidata8;
	var count_3;
	output out = percentiles3 pctlpts =  0  10 20 25 30 35 40 45 50 55 60 65 70 75 80 85 90 100   pctlpre = P;
histogram / normal (color = red)
            ctext = blue;
run;

proc print data = percentiles1;
title 'percentiles1';
run;

proc print data = percentiles2;
title 'percentiles2';
run;

proc print data = percentiles3;
title 'percentiles3';
run;





/* Creating the actual Grading Rubric, based on the percentiles calculated above */
data a.Grades1;
retain Grade;
set a.Fidata8;
if count_3 >= 10 then Grade = 'F';
else if count_3 >= 6 OR count_2 >= 4 then Grade = 'D';
else if count_3 >= 5 OR count_2 >= 2 OR count_1 >= 42 then Grade = 'C';
else if count_3 >= 2 or count_2 >= 1 OR count_1 >= 23 then Grade = 'B';
else Grade = 'A';
run;


/* Calculating distribution of the allocated grades */

proc freq data = a.Grades1;
tables LICSTATUS;
run;



/* Exporting a.Fidata7 & a.Grades1 to tab-delimited .txt files */

proc export data = a.Fidata7
     outfile = 'C:\users\njawadekar\Violations.txt'
	 dbms = tab;
	 run;


	 proc export data = a.Grades1
	 outfile = 'C:\users\njawadekar\Grades.txt'
	 dbms = tab;
	 run;








