

program define harrellC ,rclass
*! Version 1  04 abr 2017, by SPH
*! Version html 2013
*! syntax varlist(min=2 max=2)

* This program produces harrel C concordance estimator 
version 8.0
syntax varlist(min=2 max=2) [if] [in] [fweight] 

* syntax var1 = Response variable , var2= Prediction variable

tokenize "`varlist'"
local y= "`1'"
local yhat= "`2'"


local s= 0
local n=0
local ndatos=_N
  forvalues i=1(1)`ndatos' {
	forvalues j= 1(1)`ndatos' {
     
		if `i'!=`j' {
		    
			if  `y'[`i']> `y'[`j'] {
		
				local s=`s'+( `yhat'[`i'] >`yhat'[`j']) + 0.5 *( `yhat'[`i'] ==`yhat'[`j'])
				local n=`n'+1
			}
		}
    }
  }	
	
local hc=`s'/`n'
 disp " " 
 disp in green "______________________________________"
 disp in yellow "    Indice  Harrel C = " %5.2f `hc'
 disp in green  "______________________________________"

return scalar HarrellC= `hc'


end

