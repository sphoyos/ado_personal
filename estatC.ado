*! Version 1  04 abr 2018, by SPH
*! Version html 2013
*! syntax varlist(min=2 max=2)


program define estatC ,rclass

* This program produces harrel C concordance estimator  from Weibull and Cox model
version 8.0
st_is 2 analysis
syntax  [if] [in] 


marksample touse 

      if "`e(cmd)'" != "cox"  &  "`e(cmd)'" != "weibull"  {
        	 di as err /*
	        */ "A Cox or Weibull model must be fitted"
			error 301
        }
tempvar phr



cap drop `phr'
predict `phr', hr
keep if `phr'!=.
local nobs =_N
local hc=0
local num=0
local den=0
local numt=0
gsort _t -_d
forvalues i=1(1)`nobs' {
   forvalues j=`i'(1)`nobs' {
   

   
     if `i'!=`j'  {
	 
			local  s1= (_t[`i']<_t[`j'])
			local s2= (_d[`i']==1)
			local s3= ( `phr'[`i']>`phr'[`j'])
			
			local t1= ( `phr'[`i']==`phr'[`j'])
			
			local e1= (_t[`i'] ==_t[`j'])
			local e2= (_d[`i']==1 & _d[`j']==0 )
			local num=`num'+ `s1'*`s2'*`s3'+ `e1'*`e2'*`s3'
				
			local numt=`numt' + `s1'*`s2'*`t1' + `e1'*`e2'*`t1'
			local den=`den'+ `s1'*`s2'+  `e1'*`e2'
			
			
			
			
		}
	}
}
local hc=(`num'+`numt'/2) /`den'

disp " " 
 disp in green "______________________________________"
 disp in yellow "    Indice  Harrel C = " %8.4f `hc'
 disp in green  "______________________________________"

return scalar HarrellC= `hc'

end 

