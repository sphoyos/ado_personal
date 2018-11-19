cap program drop data_fromtxt
program define data_fromtxt
*! Version 20 Nov 2013  by SPH
*! syntax:  "data_fromtxt varlist, mdy"
*! This program transform string date "day/month/year" in date format variable
*! If format is "month/day/year" in date format variable optin mdy should be used

version 13
syntax varlist   [,MDY  YMD] [nodrop]

foreach var in "`varlist'" {

    replace `var'=lower(trim(`var'))
	replace `var'=subinstr(`var',"-","/",.)
	replace `var'=subinstr(`var',",","/",.)
	replace `var'=subinstr(`var',"..","/",.)
	replace `var'=subinstr(`var',".","/",.)
	
	replace `var'=subinstr(`var',"jan","/1/",.)
	replace `var'=subinstr(`var',"feb","/2/",.)
	replace `var'=subinstr(`var',"mar","/3/",.)
	replace `var'=subinstr(`var',"apr","/4/",.)
	replace `var'=subinstr(`var',"may","/5/",.)
	replace `var'=subinstr(`var',"jun","/6/",.)
	replace `var'=subinstr(`var',"jul","/7/",.)
	replace `var'=subinstr(`var',"aug","/8/",.)
	replace `var'=subinstr(`var',"sep","/9/",.)
	replace `var'=subinstr(`var',"oct","/10/",.)
	replace `var'=subinstr(`var',"nov","/11/",.)
	replace `var'=subinstr(`var',"dec","/12/",.)
	
	
	
	
	
	
	
	

	tempvar dia
	tempvar mes
	tempvar any
	tempvar  v2
	
   *** Si les dades son mes/dia/any extrau mes i  dia 
	
	if "`mdy'"=="" {
		gen `dia'= substr(`var',1,strpos(`var',"/")-1)
		gen `v2'=subinstr(`var', substr(`var',1,strpos(`var',"/")),"",1)
		gen `mes'= substr(`v2',1,strpos(`v2',"/")-1)
	}
	 *** Si lse dades son dia/mes i extrau mes i dia 
	else {
	
	    gen `mes'= substr(`var',1,strpos(`var',"/")-1)
		gen `v2'=subinstr(`var', substr(`var',1,strpos(`var',"/")),"",1)
		gen `dia'= substr(`v2',1,strpos(`v2',"/")-1)
		
	}

	gen `any'=real(substr(`v2',strpos(`v2',"/")+1,4))

	*destring ( `any'), force replace
	replace `any'= `any'+2000 if `any'<100
	
	if "`ymd'"!="" {
 	
        replace `any'=real(substr(`var',1,4))
		replace `mes'=substr(`var',6,2)
		replace `dia'=substr(`var',9,2)
	
	}
	
	local nomvar: var label `var' 
	ren `var' `var'_txt
	gen `var'= mdy(real(`mes'),real(`dia'),`any')
  
	replace `var' =mdy(real(`dia'), real(`mes'), `any') if real(`mes')>12 & real(`dia')<12 & `var'==.
	

	format `var'  %dD_m_CY
	order `var' , before(`var'_txt)
	label var `var' "`nomvar'"
		if "`drop'"=="" {
		     drop `var'_txt
		 }
}
end



