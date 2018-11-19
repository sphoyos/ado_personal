cap program drop gdummy
program define gdummy
*! Version 03 Mar 2013 2013  by SPH
*! syntax:  gen_dummy varlist ,missing 
*! Programa que serveix per preparar generar variables dummies

version 13.1

syntax varlist (min=1), [Missing] [eng]  [noetivar]
label define lab_sino 0"No" 1"Yes",modify
if "`eng'"!="" {
   label define lab_sino 0"No" 1"Si",modify
}


foreach var in `varlist' {
   count if `var'==.
   local nmis=r(N)
  *** Guarda l'etiqueta de la variable 
  local nomvar:var label `var'
  if `"`nomvar'"' =="" local nomvar="`var'"

   *** Selecciona els nivells de la variable
   local varlab: value lab `var'
  levelsof `var'  , local(levelsvar)

  
  cap drop `var'_mis
  foreach lev in `levelsvar' `missing' {
        
       if "`lev'"=="missing" {
	      if `nmis'!=0 {
	         cap drop `var'_mis
	         gen `var'_mis=(`var'==.)
			 label var `var'_mis "`nomvar'missing"
             label val `var'_mis  lab_sino
			 }
		   }
   else {	   
	   
	  if "`varlab'"!="" {
		local k `:label `varlab' `lev''
      }
	  else{
		local k=`lev'
	  }
     cap drop `var'_`lev'
	 gen `var'_`lev'=.
     replace `var'_`lev'=1 if `var'==`lev'
     replace `var'_`lev'=0 if `var'!=`lev' & `var'!=.

     if "`missing'" !="" {   
        replace `var'_`lev'=0 if `var'==.   
     }
		if "`etivar'"==""{ 
			label var `var'_`lev' "`nomvar'=`k'"
		 }
		 else {
		 	label var `var'_`lev' "`k'"
		 }	
     label val `var'_`lev'  lab_sino
   }
  } 
}

end


