cap drop program ht_spvlist

*! Version 15 May 2014  by SPH
*! Programa per obtenir una taula de  supervivencia
*! syntax: syntax [if] [in] , 
*!      ADjustfor(varlist)	Variables d'ajust per la taula 	
*!      AT(numlist sort)	Valors en els que mostrar la supervivència
*!      NA					Nelson_Aalen estimator
*!      BY(varlist) 		Variable que genera els grups
*!      Failure 			Probabilitat acumulada d'events
*!      STrata(varlist) 	Variable d'estratificació
*!      Temp(string)		Etiqueta de l'unitat de temps
*!      Eventos(string)		Etiqueta de tipo d'events
*!      Lang(string) 		Idioma de les taules
*!      years(0)		    Flag de tiempo en años. 
program define ht_spvlist


syntax [if] [in] [, ADjustfor(varlist) AT(numlist sort) NA/*
		*/ BY(varlist) Failure STrata(varlist) ] [Temp(string)] [Eventos(string)] [Lang(string)]  [Years(integer 0)]
		
	
st_is 2 analysis
		
preserve
*** Posa les etiquetes segons l'idioma seleccionat

if "`lang'"=="" local lang="cat"
if "`lang'"=="cast" local lang="esp"

if "`if'`in'" != "" {
	qui keep `if'  `in'
}
	if "`temp'"=="" {
		if "`lang'"=="esp" local temp="Tiempo"
		if "`lang'"=="cat" local temp="Temps"
	 }
	if "`eventos'"=="" {
		if "`lang'"=="esp" local eventos="Eventos"
		if "`lang'"=="cat"  local eventos="Events"
	 }

disp in yellow "******************* `years' `years'`years'`years'`years'`years'`years'	 **************** "
*** Genera fitxer i variables temporals

tempfile taula_spv	
	
tempvar time		
tempvar n_risk
tempvar n_risk_t
tempvar n_def
tempvar spv
tempvar li_spv
tempvar ls_spv
tempvar fail
tempvar li_fail
tempvar ls_fail
tempfile etiq

gen time=_t
sts gen  `spv'=s  ,  by( `by' ) 
sts gen  `n_risk'= n  ,  by( `by' ) 
gsort `by' -_t
gen `n_risk_t'=.
if "`by'" !="" {
      by `by': replace  `n_risk_t'= sum(_st) if _st==1
}
else {
	replace  `n_risk_t'= sum(_st) if _st==1
}
gsort _t 
sts gen  `n_def'=d    ,  by( `by' ) 
sts gen  `li_spv'=lb(s)    ,  by( `by' ) 
sts gen  `ls_spv'=ub(s)    ,  by( `by' ) 

***  Guarda les etiquetes
cap local varlab:variable label `by' 
cap if  `"`varlab'"' == "" local varlab = "`by'"
cap local lab_by: val label `by'




cap label save `lab_by' using `etiq'.do, replace

									  
if "`failure'"!="" {

gen `fail'=(1-`spv')*100
gen `li_fail'=(1-`ls_spv')*100 
gen `ls_fail'=(1-`li_spv')*100 

sort   `by' `strata'  time

 collapse  (max) nrisk=`n_risk' nrisk_t=`n_risk_t' ndef=`n_def' spv_func=`fail' linf=`li_fail' lsup=`ls_fail' , by (`by' `strata' `adjustf'  time) 
 
 if "`lang'"=="esp" {
	label var nrisk " A riesgo"
	label var ndef "`eventos'"
	label var spv_func "% acumulado de `eventos'"
	label var linf "L.Inf. 95%IC"
	label var lsup "L.Sup. 95%IC"
	label var time "`temp'"
	format spv_func %5.2f
	format linf %5.2f
	format lsup %5.2f
 }
 
 if "`lang'"=="cat" {
	label var nrisk "A risc"
	label var ndef "`eventos'"
	label var spv_func "% acumulat d'`eventos'"
	label var linf "L.Inf. 95%IC"
	label var lsup "L.Sup. 95%IC"
	label var time "`temp'"
	format spv_func %5.2f
	format linf %5.2f
	format lsup %5.2f
 }
 
 
 
 
 
}
else {
replace `spv'=`spv'*100
replace `li_spv'=`li_spv'*100
replace `ls_spv'=`ls_spv'*100
  sort   `by' `strata'  time

  collapse  (max) nrisk=`n_risk' nrisk_t=`n_risk_t' ndef=`n_def' spv_func=`spv' linf=`li_spv' lsup=`ls_spv',  by (`by' `strata' `adjustf'  time) 
 
if "`lang'"=="esp" {
	label var nrisk "A riesgo"
	label var ndef "`eventos'"
	label var spv_func "% libre de `eventos'"
	label var linf "L.Inf. 95%IC"
	label var lsup "L.Sup. 95%IC"
	label var time "`temp'"
	format spv_func %5.2f
	format linf %5.2f
	format lsup %5.2f
}

if "`lang'"=="cat" {
	label var nrisk "A risc"
	label var ndef "`eventos'"
	label var spv_func "% lliure de `eventos'"
	label var linf "L.Inf. 95%IC"
	label var lsup "L.Sup. 95%IC"
	label var time "`temp'"
	format spv_func %5.2f
	format linf %5.2f
	format lsup %5.2f
}
}								  
save `taula_spv' , replace
use `taula_spv'




unab listavar:*
 format spv_func %5.2f
 format linf %5.2f
 format lsup %5.2f
 
if  "`at'" == "" {
     keep if nrisk!=.
    htlist_tot  time nrisk ndef spv_fun linf lsup , noobs
	
	
	
}
else {
      
       drop nrisk
	   ren nrisk_t nrisk
       unab listavar:*
       tempfile atfile
           cap postclose spv_at
       postfile  spv_at  `listavar'  using  `atfile' 

	    
**** Si s'ha selecccionat la spv per grup
   if "`by'"!="" { 
     
       levelsof `by', local(grups)
       ** S'inicia un bucle per valor de grupa

           foreach valor in `grups' {

             local grup=`valor'
             local ndef_t_1=0
             numlist "`at'"
             foreach  tj in `r(numlist)' {
                 local tempo= `tj'
                 if "`failure'"!="" {
				    if `tj'==0 {
					    qui sum nrisk  if `by'==`valor'
						local nrisk_t=r(max)
						}
					 else	{				
					 qui sum nrisk    if time<=`tj' & `by'==`valor'
					 local nrisk_t=r(min)
					 }
                     qui sum nrisk    if time<=`tj' & `by'==`valor'
					 local nrisk_t=r(min)
                     qui summ ndef  if time<=`tj' & `by'==`valor'
                     local ndef_t=r(sum)-`ndef_t_1'
					 local ndef_t_1=r(sum)
                     qui summ spv_func  if time<=`tj' & `by'==`valor'
                     local spv_func_t=r(max)
                     qui summ linf  if time<=`tj' & `by'==`valor'
                     local linf_t=r(max)
                     qui summ lsup  if time<=`tj' & `by'==`valor'
                     local lsup_t=r(max)
                } 
                else {
                     
					 if `tj'==0 {
					    qui sum nrisk  if `by'==`valor'
						local nrisk_t=r(max)
						}
					 else {					
					 qui sum nrisk    if time<=`tj' & `by'==`valor'
					 local nrisk_t=r(min)
					 }
                     qui summ ndef  if time<=`tj' & `by'==`valor'
                     local ndef_t=r(sum)-`ndef_t_1'
					 local ndef_t_1=r(sum)
                     qui summ spv_func  if time<=`tj' & `by'==`valor'
                     local spv_func_t=r(min)
                     qui summ linf  if time<=`tj' & `by'==`valor'
                     local linf_t=r(min)
                     qui summ lsup  if time<=`tj' & `by'==`valor'
                     local lsup_t=r(min)
                }
				
				
            post  spv_at   (`grup') (`tempo')  (`nrisk_t')  (`ndef_t') (`spv_func_t') (`linf_t') (`lsup_t') 

            }
        }
		}
      else {
 
             local ndef_t_1=0
		
             numlist "`at'"
             foreach  tj in `r(numlist)' {
                 local tempo= `tj'
                 if "`failure'"!="" {
				   if `tj'==0 {
					    qui sum nrisk  
						local nrisk_t=r(max)
						}
					 else	{				
					 qui sum nrisk    if time<=`tj'
					 local nrisk_t=r(min)
					 }
                                          qui summ ndef  if time<=`tj' 
					 local ndef_t=r(sum)-`ndef_t_1'
					 local ndef_t_1=r(sum)
                     qui summ spv_func  if time<=`tj' 
                     local spv_func_t=r(max)
                     qui summ linf  if time<=`tj' 
                     local linf_t=r(max)
                     qui summ lsup  if time<=`tj'
                     local lsup_t=r(max)
                } 
                else {
				    if `tj'==0 {
					    qui sum nrisk  
						local nrisk_t=r(max)
						}
					 else  {					
					 qui sum nrisk    if time<=`tj' 
					 local nrisk_t=r(min)
					 }
                     qui  summ ndef  if time<=`tj' 
                     local ndef_t=r(sum)-`ndef_t_1'
					 local ndef_t_1=r(sum)
                     qui summ spv_func  if time<=`tj' 
                     local spv_func_t=r(min)
                     qui summ linf  if time<=`tj' 
                     local linf_t=r(min)
                     qui summ lsup  if time<=`tj' 
                     local lsup_t=r(min)
                }
            post  spv_at    (`tempo')   (`nrisk_t')  (`ndef_t') (`spv_func_t') (`linf_t') (`lsup_t') 
      }      
	 }		
	 
postclose spv_at

use `atfile',  clear 


 if "`by'" !="" {
 
 
 label var `by'   "`varlab'"
 do `etiq'.do
cap label val `by' "`lab_by'"

 tab `by'
desc
}
sort `by' time


 if "`lang'"=="esp" {
	label var ndef "`eventos' entre intervalos"
		if "`failure'"!="" {
			label var spv_func "% acumulado de `eventos'"
			label var linf "L.Inf. 95%IC"
			label var lsup "L.Sup. 95%IC"
		}
		else {
			label var spv_func "% libre de `eventos'"
			label var linf "L.Inf. 95%IC"
			label var lsup "L.Sup. 95%IC"
		}
	label var time "`temp'"
	label var nrisk " Nº a riesgo inicio intervalo"
	format spv_func %5.2f
	format linf %5.2f
	format lsup %5.2f
 }
 
 
 if "`lang'"=="cat" {
	label var ndef "`eventos' entre intervals"
	if "`failure'"!="" {
		label var spv_func "% acumulat d'`eventos'"
		label var linf "L.Inf. 95%IC"
		label var lsup "L.Sup. 95%IC"
	}
	else {
		label var spv_func "% lliure d'`eventos'"
		label var linf "L.Inf. 95%IC"
		label var lsup "L.Sup. 95%IC"
	}
	label var time "`temp'"
	label var nrisk " Nº a risc a l'inici de l'interval"
	format spv_func %5.2f
	format linf %5.2f
	format lsup %5.2f
 }
 
 

 gen text_time= string(time)
 
 label var text_time "`temp'"
 if `years' != 0  & "`lang'"=="esp" {
 replace text_time=text_time+" años"
  replace text_time= "48 horas"  if text_time==".006 años"
 replace text_time= "1 mes"  if text_time==".08 años"
 replace text_time= "3 meses" if time==0.25
 replace text_time= "6 meses" if time==0.50
 replace text_time= "9 meses" if time==0.75
 }
 
  if `years' != 0 & "`lang'"=="cat" {
 replace text_time=text_time+" anys"
  replace text_time= "48 hores"  if text_time==".006 años"
 replace text_time= "1 mes" if text_time==".08 anys"
 replace text_time= "3 mesos" if time==0.25
 replace text_time= "6 mesos" if time==0.50
 replace text_time= "9 mesos" if time==0.75
 }
  if `years' == 0  {
   replace text_time=text_time+" `temp'"
 }
 
 if "`by'" =="" {
htlist_tot text_time  nrisk ndef spv_func linf lsup   ,noobs
}
else {
 levelsof `by', local(grups)
       ** S'inicia un bucle per valor de grupa

           foreach valor in `grups' {
           if  "`lab_by'"=="" local etiq=`valor'
			  else local etiq: label `lab_by' `valor'
		   
htput  <FONT color="#993366"> <b> `varlab' =  `etiq'	</b> </FONT color>	  
htlist_tot  text_time  nrisk ndef spv_func linf lsup   if `by'==`valor' ,noobs		   
htput <BR>
		    
 }
}
}			
restore 
end		
		
		
		
		
		
		