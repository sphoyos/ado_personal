program define ht_tabla_pairtest
*! version 1.0 april 2012 de SPH
*! variacion sobre htablinrow de JJ 
*! this program makes tables in HTML format for pair test in continous 
*! syntax varlist(min=2 max=2) [if] [in],  [Head] [Total] [noTotalcol]
*!                                      [Ttest] [Sign] [Sumrank]        [CI] 
*!                                      [Pval(real -1)] 
*!			    		[color]	
	                         
*! factors if you want only some of the factors to be presented
*! head to put the header
*! close to finish



syntax varlist(min=2 max=2) [if] [in],  [Head] [CLose]   [Median] /*
*/					                            [Ttest] [Sign] [Signrank]   [CI]       /*
*/                                      [Pval_ttest(real -1)] [PVal_sign(real -1)] [PVal_signrank(real -1)]  /*
*/                                      [MIssing] [minmax] [etifila(string)] [ordendif(real 12)] /*
*/					[color]


version 9.1


/* * Guarda los datos en memoria  * */

tempfile usedata
save `usedata', replace

/* Selecciona casos **/ 

tempvar touse
generate `touse'=0
replace `touse' =1 `if' `in'
keep if `touse'


/*  Identifica variables fila y columnas */






tokenize `varlist'
local var1="`1'"
local var2="`2'"
local varlab1:variable label `var1'  
 if   `"`varlab1'"' == "" local varlab1 = "`var1'"

 local varlab2:variable label `var2'  
 if   `"`varlab2'"' == "" local varlab2 = "`var2'"

 
 keep if `var1'!=. & `var2'!=.
 
tempvar var3
if  `ordendif'==12 {
   gen `var3'= `var1'-`var2'
   local var3_lab="Diferencia `varlab1'-`varlab2'"
}
else if  `ordendif'==21 {
   gen `var3'= `var2'-`var1'
   local var3_lab="Diferencia`varlab2'-`varlab1'"
}
else  {
    display as err /*
        */ "ordendif ha de valdre 12 or 21"
        exit 198
}							



/* inicializa variables */
local ntotal=_N
local haytest_ttest=0
local haytest_sign=0
local haytest_signrank=0




/*  Identifica nom de les categories de la variable columna*/

local var1_lab: variable label `var1'
if `"`var1_lab'"' ==""  {

         local var1_lab="`var1'"
} 

local var2_lab: variable label `var2'
if `"`var2_lab'"' ==""  {

         local var2_lab="`var2'"
} 

*   "
 local rowform: format `var1'  
   
local j=0

version 11.1

forvalues  j=1(1)3 {
  
  
     		qui summarize `var`j''  ,detail
            local num=r(N)		
			local min: di `rowform' r(min)
			local max: di `rowform' r(max)
			local mitja: di `rowform' r(mean)
			local devtip: di `rowform' r(sd)
			local mediana: di `rowform' r(p50) 
			local per25: di `rowform' r(p25) 
			local per75: di `rowform' r(p75)
			local  se =  r(sd)/sqrt(r(N))
		    local li : di  `rowform' r(mean) - invttail(r(N)-1, .025)*`se'
		    local ls : di  `rowform' r(mean) + invttail(r(N)-1, .025)*`se'
	        local  cell_`j' : di `num' " <p>  " "`mitja'"     " ( " "`devtip'" ") </p>  "
	  
	       if "`ci'"!="" {
	              local  cell_`j': di  `num' " <p>  " "`mitja'" "  [" "`li'" " ; " "`ls'" " ]  </p> "
					
					
					}
		   if "`minmax'"!="" {
	              local  cell_`j': di  "`cell_`j'' "  "<p>  "  "  [" "`min'" " ; " "`max'" " ]  </p> "
					
					
					}
					
					
		    if "`median'"!="" { 	
				local cell_`j': di "`cell_`j''"   "   <p> " "`mediana'" " [" "`per25'" " ; " "`per75'" " ]  </p>"
				}	   

}			


 version 9.1

 /* CACLULA TESTS ESTADISTICS */

 
 
 if "`ttest'"!="" {
  local haytest_ttest=1
  quietly ttest `var1'= `var2' 
  local pval_ttest: di %5.3f  r(p)
  
  }
 
 
 if "`sign'"!="" {
  local haytest_sign=1
  quietly signtest `var1' =`var2' 
  local pval_sign: di %5.3f  r(p_2)
  
 }
 
 if "`signrank'"!="" {
  local haytest_signrank=1
  quietly signrank `var1'= `var2' 
  local pval_signrank: di %5.3f  2*(1- normal(abs(r(z))))
  
 }
 
 
 
 
 /* MARCA EL COLOR DELS TESTS SIGNIFICATIUS */
       local coloret=""
       local coloret_ttest=""
       local coloret_sign=""
	   local coloret_signrank=""
 
 
 

  if "`color'"!=""  {
       if round(`pval_ttest',0.001) <=0.05 & `pval_ttest'!=-1 {
            local coloret= `"BGCOLOR="#FFCCCC""'          
	        local coloret_ttest= `"BGCOLOR="#CC6699""' 
	    }
			
	   if round(`pval_sign',0.001) <=0.05 & `pval_sign'!=-1 {	
		    local coloret= `"BGCOLOR="#FFCCCC""' 
            local coloret_sign= `"BGCOLOR="#CC66FF""' 
        }
				
	   if round(`pval_signrank',0.001) <=0.05 & `pval_signrank'!=-1 {	
		    local coloret= `"BGCOLOR="#FFCCCC""' 
            local coloret_signrank= `"BGCOLOR="#FF99FF""' 
        }

	}        
 
 
 
 
 local variable: di "N <p> media (sd) </p>    		


       if "`ci'"!="" {
			     local variable: di  "N <p> media [IC 95%]  </p> " 		 
				}
				
       if "`minmax'"!="" {
			     local variable: di  "`variable'" " <p> [Min; Max]  </p> " 		 
				}
               		
               				 
		if "`median'"!="" {
					local variable :di "`variable'"  " <p> mediana [ p25;p75 ]  </p>"
                }   
 
 /* GENERA LA CAPÇALERA DE LA TAULA */
 
 if "`head'" != ""{

 * "

 htput <TABLE BORDER=1 CELLSPACING =0 CELlPADDING=2>
 htput <TR>
 
 
 htput <TH VALIGN=CENTER ALIGN=CENTER   > `variable' </TH>

  forvalues  j=1(1)3 {

    htput <TH> `var`j'_lab' </TH>
	   }
   
 
   if `haytest_ttest'==1 {
       htput <TH VALIGN=CENTER  > <p> p-valor </p> <p>  T-test apareado </p> </TH>
   }
  
  
   if `haytest_sign'==1 {
       htput <TH VALIGN=CENTER  > <p> p-valor </p> <p> Test_signo </p> </TH>
   }
   
    if `haytest_signrank'==1 {
       htput <TH VALIGN=CENTER  > <p> p-valor </p> <p> Test_signo-rango </p> </TH>
   }
 htput </TR>
 
 

	
	
}	
	/* FI CAPÇALERA*/	 
	
	
/* COMENCEN FILES AMB LES DADES */


htput <TR> 	
htput <TD  ALIGN= left  `coloret'>   `etifila'   </TD>

   
     forvalues  j=1(1)3  {
	 
	   
	  
	   htput  <TD ALIGN=center `coloret'>  `cell_`j'' </TD>
	 }
	
 
   if `haytest_ttest' ==1   {
           htput <TD ALIGN=center `coloret_ttest' >   `pval_ttest'  </TD>
   }
   
   if `haytest_sign' ==1   {
           htput <TD ALIGN=center  `coloret_sign' >   `pval_sign'  </TD>
   }
   
     if `haytest_signrank' ==1   {
           htput <TD ALIGN=center  `coloret_signrank' >   `pval_signrank'  </TD>
   }
htput </TR> 		


	


	
	
/* TANCA LA TAULA */

  if "`close'" !="" {
    htput </TABLE>
	htput </BR> 	
  }

	
/* ACTIVA LES DADES ORIGINALES */
	
use `usedata', replace


end 





