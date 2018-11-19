program define ht_tablacat_eng
*! version 1.0 July 20011 de SPH
*! variacion sobre htablinrow de JJ 
*! this program makes tables in HTML format
*! syntax varlist(min=2 max=2) [if] [in], [Head] [CLose]  [PRow] [PCol] [ROWtotal] [COLtotal]
*!                                      [CHI] [EXACT]    [SYMMETRY] [SYMEXACT]  [SKILMACK]         [CI]   
*!                                      [Pval(real -1)]  [POSTHOC]  [EXPECTED]
*!                                      [MIssing]
*!			    		[color]	
	                         
*! factors if you want only some of the factors to be presented
*! head to put the header
*! close to finish



syntax varlist(min=2 max=2) [if] [in],  [Head] [CLose]  [PRow] [PCol] [ROWtotal] [COLtotal] [id(string)] /*
*/					                            [CHI] [EXACT]  [CI]  [SYMMETRY] [SYMEXACT]  [SKILMACK]  /*
*/                                      [Pval(real -1)]  [POSTHOC]  [EXPECTED]  /*
*/                                      [MIssing] /*
*/					[color]


version 9.1

if "`prow'" !="" &  "`pcol'" !="" {
 noisily di in red " Has te triar entre percentage per files (prow) o per columnes (pcol) "
 error 9
 }


 if "`chi'" !="" & "`exact'" !="" & "`symmetry'" !="" & "`symexact'" !=""  & "`skilmack'" !="" {
  noisily di in red " Has de triar entre el test chi o la proba exacta de Fisher o el test de simetria"
  error 9
 }
 if "`skilmack'"!="" & "`id'"=="" {
 noisily di in red " Has de indicar quin es l'identificador dels individus si vols fer el test skilmack"
 error 9
 }
 

/* * Guarda los datos en memoria  * */

tempfile usedata
save `usedata', replace

/* Selecciona casos **/ 

tempvar touse
generate `touse'=0
replace `touse' =1 `if' `in'
keep if `touse'


/*  Identifica variables fila y columnas */



disp in yellow _N

tokenize `varlist'
local row="`1'"
local col="`2'"





 /* Elimina los perdidos si no se selecciona la opción missing y recuenta total casos*/
 



/* inicializa variables */
local n_total=r(N)
local haytest=0
local total_fila=0
local total_col=0



/*  Identifica nombre de variables y etiquetas de fila */

local var_row_lab: variable label `row'
if `"`var_row_lab'"' ==""  {

         local var_row_lab="`row'"
   } 


   
   
local val_row_lab: value label `row'
levelsof `row', local(rowlevels) `missing' /* missing incluye los casos perdidos como un nuevo valor */ 
local nrow= wordcount("`rowlevels'" )


local var_col_lab: variable label `col'
if `"`var_col_lab'"'  ==""  {
         local var_col_lab= "`col'" 
   }

local val_col_lab: value label `col'

disp in yellow "`val_col_lab'"

levelsof `col', local(collevels) `missing'  /* missing incluye los casos perdidos como un nuevo valor */ 
local ncol= wordcount("`collevels' ")

local i=0

if "`missing'"=="" {
keep if `row' !=. & `col'!=.
local n_total=_N
}


foreach  r  in `rowlevels'  {
  local i=`i'+1
  qui count if `row'==`r' 
  local denom_row=r(N)
  local j=0     
        foreach c in `collevels'{
             local j=`j'+1
			qui count if `col'==`c' 
			local denom_col=r(N)
			qui count if `col'==`c'  & `row'== `r' 
            local num=r(N)
			local expect= (`denom_row' * `denom_col')/ `n_total' 
			local adjust_res = ( `num' -`expect') / sqrt( `expect' *( 1- (`denom_row' /`n_total') )* ( 1- (`denom_col' /`n_total') )) 
			
			
version 11.1		
		local pct_row: di %5.2f (`num'/`denom_row')*100
			local ls_pct_row: di %5.2f invbinomial(`denom_row',`num',.025) *100
			local li_pct_row: di %5.2f invbinomial(`denom_row',`num',(1-.025)) *100
			local pct_col: di %5.2f (`num'/`denom_col')*100
			local ls_pct_col: di %5.2f invbinomial(`denom_col',`num',.025) *100
			local li_pct_col: di %5.2f invbinomial(`denom_col',`num',(1-.025)) *100
			local cell_`i'_`j' : di `num'
			local exp_`i'_`j' : di %5.2f `expect'
			local adjres_`i'_`j' : di %5.2f `adjust_res'
            local variable: di "N "
			local  coloret_`i'_`j'=""
			if  abs(`adjust_res') >1.96  &  `adjust_res'!=. {
			local coloret_`i'_`j'=  `"BGCOLOR="#FDA9FF""'  
            } 
version 9.1

/* CALCULA ELS PERCENTATGES PER FILA */ 			   
			 if "`prow'"!="" {
			      local cell_`i'_`j' : di `num' " (" `pct_row' "%)"
				  local variable: di "N (% fila) "
				if "`ci'"!="" {
			      local  cell_`i'_`j' : di   `num'  " (" `pct_row' "%)"  " <p> [ " `li_pct_row' "% ; "  `ls_pct_row'  "% ] </p> " 
				  local variable: di "N (% fila) <p> [ IC 95% ] </p> "
				}
				  
			 }
/* CALCULA ELS PERCENTATGES PER COLUMNA */ 
			 if "`pcol'"!="" {
			      local cell_`i'_`j' : di `num' " (" `pct_col' "%)"
				  local variable: di "N (% columna)"
			 
			 
			    if "`ci'"!="" {
                  local  cell_`i'_`j' : di   `num'  " (" `pct_col' "%)" " <p> [ " `li_pct_col' "% ; "  `ls_pct_col'  "% ] </p> " 
				  local variable: di "N (% columna) <p> [ IC 95% ] </p> "           
				}
			 }
			 }
}





/* CALCULA ELS PERCENTATGES PER FILA  SI SE DEMANEN ELS TOTALS PER FILA */ 
 if "`rowtotal'" !="" {
    local total_fila=1
   	local i=0
	    foreach r in `rowlevels'{
	       local i=`i'+1
	       qui count if `row'==`r' 
		   local num_row=r(N)
           local pct_row: di %5.2f (r(N)/ `n_total' )*100
           local cell_`i'_ncol : di r(N) "(" `pct_row' "%)"		
           local li_pct_row: di %5.2f invbinomial(`n_total',`num_row',.025) *100
		   local ls_pct_row: di %5.2f invbinomial(`n_total',`num_row',(1-.025)) *100
				   
 if "`ci'"!="" {
                  local  cell_`i'_ncol  : di   `num_row'  " (" `pct_row' "%)" " <p> [ " `li_pct_row' "% ; "  `ls_pct_row'  "% ] </p> " 
				            
				}   
		   
        }
 }

 
/* CALCULA ELS PERCENTATGES PER COLUMNA SI SE DEMANEN ELS TOTALS PER COLUMNA */ 
 if "`coltotal'" !="" {
    local total_col=1
	local j=0
	    foreach c in `collevels'{
	       local j=`j'+1
	       qui count if `col'==`c' 
		   local num_col=r(N)
           local pct_col: di %5.2f (r(N)/ `n_total' )*100
           local cell_nrow_`j' : di r(N) "(" `pct_col' "%)"	
		   local li_pct_col: di %5.2f invbinomial(`n_total',`num_col',.025) *100
		   local ls_pct_col: di %5.2f invbinomial(`n_total',`num_col',(1-.025)) *100
		   
		   
 if "`ci'"!="" {
                  local  cell_nrow_`j' : di   `num_col'  " (" `pct_col' "%)" " <p> [ " `li_pct_col' "% ; "  `ls_pct_col'  "% ] </p> " 
				            
				}		   
        }
 }

 
 
 
 /* CACLULA TESTS ESTADISTICS */
 
 
 
 if "`chi'"!="" {
  local haytest=1
  quietly tab `row' `col' , chi `missing'
  local pval: di %5.3f r(p)
  local method  Chi-cuadrado
 }
 
 
 if "`exact'"!="" {
 version 13.1
  local haytest=1
  quietly cap tab `row' `col' , exact `missing'
   if _rc !=910{
       local pval: di %5.3f r(p_exact)
       local method Exacto de Fisher
	   }
	else {
     	 quietly tab `row' `col' , chi `missing'
          local pval: di %5.3f r(p) 
		  local method chi-cuadrado 
        }
   }
  
 
 if "`symmetry'"!="" {
  local haytest=1
  quietly symmetry `row' `col' 
  local pval: di %5.3f r(p)
  local method  Simetria
 }
 
 
 
 if "`symexact'"!="" {
  local haytest=1
  quietly symmetry `row' `col' , exact
  local pval: di %5.3f r(p_exact)
  local method  Simetria exacto
 }
 
  if "`skilmack'"!="" {
  local haytest=1
  qui skilmack `row' , id(`id') repeated(`col')
  
  local pval: di %5.3f r(p_2)
  local method Skillings-Mack
  }
 
 
 /* MARCA EL COLOR DELS TESTS SIGNIFICATIUS */
 
  if "`color'"!="" {
     if round(`pval',0.001) <=0.05 & `pval'!=-1 {
      local coloret= `"BGCOLOR="#FFCCCC""' 
     }
	else local coloret=""
  }
	else local coloret=""
 
 
 forvalues i=1(1)`nrow' {
  forvalues j=1(1)`ncol' {
 

   
  }
}
 
  if 	"`expected'"!="" {
   local variable= "   `variable'  <br> Esperat <br> Residus ajustats"
}   
			 
 

 
 
 /* GENERA LA CAPÇALERA DE LA TAULA */
 
 if "`head'" != ""{

 

 htput <TABLE BORDER=1 CELLSPACING =0 CELlPADDING=2>
 htput <TR>
 
 
 htput <TH VALIGN=CENTER ALIGN=CENTER ROWSPAN =2  > `variable' </TH>
 htput <TH ALIGN=CENTER COLSPAN=`ncol'> `var_col_lab' </TH>
 
   if `total_fila' ==1  {
       htput <TH VALIGN=BOTTOM ROWSPAN=2 > Total  </TH>
   }
 
   if `haytest'==1 {
       htput <TH VALIGN=BOTTOM ROWSPAN=2 > <p> p-valor </p> <p> `method' </p> </TH>
   }
  
 htput </TR>
 
 htput <TR>
  foreach c in `collevels' {
    if "`val_col_lab'"=="" local group=`c'
    else local group: label `val_col_lab' `c'
	if "`group'"=="." local group "Missing"
	htput <TH> `group' </TH>
	   }
	   

	htput </TR> 

	
	
}	
	/* FI CAPÇALERA*/	 
	
	
/* FILA AMB EL NOM DE LA VARIABLE */
	
local ncolspan= `ncol'+`total_fila' + `haytest'	+1


htput <TR> 	
htput <TH  ALIGN= left COLSPAN=`ncolspan' >  `var_row_lab'  </TH>
htput </TR> 	
	
	
/* COMENCEN FILES AMB LES DADES */

local i=0
foreach  r  in `rowlevels'  {
  local i=`i'+1
   if "`val_row_lab'"=="" local etiq=`r'
   else local etiq: label `val_row_lab' `r'
   if "`etiq'"=="." local etiq "Missing"
htput <TR> 		
	
  htput <TD ALIGN=left >  `etiq' </TD>
   local j=0
    foreach c in `collevels' {
   local j=`j' +1


    if "`coloret'"=="" local coloret_`i'_`j'=""
	if "`posthoc'"=="" {
      local coloret_`i'_`j'=""
	}
	
    if 	"`expected'"!="" {
	htput  <TD ALIGN=center `coloret_`i'_`j''>  `cell_`i'_`j''  <br>  `exp_`i'_`j''  <br> `adjres_`i'_`j'' </TD>
	
	
	}
	else { 
	htput  <TD ALIGN=center `coloret_`i'_`j''>  `cell_`i'_`j''   </TD>
	}
	
	
	
	 }
	 if `total_fila' ==1  {
       htput <TD ALIGN=right  >   `cell_`i'_ncol'  </TD>
   }
 
   if `haytest' ==1  & `i'==1 {
    local rowspan= `nrow'+`total_col'
       htput <TD ALIGN=right ROWSPAN=`rowspan'  `coloret' >   `pval'  </TD>
   }
htput </TR> 		

}

  if `total_col' ==1  {
  
  
  
 htput <TR> 		
	
  htput <TD ALIGN=left >  Total </TD>

   local j=0
    foreach c in `collevels' {
	  local j=`j' +1
	   htput  <TD ALIGN=right >  `cell_nrow_`j'' </TD>
	 }
	 if `total_fila' ==1  {
       htput <TD ALIGN=right  >   `n_total' (100%)  </TD>
     }
 
     if `haytest' ==1  & `i'==1 {
       htput <TD ALIGN=right  >     </TD>
     }
  
   }	

   
	
/* AFEGEIX TOTAL PER FILA */

	
	
	
/* TANCA LA TAULA */

  if "`close'" !="" {
    htput </TABLE>
  }

	
/* ACTIVA LES DADES ORIGINALES */
	
use `usedata', replace


end 





