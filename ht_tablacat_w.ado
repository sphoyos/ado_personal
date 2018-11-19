program define ht_tablacat_w
*! version 1.0 July 20011 de SPH
*! variacion sobre htablinrow de JJ 
*! this program makes tables in HTML format
*! syntax varlist(min=2 max=2) [if] [in], [Head] [CLose]  [PRow] [PCol] [ROWtotal] [COLtotal]
*!                                      [CHI] [EXACT]          [CI]   
*!                                      [Pval(real -1)] 
*!                                      [MIssing]
*!			    		[color]	
	                         
*! factors if you want only some of the factors to be presented
*! head to put the header
*! close to finish



syntax varlist(min=2 max=2) [if] [in] [iw],  [Head] [CLose]  [PRow] [PCol] [ROWtotal] [COLtotal]/*
*/					                            [CHI] [EXACT]  [CI]  [SYMMETRY] [SYMEXACT]      /*
*/                                      [Pval(real -1)]  /*
*/                                      [MIssing] /*
*/					[color]


version 9.1

if "`prow'" !="" &  "`pcol'" !="" {
 noisily di in red " Has te triar entre percentage per files (prow) o per columnes (pcol) "
 error 9
 }


 if "`chi'" !="" & "`exact'" !="" & "`symmetry'" !="" & "`symexact'" !="" {
  noisily di in red " Has de triar entre el test chi o la proba exacta de Fisher o el test de simetria"
  error 9
 }
 
 if "`weight'" != "" {
			tempvar wgt
			gen double `wgt' `exp'
			local wgte "[`weight'=`wgt']"
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


disp in yellow "`missing'"


 /* Elimina los perdidos si no se selecciona la opción missing y recuenta total casos*/
 



/* inicializa variables */
 qui sum `wgt' if `row'!=. & `col'!=. 
local ntotal=r(sum)
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
  qui sum `wgt' if `row'==`r' 
  local denom_row=r(sum)
  local j=0     
        foreach c in `collevels'{
             local j=`j'+1
			qui sum `wgt' if `col'==`c' 
			local denom_col=r(sum)
			qui sum `wgt' if `col'==`c'  & `row'== `r' 
            local num=r(sum)
version 11.1		
		local pct_row: di %5.2f (`num'/`denom_row')*100
			local ls_pct_row: di %5.2f invbinomial(`denom_row',`num',.025) *100
			local li_pct_row: di %5.2f invbinomial(`denom_row',`num',(1-.025)) *100
			local pct_col: di %5.2f (`num'/`denom_col')*100
			local ls_pct_col: di %5.2f invbinomial(`denom_col',`num',.025) *100
			local li_pct_col: di %5.2f invbinomial(`denom_col',`num',(1-.025)) *100
			local cell_`i'_`j' : di %7.1f `num'
            local variable: di "N "
			 
version 9.1

/* CALCULA ELS PERCENTATGES PER FILA */ 			   
			 if "`prow'"!="" {
			      local cell_`i'_`j' : di %7.1f `num' " (" `pct_row' "%)"
				  local variable: di "N (% fila) "
				if "`ci'"!="" {
			      local  cell_`i'_`j' : di  %7.1f `num'  " (" `pct_row' "%)"  " <p> [ " `li_pct_row' "% ; "  `ls_pct_row'  "% ] </p> " 
				  local variable: di "N (% fila) <p> [ IC 95% ] </p> "
				}
				  
			 }
/* CALCULA ELS PERCENTATGES PER COLUMNA */ 
			 if "`pcol'"!="" {
			      local cell_`i'_`j' : di %7.1f `num' " (" `pct_col' "%)"
				  local variable: di "N (% columna)"
			 }
			 
			    if "`ci'"!="" {
                  local  cell_`i'_`j' : di  %7.1f  `num'  " (" `pct_col' "%)" " <p> [ " `li_pct_col' "% ; "  `ls_pct_col'  "% ] </p> " 
				  local variable: di "N (% columna) <p> [ IC 95% ] </p> "           
				}
			 
			 }
}



disp in red  "`rowtotal'"

/* CALCULA ELS PERCENTATGES PER FILA  SI SE DEMANEN ELS TOTALS PER FILA */ 
 if "`rowtotal'" !="" {
    local total_fila=1
   	local i=0
	    foreach r in `rowlevels'{
	       local i=`i'+1
	       qui sum `wgt' if `row'==`r' 
           local pct_row: di %5.2f (r(sum)/ `ntotal' )*100
           local cell_`i'_ncol : di %7.1f r(sum) "(" `pct_row' "%)"		   
        }
 }

 
/* CALCULA ELS PERCENTATGES PER COLUMNA SI SE DEMANEN ELS TOTALS PER COLUMNA */ 
 if "`coltotal'" !="" {
    local total_col=1
	local j=0
	    foreach c in `collevels'{
	       local j=`j'+1
	       qui sum `wgt' if `col'==`c' 
           local pct_col: di %5.2f (r(sum)/ `ntotal' )*100
           local cell_nrow_`j' : di  %7.1f  r(sum) "(" `pct_col' "%)"		   
        }
 }

 
 
 
 /* CACLULA TESTS ESTADISTICS */
 
 
 
 if "`chi'"!="" {
  local haytest=1
  svyset _n  `wgte'
  quietly svy: tab `row' `col' ,`missing'
  local pval: di %5.4f   e(p_Pear)
  local method  Chi-cuadrado
  svyset, clear
 }
 
 
 if "`exact'"!="" {
  local haytest=1
  quietly tab `row' `col' , exact `missing'
  local pval: di %5.3f r(p_exact)
  local method Exacto de Fisher
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
 
   disp in yellow   "`i' ; `j' ; `cell_`i'_`j'' "
   
  }
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

disp in yellow `ncolspan'
htput <TR> 	
htput <TH  ALIGN= left COLSPAN=`ncolspan' `coloret'>  `var_row_lab'  </TH>
htput </TR> 	
	
	
/* COMENCEN FILES AMB LES DADES */

local i=0
foreach  r  in `rowlevels'  {
  local i=`i'+1
   if "`val_row_lab'"=="" local etiq=`r'
   else local etiq: label `val_row_lab' `r'
   if "`etiq'"=="." local etiq "Missing"
htput <TR> 		
	
  htput <TD ALIGN=left `coloret'>  `etiq' </TD>
   local j=0
    foreach c in `collevels' {
	  local j=`j' +1
	   htput  <TD ALIGN=center `coloret'>  `cell_`i'_`j'' </TD>
	 }
	 if `total_fila' ==1  {
       htput <TD ALIGN=right `coloret' >   `cell_`i'_ncol'  </TD>
   }
 
   if `haytest' ==1  & `i'==1 {
    local rowspan= `nrow'+`total_col'
       htput <TD ALIGN=right ROWSPAN=`rowspan' `coloret' >   `pval'  </TD>
   }
htput </TR> 		

}

  if `total_col' ==1  {
  
  
  
 htput <TR> 		
	
  htput <TD ALIGN=left `coloret'>  Total </TD>

   local j=0
    foreach c in `collevels' {
	  local j=`j' +1
	   htput  <TD ALIGN=right `coloret'>  `cell_nrow_`j'' </TD>
	 }
	 if `total_fila' ==1  {
	 local ntotaltot: di %7.1f `ntotal'
       htput <TD ALIGN=right `coloret' >   `ntotaltot' (100%)  </TD>
     }
 
     if `haytest' ==1  & `i'==1 {
       htput <TD ALIGN=right `coloret' >     </TD>
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





