program define ht_tablacont_eng, rclass
*! version 1.0 July 20011 de SPH en angles
*! variacion sobre htablinrow de JJ 
*! this program makes tables in HTML format
*! syntax varlist(min=2 max=2) [if] [in],  [Head] [Total] [noTotalcol]
*!                                      [Anova] [KWallis]        [CI] 
*!                                      [Pval(real -1)] 
*!                                      [MIssing]
*!			    		[color]	
	                         
*! factors if you want only some of the factors to be presented
*! head to put the header
*! close to finish



syntax varlist(min=2 max=2) [if] [in],  [Head] [CLose]   [TOTal] [NOMedia] [Median] [Ptiles] [id(string)]  /*
*/					                            [Anova] [KWallis]  [Skilmack] [CI]   [UMan] [Manexact] [Swilk]  /*
*/                                      [Pval_aov(real -1)] [PVal_kw(real -1)]  [PVal_skil(real -1)] [PVal_uman(real -1)]  [PVal_manexact(real -1)]  /*
*/                                      [MIssing] [minmax] [etifila(string)] [Format(string)] [psig(real 0.05)] /*
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
local row="`1'"
local col="`2'"





 /* Elimina los perdidos si no se selecciona la opción missing y recuenta total casos*/
 
if "`missing'"=="" {
keep if `col' !=. 
}


/* inicializa variables */
local ntotal=_N
local haytest_aov=0
local haytest_kw=0
local haytest_skil=0
local haytest_uman=0
local haytest_manexact=0
local total_fila=0


/*  Identifica nom de les categories de la variable columna*/

local var_col_lab: variable label `col'
if `"`var_col_lab'"' ==""  {

         local var_col_lab="`col'"
} 

*** Identifica si la variable es normal i assigna nota peu pagina 

 if "`uman'"=="" & "`swilk'"!="" & "`mannexact'"==""{
swilk `row'
local tparam=""
local tnoparam=""
local pnorm=r(p)
}
   
   
local val_col_lab: value label `col'
levelsof `col', local(collevels) `missing' /* missing incluye los casos perdidos como un nuevo valor */ 
local ncol= wordcount("`collevels'" )

local rowform: format `row'
if "`format'"!="" local rowform="`format'"
local var_row_lab: variable label `row'
if `"`var_row_lab'"'  ==""  {
         local var_row_lab= "`row'" 
}

local j=0

version 11.2

foreach  c  in `collevels' {
  local j=`j'+1
  
     		qui summarize `row'  if `col'==`c' ,detail
            local num=r(N)		
			local min: di `rowform' r(min)
			local max: di `rowform' r(max)
			local mitja: di `rowform' r(mean)
			local devtip: di `rowform' r(sd)
			local mediana: di `rowform' r(p50) 
			local per25: di `rowform' r(p25) 
			local per75: di `rowform' r(p75)
			local per5: di `rowform' r(p5)
			local per95: di `rowform' r(p95)
			local  se =  r(sd)/sqrt(r(N))
		    local li : di  `rowform' r(mean) - invttail(r(N)-1, .025)*`se'
		    local ls : di  `rowform' r(mean) + invttail(r(N)-1, .025)*`se'
			local cell_`j': di `num'
	     
		 if "`nomedia                    '" == "" {
		   local  cell_`j' : di  "`cell_`j''" " <p>  " "`mitja'"     " ( " "`devtip'" ") </p>  "
	      }
	       if "`ci'"!="" {
	              local  cell_`j': di  "`cell_`j''"  " <p>  "  "  [" "`li'" " ; " "`ls'" " ]  </p> "
					
					
					}
		   if "`minmax'"!="" {
	              local  cell_`j': di  "`cell_`j''"  " <p>  "  "  [" "`min'" " ; " "`max'" " ]  </p> "
					
					
					}
					
					
		    if "`median'"!="" { 	
				local cell_`j': di "`cell_`j''"   "   <p> " "`mediana'" " [" "`per25'" " ; " "`per75'" " ]  </p>"
				}	   
            
			if "`ptiles'"!="" { 	
				local cell_`j': di "`cell_`j''"   "   <p> "  " [" "`per5'" " ; " "`per95'" " ]  </p>"
				}	 
}			



 
/* CALCULA ELS PERCENTATGES PER COLUMNA SI SE DEMANEN ELS TOTALS PER COLUMNA */ 
 if "`total'" !="" {
            local total_fila=1
     	    qui summarize `row'  ,detail
			local num=r(N)	
			local min: di  `rowform' r(min)
			local max: di  `rowform' r(max)			
			local mitja: di  `rowform' r(mean)
			local devtip: di `rowform' r(sd)
			local mediana: di `rowform' r(p50) 
			local per25: di  `rowform' r(p25) 
			local per75: di `rowform' r(p75)
			local per5: di `rowform' r(p5)
			local per95: di `rowform' r(p95)
			local  se =  r(sd)/sqrt(r(N))
		    local li : di  `rowform' r(mean)- invttail(r(N)-1, .025) *`se'
		    local ls : di  `rowform'  r(mean)+ invttail(r(N)-1, .025) *`se'
			local cell_ncol: di "`num'"
	    if "`nomedia'" == "" {
	   local  cell_ncol :  di "`cell_ncol'"   "  <p>  " "`mitja'" " ( " "`devtip'" ") </p>  "
	   }
	     if "`ci'"!="" {
		     local  cell_ncol:  di  "`cell_ncol'"  "  <p>  "  "  [" "`li'" " ; " "`ls'" " ]  </p>  "
				}
		 if "`minmax'"!="" {
		     local  cell_ncol:  di  "`cell_ncol'"  "  <p>  "  "  [" " `min'" " ; " "`max'" " ]  </p>  "
				}
		 if "`median'"!="" { 	
				local cell_ncol: di "`cell_ncol'"   "  <p> " "`mediana'" " [" "`per25'" " ; " "`per75'" " ]  </p>"
				}
		 if "`ptiles'"!="" { 	
				local cell_ncol: di "`cell_ncol'"   "  <p> "  " [" "`per5'" " ; " "`per95'" " ]  </p>"
				}
				
 }

 version 9.1

 /* CALCULA TESTS ESTADISTICS */

 
 
 if "`anova'"!="" {
  local haytest_aov=1
 capture  quietly oneway `row' `col' 
  local pval_aov: di %9.5f   fprob(r(df_m), r(df_r), r(F)) 
  if r(df_m)==1 local method_aov T-Test
  else local method_aov  ANOVA
 }
 
 
 if "`kwallis'"!="" {
  local haytest_kw=1
  capture quietly kwallis `row', by(`col')
  local pval_kw: di %9.5f  chiprob( r(df), r(chi2_adj))
  if r(df)==1 local method_kw Mann-Whitney
  else local method_kw  Kruskal-Wallis
 }
 
 

 if "`uman'"!="" {
  local  haytest_uman=1
  capture noisily ranksumex `row', by(`col')
  
  di in blue %9.5f  (r(nx1)/r(den))
  local pval_uman: di %9.5f  (r(nx1)/r(den))
  local method_uman exact one_side Mann-Whitney
  if r(nx1)==.  local pval_uman: di %9.5f  1-normal(abs(r(z)))
  if r(nx1)==.  local method_uman one_side Mann-Whitney
  
 }
 
if "`manexact'"!="" {
  local  haytest_manexact=1
  capture noisily ranksumex `row', by(`col')
  
  local pval_manexact: di %9.5f  r(p)
  local method_manexact exact two side U Mann-Whitney
  if r(nx1)==.  local pval_manexact: di %9.5f  1-2*normal(abs(r(z)))
  if r(nx1)==.  local method_manexact  two side U Mann-Whitney
  
 }
 
   if "`skilmack'"!="" {
  local haytest_skil=1
  qui skilmack `row' , id(`id') repeated(`col')
  local pval_skil: di %9.5f  r(p_2)
  if r(p_2)==.  local pval_skil: di %9.5f  r(p) 
  local method_skil Skillings-Mack
  }
 
 
 
 
 
 
 
 /* MARCA EL COLOR DELS TESTS SIGNIFICATIUS */
 
  if "`color'"!=""  {
       if round(`pval_aov ',0.00001) <=`psig' & `pval_aov'!=-1 {
            local coloret= `"BGCOLOR="#FFCCCC""' 
	        local coloret_aov= `"BGCOLOR="#CC6699""' 
	          if round(`pval_kw ',0.00001) <=`psig' & `pval_kw'!=-1 {	
		         local coloret_kw= `"BGCOLOR="#CC66FF""' 
                }
             else { 
			      local coloret_kw=""
				}  
	     }
	   else { 
            local coloret_aov=""
               if round(`pval_kw ',0.00001) <=`psig' & `pval_kw'!=-1 {	
	                local coloret= `"BGCOLOR="#FFCCCC""' 
	                local coloret_kw= `"BGCOLOR="#CC66FF""' 
			    }
		       else	{
                    local coloret=""
                    local coloret_kw=""
	            }
	     }
	}	 
 else {
       local coloret=""
       local coloret_aov=""
       local coloret_kw=""
	   local coloret_skil=""
	   local coloret_uman=""
 }
 
 
 if "`color'"!=""  {
             if round(`pval_skil',0.00001) <=`psig' & `pval_skil'!=-1 {
              local coloret= `"BGCOLOR="#FFCCCC""' 
	        local coloret_skil= `"BGCOLOR="#CC6699""' 
		}
		}
		
		
  if "`color'"!=""  {
             if round(`pval_uman',0.00001) <=`psig' & `pval_uman'!=-1 {
              local coloret= `"BGCOLOR="#FFCCCC""' 
	        local coloret_uman= `"BGCOLOR="#CC6699""' 
		}
		}
	

  if "`color'"!=""  {
             if round(`pval_manexact',0.00001) <=`psig' & `pval_manexact'!=-1 {
              local coloret= `"BGCOLOR="#FFCCCC""' 
	        local coloret_manexact= `"BGCOLOR="#CC6699""' 
		}
		}
			
		
     local variable="N "
     if "`nomedia'" == "" {
              local variable: di "`variable'" "<p> Mean (sd) </p>    		
                } 

       if "`ci'"!="" {
			     local variable: di  "`variable'" " <p> [IC 95%]  </p> " 		 
				}
				
       if "`minmax'"!="" {
			     local variable: di  "`variable'" " <p> [Min; Max]  </p> " 		 
				}
               		
               				 
		if "`median'"!="" {
					local variable :di "`variable'"  " <p> Median [ p25;p75 ]  </p>"
                }   
 
        if "`ptiles'"!="" {
					local variable :di "`variable'"  " <p> [ p5;p95 ]  </p>"
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
 
   if `haytest_aov'==1 {
       htput <TH VALIGN=BOTTOM ROWSPAN=2 > <p> p-value </p> <p> `method_aov' </p> </TH>
   }
  
  
   if `haytest_kw'==1 {
       htput <TH VALIGN=BOTTOM ROWSPAN=2 > <p> p-value </p> <p> `method_kw' </p> </TH>
   }
   
   if `haytest_skil'==1 {
       htput <TH VALIGN=BOTTOM ROWSPAN=2 > <p> p-value </p> <p> `method_skil' </p> </TH>
   }
   
    if `haytest_uman'==1 {
       htput <TH VALIGN=BOTTOM ROWSPAN=2 > <p> p-value </p> <p> `method_uman' </p> </TH>
   }
      if `haytest_manexact'==1 {
       htput <TH VALIGN=BOTTOM ROWSPAN=2 > <p> p-value </p> <p> `method_manexact' </p> </TH>
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
	
	
/* COMENCEN FILES AMB LES DADES */


htput <TR> 	

if "`etifila'"!=""  {
   local var_row_lab="`etifila'"
 }
htput <TD  ALIGN= left  `coloret'>  `var_row_lab'  </TD>

   local j=0
    foreach c in `collevels' {
	  local j=`j' +1
	 
	   
	  
	   htput  <TD ALIGN=center `coloret'>  `cell_`j'' </TD>
	 }
	 if `total_fila' ==1  {
	 
	
	 
       htput <TD ALIGN=center `coloret' >   `cell_ncol'  </TD>
   }
 
   if `haytest_aov' ==1   {
           htput <TD ALIGN=center `coloret_aov' >   `pval_aov' 
		   local pvnorm: di %9.5f  `pnorm'
		   if "`swilk'"!="" {
		     if `pnorm' >=`psig'  {
               htsupput Better test: Shapiro_Wilk Normality test for  `var_row_lab' p=`pvnorm'
             }
		   }	 

		  htput  </TD>
   }
   
   if `haytest_kw' ==1   {
           htput <TD ALIGN=center  `coloret_kw' >   `pval_kw' 
           if "`swilk'"!="" {
		      if `pnorm' <0.05 {
                   local pvnorm: di %9.5f  `pnorm'
               htsupput Better test: Shapiro_Wilk Normality test for  `var_row_lab' p=`pvnorm'
             }
		   }	 

		 htput  </TD>
		 
   }
   
      
   if `haytest_skil' ==1   {
           htput <TD ALIGN=center  `coloret_skil' >   `pval_skil'  </TD>
		   
   }
      if `haytest_uman' ==1   {
           htput <TD ALIGN=center  `coloret_uman' >   `pval_uman'  </TD>
   }
   
         if `haytest_manexact' ==1   {
           htput <TD ALIGN=center  `coloret_manexact' >   `pval_manexact'  </TD>
   }
htput </TR> 		

		if "`pnorm'" =="" local pnorm=0
  if `pnorm' <0.05 {
     local pvalue= `pval_kw' 
     local method= "`method_kw'"
    }
	else {
  local pvalue= `pval_aov'
  local method= "`method_aov'"
  }  
  
if `haytest_skil' ==1   {
 local pvalue= `pval_skil'
  local method= "`method_aov'"
}	



if `haytest_uman'==1 {
 local pvalue= `pval_uman'
  local method= "`method_uman'"
}	
	
	if `haytest_manexact'==1 {
 local pvalue= `pval_manexact'
  local method= "`method_manexact'"
}	
/* TANCA LA TAULA */

  if "`close'" !="" {
    htput </TABLE>
	htsupwri
  }

 

/* ACTIVA LES DADES ORIGINALES */
	
use `usedata', replace


 ret scalar pvalue= `pvalue'
 ret  local   metodo="`method'"
end 





