capture program drop ht_descrip_cuant
program define ht_descrip_cuant
*! version 1.0 Juny 2015 de SPH
*! this program makes descriptive  table and graph for relation among quantitative vs categorical variables
*! syntax varlist ( min=2 max=2)                         |indicates catetorical and vargrup variable
*! [graftext(string)]                                     | adds a suffix to graph name
*! [cat_options(string)]                                 | options for quantitative  tables . By default  pcol coltotal rowtotal  chi exact color
*! [nocolor] [nografcat][nografcuant]                    | indicates if no color and no grafs are required
*! [lang(string)]                                        | indicates language of output cast=castellano cat= catala eng= english
*! [txtest_cat(string)]                                  | Text relatiu als tests categorics
*! psig(0.05)                                            | Nivell de significació  



	syntax varlist( min=1 max=2)	///
		[if] [in] [ fweight],		///
     					 [cuant_options(string)] [nocolor] [nografcuant] [lang(string)] [notable] ///
						 [txtest_cuant(string)] [psig(real 0.05)] [graftxt(string)]  [labgrupsize(string)]  [labtitlesize(string)]
						 
						 
								 
version 13.1						 
****** INICIALITZA ELS PARAMETRES PER DEFECTE *****************
local grafics_cat=0
local grafics_cuant=0

if "`grafcuant'"=="" local grafics_cuant=1
if "`color'"=="" local coloret="color"
if "`lang'"=="" local lang="cat"				 
if "`labgrupsize'"=="" local labgrupsize="small"
if "`labtitlesize'"=="" local labtitlesize="medium"           


marksample touse
preserve
quietly keep if `touse'
	tokenize `varlist'
	local var = `"`1'"'
	

if "`2'" !="" {
	local vargrup = `"`2'"'
	 local grafname = "box_"+"`graftxt'"+"_"+"`vargrup'"+"_"+"`var'"
	 if "`cuant_options'"=="" local cuant_options="anova kwallis swilk total median minmax"
}
 else { 	
tempvar total
gen `total'=1
label var `total' "_"
label define lab_total 1"_", modify
label val `total' lab_total
local vargrup= " `total'  " 
 local grafname = "box_"+"`graftxt'"+"_"+"total"+"_"+"`var'"
if "`cuant_options'"=="" local cuant_options="median minmax"
}	

htput <BR>

  qui tab `vargrup'
   local ncatgrup=r(r)

******************** EFECTUA LA TAULA. CANVIAR PERCENTAGES SEGONS ES VULLGUI 
  local nomvar: var label `var' 
    if   `"`nomvar'"' == "" local nomvar = "`var'"
	
	local varlabgraf="`nomvar'"
 local varlabgraf=subinstr("`varlabgraf'" ,"&mu;" ,"{&mu}",.)
 local varlabgraf=subinstr("`varlabgraf'" ,"&alpha;" ,"{&alpha}",.)
 local varlabgraf=subinstr("`varlabgraf'" , "<SUP>","{superscript:" ,.)
 local varlabgraf=subinstr("`varlabgraf'" , "</SUP>","}" ,.)

 *  htput  <b>`nomvar' </b> 

    htput   <b>`txtest_cuant' </b>  <p> <br>
    format %5.2f `var'
 if "`table'"=="" {
    ********** AFEGEIX LES FILES A LA TAULA EN HTML AMB LES FREQUENCIES PER COLUMNA SELECCIONA LA TAULA A FER ****************
	   cap    tab `var'
	   local robs=r(r) 
		 if  `robs'> 1 {
		   ht_tablacont_`lang' `var' `vargrup' if `touse', head close `cuant_options' `coloret'
		 
		   **************************ACTIVAR SI ES VOL POSTHOC TEST 
			  if `ncatgrup'> 2  & strpos("`cuant_options'","anova")>0 { 
				 anova `var' `vargrup'  if `touse'
				   if F(e(df_m), e(df_r), e(F_1))>.95 {
					htput <BR>
					ht_scheffe `vargrup' if `touse'
				   }
			   }
			}
			else {
			 htput <b> No hay màs de 2 valores diferentes </b>
			}
	}		
		 cap    tab `vargrup'  if `var'!=.	
	if `grafics_cuant'==1  { 
	    if r(r)>1 {
		   
        ******* EXTRAU EL NOM DE LA VARIABLE I DEL GRÀFIC *************************
 
        local nomvar: var label `var' 
        if   `"`nomvar'"' == "" local nomvar = "`var'"

        local nomvargrup: var label `vargrup' 
        if   `"`nomvargrup'"' == "" local nomvargrup = "`vargrup'"
 
        local grafname = "box_"+"`graftxt'"+"_"+"`vargrup'"+"_"+"`var'"

        ********* GENERA UNA TEST KRUSKAL-WALLIS I EXTRAU EL VALOR P **********************
		local pvalue=""
        qui  kwallis `var' , by(  `vargrup') 
        local pvalue: di  %8.4f chiprob(r(df),r(chi2)) 

        ************************* DIBUIXA ELS DIAGRAMES DE CAPSES *****************
        local formvar: format `var' 
        graph box `var' if `touse' , over(`vargrup',label(labsize(`labgrupsize'))) title ("`varlabgraf'",size(`labtitlesize') ) subtitle(`txtest_cuant',size(`labtitlesize')) caption (" `nomvargrup'") ytitle(" ") note(P value KW=`pvalue') ylabel(, format(`formvar') angle(h)) $boxcolor 
      }
      else  {
   ******* EXTRAU EL NOM DE LA VARIABLE I DEL GRÀFIC *************************
 
        local nomvar: var label `var' 
        if   `"`nomvar'"' == "" local nomvar = "`var'"

        local nomvargrup: var label `vargrup' 
        if   `"`nomvargrup'"' == "" local nomvargrup = "`vargrup'"
    ************************* DIBUIXA ELS DIAGRAMES DE CAPSES *****************
        local formvar: format `var' 
        graph box `var' if `touse'  ,  title ("`nomvar'" ,size(`labtitlesize'))  ytitle(" ")  ylabel(, format(`formvar') angle(h)) $boxcolor  subtitle(`txtest_cuant',size(`labtitlesize'))
      }

     
    *********** GUARDA ELS GRAFICS ******************************************
     graph export $htm\png\gr_`grafname'.png, replace
     graph export $gph\wmf\gr_`grafname'.wmf, replace
     graph save $gph\gph\gr_`grafname'.gph, replace
     htput <IMG SRC=png\gr_`grafname'.png ALT="grafic evolució `namegraf' ">
     htput <BR>
    ***************** TANCA EL BUCLE DELS GRAFICS

 }     
   restore
 
   end
   
	
	
	