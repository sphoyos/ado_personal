capture program drop ht_descrip_cat
program define ht_descrip_cat
*! version 1.0 Juny 2015 de SPH
*! this program makes descriptive  table and graph for relation among categorical vs categorical variables
*! syntax varlist ( min=2 max=2)                         |indicates catetorical and vargrup variable
*! [graftext(string)]                                     | adds a suffix to graph name
*! [cat_options(string)]                                 | options for categorical tables . By default  pcol coltotal rowtotal  chi exact color
*! [nocolor] [nografcat][nografcuant]                    | indicates if no color and no grafs are required
*! [lang(string)]                                        | indicates language of output cast=castellano cat= catala eng= english
*! [txtest_cat(string)]                                  | Text relatiu als tests categorics
*! psig(0.05)                                            | Nivell de significació  



	syntax varlist( min=1 max=2)	///
		[if] [in] [ fweight],		///
     					 [cat_options(string)] [nocolor] [nografcat] [lang(string)]  [legendsize(string)] [notable] ///
						 [txtest_cat(string)] [psig(real 0.05)] [graftxt(string)] [nografcum]  [labgrupsize(string)] [labtitlesize(string)] ///
						 [bcolor1(string)] [bcolor2(string)] [bcolor3(string)] [bcolor4(string)] [bcolor5(string)] [hbar]
						 
						 
								 
version 13.1						 
****** INICIALITZA ELS PARAMETRES PER DEFECTE *****************
local grafics_cat=0
local grafics_cuant=0
local graf_cum=0
if "`grafcat'"=="" local grafics_cat=1
if "`grafcum'"=="" local graf_cum=1

if "`color'"=="" local coloret="color"
if "`bcolor1'"=="" local bcolor1="196 255 118"
if "`bcolor2'"=="" local bcolor2="255 118 128"
if "`bcolor3'"=="" local bcolor3="205 205 255"
if "`bcolor4'"=="" local bcolor4="255 231 206"
if "`bcolor5'"=="" local bcolor5="205 231 255"

if "`labgrupsize'"=="" local labgrupsize="small"
if "`labtitlesize'"=="" local labtitlesize="medium"
if "`lang'"=="" local lang="cat"		


if "`hbar'"=="" local hbar="bar"		 

marksample touse
preserve
quietly keep if `touse'

	tokenize `varlist'
	local var = `"`1'"'
	
                   

if "`2'" !="" {
	local vargrup = `"`2'"'
	 local grafname = "bar_"+"`graftxt'"+"_"+"`vargrup'"+"_"+"`var'"
	 if "`cat_options'"=="" local cat_options="pcol coltotal rowtotal  chi exact"

}
 else { 	
tempvar total
gen `total'=1
label var `total' "_"
label define lab_total 1"_", modify
label val `total' lab_total
local vargrup= "`total'" 
 local grafname = "bar_"+"`graftxt'"+"_"+"total"+"_"+"`var'"
 if "`cat_options'"=="" local cat_options="pcol coltotal "
}
 if "`table'"=="" {
	htput <BR>
	
    ******************** EFECTUA LA TAULA. CANVIAR PERCENTAGES SEGONS ES VULLGUI 
   ht_tablacat_`lang'  `var' `vargrup' if `touse' , head close   `cat_options' `coloret' 
    
   htput <BR>

}
    if `grafics_cat'==1 {
        
          ******* EXTRAU EL NOM DE LA VARIABLE I DEL GRÀFIC *************************
 
            local nomvar: var label `var' 
            if   `"`nomvar'"' == "" local nomvar = "`var'"
            qui tabulate `var' `vargrup', chi 
            local pvalue: di %9.4f  r(p)
			local note1= "P valor Chi="
			if strpos("`cat_options'","skilmack")>0 {
			qui skilmack `var', id(`ident') repeated(`vargrup') 
			local pvalue: di %9.4f r(p_2)
		    local note1="p Skilmack test="
			}
			
			if strpos("`cat_options'","symmetry")>0 {
			qui symmetry `var' `vargrup' 
			local pvalue: di %9.4f r(p)
		    local note1="p Symmetry test="
			}
			
			if strpos("`cat_options'","symexact")>0 {
			qui symmetry `var' `vargrup' , exact
			local pvalue: di %9.4f r(p_exact)
		    local note1="p Symmetry exact test="
			}
		
            if "`vargrup'"=="`total'"  {
              local note1=""
			  local pvalue=""
			  local nomvargrup=""
			  }

			
         if `graf_cum'==0{  
		 
		    local grup="`vargrup'"
			tab `vargrup'
			if r(r)==1 local grup=""
            local ytitle_cat="N casos" 		 
		    local ytitle_cast="N casos"
            local ytitle_eng="N cases"			
            ************************ DIBUIXA EL GRAFIC DE FREQÜENCIES ACUMULAT *****************
            catplot `hbar'  `vargrup' `var'  if `touse',  title ("`nomvar'",size(`labtitlesize')) stack asyvars ///
            bar(1, bcolor(`bcolor2'))      bar(2, bcolor(`bcolor1')) bar(3, bcolor(`bcolor3'))  bar(4,bcolor(`bcolor4'))  bar(5, bcolor(`bcolor5'))  ///
		    oversubopts(label(labsize(`labgrupsize'))) ylabel( ,labsize(`labgrupsize') angle(h)) ///
        	ytitle("`ytitle_`lang''" ,size(`labgrupsize') )  legend(title( ""))  note(`note1'`pvalue') 		

         }
         else {
		 
		    local ytitle_cat="% acumulat" 		 
		    local ytitle_cast="% acumulado"
            local ytitle_eng="Cumulative %"	
		    local segons_cat="segons"
			local segons_cast="segun"
			local segons_eng="by" 
		 	local nomvargrup: var label `vargrup' 
            if   `"`nomvargrup'"' == "" local nomvargrup = "`vargrup'"
            ************************ DIBUIXA EL GRAFIC DE FREQÜENCIES ACUMULAT *****************
            catplot `hbar' `var'  `vargrup' if `touse', percent("`vargrup'") stack asyvars title ("`nomvar'",size(`labtitlesize'))   ///
            bar(1, bcolor(`bcolor1'))      bar(2, bcolor(`bcolor2')) bar(3, bcolor(`bcolor3'))  bar(4,bcolor(`bcolor4'))  bar(5, bcolor(`bcolor5'))  ///
		    oversubopts(label(labsize(`labgrupsize'))) ylabel( ,labsize(vsmall) angle(h)) ytitle("`ytitle_`lang''" ,size(`labgrupsize') )  ///
			legend(title( "") size(vsmall)) subtitle("`segons_`lang'' `nomvargrup'" )  note(`note1'`pvalue') 		
        }

*    bar(1, bcolor( 196 255 118))      bar(2, bcolor(255 118 128)) bar(3, bcolor(205 205 255))  bar(4,bcolor(255 231 206))  bar(5, bcolor(205 231 255))  ///
		 
    *********** GUARDA ELS GRAFICS ******************************************
    graph export $htm\png\gr_`grafname'.png, replace
    graph export $gph\wmf\gr_`grafname'.wmf, replace
    graph save $gph\gph\gr_`grafname'.gph, replace
    htput <IMG SRC=png\gr_`grafname'.png ALT="grafic evolució `namegraf' ">
    htput <BR>
    *************** TANCA EL BUCLE DELS GRAFICS
   
   }
  restore 
   end
   
	
	
	