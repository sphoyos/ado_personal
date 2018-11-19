capture program drop ana_descriptiu_eng
program define ana_descriptiu_eng
*! version 1.0 July 20011 de SPH

syntax, varsgrup(string) varcat1(string) [varcat2(string)] [varcat3(string)] [varcat4(string)]  ///
                         varcuant1(string)  [varcuant2(string)] [varcuant3(string)] [varcuant4(string)] [grafver(string)]
						 
version 13.1						 
****** INDICA SI ES MOSTREN ELS GRAFICS (POR DEFECTE SI) *****************
local grafics_cat=1
local grafics_cuant=1

foreach vargrup of varlist `varsgrup'{
 local nomvargrup: var label `vargrup' 
 if   `"`nomvargrup'"' == "" local nomvargrup = "`vargrup'"
 

htput <BR>

******** GENERAR TAULES DE 2 X 2 PER A VARIABLES CUALITATIVES ***************************************
if "`varcat1'"!=" " {

******************** ACTIVA  EL CATALA O CASTELLA ***************************************
htput  </font color>
htput <BR>
htput <FONT color="#993366"> 
htput  For qualitative variables a contingency table showing frequency and percentage of each category by treatment branch A or B is presented.
htput One sided exact Fisher p value is calculated . To an easier identification of p values under 5%  cells are  colour marked. A bar plot graph is also showed.
htput  </font color>


***** Analisi per variables cualitativas ***************


  
     foreach var in   `varcat1' `varcat2'  `varcat3' `varcat4'    {
     local var=subinstr("`var'","i.","",1)
     local nomvar: var label `var' 
  if   `"`nomvar'"' == "" local nomvar = "`var'"
 htput <H4> <b>`nomvar' </b> </H4>

htput <BR>
******************** EFECTUA LA TAULA. CANVIAR PERCENTAGES SEGONS ES VULLGUI 

    ht_tablacat_eng `var' `vargrup' , head close pcol coltotal rowtotal  exact1  color

htput <BR>

    if `grafics_cat'==1 {

          ******* EXTRAU EL NOM DE LA VARIABLE I DEL GRÀFIC *************************
 
            local nomvar: var label `var' 
            if   `"`nomvar'"' == "" local nomvar = "`var'"
            local grafname = "bar_"+"`vargrup'"+"_"+"`var'"+"`grafver'" 

            ********* GENERA UNA TAULA DE 2X2 I EXTRAU EL VALOR CHI **********************
			
			quietly cap tab `var' `vargrup' , exact `missing'
			local pvalue: di %5.3f r(p1_exact)
			if `pvalue'>0.05 | `pvalue'==. {
			  htput <FONT color="#993366"> 
			  htput  No relation between `nomvar' & `nomvargrup' has been found <br>
			  htput  </font color>
			  }
			  else {
			  htput <FONT color="#993366"> 
			  htput `nomvar' is statistically related to `nomvargrup' (pvalue<`pvalue') <br>
			  htput  </font color>
			  }
			  
			
            ************************ DIBUIXA EL GRAFIC DE FREQÜENCIES ACUMULAT *****************
            catplot bar `var'  `vargrup', percent("`vargrup'") stack asyvars title (" Diagrama de barras") subtitle( "`nomvargrup'")  ///
            bar(1, bcolor( 196 255 118))      bar(2, bcolor(255 118 128)) bar(3, bcolor(205 205 255))  bar(4,bcolor(255 231 206))  bar(5, bcolor(205 231 255))  ///
		    oversubopts(label(labsize(vsmall))) ylabel( ,labsize(vsmall))ytitle("% acumulado" ,size(small) ) note(One sided Fisher exact=`pvalue')  legend(title( "`nomvar'")) 
          
    *********** GUARDA ELS GRAFICS ******************************************
    graph export $htm\png\gr_`grafname'.png, replace
    graph export $gph\wmf\gr_`grafname'.wmf, replace
    graph save $gph\gph\gr_`grafname'.gph, replace
    htput <IMG SRC=png\gr_`grafname'.png ALT="grafic evolució `namegraf' ">
    htput <BR>
    *************** TANCA EL BUCLE DELS GRAFICS
   
   }
 }
 
 }
*************** TANCA EL BUCLE DE LES VARIABLES CATEGORIQUES
htput <BR>

******** GENERAR TAULES DESCRIPTIVES  PER A VARIABLES QUANTITATIVES ***************************************

if "`varcuant1'"!=" " {

htput <FONT color="#993366"> 

htput  For quantitative variables descriptive summary measures has been calculated (mean and standard deviation, minimum and maximum  and median and interquartile interval
htput  Treatment groups have been compared calculating one side Sum Rank Test p value. 
htput  A box-plot graph has been also plotted . Differences tables are marked with colour. 
htput <BR> </FONT color>
htput <BR>


***** FA UN BUCLE  PER A CADASCUNA DE LES VARIABLES QUANTITATIVES ***************

foreach var of varlist   `varcuant1'  `varcuant2' `varcuant3'  `varcuant4'    {
 local nomvar: var label `var' 
 if   `"`nomvar'"' == "" local nomvar = "`var'"
 htput <H4> <b>`nomvar' </b> </H4>

htput <BR>
format %5.2f `var'

********** AFEGEIX LES FILES A LA TAULA EN HTML AMB LES FREQUENCIES PER COLUMNA SELECCIONA LA TAULA A FER ****************

ht_tablacont_eng `var' `vargrup' , head close anova kwallis   total median minmax color

**************************ACTIVAR SI ES VOL POSTHOC TEST 

           *anova `var' `vargrup' 
           *if F(e(df_m), e(df_r), e(F_1))>.95 {
           *htput <BR>
           *ht_scheffe `vargrup'
           *}

	cap    tab `vargrup'  if `var'!=.
    if `grafics_cuant'==1 & r(r)>1 {
		   
        ******* EXTRAU EL NOM DE LA VARIABLE I DEL GRÀFIC *************************
 
        local nomvar: var label `var' 
        if   `"`nomvar'"' == "" local nomvar = "`var'"
        local nomvargraf=subinstr("`nomvar'" ,"&mu;" ,"{&mu}",.)
        local nomvargraf=subinstr("`nomvargraf'" ,"&alpha;" ,"{&alpha}",.)
		local nomvargraf=subinstr("`nomvargraf'" ,"&alpha;" ,"{&alpha}",.)
		local nomvargraf=subinstr("`nomvargraf'" ,"<SUP>" ,"{superscript:",.)
		local nomvargraf=subinstr("`nomvargraf'" ,"</SUP>" ,"}",.)

		
 
        local nomvargrup: var label `vargrup' 
        if   `"`nomvargrup'"' == "" local nomvargrup = "`vargrup'"
 
        local grafname = "box_"+"`vargrup'"+"_"+"`var'"+"`grafver'" 

        ********* GENERA UNA TEST KRUSKAL-WALLIS I EXTRAU EL VALOR P **********************
		
		  capture noisily ranksumex `var', by(`vargrup')
          local pvalue: di %5.3f (r(nx1)/r(den))
	       if `pvalue'>0.05 | `pvalue'==. {
			  htput <FONT color="#993366"> 
			  htput  No relation between `nomvar' & `nomvargrup' has been found <br>
			  htput  </font color>
			}
			else {
			  htput <FONT color="#993366"> 
			  htput `nomvar' is statistically related to `nomvargrup' (pvalue<`pvalue') <br>
			  htput  </font color>
			}
        ************************* DIBUIXA ELS DIAGRAMES DE CAPSES *****************
        local formvar: format `var' 
        graph box `var'  , over(`vargrup') title (" `nomvargraf' Box-Plot" ) subtitle ( " By `nomvargrup'")  caption (" `nomvargraf'") ytitle(" ") note(Pvalue One sided Sum Rank test=`pvalue') ylabel(, format(`formvar')) $boxcolor 

   
     
    *********** GUARDA ELS GRAFICS ******************************************
     graph export $htm\png\gr_`grafname'.png, replace
     graph export $gph\wmf\gr_`grafname'.wmf, replace
     graph save $gph\gph\gr_`grafname'.gph, replace
     htput <IMG SRC=png\gr_`grafname'.png ALT="grafic evolució `namegraf' ">
     htput <BR>
    ***************** TANCA EL BUCLE DELS GRAFICS

    }
  }
}
  }

end 


