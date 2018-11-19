capture program drop ana_descriptiu_std
program define ana_descriptiu_std
*! version 1.0 March 2015 de SPH
*! this program makes descriptive analyses from a database
*! syntax,  varsgrup(string)                                             |indicates the gropuing variables
*! varcat1(string) [varcat2(string)] [varcat3(string)] [varcat4(string)] |indicates de categorical variables
*! varcuant1(string)  [varcuant2(string)] [varcuant3(string)] [varcuant4(string)] | indicates de quantiative  variables
*! [graftxt(string)]                                     | adds a suffix to graf name
*! [cat_options(string)]                                 | options for categorical tables . By default coltotal rowtotal  chi exact  
*! [cuant_options(string)]                               | options for categorical tables . By default anova kwallis swilk total median minmax 
*! [nocolor] [nografcat][nografcuant]                    | indicates if no color and no grafs are required
*! [lang(string)]                                        | indicates language of output cast=castellano cat= catala eng= english
*! [txtest_cat(string)]                                  | Text relatiu als tests categorics
*! [txttest_cuant(string)]                               | Text relatiu als tests quantitatius  
*! psig(0.05)                                            | Nivell de significació  





syntax, varsgrup(string) [varcat1(string)] [varcat2(string)] [varcat3(string)] [varcat4(string)]  ///
                         [varcuant1(string)]  [varcuant2(string)] [varcuant3(string)] [varcuant4(string)] [graftxt(string)] ///
						 [cat_options(string)] [cuant_options(string)] [nocolor] [nografcat][nografcuant] [lang(string)] ///
						 [txtest_cat(string)] [txtest_cuant(string)] [ident(string)] [psig(real 0.05)] [grafcol] [missing] ///
						 [labgrupsize(string)]
						 
						 
version 9.1						 
****** INICIALITZA ELS PARAMETRES PER DEFECTE *****************
local grafics_cat=0
local grafics_cuant=0

if "`grafcat'"=="" local grafics_cat=1
if "`grafcuant'"=="" local grafics_cuant=1
if "`cat_options'"=="" local cat_options="pcol coltotal rowtotal  chi exact"
if "`cuant_options'"=="" local cuant_options="anova kwallis swilk total median minmax"
if "`color'"=="" local coloret="color"
if "`lang'"=="" local lang="cat"
if "`labgrupsize'"=="" local labgrupsize="small"

****** SELECCIONA ELS TEXTES SEGONS L'IDIOMA  IDIOMA  *****************
disp in yellow  "*****************************************************************************"
disp in yellow  "*****************************************************************************"
disp in yellow  "*****************************************************************************"
disp in yellow  "*****************************************************************************"
disp in yellow   "grafcat        `grafcat'"
disp in yellow   "grafcuant        `grafcuant'"
disp in yellow   "cat_options        `cat_options'"
disp in yellow   "cuant_options        `cuant_options'"
disp in yellow   "color        `color'"
disp in yellow   "lang        `lang'"
disp in yellow   "grafcol        `grafcol'"
disp in yellow  "*****************************************************************************"
disp in yellow  "*****************************************************************************"
disp in yellow  "*****************************************************************************"
disp in yellow  "*****************************************************************************"

local nvgrup=0
foreach vargrup of varlist `varsgrup' {
 local nvar=0 
   local nomvargrup: var label `vargrup' 
   if   `"`nomvargrup'"' == "" local nomvargrup = "`vargrup'"
   local nvgrup=`nvgrup'+1
   local nomg_`nvgrup'="`nomvargrup'" 
 
    if "`lang'"=="cast" { 

		htput <H2>Análisis descriptivo según `nomvargrup' </H2>
        htput <BR>
        
        htput <FONT color="#993366">
        htput   <p> En primer lugar se presenta un análisis descriptivo para cada una de las variables cualitativas y cuantitativas seleccionadas en función de 
        htput la variable de agrupación `nomvargrup'  </p><br>
		htput </FONT color>
    }   
   
    if "`lang'"=="cat" { 

		htput <H2>Anàlisis descriptiva segons `nomvargrup' </H2>
        htput <BR>
        
        htput <FONT color="#993366">
        htput  <p>En primer lloc es presenta una anàlisi descriptiva per a cadascuna de les variables de la base de dades en funció de les variables qualitatives
        htput i quantitatives seleccionades en funció de la variable de agrupació </p> <br>
		htput </FONT color>
	}	
      
	if "`lang'"=="eng" { 

		htput <H2>Descriptive analysis by `nomvargrup' </H2>
        htput <BR>
        
        htput <FONT color="#993366">
        htput  <p>Firstly a descriptive analysis for each qualitative and quantitative variable is presented
        htput by grouping variable </p><br>
		htput </FONT color>
    }   
   
   
 
 if trim("`varcat1'")!="" {    

******************** ACTIVA  EL CATALA O CASTELLA O ANGLES  ******************************

    if "`lang'"=="cast" { 
	
******** GENERAR TAULES DE 2 X 2 PER A VARIABLES CUALITATIVES ***************************************
        
        htput <H3> <font color="#993489">Variables Cualitativas </font color></H3>
        htput <BR>
        if "`txtest_cat'"==""  local txtest_cat=" la prueba de Chi cuadrado o el valor p exacto de Fisher cuando el valor de casos esperados en cada celdilla es menor de 5. " 
		
       htput <FONT color="#993366">
       htput  <p>Para las variables cualitativas se muestra una tabla de frecuencias con el número de casos y el % de en cada categoria. 
       htput Para comparar la asociación entre las variables de agrupación y cada variable de cualitativa se utiliza 
	   htput  `txtest_cat'
	   htput  Para identificar más fácilmente aquellas tablas con un valor p inferior al 5%
       htput  se han marcado el fondo de las celdillas del p-valor <`psig'
       htput  </p></FONT color>
       htput <BR>
       local ytit1= "% acumulado"
       local note1= "P valor Chi="
	
	   
    }   
	   
	if "`lang'"=="cat" { 

******** GENERAR TAULES DE 2 X 2 PER A VARIABLES CUALITATIVES ***************************************
        
        htput <H3> <font color="#993489">Variables Qualitatives </font color></H3>
        htput <BR>
      if "`txtest_cat'"==""  local txtest_cat=" la proba Khi quadrada o el p valor exacte de Fisher quan el valor del casos esperats en cada cel·la es menor de 5." 
		
       htput <FONT color="#993366"> <p>
       htput  Per a les variables qualitatives es mostra una taula de freqüències amb el nombre de casos i el% de cada categoria.
       htput Per comparar l'associació entre les variables d'agrupació i cada variable d'qualitativa s'utilitza
       htput `txtest_cat'
	   htput  Per identificar mès facilment aquelles taules amb un p valor inferior al 5% 
       htput  se ha  marcat el fons de les cel·les del p valor<`psig'
       htput  </p></FONT color>
       htput <BR>

	   local ytit1= "% acumulat"
	   local note1= "P valor Khi="
	 
	   
    }   
     
    if "`lang'"=="eng" { 


******** GENERAR TAULES DE 2 X 2 PER A VARIABLES CUALITATIVES ***************************************
   
        htput <H3> <font color="#993489">Qualitative Variables </font color></H3>
        htput <BR>
      if "`txtest_cat'"==""  local txtest_cat=" Chi square test for or Fisher's exact p value when the value of expected cases in each cell is less than 5." 
		
       htput <FONT color="#993366">
       htput  <p>For qualitative variables, a frequency table shows the number of cases and % in each category.
       htput To compare the association between the grouping variables and each qualitative variable used 
       htput `txtest_cat'
	   htput  To identify in easier way those tables wih a p value lower than 5%
       htput  Background of the celles has been colored for p values<`psig'
       htput  </p></FONT color>
       htput <BR>
	   local ytit1= "Cumulative %"
	   local note1= "P value Chi="


    }   
		   

    foreach var in   `varcat1' `varcat2'  `varcat3' `varcat4'    {

      
      local var=subinstr("`var'","i.","",1)
      local nomvar: var label `var' 
      if   `"`nomvar'"' == "" local nomvar = "`var'"
      local nvar=`nvar'+1
      local nom_`nvar'="`nomvar'"
 
      disp in red  "`nvar' ; `nom_`nvar''"
 	  if "`var'"!="`vargrup'" {
       htput <H4> <b>`nomvar' </b> </H4>

       htput <BR>
     do	$labels\coments  "`vargrup'" "`var'"
	   htput <BR>
      ******************** EFECTUA LA TAULA. CANVIAR PERCENTAGES SEGONS ES VULLGUI    ************
        cap    tab `var' `vargrup'
        local robs=r(r) 
        local n_`nvgrup'_`nvar'=r(N)
       ht_tablacat_`lang'  `var' `vargrup' , head close   `cat_options' `coloret' id("`ident'") `missing'
       local p_`nvgrup'_`nvar' =r(pvalue)
	   if `p_`nvgrup'_`nvar''==-1  local p_`nvgrup'_`nvar' =.
       local m_`nvgrup'_`nvar' =r(metodo) 
       htput <BR>

         if `grafics_cat'==1 {

          ******* EXTRAU EL NOM DE LA VARIABLE I DEL GRÀFIC *************************
 
            local nomvar: var label `var' 
            if   `"`nomvar'"' == "" local nomvar = "`var'"
            local grafname = "bar_"+"`graftxt'"+"_"+"`vargrup'"+"_"+"`var'"

            ********* GENERA UNA TAULA DE 2X2 I EXTRAU EL VALOR CHI **********************
            qui tabulate `var' `vargrup', chi 
            local pvalue: di %9.5f   r(p)
			if strpos("`cat_options'","skilmack")>0 {
			qui skilmack `var', id(`ident') repeated(`vargrup') 
			local pvalue: di %9.5f  r(p_2)
		    local note1="p Skilmack test="
			}
			
			if strpos("`cat_options'","symmetry")>0 {
			qui symmetry `var' `vargrup' 
			local pvalue: di %9.5f  r(p)
		    local note1="p Symmetry test="
			}
			
			if strpos("`cat_options'","symexact")>0 {
			qui symmetry `var' `vargrup' , exact
			local pvalue: di %9.5f  r(p_exact)
		    local note1="p Symmetry exact test="
			}
		
            if "`vargrup'"=="total"  {
              local note1=""
			  local pvalue=""
			  local nomvargrup=""
			  }


		
	if "`grafcol'"=="" { 		
            ************************ DIBUIXA EL GRAFIC DE FREQÜENCIES ACUMULAT *****************
            catplot bar `var'  `vargrup', percent("`vargrup'") stack asyvars title ("`nomvar'") subtitle( "`nomvargrup'")  ///
            bar(1, bcolor( 196 255 118))      bar(2, bcolor(255 118 128)) bar(3, bcolor(205 205 255))  bar(4,bcolor(255 231 206))  bar(5, bcolor(205 231 255))  ///
		    oversubopts(label(labsize(`labgrupsize'))) ylabel( ,labsize(small) angle(h))ytitle("`ytit1'" ,size(small) ) note(`note1'`pvalue')  legend(title( "`nomvar'",size(small)) cols(3) bmargin(zero) size(small)) 
          }
		  else {
		    ************************ DIBUIXA EL GRAFIC DE FREQÜENCIES ACUMULAT *****************
            catplot bar `vargrup' `var', percent("`var'") stack asyvars title ("`nomvar'") subtitle( "`nomvar'")  ///
            bar(1, bcolor( 196 255 118))      bar(2, bcolor(255 118 128)) bar(3, bcolor(205 205 255))  bar(4,bcolor(255 231 206))  bar(5, bcolor(205 231 255))  ///
		    oversubopts(label(labsize(`labgrupsize'))) ylabel( ,labsize(small) angle(h) )ytitle("`ytit1'" ,size(small) ) note(`note1'`pvalue')  legend(title( "`nomvargrup'",size(small)) cols(3) bmargin(zero) size(small)) 
          }

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
 }
	*************** TANCA EL BUCLE DE LES VARIABLES CATEGORIQUES***************
   htput <BR>
  if trim("`varcuant1'")!="" {   
******** GENERAR TAULES DESCRIPTIVES  PER A VARIABLES QUANTITATIVES ***************************************
   qui tab `vargrup'
   local ncatgrup=r(r)

    if "`lang'"=="cast" { 

    ******************** ACTIVA  EL CATALA O CASTELLA *****************************************

    htput <H3> <font color="#993489">Variables cuantitativas </font color></H3>
    htput <BR>
    htput <FONT color="#993366">
    htput Para las variables cuantitativas se muestran las medidas descriptivas habituales, media  y desviación típica, mínimo y màximo, mediana
    htput  y percentiles, según la variable "`nomvargrup'".
	if `ncatgrup'==2 & "`txttest_cuant'"=="" {
        htput<p>   Se presentan los p valores de dos pruebas de contraste de igualdad de las variables en los grupos:
	    htput La prueba t y la prueba no paramètrica de Mann-Whitney.   
	    htput Se elegirà el valor p de una u otra según la simetria de la distribución o la prueba de normalidad  y si el tamaño es suficiente.</p>
    } 
	else if `ncatgrup'>2 & "`txttest_cuant'"=="" {
	    htput  <p> Se presentan los p valores de dos pruebas de contraste de igualdad de las variables en los grupos:
	    htput La prueba de ANOVA y la prueba no paramètrica de Kruskal-Wallis.   
	    htput Se elegirà el valor p de una u otra según la simetria de la distribución o la prueba de normalidad  y si el tamaño es suficiente. </p>
    } 
	else  {
	    htput `txtest_cuant'
    }	
    htput <p> En el diagrama de cajas que se presenta después de cada tabla . La linea de la caja representa la mediana 
    htput y los límites el percentil 25 y 75. Si la caja es simètrica podremos utilizar las medias como medida resumen. 
    htput </p></FONT color>
    htput <BR>
    local note2="P valor KW="
    }




    if "`lang'"=="cat" { 

    ******************** ACTIVA  EL CATALA O CASTELLA *****************************************
    htput <H3> <font color = "#993489"> Variables quantitatives </font color> </H3>
    htput <BR>
    htput <FONT color = "#993366">
    htput Per a les variables quantitatives es mostren les mesures descriptives habituals, mitjana i desviació típica, mínim i màxim, mediana
    htput i percentils, segons la variable "`nomvargrup'".
    if `ncatgrup '== 2 & "`txttest_cuant'" == "" {
        htput<p> Es presenten els p valors de dues proves de contrast d'igualtat de les variables en els grups:
        htput La prova t i la prova no paramètrica de Mann-Whitney.
        htput Es triarà el valor p d'una o altra segons la simetria de la distribució o la prova de normalitat i si la mida és suficient.</p>
    }                                            
    else if `ncatgrup '> 2 & "`txttest_cuant'" == "" {
        htput<p> Es presenten els p valors de dues proves de contrast d'igualtat de les variables en els grups:
        htput La prova  ANOVA i la prova no paramètrica de Kruskal-Wallis.
        htput Es triarà el valor p d'una o altra segons la simetria de la distribució o la prova de normalitat i si la mida és suficient.</p>
    }
    else {
        htput `txtest_cuant'
    }
        htput<p> En el diagrama de caixes que es presenta després de cada taula. La línia de la caixa representa la mitjana
        htput i els límits el percentil 25 i 75. Si la caixa és simètrica podrem utilitzar les mitjanes com mesura resum.
        htput </p></FONT color>
        htput <BR>
		   local note2="P valor KW="
    }


   if "`lang'"=="eng" { 

    ******************** ACTIVA  EL CATALA O CASTELLA *****************************************
     htput <H3> <font color = "#993489"> Quantitative Variables </font color> </H3>
    htput <BR>
    htput <FONT color = "#993366">
    htput <p>For quantitative variables usual descriptive variables are calculated, mean, standard deviation, minimum and maximum and median 
    htput and percentiles according to grouping variable.</p>
	
    if `ncatgrup'== 2 & "`txttest_cuant'" == "" {
        htput <p>P values of two hypothesis test for equality of distribution variables in the groups are presented:
        htput The t test and the nonparametric Mann-Whitney test.
        htput The p value will be chosen according to the symmetry of the distribution or normality test and if the  sample size is large enough</p>
    }
    else if `ncatgrup'> 2 & "`txttest_cuant'" == "" {
        htput <p>P values of two hypothesis test for equality of distribution variables in the groups are presented:
        htput The ANOVA test and the nonparametric Kruskal-Wallis test.
        htput The p value will be chosen according to the symmetry of the distribution or normality test and if the  sample size is large enough.</p>
    }
    else {
        htput `txtest_cuant'
    }
        htput <p>The box plot is presented after each table. The line of the box represents the median
        htput and limits the percentile 25 and 75. If the box is symmetric we can use the mean as a summary measure
        htput </p></FONT color>
        htput <BR>
		   local note2="P vaue KW="
    }


***** FA UN BUCLE  PER A CADASCUNA DE LES VARIABLES QUANTITATIVES ***************

   foreach var of varlist   `varcuant1'  `varcuant2' `varcuant3'  `varcuant4'    {

    local nomvar: var label `var' 
    if   `"`nomvar'"' == "" local nomvar = "`var'"
    local nvar=`nvar'+1
    local nom_`nvar'="`nomvar'"
    disp in red  "`nvar' ; `nom_`nvar''"
    htput <H4> <b>`nomvar' </b> </H4>
        htput <BR>
     do	$labels\coments  "`vargrup'" "`var'"
	   htput <BR>
    htput <BR>
    format %9.6f `var'

    ********** AFEGEIX LES FILES A LA TAULA EN HTML AMB LES FREQUENCIES PER COLUMNA SELECCIONA LA TAULA A FER ****************
   cap    tab `var' `vargrup'
   local robs=r(r) 
   local n_`nvgrup'_`nvar'=r(N)
     if  `robs'> 1 {
       ht_tablacont_`lang' `var' `vargrup' , head close `cuant_options' `coloret' psig(`psig')
	   
       local p_`nvgrup'_`nvar' =r(pvalue)
	   if `p_`nvgrup'_`nvar''==-1  local p_`nvgrup'_`nvar' =.
       local m_`nvgrup'_`nvar' =r(metodo)
       **************************ACTIVAR SI ES VOL POSTHOC TEST 
          if `ncatgrup'> 2  & strpos("`cuant_options'","anova")>0 { 
             anova `var' `vargrup' 
               if F(e(df_m), e(df_r), e(F_1))>.95 {
                htput <BR>
                ht_scheffe `vargrup'
               }
           }
		}
		else {
		 htput <b> No hay màs de 2 valores diferentes </b>
		 local m_`nvgrup'_`nvar' ="No +2 valores diferentes"
		 local p_`nvgrup'_`nvar' =""
		}
 if "`vargrup'"!="total"  {
 
  	cap    tab `vargrup'  if `var'!=.	
	 if `grafics_cuant'==1 & r(r)>1 {
		   
        ******* EXTRAU EL NOM DE LA VARIABLE I DEL GRÀFIC *************************
 
        local nomvar: var label `var' 
        if   `"`nomvar'"' == "" local nomvar = "`var'"

        local nomvargrup: var label `vargrup' 
        if   `"`nomvargrup'"' == "" local nomvargrup = "`vargrup'"
 
        local grafname = "box_"+"`graftxt'"+"_"+"`vargrup'"+"_"+"`var'"
		

 
        ********* GENERA UNA TEST KRUSKAL-WALLIS I EXTRAU EL VALOR P **********************
        qui  kwallis `var' , by(  `vargrup') 
        local pvalue: di  %9.5f chiprob(r(df),r(chi2)) 

             			 
        ************************* DIBUIXA ELS DIAGRAMES DE CAPSES *****************
        local formvar: format `var' 
        graph box `var'  , over(`vargrup',label(labsize(`labgrupsize'))) title ("`nomvar'" ) subtitle ( "`nomvargrup'")  caption (" `nomvar'") ytitle(" ") note(`note2'`pvalue') ylabel(, labsize(small) format(`formvar') angle(h)) $boxcolor 
    
	*********** GUARDA ELS GRAFICS ******************************************
     graph export $htm\png\gr_`grafname'.png, replace
     graph export $gph\wmf\gr_`grafname'.wmf, replace
     graph save $gph\gph\gr_`grafname'.gph, replace
     htput <IMG SRC=png\gr_`grafname'.png ALT="grafic evolució `namegraf' ">
     htput <BR>
    ***************** TANCA EL BUCLE DELS GRAFICS    

	}
    }
	else {
	 if `grafics_cuant'==1 {
	    local nomvar: var label `var' 
        if   `"`nomvar'"' == "" local nomvar = "`var'"
    	 local grafname = "box_"+"`graftxt'"+"_"+"`vargrup'"+"_"+"`var'"
		
	     local formvar: format `var' 
        graph box `var',   title ("`nomvar'" )    ytitle(" ")  ylabel(, labsize(small) format(`formvar') angle(h)) $boxcolor 

	}
     
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



**************************************************************************************************
**********************  RESUM DE LES ASSOCIACIONS                          ***********************
**************************************************************************************************
if "`lang'"=="cat" { 
     htput <BR>
     htput <H3> Resum de les associacions </H3>
     htput <BR>
     htput <FONT color="#993366"  >  
     htput <p>A la següent taula es presenta un resum de les associacions entre les variables considerades
     htput i les variables que generen grup. <br>
     htput Es mostra el nombre de casos vàlids utilitzats, la prova estadística escollida i el p valor </p>
     htput </FONT color  >
     htput <BR>	 
}

if "`lang'"=="cast" { 
     htput <BR>
     htput <H3> Resumen de las asociaciones </H3>
     htput <BR>
     htput <FONT color="#993366"  >  
     htput  <p>En la siguiente tabla se presenta un resumen de las asociaciones entre las variables consideradas
     htput y las variables que generan grupo. <br>
     htput Se muestra el número de casos con datos válidos usados,  la prueba estadística elegida y el p valor </p>
     htput </FONT color  >
     htput <BR>	 
}

if "`lang'"=="eng" { 
     htput <BR>
     htput <H3> Summary of Associations </H3>
     htput <BR>
     htput <FONT color="#993366"  >  
     htput  <p>The following table summarizes the associations between  considered variables 
     htput and groupong variables.  <br>
     htput Number of nonmissing cases used, selected statistical test and p values are shown </p>
     htput </FONT color  >
     htput <BR>	 
}


**************************************************************************************************
**********************  TAULA EN COLUMNA GRUPS I EN FILA VARCAT, VARCUANT ***********************
**************************************************************************************************
     do	$labels\coments  "resumen"



htput <TABLE BORDER=1 CELLSPACING =0 CELlPADDING=2>
   htput <TR>

 htput <TH VALIGN=CENTER ALIGN=CENTER  > Variables</TH>



local nvgrup=0
foreach vargrup of varlist `varsgrup' {
local  nvgrup=`nvgrup'+1
htput <TH VALIGN=CENTER ALIGN=CENTER  >  `nomg_`nvgrup'' <p>N<p> Test <p> p valor </TH>
}
htput </TR>




local coloret=""
local coloret= `"BGCOLOR="#CC66FF""' 

local nvar=0
foreach var of varlist   `varcat1'  `varcat2' `varcat3'  `varcat4'  `varcuant1'  `varcuant2' `varcuant3'  `varcuant4'    {
htput <TR>
local nvar=`nvar'+1
disp in yellow "`nvar' ; `nom_`nvar''"
htput <TH VALIGN=CENTER ALIGN=LEFT  > `nom_`nvar''</TH>

local nvgrup=0
foreach vargrup of varlist `varsgrup' {
local  nvgrup=`nvgrup'+1

if round(`p_`nvgrup'_`nvar'',0.00001) <=`psig' & "`p_`nvgrup'_`nvar''"!="" &  {	
	                local coloret= `"BGCOLOR="#FFCCCC""' 
	               		    }
		       else	{
                    local coloret=""
                   }

local pval:disp %9.5f `p_`nvgrup'_`nvar''

htput <TD VALIGN=CENTER ALIGN=CENTER `coloret' > `n_`nvgrup'_`nvar'' <br>`m_`nvgrup'_`nvar'' <br> `pval'</TD>
}
htput </TR>

}

htput </TABLE>


/*

**************************************************************************************************
**********************  TAULA EN COLUMNA VARCAT, VARCUANT I EN FILA VARGRUP   ********************
**************************************************************************************************

htput <TABLE BORDER=1 CELLSPACING =0 CELlPADDING=2>
   htput <TR>

 htput <TH VALIGN=CENTER ALIGN=CENTER  > Variables </TH>



local nvar=0
foreach var of varlist   `varcat1'  `varcat2' `varcat3'  `varcat4'  `varcuant1'  `varcuant2' `varcuant3'  `varcuant4'  {
local  nvar=`nvar'+1
htput <TH VALIGN=CENTER ALIGN=CENTER  >  `nom_`nvar'' </TH>
}
htput </TR>




local coloret=""
local coloret= `"BGCOLOR="#CC66FF""' 

local nvgrup=0
foreach vargrup of varlist `varsgrup1' `varsgrup2'  `varsgrup3' {
htput <TR>
local  nvgrup=`nvgrup'+1

disp in yellow "`nvar' ; `nomg_`nvgrup''"
htput <TH VALIGN=CENTER ALIGN=LEFT  > `nomg_`nvgrup''</TH>


local nvar=0
foreach var of varlist   `varcat1'  `varcat2' `varcat3'  `varcat4'  `varcuant1'  `varcuant2' `varcuant3'  `varcuant4'    {
local nvar=`nvar'+1



if round(`p_`nvgrup'_`nvar'',0.00001) <=`psig' {	
	                local coloret= `"BGCOLOR="#FFCCCC""' 
	               		    }
		       else	{
                    local coloret=""
                   }

local pval:disp %9.5f `p_`nvgrup'_`nvar''
htput <TD VALIGN=CENTER ALIGN=CENTER `coloret' > `m_`nvgrup'_`nvar'' <br> `pval'</TD>
}
htput </TR>

}

htput </TABLE>

*/



htput <br>

end 


