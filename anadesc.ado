*! Version 26 Nov 2017  by SPH
*! Version 26 Nov 2017  by SPH
*! anadesc.ado genera taules i grafics descriptives 
*! Cal indicar cop opcions quines son les variables d'agrupació i les variables a analitzar en vardesc 1-8
*! Les variables cualitatives s'indiquen posant un i. al davant
*! syntax  ,varsgrup(string)  vardesc1(string)  [vardesc2(string)] [vardesc3(string)] [vardesc4(string)] [vardesc5(string)] ///
*!           [vardesc6(string)] [ vardesc7(string)] [vardesc8(string)] 
 
 
cap program drop anadesc 

program define  anadesc
 syntax  ,varsgrup(string)  vardesc1(string)  [vardesc2(string)] [vardesc3(string)] [vardesc4(string)] [vardesc5(string)] ///
           [vardesc6(string)] [ vardesc7(string)] [vardesc8(string)] [noDot] [Row] [Gname(string)]  [Lang(string) ] [rowtotal] ///
		    [noGraf] [noTable] [xlab(string)] [minmax] [ngrafsrow(real 4)] [CI]
 local nrows wordcount("`vardesc1'") +wordcount("`vardesc2'") +wordcount("`vardesc3'") +wordcount("`vardesc4'") +wordcount("`vardesc5'")+wordcount("`vardesc6'")+wordcount("`vardesc7'")+wordcount("`vardesc8'") 
 
 
if "`lang'"=="" local lang="cat"
   if "`lang'"=="cat"  {
	local tit1="Anàlisi para tota la mostra"
	local tit2="Anàlisi per"
	local tit3="Anàlisi descriptiu"
   }	
   if "`lang'"=="cast"  {
	local tit1="Análisis para toda la muestra"
	local tit2="Análisis para"
	local tit3="Análisis descriptivo"
   }	
      if "`lang'"=="eng"  {
	local tit1="Analysis for all the sample"
	local tit2="Analysis for"
	local tit3="Descriptive analysis "
   }	
   
   

foreach vargrup in `varsgrup' {
  disp in yellow "`vargrup'"
  local vargrupt="`vargrup'"
	if "`vargrup'"=="total" {
	  local vargrupt=""
	  cap gen total=1
	  htput <p> <b> <font color="#993366" > `tit1' </font> </b></p> 
	htput 	<br>
	}
	else {
	
    local nom_vargrup: var label `vargrup'
	if   `"`nom_vargrup'"' == "" local nom_vargrup = "`vargrup'"
	
	htput <b> <p> <font color="#993366">`tit2' `nom_vargrup'   </font>   </b> </p>
	htput 	<br>
	}
	disp in green  "Variable grup `vargrup'"
	local k=0
	graph drop _all
	foreach var in `vardesc1' `vardesc2' `vardesc3' `vardesc4' `vardesc5' `vardesc6' `vardesc7' `vardesc8' {
	disp in green " Taula per  `var' ; "
			local k=`k'+1
		
		local ttag=""
		if `k'==1 local ttag="head"
		if `k'==`nrows' local ttag="`ttag' close"
		


		if "`table'"=="" { 

			if strpos("`var'","i.")!=0 {
				local var=subinstr("`var'","i.","",1) 
				ht_summary  `var'   `vargrupt'  , lang("`lang'")  freq  test  color(0.05) `row'   `rowtotal'      `ci' `ttag'
			} 
			else {
				ht_summary   `var'   `vargrupt', lang("`lang'")  format(%5.1f) allstat test  color(0.05)    `minmax'    `ci' `ttag'
			}
		}
	}

 ******************************************************************************************
 ******************************************************************************************
 *********************** FIN TAULES DESCRIPTIVES            ******************
 ******************************************************************************************

  ******************************************************************************************
 ******************************************************************************************
 *********************** GENERA GRAFICS DESCRIPTIUS  ******************
 ******************************************************************************************
 ** Indica les variables 
		if "`graf'"=="" { 
			local vargraph1="`vardesc1' `vardesc2' `vardesc3' `vardesc4' "
			local vargraph2="`vardesc5' `vardesc6' `vardesc7' `vardesc8' "
			*local vargrup=""

			local graftitle ="`tit3'  `nom_vargrup'"
			local ggname="`vargrup'"
		 
				******************************************************************************************
			disp in yellow wordcount("`vargraph1'") +wordcount("`vargraph2'")
			local ngrfs= wordcount("`vargraph1'") +wordcount("`vargraph2'") 
			local ngrafs= round((wordcount("`vargraph1'") +wordcount("`vargraph2'") )/4+0.25)
			disp in yellow "************************************************   "`ngrafs'

			local k=0
			graph drop _all
				foreach var in `vargraph1' `vargraph2' {
					disp in green " Gràfic per `var' ; "
					local k=`k'+1

					if strpos("`var'","i.")!=0 {
						local var=subinstr("`var'","i.","",1) 
						if "`var'"!="`vargrup'" {
							graf_cat `var'   `vargrup' , noprint notest  xlab(`"`xlab'"') lang("`lang'")   gname("bar_`var'_`vargrup'_`gname'")
							}
							else {
							twoway scatteri 1 1,               ///
								   msymbol(i)                  ///
								   ylab("") xlab("")           ///
								   ytitle("") xtitle("")       ///
								   yscale(off) xscale(off)     ///
							
						}		   
					}
					else {
						graf_cont `var'   `vargrup' , noprint  `dot'  xlab(`"`xlab'"') notest lang("`lang'")  gname("bplot_`var'_`vargrup'_`gname'")
					}
					tempname graph`k'
					graph rename Graph `graph`k''
				}

				// create a blank graph
				twoway scatteri 1 1,               ///
				   msymbol(i)                  ///
				   ylab("") xlab("")           ///
				   ytitle("") xtitle("")       ///
				   yscale(off) xscale(off)     ///
				   plotregion(lpattern(blank)) ///
				   name(blank, replace)

				forvalues k=1(1)`ngrafs' {

				if `ngrafsrow'==2 {
				
					
					local g1=1+(`k'-1)*4
					if `g1'>`ngrfs' local graph`g1' ="blank"
					local g2=2+(`k'-1)*4
					if `g2'>`ngrfs' local graph`g2' ="blank"
		
					local grafname= "`ggname'_`k'"+"`gname'"
					graph combine  `graph`g1'' `graph`g2''    , title("`graftitle'") 
				}	
				else{
					local g1=1+(`k'-1)*4
					if `g1'>`ngrfs' local graph`g1' ="blank"
					local g2=2+(`k'-1)*4
					if `g2'>`ngrfs' local graph`g2' ="blank"
					
					local g3=3+(`k'-1)*4
					if `g3'>`ngrfs' local graph`g3' ="blank"
					local g4=4+(`k'-1)*4
					if `g4'>`ngrfs' local graph`g4' ="blank"
					
					local grafname= "`ggname'_`k'"+"`gname'"
					graph combine  `graph`g1'' `graph`g2''   `graph`g3''  `graph`g4'' , title("`graftitle'") 
				}
					
					graph export $htm\png\gr_`grafname'.png, replace
					graph export $gph\wmf\gr_`grafname'.wmf, replace
					graph save $gph\gph\gr_`grafname'.gph, replace

					htput <br>
					htput <IMG SRC=png\gr_`grafname'.png ALT="grafic evolució `namegraf' " >
					htput <BR>

				}

				graph drop _all

	 ******************************************************************************************
	 ******************************************************************************************
	 *********************** FIN GRAFICS DESCRIPTIUS  ******************
	 ******************************************************************************************
		}
	 }
  cap drop total
end   


