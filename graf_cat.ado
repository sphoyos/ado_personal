cap program drop graf_cat
program define graf_cat
*! Version 20 Nov 2013  by SPH
*! syntax:  "graf varcat vargrup, mdy"
*! This program plot and save to html file catplot from two variables


version 13
syntax varlist(min=2 max=2 numeric), [Gname(string)]  [noPrint]   [ xlab(string)] [nografcum]  [labgrupsize(string)] [labtitlesize(string)] [notest] [hbar]  [noCien] [options(string)] [Lang(string) ]

if "`lang'"=="" local lang="cat"
   if "`lang'"=="cat"  {
	local txt1="N de casos"
	local txt2="P valor Khi quadrat"
	
   }	

   if "`lang'"=="cast"  {
	local txt1="N de casos"
		local txt2="P valor Chi cuadrado"
   }	  

  if "`lang'"=="eng"  {
	local txt1="N of cases"
		local txt2="P value Chi squared"
   }	

     if  "`xlab'"=="" local xlab="labsize(small)"

     local graf_cum=0
       if "`grafcum'"=="" local graf_cum=1
	  if "`labtitlesize'"==""    local labtitlesize="small"
      if "`hbar'"=="" local hbar="bar"    
     tokenize `varlist'
	 local var `1'
	 local vargrup `2'
	 local nomvargrup: var label `vargrup' 
     if   `"`nomvargrup'"' == ""  & "`vargrup'"!="total" local nomvargrup = "`vargrup'"
	 
      local nomvar: var label `var' 
      if   `"`nomvar'"' == "" local nomvar = "`var'"
      local grafname = "bar_"+"`vargrup'"+"_"+"`var'"+"`gname'"
	  if "`cien'"=="" local stack="stack" 
	  
	  
	  if `graf_cum'==0{  
		  
		  if "`vargrup'"!="total" {
				********* GENERA UNA TAULA DE 2X2 I EXTRAU EL VALOR CHI **********************
				qui tabulate `var' `vargrup', chi 
				local pvalue: di %9.4f  r(p)
				 if "`test'"=="" local note: disp " `txt2' =`pvalue'"
			
				************************ DIBUIXA EL GRAFIC DE FREQÜENCIES ACUMULAT *****************
				catplot `hbar' `var' `vargrup'   ,`stack' asyvars title ( "`nomvar'",size(`labtitlesize')) ///
				subtitle( "by  `nomvargrup'",size(small))  ///
				bar(1, bcolor( 196 255 118))      bar(2, bcolor(255 118 128)) bar(3, bcolor(205 205 255))  bar(4,bcolor(255 231 206))  bar(5, bcolor(205 231 255))  ///
				oversubopts(label(`xlab')) ylabel( ,labsize(tiny) angle(horizontal))ytitle("`txt1'" ,size(tiny) )  ///
				note("`note'",size(vsmall))  ///
				legend(title( "`nomvar'",size(small)) size(vsmall) symy(1.5)  symx(1.5)   region(lwidth(none)) ) `options'
			 }
		  else {
			label define total 1"_",modify
			label val `vargrup' total 
			  ************************ DIBUIXA EL GRAFIC DE FREQÜENCIES ACUMULAT *****************
				catplot `hbar'   `var' `vargrup'  , `stack'  asyvars title ( "`nomvar'",size(`labtitlesize'))  ///
				bar(1, bcolor( 196 255 118))      bar(2, bcolor(255 118 128)) bar(3, bcolor(205 205 255))  bar(4,bcolor(255 231 206))  bar(5, bcolor(205 231 255))  ///
				oversubopts(label(`xlab')) ylabel( ,labsize(tiny) angle(horizontal))ytitle("`txt1'" ,size(tiny) )  ///
				legend(title( "`nomvar'",size(vsmall))  symy(1.5)  symx(1.5)    row(1) region(lwidth(none)))  `options'
		  }
	}
	else {
	 if "`vargrup'"!="total" {
            ********* GENERA UNA TAULA DE 2X2 I EXTRAU EL VALOR CHI **********************
            qui tabulate `var' `vargrup', chi 
            local pvalue: di %9.4f  r(p)
            ************************ DIBUIXA EL GRAFIC DE FREQÜENCIES ACUMULAT *****************
            catplot `hbar' `var'  `vargrup', percent("`vargrup'") `stack' asyvars title ( "`nomvar'",size(`labtitlesize')) ///
			subtitle( "by `nomvargrup'",size(small))  ///
            bar(1, bcolor( 196 255 118))      bar(2, bcolor(255 118 128)) bar(3, bcolor(205 205 255))  bar(4,bcolor(255 231 206))  bar(5, bcolor(205 231 255))  ///
		    oversubopts(label(`xlab')) ylabel( ,labsize(tiny) angle(horizontal))ytitle(" % " ,size(tiny) )  ///
			note("`note'",size(vsmall))  ///
			legend(title( "`nomvar'",size(small)) size(vsmall) symy(1.5)  symx(1.5)   region(lwidth(none)) ) `options'
         }
      else {
	    label define total 1"_",modify
		label val `vargrup' total 
          ************************ DIBUIXA EL GRAFIC DE FREQÜENCIES ACUMULAT *****************
            catplot `hbar'  `var'  `vargrup', percent("`vargrup'") `stack' asyvars title ( "`nomvar'",size(`labtitlesize'))  ///
            bar(1, bcolor( 196 255 118))      bar(2, bcolor(255 118 128)) bar(3, bcolor(205 205 255))  bar(4,bcolor(255 231 206))  bar(5, bcolor(205 231 255))  ///
		    oversubopts(label(`xlab')) ylabel( ,labsize(tiny) angle(horizontal))ytitle("%" ,size(tiny) )  ///
			legend(title( "`nomvar'",size(vsmall))  symy(1.5)  symx(1.5)  row(1) region(lwidth(none)))  `options'
		}
	  }
	  graph save $gph\gph\gr_`grafname'.gph, replace
	if "`print'"=="" { 	 
		*********** GUARDA ELS GRAFICS ******************************************
		graph export $htm\png\gr_`grafname'.png, replace
		graph export $gph\wmf\gr_`grafname'.wmf, replace
		*graph save $gph\gph\gr_`grafname'.gph, replace
		htput <IMG SRC=png\gr_`grafname'.png ALT="grafic evolució `namegraf' ">
		htput <BR>
		*************** TANCA EL BUCLE DELS GRAFICS
    }
end






