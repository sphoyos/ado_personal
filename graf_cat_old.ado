cap program drop graf_cat
program define graf_cat
*! Version 20 Nov 2013  by SPH
*! syntax:  "graf varcat vargrup, mdy"
*! This program plot and save to html file catplot from two variables


version 13
syntax varlist(min=2 max=2 numeric), [Gname(string)]  [noPrint]
   
   
     tokenize `varlist'
	 local var `1'
	 local vargrup `2'
	 local nomvargrup: var label `vargrup' 
     if   `"`nomvargrup'"' == ""  & "`vargrup'"!="total" local nomvargrup = "`vargrup'"
	 
      local nomvar: var label `var' 
      if   `"`nomvar'"' == "" local nomvar = "`var'"
      local grafname = "bar_"+"`vargrup'"+"_"+"`var'"+"`gname'"
      if "`vargrup'"!="total" {
            ********* GENERA UNA TAULA DE 2X2 I EXTRAU EL VALOR CHI **********************
            qui tabulate `var' `vargrup', chi 
            local pvalue: di %9.4f  r(p)
            ************************ DIBUIXA EL GRAFIC DE FREQÜENCIES ACUMULAT *****************
            catplot bar `var'  `vargrup', percent("`vargrup'") stack asyvars title ( `nomvar',size(small)) ///
			subtitle( según `nomvargrup',size(small))  ///
            bar(1, bcolor( 196 255 118))      bar(2, bcolor(255 118 128)) bar(3, bcolor(205 205 255))  bar(4,bcolor(255 231 206))  bar(5, bcolor(205 231 255))  ///
		    oversubopts(label(labsize(small))) ylabel( ,labsize(tiny) angle(horizontal))ytitle("% acumulado" ,size(tiny) )  ///
			note(Pvalor Chi=`pvalue',size(vsmall))  ///
			legend(title( "`nomvar'",size(small)) size(vsmall) symy(1.5)  symx(1.5) textw(1.5) row(1) region(lwidth(none)) )
         }
      else {
	    label define total 1"_",modify
		label val `vargrup' total 
          ************************ DIBUIXA EL GRAFIC DE FREQÜENCIES ACUMULAT *****************
            catplot bar `var'  `vargrup', percent("`vargrup'") stack asyvars title ( `nomvar',size(small))  ///
            bar(1, bcolor( 196 255 118))      bar(2, bcolor(255 118 128)) bar(3, bcolor(205 205 255))  bar(4,bcolor(255 231 206))  bar(5, bcolor(205 231 255))  ///
		    oversubopts(label(labsize(small))) ylabel( ,labsize(tiny) angle(horizontal))ytitle("% acumulado" ,size(tiny) )  ///
			legend(title( "`nomvar'",size(vsmall)) size(vsmall) size(vsmall) symy(1.5)  symx(1.5) textw(1.5)  row(1) region(lwidth(none))) 
	  }
	
	if "`print'"=="" { 	 
		*********** GUARDA ELS GRAFICS ******************************************
		graph export $htm\png\gr_`grafname'.png, replace
		graph export $gph\wmf\gr_`grafname'.wmf, replace
		graph save $gph\gph\gr_`grafname'.gph, replace
		htput <IMG SRC=png\gr_`grafname'.png ALT="grafic evolució `namegraf' ">
		htput <BR>
		*************** TANCA EL BUCLE DELS GRAFICS
    }
end






