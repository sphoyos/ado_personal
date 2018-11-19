cap program drop graf_cont
program define graf_cont
*! Version 20 Nov 2013  by SPH
*! syntax:  "graf_cont varcont vargrup"
*! This program plot and save to html file catplot from two variables


version 13
syntax varlist(min=2 max=2 numeric), [Gname(string)]  [noPrint] [noDot] [noTest] [Lang(string) ] [ xlab(string)] [ylab(string)]


if "`lang'"=="" local lang="cat"
   if "`lang'"=="cat"  {
	local txt1="P valor"
	local txt2="segons"
   }	

   if "`lang'"=="cast"  {
	local txt1="P valor"
	local txt2="según"
   }	

  if "`lang'"=="eng"  {
	local txt1="P value"
	local txt2="by"
   }	

     if  "`xlab'"=="" local xlab="small"
    if  "`ylab'"=="" local ylab=""
     tokenize `varlist'
	 local var `1'
	 local vargrup `2'
	qui tab `var' `vargrup'
	
	local ncol=r(c)
	 disp in red "El nombre de columnes es *****************    " `ncol'
    *************** TANCA EL BUCLE DELS GRAFICS
 local nomvar: var label `var' 
        if   `"`nomvar'"' == "" local nomvar = "`var'"

        local nomvargrup: var label `vargrup' 
        if   `"`nomvargrup'"' == "" local nomvargrup = "`vargrup'"
 
        local grafname = "box_"+"`vargrup'"+"_"+"`var'"+"`gname'"
  if "`vargrup'"!="total" & `ncol'>1 {
        ********* GENERA UNA TEST KRUSKAL-WALLIS I EXTRAU EL VALOR P **********************
        qui  kwallis `var' , by(  `vargrup') 
        local pvalue: di  %8.4f chiprob(r(df),r(chi2)) 
        if "`test'"=="" local note: disp " `txt1' KW=`pvalue'"
        ************************* DIBUIXA ELS DIAGRAMES DE CAPSES *****************
        local formvar: format `var' 
		 if "`dot'"!="" {  
		  if  "`xlab'"!=""  {
		       local cxlab="label(`xlab')"
			 }
            			 
			graph box `var'  , over(`vargrup', `cxlab')  title (`nomvar' ,size(small)) ///
			subtitle ( `txt2' `nomvargrup',size(small))  ytitle(" ") ///
			note(`note',size(vsmall)) ylabel(`ylab', format(`formvar') angle(horizontal) labsize(tiny))  $boxcolor   
         }
	    
		 if "`dot'"=="" {  
			stripplot `var',over(`vargrup')  ms(o) msize(small) box( bfcolor("255 204 204") blcolor("153 52 137")) ///
	        whiskers(recast(rbar) 	bcolor("153 52 137") barw(0.005)) iqr(1.5) ///
			 vertical centre title (`nomvar' ,size(small)) subtitle ( `txt2' `nomvargrup',size(small)) ytitle(" ") ///
			note(`note',size(vsmall)) ylabel(`ylab', format(`formvar') angle(horizontal) labsize(vsmall)) 
			*xlabel( ,labsize(`xlab'))
		
         }
		
    }
      else {
	    label define total 1"_",modify
		cap label val total`vargrup' total 
   
      ************************* DIBUIXA ELS DIAGRAMES DE CAPSES *****************
        local formvar: format `var' 
		if "`dot'"!="" {  
        graph box `var'  , title (`nomvar' ,size(small)) ytitle(" ") ///
		 ylabel(`ylab', format(`formvar') angle(horizontal) labsize(tiny)) $boxcolor 
		 }
		 
		 disp "*******************************   `xlab'"
		 disp "`ylab'##########################   `ylab'"

	 	 if "`dot'"=="" {  
			stripplot `var', ms(o) msize(small) box( bfcolor("255 204 204") blcolor("153 52 137")) ///
	        whiskers(recast(rbar) 	bcolor("153 52 137") barw(0.005))  iqr(1.5) ///
			  vertical centre  title (`nomvar' ,size(small))  ytitle(" ") ///
		 ylabel(`ylab', format(`formvar') angle(horizontal) labsize(vsmall))  
		 *xlabel( ,labsize(`xlab'))
		
         }
	} 
   graph save $gph\gph\gr_`grafname'.gph, replace
    if "`print'"=="" { 
		*********** GUARDA ELS GRAFICS ******************************************
		 graph export $htm\png\gr_`grafname'.png, replace
		 graph export $gph\wmf\gr_`grafname'.wmf, replace
		* graph save $gph\gph\gr_`grafname'.gph, replace
		 htput <IMG SRC=png\gr_`grafname'.png ALT="grafic evolució `namegraf' ">
		 htput <BR>
		***************** TANCA EL BUCLE DELS GRAFICS
	}
end






