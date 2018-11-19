capture program drop  ht_correla
program define ht_correla 
syntax  varlist(min=2 max=2) [if] [in] ,[Quadra] [graftxt(string)] [regression] [noPearson] [noSpearman] [reg2]
version 13.1
preserve



local mostrapearson=0
local mostraspearman=0

 if "`pearson'"==""  local mostrapearson=1
 if "`spearman'"=="" local mostraspearman=1 

if "`if'`in'" != "" {

	keep `if'  `in'
}
gen intercept=1
label var intercept"Intercept"

local varx=`"`1'"'
local vary=`"`2'"'

foreach varX of varlist `varx'  {
 
	local nomvarX: var label `varX' 
	if   `"`nomvarX'"' == "" label var `varX' "`varX'" 
	if   `"`nomvarX'"' == "" local nomvarX = "`varX'" 
 
	 foreach varY of varlist `vary' {
	 
		local nomvarY: var label `varY' 
		if   `"`nomvarY'"' == "" label var  `varY' "`varY'" 
		if   `"`nomvarY'"' == "" local nomvarY = "`varY'" 
		cap drop repe 

		duplicates tag `varY' `varX' if  `varY'!=. & `varX'!=.  , generate(repe)

		replace repe= repe+1
		local nomvarY: var label `varY' 
		if   `"`nomvarY'"' == "" local nomvarY = "`varY'"
		local grafname = "sc_cor_"+"`graftxt'"+"`varX'"+"_"+"`varY'"

		  **** Correlació de Pearson 
		disp in yellow "****************************************************************"
		qui tab `varY'
		qui tab `varX'

		correlate `varY' `varX'
		* if `varY' !=0 

		local corr: di  %5.3f  r(rho)
		local pvalue: di  %6.4f  tprob((r(N)-2),r(rho)* sqrt( (r(N)-2)/(1-r(rho)^2)))
		 cii2 r(N) r(rho), corr
		local r_inf: di %5.3f r(lb)
		local r_sup: di %5.3f r(ub)

		***** Correlació de Spearman
		qui spearman  `varY' `varX'
		* if `varY' !=0 
		local corrs: di  %5.3f  r(rho)
		local pvalues: di  %6.4f  tprob((r(N)-2),r(rho)* sqrt( (r(N)-2)/(1-r(rho)^2)))
		qui cii2 r(N) r(rho), corr
		local r_infs: di %5.3f r(lb)
		local r_sups: di %5.3f r(ub)

		htput <br>
		htput <TABLE BORDER=1 CELLSPACING =0 CELlPADDING=2>
		htput <TR>
		htput <TH> X Variable  </TH>
		htput <TH> Y Variable  </TH>
		if `mostrapearson'==1 {
			htput <TH> Pearson Correlation</TH>
			htput <TH> pvalor </TH>
		}
		if `mostraspearman'==1 {
		htput <TH> Spearman Correlation </TH>
		htput <TH> p value </TH>
		}
		htput </TR>

		htput <TR>
		htput <TD>  `nomvarX' </TD>
		htput <TD>  `nomvarY'  </TD>
		if `mostrapearson'==1 {
			htput <TD>  `corr' (`r_inf';`r_sup') </TD>
			htput <TD>  `pvalue' </TD>
		}
		if `mostraspearman'==1 {		
			htput <TD>  `corrs' (`r_infs';`r_sups') </TD>
			htput <TD>  `pvalues' </TD>
		}	
		htput </TR>
		htput </TABLE>

		htput <BR>

		if `mostrapearson'==1 {
			local nota:disp "Pearson Correlation =`corr' [95%CI `r_inf'; `r_sup']   pvalue= `pvalue'"
		}
        else {
			if `mostraspearman'==1 {
				local nota:disp "Spearman Correlation =`corrs' [95%CI `r_infs'; `r_sups']   pvalue= `pvalues'"
		    }
		}	
		if "`quadra'"!="" {
				twoway sc `varY'  `varX' [fweight=repe] ,mcolor("253 169 255") msize(1.5)  ///
					|| lfit `varY'  `varX' ,lcolor("153 52 137")  lw(medthick) ///
					||   qfit `varY'  `varX' ,lcolor("132 0 208")  lw(medthick) ///
					||,  title(Correlation `nomvarY' with `nomvarX',size(medsmall) ) ///
					xtitle(`nomvarX',size(vsmall)) ytitle(`nomvarY',size(medsmall)) ///
					note("`nota'")  xlabel(,labsize(vsmall)) ylabel(,angle(h) labsize(vsmall)) ///
					legend( label(1 "Observations") label( 2 "Linear fit") label( 3 "Quadratic fit") ) 
		}
		else { 		
				twoway sc `varY'  `varX' [fweight=repe] ,mcolor("253 169 255") msize(1.5)  ///
					|| lfit `varY'  `varX' ,lcolor("153 52 137")  lw(medthick) ///
					||,  title(Correlation `nomvarY' with `nomvarX',size(vsmall) ) ///
					xtitle(`nomvarX',size(vsmall)) ytitle(`nomvarY',size(vsmall)) ///
					note("`nota'") xlabel(,labsize(vsmall)) ylabel(,angle(h) labsize(vsmall)) ///
					legend( label(1 "Observations") label( 2 "Linear fit") )
		}		

		*********** GUARDA ELS GRAFICS ******************************************
		*********** GUARDA ELS GRAFICS ******************************************
		*********** GUARDA ELS GRAFICS ******************************************
		graph export $htm\png\gr_`grafname'.png, replace
		graph export $gph\wmf\gr_`grafname'.wmf, replace
		graph save $gph\gph\gr_`grafname'.gph, replace
		htput <IMG SRC=png\gr_`grafname'.png ALT="grafic evolució `namegraf' ">
		htput <BR>
        
		if "`regression'"!=""  {
			htputvarcox , reg("regress" ) dep("`varY'") indep (" intercept `varX'") color  rr("Coeffficient")  multi  options("noconstant")
			htput <br>
				if "`quadra'"!="" {
					gen varX2=`varX' *`varX'
					label var varX2 "`nomvarX' al cuadrado"
					htputvarcox , reg("regress" ) dep("`varY'") indep ("intercept `varX' varX2 ") color  rr("Coeffficient")  multi  options("noconstant")
					htput <br>
				}
		}	
				if "`reg2'"!=""  {
			htputvarcox , reg("regress" ) dep("`varY'") indep ("  `varX'") color  rr("Coeffficient")  multi  
			htput <br>
				if "`quadra'"!="" {
					gen varX2=`varX' *`varX'
					label var varX2 "`nomvarX' al cuadrado"
					htputvarcox , reg("regress" ) dep("`varY'") indep (" `varX' varX2 ") color  rr("Coeffficient")  multi  
					htput <br>
				}
		}		
	}
}

cap drop intercept
cap drop varX2

restore
end






