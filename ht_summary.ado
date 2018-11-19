 *! version 4.0 Apr 2012 by SPH
 *! version 3.4 Apr 2012 by LQ
*! version 3.3 Dec 2011 by LQ
*! version 3.2 Oct 2011 by LQ
*! version 3.1 Sep 2011 by LQ
*! version 3.0 Jul 2011 by LQ
*! version 2.1 May 2011 by LQ
*! version 2.0 Apr 2011 by LQ
*! version 1.1 Jun 2009 by LQ

*! This program makes a table of summary statistics in HTML format
*! Disclaimer: This program is provided "AS IS".
*! Authors are not responsible of any kind of damage, derived from the use of this program.

program define ht_summary
syntax varlist(min=1 max=2) [if] [in], [Head] [noTotal] [noPercent] [ROWTotal] [CLose] [Minmax] /*
*/                          [Log] [allstat] [MEDian] [FReq] [Row] [Add(real 0)] [REcode(string asis)] /*
*/ 			    			[Anova] [Kw] [CHI] [EXACT] [TEST] /*
*/ 			    			[Format(string)] /*
*/                          [Pval(real -1)] [MEthod(string asis)] [CI] /*
*/			    			[MIssing] /*
*/			    			[Color(real -1)] [Lang(string) ] [Girada] [Valid]

version 8
global HTcolor "`color'" /* color option for p-values */	

if "`lang'"=="" local lang="cat"

if "`lang'"=="cat" {
   local txt1 ="N<br><b>Mitjana Aritmètica</b>(DE)"
   if "`ci'"!="" local txt1 ="N<br>Mitjana Aritmètica</b>(DE)<br>[Interval de Confiança al 95%]"
   local txt2 ="<b>Mitjana Geomètrica</b> (DE) N"
   local txt3 =" N<br><b> Mediana</b> (P25;P75)"
   local txt4 ="N<br><b>Mitjana Aritmètica</b>(DE)<br> Mediana (P25;P75)"
   if "`ci'"!="" local txt4 ="N<br><b>Mitjana Aritmètica(DE)</b><br><font color=blue> [Interval de Confiança al 95%]</font><br><b>Mediana</b> (P25;P75)"
   local txt5 ="n (percentatge columna)"
   if "`ci'"!=""  local txt5 ="n (percentatge columna)<br><font color=blue>[Interval de Confiança al 95%]</font>"
   local txt6 ="n (percentatge fila)"
   if "`ci'"!=""   local txt6 ="n (percentatge fila)<br><font color=blue>[Interval de Confiança al 95%]</font>"
   local txt7 ="Estadístics resum"
   local txt8="<br> Mínim/Màxim"
   local txt9="n( % casos vàlids)"
   
}   

if "`lang'"=="cast" {
   local txt1 ="N<br><b>Media Aritmética(DE)  "
   if "`ci'"!="" local txt1 ="N<br><b>Media Aritmética(DE)</b><br><font color=blue>[Intervalo de Confianza al 95%]</font>"
   local txt2 ="<b>Media Geométrica(DE) N"
   local txt3 ="N<br><b> Mediana (P25;P75)"
   local txt4 ="N<br>Media Aritmética(DE)   <br> Mediana (P25;P75)"
   if "`ci'"!="" local txt4 ="N<br><b>Media Aritmética</b>(DE)<br><font color=blue>[Intervalo de Confianza al 95%]</font><br><b>Mediana</b> (P25;P75)"
   local txt5 ="n (porcentaje columna)"
   if "`ci'"!=""  local txt5 ="n (porcentaje columna)<br><font color=blue>[Intervalo de Confianza al 95%]</font>"
   local txt6 ="n (porcentaje fila)"
   if "`ci'"!=""   local txt6 ="n (porcentaje fila)<br><font color=blue>[Intervalo de Confianza al 95%]</font>"
   local txt7 ="Estadísticos resumen"
   local txt8="<br> Mínimo/Màximo"
   local txt9="n( % casos válidos)"
}   


if "`lang'"=="eng" {

   local txt1 ="N<br><b>Arithmetic Mean</b> (SD)"
   if "`ci'"!="" local txt1 ="N<br><b>Arithmetic Mean</b> (SD)<br><font color=blue>[95% Confidence Interval]</font>"
   local txt2 ="<b>Geometric Mean</b> (SD) N"
   local txt3 ="N<br><b> Median</b> (P25;P75)"
   local txt4 ="N<br><b>Arithmetic Mean</b> (SD)  <br><b> Median</b> (P25;P75)"
   if "`ci'"!="" local txt4 ="N<br><b>Arithmetic Mean</b> (SD)<br><font color=blue>[95% Confidence Interval]<br><b>Median</b> (P25;P75)"
   local txt5 ="n (column percentage)"
   if "`ci'"!=""  local txt5 ="n (column percentage<br><font color=blue>[95% Confidence Interval]</font>"
   local txt6 ="n (row percentage)"
   if "`ci'"!=""   local txt6 ="n (row percentage)<br><font color=blue>[95% Confidence Interval]</font>"
   local txt7 ="Summary statistics"
   local txt8="<br> Minimum/Maximum"
   local txt9=" n( % valid cases)"
}
/* save data in memory */			
	tempfile data
	qui save `data', replace

/* GENERE TYPE OF SUMMARY */
	if "`log'" == "" & "`median'" == "" & "`allstat'" == "" & "`freq'" == ""  {
		local type arithmetic
		local footnote "`txt1'"
	}
	if "`log'" != "" & "`median'" == "" & "`allstat'" == "" & "`freq'" == "" {
		local type geometric
		local footnote "`txt2'"
	}
	if "`log'" == "" & "`median'" != "" & "`allstat'" == "" & "`freq'" == "" {
		local type median
		local footnote "`txt3'"
	}
	
	
	if "`log'" == "" & "`median'" == "" & "`allstat'" != "" & "`freq'" == "" {
		local type allstat
		local footnote "`txt4'"
	}
	if "`log'" == "" & "`median'" == "" & "`allstat'" == ""& "`freq'" != "" {
		local type freqs
		if "`row'" == "" local footnote "`txt5'"
		else local footnote "`txt6'"
	}
	if "`type'" == "" {
		di ase err "Only one of log, median,allstat  or freq are allowed"
		exit
	}

	if  "`minmax'" != "" {
		local type2 minmax
		local footnote "`footnote' `txt8'"
	}
	
	if  "`valid'" != "" {
		local footnote2 `txt9'"
	}
	
	
	
	
/* ASSERTIONS */
	if "`head'" == "" {
		cap assert "$HTsummary" == "on"
		if _rc != 0 {
			di as err "use option head for first row"
			exit
		}
	}	 
	else {
		cap assert "$HTsummary" != "on" 
		if _rc != 0 {
			di as err "htsummary in use, option head not allowed"
			exit
		}
	}	 
	
	if "`anova'" != "" | "`kw'" != "" {
		cap assert "`freq'" == ""
		if _rc != 0 {
			di as err "method not allowed with freq"
			exit
		}
	}
	
	if "`chi'" != "" | "`exact'" != "" { 
		cap assert "`freq'" != ""
		if _rc != 0 {
			di as err "this method is only allowed in combination with freq"
			exit
		}
	}
	
	if "`missing'" != "" {
		cap assert "`freq'" != ""
		if _rc != 0 {
			di as err "missing is only allowed in combination with freq"
			exit
		}
	}
	
	if "`row'" != "" {
		cap assert "`freq'" != ""
		if _rc != 0 {
			di as err "row is only allowed in combination with freq"
			exit
		}
	}
	
	if "`rowtotal'" != "" {
		cap assert "`freq'" != ""
		if _rc != 0 {
			di as err "rowtotal is only allowed in combination with freq"
			exit
		}
	}
	
		if "`valid'" != "" {
		cap assert "`freq'" != "" & "`rowtotal'" != "" 
		if _rc != 0 {
			di as err "valid is only allowed in combination with freq & rowtotal"
			exit
		}
	}
	
	
	
	
	if "`test'" != "" {
		cap assert "`anova'" == "" & "`kw'" == "" & "`chi'" == "" & "`exact'" == ""
		if _rc != 0 {
			di as err "test is not allowed in combination with anova, kw, chi or exact"
			exit
		}
	}
	
/* KEEP VALID RECORDS */
	tokenize `varlist'
	local vrow = "`1'"
	local vcol = "`2'"
	
	if "`format'" != "" format `vrow' `format'

	if "`vcol'" == "" { /* overall descriptive */
		tempvar all
		gen `all' = 1
		label var `all' "`txt7'"
		local vcol = "`all'"
		local total "nototal"
		local chi 
		local exact
		local anova
		local kw
		local test
	}

	/* convert to numeric, if needed */
		foreach var in vrow vcol {
			cap confirm string variable ``var''
			if _rc == 0 {
				tempvar new`var'
				encode ``var'', gen(`new`var'')
				local `var' `new`var''
			}	
		}	
	
	if `"`if'"' != "" keep `if'
	if `"`in'"' != "" keep `in'
	local nvalid=_N
	quietly drop if `vcol' == .
	if `"`recode'"' != "" quiet recode `vrow' `recode'

/* LABEL MANAGEMENT */
	local collab: variable label `vcol'
	if `"`collab'"' == "" local collab = "`vcol'"
	qui levelsof `vcol', local(collevels)
	local ncolum = wordcount("`collevels'")
	if "`missing'" == "" quietly drop if `vrow' == .
	
	local rowlab: variable label `vrow'
	if `"`rowlab'"' == "" local rowlab= "`vrow'"
	if `"`girada'"'!=""{
    local aaa="`rowlab'" 
   	local rowlab="`collab'"
	local collab="`aaa'"
	}
		if "`type'" != "freqs" local rowform: format `vrow'
	else {
		qui levelsof `vrow', local(rowlevels) `missing' /* show missings as a new level */ 
		local nrow = wordcount("`rowlevels'")
		if `nrow' == 1 local rowtotal ""
	}

/* QUERY TEST */
	if "`test'" != "" {
		if inlist("`type'", "arithmetic", "geometric","allstat") local anova anova
		if inlist("`type'", "median","allstat") local kw kw
		if "`type'" == "freqs" {
			querytest `vrow' `vcol'
			local test = r(test)
			if "`test'" == "chi" local chi chi
			else local exact exact
		}
		else noi di in y "Test: `anova'  " " `kw'"
	}	
		
/* TABLE HEADER */
	if "`head'" != "" {
		if "`total'" == "" global HTptot = "on"
		else global HTptot = "off"

		local colspan = (`ncolum'+cond("$HTptot"=="on",1,0))

		htput <TABLE BORDER=1 CELLSPACING=0 CELLPADDING=2>
		htput <TR>
			local span = 1 + cond("`vcol'" == "`all'", 0, 1)
			htput <TH VALIGN=BOTTOM ROWSPAN=`span' COLSPAN=2> Variable </TH> 
			***** htput <TH ALIGN=CENTER COLSPAN=`colspan'> `collab' </TH>
			htput <TH ALIGN=CENTER COLSPAN=`ncolum'> `collab' </TH>
			if "$HTptot" == "on" htput <TH ROWSPAN=2> Total </TH>
			if "`anova'" != "" | "`kw'" != "" | "`chi'" != "" | "`exact'" != "" | `pval' != -1 { /* p-values are required */
				htput <TH VALIGN=BOTTOM ROWSPAN=2>p-value</TH>
			}
		htput </TR>

		htput <TR>
			if "`vcol'" != "`all'" {
				foreach i in `collevels' {
					local group: label (`vcol') `i'	
					htput <TH>`group'</TH>
				}
			}
			***** if "$HTptot" == "on" htput <TH> Total </TH>
			*global HTcolor "`color'" /* color option for p-values */
		disp in yellow "************************* `color'"
		htput </TR>
		global HTsummary = "on"
	} /* end of header */

/* COMPUTE SUMMARY STATISTICS */
	if "`log'" != "" quiet replace `vrow' = log(`vrow'+`add')
	else quiet replace `vrow' = `vrow' +`add'

	/* compute cell contents */
	if "`type'" != "freqs" {
		foreach i in `collevels' {
			quietly summ `vrow' if `vcol' == `i', detail
			local n = r(N)
			if "`type'" == "arithmetic" {
				local average :di `rowform'  r(mean)
				local dispersion: di  `rowform'  r(sd)
				
			}
			if "`type'" == "geometric" {
				local average: di `rowform'  exp(r(mean))
				local dispersion : di  `rowform' exp(r(sd))
			}
			if "`type'" == "median" {
				local average :di `rowform' r(p50)
				local dispersion :di `rowform' r(p25) ";"   `rowform' r(p75)
			}
			if "`type'" == "allstat" {
				local average :di `rowform'  r(mean)
				local dispersion: di  `rowform'  r(sd)
				local average1:di `rowform' r(p50)
				local dispersion1 :di `rowform' r(p25) ";"   `rowform' r(p75)
			}
			
			if "`type2'" != "" {
              
			 local minmax :  di "<br>" `rowform' r(min) "/"   `rowform' r(max)
            }	
			else{
			local minmax=""
			}
			
			if  "`ci'"!="" {
						qui cii r(N) r(mean) r(sd)
			 local ci95: di		 "<br><font color=blue>[" `rowform' r(lb) ";"   `rowform' r(ub) "]<br></font>"	
			}
			else {
			local ci95=""
			}
			 
			if "`type'" != "allstat" {
			    local cell_`i': di " `n'  <br> <b>`average' </b> ( `dispersion' ) `ci95' `minmax'"
			}
			else {
			    local cell_`i': di " `n'  <br><b>`average' </b> ( `dispersion' )  `ci95' <br> <b>`average1' </b> ( `dispersion1' ) `minmax'"
			 
			}
				
		}
		if "$HTptot" == "on" {
			quietly summ `vrow', detail
			local n = r(N)
			if "`type'" == "arithmetic" {
				local average :di `rowform'  r(mean)
				local dispersion: di  `rowform'  r(sd)
			}
			if "`type'" == "geometric" {
				local average: di `rowform'  exp(r(mean))
				local dispersion : di  `rowform' exp(r(sd))
			}
			if "`type'" == "median" {
				local average :di `rowform' r(p50)
				local dispersion :di `rowform' r(p25) ";"   `rowform' r(p75)
			}
			if "`type'" == "allstat" {
			   local average :di `rowform'  r(mean)
				local dispersion: di  `rowform'  r(sd)
				local average1:di `rowform' r(p50)
				local dispersion1 :di `rowform' r(p25) ";"   `rowform' r(p75)
			}
			
			if "`type2'" != "" {
              
			 local minmax :  di "<br>" `rowform' r(min) "/"   `rowform' r(max)
            }	
			else{
			local minmax=""
			}
			if  "`ci'"!="" {
			qui cii r(N) r(mean) r(sd)
			   local ci95: di		 "<br><font color=blue>[" `rowform' r(lb) ";"   `rowform' r(ub) "]</font>"	
			}
			else {
			local ci95=""
			}
			if "`type'" != "allstat" {
			    local cell_tot: di " `n'  <br><b>`average'</b>  ( `dispersion' )   `ci95' `minmax'"
			}
			else {
			    local cell_tot:  di " `n'  <br><b>`average' </b> ( `dispersion' )   `ci95'<br> <b>`average1' </b> ( `dispersion1' )  `minmax'"
			 
			}
		}
			
	}
	else {  
		foreach i in `rowlevels' {
			foreach j in `collevels' {
				if "`row'" == "" quietly count if `vcol' == `j'
				else quietly count if `vrow' == `i'
				local denom = r(N)
				quietly count if `vrow' == `i' & `vcol' == `j'
				local n = r(N)
				if `denom' > 0 {
						local pct: di %5.2f (`n'/`denom') * 100
						if  "`ci'"!="" {
			               qui cii `denom' `n', binomial  
			              local ci95: di		 "<br><font color=blue>[" %5.2f r(lb)*100 ";" %5.2f r(ub)*100 "]</font>"	
						} 
			          else {
					    local ci95=""
					    }	
				}		
				else local pct 0
				if "`i'" != "." local cell_`i'`j': di `n' " ("`pct' "%)" "`ci95'"
				else  local cell_miss`j': di `n' " ("`pct' "%)" "`ci95'"
				if "`percent'"!=""   &  "`i'" != "."   local cell_`i'`j': di `n'
				if "`percent'"!=""   &  "`i'" == "."   cell_miss`j': di `n'
			}
		}
		   if "`rowtotal'" != "" {
		     foreach j in `collevels' {
			
				if "`row'" == ""  qui count if `vcol' == `j'
					else  qui count
				local denom = r(N)
				if "`valid'" != ""  local denom=`nvalid'
				quietly count if `vcol' == `j'
				local n = r(N)
				if `denom' > 0  {
					local pct: di %5.2f (`n'/`denom') * 100
						if  "`ci'"!="" {
			               qui cii `denom' `n', binomial  
			              local ci95: di		 "<br><font color=blue>[" %5.2f r(lb)*100 ";" %5.2f  r(ub)*100 "]</font>"	
						} 
			          else {
					    local ci95=""
					    }	
				}		
				else local pct 0
				local cell_tot`j': di `n' " ("`pct' "%)" "`ci95'" 
				if "`percent'"!=""   local cell_tot`j': di `n'
			}
		}
		if "$HTptot" == "on" {
			if "`row'" == "" {
				quietly	count
				local denom = r(N)
			}	
			foreach i in `rowlevels' {
				quietly count if `vrow' == `i'
				local n = r(N)
				if "`row'" != "" local denom = r(N)
				local pct: di %5.2f (`n'/`denom') * 100
				  	if  "`ci'"!="" {
			               qui cii `denom' `n', binomial  
			              local ci95: di		 "<br><font color=blue>[" %5.2f  r(lb)*100 ";"  %5.2f r(ub)*100 "]</font>"	
						} 
			          else {
					    local ci95=""
					    }	
				if "`i'" == "." local i "miss"
				local cell_`i'tot: di `n' " ("`pct' "%)"  "`ci95'"
				if "`percent'"!=""   local  cell_`i'tot: di `n'
			}
			if "`rowtotal'" != "" {
				quietly count
				local n = r(N)
				if "`row'" != "" local denom = r(N)
				local pct:di %5.2f (`n'/`denom') * 100
				if  "`ci'"!="" {
			               qui cii `denom' `n', binomial  
			              local ci95: di		 "<br><font color=blue>[" %5.2f r(lb)*100 ";" %5.2f  r(ub)*100 "]</font>"	
						} 
			          else {
					    local ci95=""
					    }	
				local cell_tottot: di `n' " ("`pct' "%)"  "`ci95'"
		        if "`percent'"!=""   local  cell_tottot: di `n'
			}
		}
	}	

/* COMPUTE P-VALUES */
	if "`anova'" != "" {
		cap quiet anova `vrow' `vcol' `if' `in', category(`vcol')
		quiet test `vcol'
		local pval = fprob(r(df),r(df_r),r(F))
		local pval_aov = fprob(r(df),r(df_r),r(F))
		if r(df) == 1 local method    t-test
		else local method  ANOVA
		local method_allstat `method'
	}
	if "`kw'" != "" {
		if `ncolum' == 2 {
			cap quiet ranksum `vrow' `if' `in', by(`vcol')
			local pval = (1-normprob(abs(r(z))))*2
			local pval_kw = (1-normprob(abs(r(z))))*2
			local method  U Mann-Whitney test
		}
		else {
			cap quiet kwallis `vrow' `if' `in', by(`vcol')
			local pval= chiprob(r(df),r(chi2_adj))
			local pval_kw= chiprob(r(df),r(chi2_adj))
			local method  Kruskal-Wallis test
		}
		local method_allstat `method_allstat'  / `method'
	}
	if "`chi'" != "" {
		cap quietly tab `vrow' `vcol', chi `missing'
		local pval = r(p)
		local method  Chi-squared test
	}
	if "`exact'" != "" {
		cap quietly tab `vrow' `vcol', exact `missing'
		local pval = r(p_exact)
		local method  Fisher's exact test
	}
	
	if `pval' != -1 local pval: di %6.4f `pval'

/* CELL COLOR */
	if "$HTcolor" != "-1" {
		if `pval' != -1 & ( ( round(`pval',0.0001) <= `color' ) | ( round(`pval_aov',0.0001) <= `color' & "`pval_aov'"!="") | ( round(`pval_kw',0.0001) <= `color' & "`pval_kw'"!="" )) & `pval'!=.    local color `"BGCOLOR="#FFCCCC""' 
		else local color ""	
		disp in yellow 
	}
	else local color ""

/* BODY TABLE */
	if "`type'" != "freqs" {
		htput <TR>
			htput <TD COLSPAN=2 `color'>`rowlab'
			htsupput `footnote'
			htput </TD>
			foreach i in `collevels' {
				htput <TD ALIGN=CENTER `color'> `cell_`i''</TD>
			}
			if "$HTptot" == "on" htput <TD ALIGN=CENTER `color'> `cell_tot'</TD>
			if "`type'" != "allstat" {
			    if `pval' != -1 {
			       	if round(`pval',0.0001) >= 0.0001 htput <TD ALIGN=CENTER VALIGN=CENTER `color'> `pval'
				    else htput <TD ALIGN=CENTER VALIGN=CENTER  `color'> < 0.0001
				    if `pval' != . htsupput `method'
			    }
			}
			else{ 
				if `pval' != -1 {

			       	if ( round(`pval_aov',0.0001) < 0.0001 & round(`pval_aov',0.0001) > 0 )  local ptest : di  " <0.0001" 
					else local   ptest: di %6.4f `pval_aov'
					if (round(`pval_kw',0.0001) < 0.0001  & round(`pval_kw',0.0001) >0)  local   ptest : di  "`ptest' <br> <0.0001" 
					else local  ptest: di "`ptest' <br>"  %6.4f `pval_kw'
	
					 htput <TD ALIGN=CENTER VALIGN=CENTER `color'> `ptest'
				    if `pval' != . htsupput `method_allstat'

			    }

			}
		htput </TR>
	}
	else {
		local first = word("`rowlevels'",1)
		foreach i in `rowlevels' `rowtotal' {

			htput <TR>
			if "`i'" == "`first'" {
					htput <TD ROWSPAN=`= `nrow' + cond("`rowtotal'"=="",0,1)' valign=top `color'  style="width: 200px;" >`rowlab'
					htsupput `footnote'
					htput </TD>
			}
			htput <TD `color'>
				if "`i'" != "rowtotal" {
					local thisl: label (`vrow') `i'
					htput `thisl' 
				}
				else {
				htput Total 
				if "`valid'"!= "" 	htsupput `footnote2'
				}
			htput </TD>

			if "`i'" == "." local i "miss"
			if "`i'" == "rowtotal" {
				local i "tot"
			
			}
			foreach j in `collevels' {
				htput <TD ALIGN=CENTER `color'>`font' `cell_`i'`j'' </TD>
			}
			if "$HTptot" == "on" htput <TD ALIGN=CENTER `color'>`font' `cell_`i'tot'</TD>
			
			if "`i'" == "`first'" {
					if `pval' != -1 {
						if round(`pval',0.0001) >= 0.0001 htput <TD ALIGN=CENTER VALIGN=TOP ROWSPAN=`= `nrow' + cond("`rowtotal'"=="",0,1)' `color'> `pval'
						else htput <TD ALIGN=CENTER VALIGN=TOP ROWSPAN=`= `nrow' + cond("`rowtotal'"=="",0,1)' `color'> < 0.0001
						if `pval' != . htsupput `method'
					}
			}
			htput </TR>
        	}
	} /* freqs */
		
	if "`close'" != "" {
		htput </TABLE>
		htsupwri
		htput <br>
		macro drop HTptot HTcolor HTsummary
	}

/* restore data in memory */			
	qui use `data', replace
end

*!
*! querytest determines the appropriate statistical test 
*! for each type of variables
program querytest, rclass
syntax varlist(min=2 max=2)

	tokenize `varlist'
	
	quietly {
		preserve
			contract `1' `2'
			drop if `1' == . | `2' == .
			egen total= sum(_freq)
			bysort `1': egen total_row = sum(_freq)
			bysort `2': egen total_col = sum(_freq)
			gen expected = (total_row * total_col)/total
			gen low5 = (expected <= 5)
			sum low5
			if r(mean) >= 0.2 local test exact
			else local test chi
			local pct: di %5.2f r(mean)*100
		restore
	}
	noi di in y "`pct' % cells have an expected frequency of 5 or lower --> Test: `test'"
	
	return scalar low5 = `pct'
	return local test = "`test'"
end

*!
*! htsupput put a superscript in HT format
*! and saves the content in a global macro
program define htsupput
	if `"$HTsup"' == "" {
		global HTsup = 1
		global HT1 = `"`0'"'
		local return = 1 
	}
	else{
		local i = 1
		while `i' <= $HTsup {
			local curht = "HT`i'"
			if `"$`curht'"' == `"`0'"' {
				local return = `i'
				local `i' = $HTsup+1
			}
			local i = `i'+1
		}
		if "`return'" == "" {
			global HTsup = $HTsup+1
			local curht = `"HT$HTsup"'
			global `curht' = `"`0'"'
			local return = $HTsup
		}
	}
	* This part put the coefficient
	htput <SUP>`return'</SUP>   
end

*!
*! htsupwri writes the content of superscripts
*! and clean the macros
program define htsupwri
if `"$HTsup"' != "" {
	local i = 1
	while `i' <= $HTsup {
		local xtxt "HT`i'"
		local xtxt = `"$`xtxt'"'
		htput <font size="1">`i': `xtxt' </font><BR>
		macro drop HT`i'
		local i = `i'+1
	}
}
macro drop HTsup
end 

