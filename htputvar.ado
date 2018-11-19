*! Version 1.0 Aug 2000 by JJAV
*! Version 2.0 Dec 2002 by LlQD, añadido el cambio de un aumento del 25%
*! version 3.0 Apr 20 2008 by LQ, Monte Carlo permutation tests 
*! version 3.1 Sep 09 2008 by LQ, use of the Student's t distribution for the calculation of the 95%CI 

*!syntax varlist(min=1 max=1), [define]           Start variable
*!                           [strata(integer 1)]  Strata number
*!			     [pval(real -1)]      p-value
*!                           coef(string)         Coeficient variable (baseline)
*!                                                for 1 & (omit) for Omited..
*!                           [val(integer 1)]     Strata value 
*!                           [per(string)]        Per increment string
*!                           [trans(string)]      Transformation of coef exp2 log2 VE
*!			     [head]               Start table
*!			     [rr(string)]         RR string
*!                           [close]              Close table                         
*!			     [PCor(real -1)] [LPCor(real -1)] [UPCor(real -1)] [MEtcor(string asis)] corrected p-value, lower and upper limit and method for corrected p-value
*!			     [color]	Use colored cells when p-values are <= 0.05, it only works with head option
*!			     [tdf(real -1)]        degrees of freedom from the Student's t distribution


program define htputvar
* This program help to put  analysis in a table
syntax varlist(min=1 max=1), [Define] /*
*/                           [Strata(integer 1)] /*
*/                            Coef(string) /*
*/                           [Val(integer 1)] /*
*/                           [PEr(string)] /*
*/                           [Trans(string)] /*
*/			     [PVal(real -1)] /*
*/			     [HEad] /*
*/			     [Rr(string)] /*
*/                           [Close] /*
*/			     [PCor(real -1)] [LPCor(real -1)] [UPCor(real -1)] [metcor(string asis)] /*
*/			     [color]	/*
*/			     [tdf(real -1)] 

local varlab: variable label `varlist'
local vallab: value label `varlist'

* look for regression type and df
if `tdf' == -1  {
	local dist "invnorm(0.975)"
	if "`e(title)'" == "Linear regression" local metse "Normal approximation"
}
else {
	local dist "abs(invttail(`tdf',0.975))"
}

if "`head'" != "" {
	htput <TABLE BORDER=1 CELLSPACING=0 CELLPADDING=2>
	htput <TR>
	htput <TH COLSPAN=2 ALIGN=CENTER> VARIABLE </TH>
	if "`rr'" == "" {
		htput <TH> RR </TH>
	}
	else {
		htput <TH> `rr' </TH>
	}

	if "`metse'" != "" {
	htdi "<TH> (95%CI)"
	htsupput `metse'
	htdi "<BR></TH>"
	}
	else htput <TH> (95%CI) </TH>

	if `pval' != -1  {
		if `pcor' == -1 {
			htput <TH> p-value </TH>
		}
		else {
			htput <TH> observed<BR>p-value </TH>
			htput <TH> corrected<BR>p-value<BR>(95%CI)</TH>
		} 			
	}
	else {
		if `pcor' != -1 {
			htput <TH> corrected<BR>p-value<BR>(95%CI)</TH>
		}
	}
	htput </TR>
}

* control color
if "`head'" != "" {
	global Htcolor "`color'"
}

if "`define'" != "" {
	if "$Htcolor" != "" {
		if `pval' != -1 & round(`pval',0.001) <= 0.05 | `pcor' != -1 & round(`pcor',0.001) <= 0.05 { 
			global color `"BGCOLOR="#FFCCCC""' 
		}
		else global color ""
	}
	else global color ""
}
local color `"$color"'
if `strata'==1 {
local ncolum=2
}
else {
local ncolum=1
}


htput <TR>
if "`define'" != "" {
	if "`varlab'" == "" {
			htput  <TD VALIGN=TOP ROWSPAN=`strata'  COLSPAN=`ncolum' `color' > "`varlist'"
			* control total number of observations, 
			* it only works when executed from htputvarall 
			* ($HTtot and $HTmitot comes from htputvarall.ado)
			if "$HTmitot" != "" & "$HTtot" != "" {
				if $HTmitot != $HTtot {
					htsupput Number of obs = $HTmitot
				}
			}
	}
	else {
		htput  <TD VALIGN=TOP ROWSPAN=`strata' COLSPAN=`ncolum'  `color' > `varlab' 	
			* control total number of observations, 
			* it only works when executed from htputvarall 
			* ($HTtot and $HTmitot comes from htputvarall.ado)
			if "$HTmitot" != "" & "$HTtot" != "" {
				if $HTmitot != $HTtot {
					htsupput Number of obs = $HTmitot
				}
			}
	}
}
if "`per'" != "" {
	htput <TD ALIGN=RIGHT `color'>`per'</TD>
}
else{

	if "`vallab'"=="" {
		local di = `val'
	}
	else {
		local di: label `vallab' `val'
	}
	if `ncolum'==1 {
	htput <TD ALIGN=RIGHT `color'>`di'</TD>
	}
}

if "`coef'" == "baseline" {
    if "`trans'" == "exp" {
        local rrbase = 1  
    }
    else if "`trans'" == "exp2"{
        local rrbase = 1  
    }
    else if "`trans'" == "exp25"{
        local rrbase = 1  
    }
    else {
        local rrbase = 0
	}
    htput <TD ALIGN=RIGHT `color'> `rrbase'  </TD>
	htput <TD `color'> &nbsp </TD>
} 
else if "`coef'" == "omit" {
	htput <TD ALIGN=RIGHT `color'>&nbsp </TD>
	htput <TD `color'> &nbsp </TD>
} 
else {

	if "`trans'" == "exp" {
		local rr: di %5.2f exp(_b[`coef'])
		local li: di %5.2f exp(_b[`coef']-`dist'*_se[`coef'])
		local ls: di %5.2f exp(_b[`coef']+`dist'*_se[`coef'])
	  if  strpos("`rr'", ".00")!=0 {
	    local rr: di %6.3f exp(_b[`coef'])
		local li: di %6.3f exp(_b[`coef']-`dist'*_se[`coef'])
		local ls: di %6.3f exp(_b[`coef']+`dist'*_se[`coef'])
	  }
	
	} 
	else if "`trans'" == "exp2" {
		local rr: di %5.2f 2^(_b[`coef'])
		local li: di %5.2f 2^(_b[`coef']-`dist'*_se[`coef'])
		local ls: di %5.2f 2^(_b[`coef']+`dist'*_se[`coef'])
	}
	else if "`trans'" == "exp25" {
		local rr: di %5.2f 1.25^(_b[`coef'])
		local li: di %5.2f 1.25^(_b[`coef']-`dist'*_se[`coef'])
		local ls: di %5.2f 1.25^(_b[`coef']+`dist'*_se[`coef'])
	}
	else if "`trans'" == "log2" {
		local rr: di %5.2f log(2^(_b[`coef']))
		local li: di %5.2f log(2^(_b[`coef']-`dist'*_se[`coef']))
		local ls: di %5.2f log(2^(_b[`coef']+`dist'*_se[`coef']))
	}
	else if "`trans'" == "log25" {
		local rr: di %5.2f log(1.25^(_b[`coef']))
		local li: di %5.2f log(1.25^(_b[`coef']-`dist'*_se[`coef']))
		local ls: di %5.2f log(1.25^(_b[`coef']+`dist'*_se[`coef']))
	}
	else if "`trans'" == "VE" {
		local rr: di %5.1f (1-exp(_b[`coef']))*100 "%"
		local ls: di %5.1f (1-(exp(_b[`coef']-`dist'*_se[`coef'])))*100 "%"
		local li: di %5.1f (1-(exp(_b[`coef']+`dist'*_se[`coef'])))*100 "%"
	}
	else {
		local rr: di %6.3f (_b[`coef'])
		local li: di %6.3f (_b[`coef']-`dist'*_se[`coef'])
		local ls: di %6.3f (_b[`coef']+`dist'*_se[`coef'])
	}

	htput <TD ALIGN=RIGHT `color'> `rr' </TD>
	htput <TD ALIGN=CENTER `color'> (`li';`ls') </TD>
}

if `pval' > 0  {
*	local di : di %5.3f `pval'
	local di : di %6.4f `pval'

	htput <TD ROWSPAN=`strata' VALIGN=CENTER ALIGN=CENTER `color'> `di' </TD>
}
if `pcor' != -1 {
	local pcor : di %5.3f `pcor'
	local lpcor : di %5.3f `lpcor'
	local upcor : di %5.3f `upcor'

	htdi `"<TD ROWSPAN=`strata' ALIGN=CENTER VALIGN=CENTER `color'>`pcor'"' _cont
	htsupput `metcor'
	htdi "<BR><i>(`lpcor'; `upcor')</i>"
}

htput </TR>

if "`close'" != "" {
	htput </TABLE>
	htsupwri
	macro drop HTci HTline HTptot Htcolor
}

end

