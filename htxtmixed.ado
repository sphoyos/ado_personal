program define htxtmixed, rclass
*! version 1.0 jan 2010 by LQ
*! version 1.1 feb 2010 by LQ
*! batch is for batch
*! id is for study number
*! overall shows overall p-values for categorical variables
*! syntax varlist(min=1 max=1 numeric) [if] [,adjust(string) logscale indeplog(string)] [batch(string)] id(string) [overall intraclass] [rr(string)] [pvaltest(string)]


version 10.1

syntax varlist(min=1 max=1 numeric) [if] [,adjust(string) logscale indeplog(string)] [batch(string)] id(string) [overall intraclass] [rr(string)] [pvaltest(string)]

tokenize "`varlist'"
local dep `1'
	
/* save data in memory */			
tempfile data
quietly save `data', replace

if "`if'" != "" keep `if'
if "`pvaltest'" == "" local pvaltest wald

/* model estimation */
if "`batch'" != "" local modlevels "|| _all: R.`batch' || `id':"
else local modlevels "|| `id':"

if "`logscale'" != "" replace `dep' = log(`dep')

version 10.1: xi: xtmixed `dep' `adjust' `modlevels' , mle
esti store full
tempvar sample
gen `sample' = e(sample)

/* save results to be exported */
	local N = e(N)
	local ll = e(ll)
	local chi2 = e(chi2)
	local df_m = e(df_m)
	local p_mod = e(p)

	tempvar coef variances
	matrix define `coef' = e(b)
	matrix define `variances' = e(V)
	local nfix = e(k_f)
	local nrnd = e(k_r)

	local names: colnames `coef'

	preserve /* number of subjects */
		keep if e(sample)
		tempvar subj
		bysort `id': gen `subj' = (_n == 1)
		count if `subj' == 1
		local n_subjects = r(N)
	restore	

tempfile postest
save `postest', replace /* save estimation dataset to preserve dummies */

/* TABLE */
/* save labels */
/* lab_varname for variable label */
/* var_varname for name of varaible, usefull when refers to dummy variables */
/* dum_varname for syntax of dummies, for ex. _Icontact_ */
/* def_varname for the name value label definitions */
/* base_varname for the base value of a categorical variable */
/* lev_varname for the number of levels in model for a variable */
/* labv_varname for the lab values */

local deplab:variable label `dep'
if "`deplab'" == "" local deplab "`dep'"

tokenize "`adjust'"

while "`1'" != "" {
	if substr("`1'", 1, 2) != "i." { /* continuos variable */
		local lab_`1':variable label `1'
		if "`lab_`1''" == "" local lab_`1' "`1'"
		local var_`1' `1' /* save the name of the variable */
	}
	else { /* dummies */
		local vname "`= subinstr("`1'","i.","",1)'"
		xi i.`vname'
		local dum_`vname' = subinstr("$S_1","*","",.) /* save the syntax of the dummies */
		
		local def_`vname':value label `vname'
		quietly levelsof `vname', local(levels)
		local base_`vname' ""
		local lev_`vname' = 0

		foreach level in `levels' {
			quietly count if `vname' == `level' & `dep' != .
			if `r(N)' != 0 {
				if "`base_`vname''" == "" {
					local lab_`vname':variable label `vname'
					if "`lab_`vname''" == "" local lab_`vname' "`vname'"
					if "`def_`vname''" != "" local base_`vname':label `def_`vname'' `level'
					else local base_`vname' "`level'"
					local lev_`vname' = 1
				}
				else {
					local var_`dum_`vname''`level' "i.`vname'" /* save the name of the variable corresponding to those dummies */
					local lab_`dum_`vname''`level':variable label `vname'
					if "`lab_`dum_`vname''`level''" == "" local lab_`dum_`vname''`level' "`vname'"
					if "`def_`vname''" != "" local labv_`dum_`vname''`level':label `def_`vname'' `level'
					else local labv_`dum_`vname''`level' "`level'"
					local lev_`vname' = `lev_`vname'' + 1
				}
			}
		}
	}
	mac shift
}

if "`batch'" != "" local lab_`batch':variable label `batch'
local lab_`id':variable label `id'
local lab_residual Residual

	/* save estimates */
	use `postest', replace
	tempname export	
	tempfile estimates
	postfile `export' str15 vname str15 coef str60 varlab str20 vallab estim ee low up p using `estimates'
	
	tokenize "`names'"

	forvalues i=1(1)`= `nfix' + `nrnd'' {
		local p = .
		if `i' <= `nfix' {
			local coef_name: di "``i''"
			local estim: di `coef'[1,`i']
			local ee: di sqrt(`variances'[`i',`i'])
			local low: di `estim' - invnormal(0.975) * `ee'
			local up: di `estim' + invnormal(0.975) * `ee'
			if `i' < `nfix' {
				quietly testparm `coef_name'
				local p = r(p)
			}
		} 
		if `i' > `nfix' {
			if "`batch'" != "" {
				if (`i' == `nfix' + 1) local coef_name "`batch'"
				if (`i' == `nfix' + 2) local coef_name "`id'"
			}
			else {
				if (`i' == `nfix' + 1) local coef_name "`id'"
			}
			if (`i' == `nfix' + `nrnd') local coef_name "residual"
			local estim: di exp(`= `coef'[1,`i']')
			local ee: di exp(`= `coef'[1,`i']')*sqrt(`= `variances'[`i',`i']')
			local low = .
			local up = .
		} 
		post `export' ("`var_`coef_name''") ("`coef_name'") ("`lab_`coef_name''") ("`labv_`coef_name''") (`estim') (`ee') (`low') (`up') (`p')
	}
	postclose `export'

	/* OVERALL */
	if "`overall'" != "" {
		tempname op_exp
		tempfile op_values
		postfile `op_exp' str15 vname op using `op_values'
		tokenize "`adjust'"
		while "`1'" != "" {
			if substr("`1'",1,2) == "i." { /* categorical */
				local vname "`= subinstr("`1'","i.","",1)'" 

				if "`pvaltest'" == "wald" {
					testparm `dum_`vname''*
					local op = r(p)
					post `op_exp' ("`1'") (`op')
					local testype = "Overall p-value using Wald test"
				}
				else { /* lrtest */
					if `lev_`vname'' > 2 {
						local dropvar = subinstr("`adjust'","`1'","",.)
						xi: xtmixed `dep' `dropvar' `modlevels' if `sample' == 1, mle
						esti store nested
						lrtest full nested
						local op = r(p)
						post `op_exp' ("`1'") (`op')
					}
					local testype = "Overall p-value using Likelihood-ratio test"
				}
			}	
			mac shift
		}
		postclose `op_exp'

		preserve
			use `op_values', replace
			sort vname
			save `op_values', replace
			
			use `estimates', replace
			gen order = _n /* preserve the order of variables in model */
			sort vname			
			merge vname using `op_values'
			sort order
			drop order _merge
			compress
			save `estimates', replace
		restore
	}	
	
	/* TABLE */
	/* header table */
	if "`rr'" == "" {
	if "`logscale'" == "" local rr "Arithmetic<br>difference"
	else local rr "Proportional<br>difference"
	}	

   	htput <BR>
    htput <b>Mixed-effects ML regression for `deplab'</b>

	htput <TABLE BORDER="2" CELLSPACING="0" CELLPADDING="0" WIDTH="85%">

	htput <TR>
	htput <B>
    htput <TH COLSPAN="2"> Variable </TH>
    htput <TH> `rr' </TH>
    htput <TH> (95% Conf. Interval) </TH>
    htput <TH> p-value </TH>
    if "`overall'" != "" htput <TH> Overall<BR>p-value </TH>
	htput </B>
    htput </TR>
		
    htput <TR>
    if "`overall'" != "" htput <TD COLSPAN="6">
	else htput <TD COLSPAN="5">
    htput <i>Fixed part</i> </TD>
    htput </TR>

	/* body */	
	use `estimates', replace
	compress

	/* transformations */
	quietly {
		if "`indeplog'" != "" { /* put varlist of indeplog in list format (comma sepparated) */
			forvalues i=1(1)`= wordcount("`indeplog'")' {
				local indeploglist = "`indeploglist' `= word("`indeplog'", `i')'"
			}	
			local indeploglist = subinstr("`indeploglist'"," ",`"",""',.)
		}

		if "`logscale'" != "" {
			forvalues i=1(1)`= _N' {
				if substr("`= vname[`i']'",1,2) == "i."  { /* exp */
					replace estim = exp(estim) in `i'
					replace low = exp(low) in `i'
					replace up = exp(up) in `i'
				}
				else {
					if "`indeplog'" != "" & inlist("`= vname[`i']'","`indeploglist'") { /* exp2 */
						replace estim = 2^estim in `i'
						replace low = 2^low in `i'
						replace up = 2^up in `i'
						local trans`= coef[`i']' " <i>(two-fold increment)</i>"
					}
					else { /* exp */
						if `i' <= `nfix' { /* transform is only for fix estimates */
							replace estim = exp(estim) in `i'
							replace low = exp(low) in `i'
							replace up = exp(up) in `i'
							local trans`= coef[`i']' ""
						}
					}
				}
			} /* for */
		} /* if */
		else { 
			forvalues i=1(1)`= _N' {
				if "`indeplog'" != "" & inlist("`= vname[`i']'","`indeploglist'") { /* log2 */
					replace estim = log(2)*estim in `i'
					replace low = log(2)*low in `i'
					replace up = log(2)*up in `i'
					local trans`= coef[`i']' " <i>(two-fold increment)</i>"
				}
				else local trans`= coef[`i']' ""
			} /* for */
		} /* else */
	} /* quietly */

	/* body table */
	forvalues i=1(1)`= `nfix' - 1' {
		local vname: di vname[`i']
		local prev_vname: di vname[`= `i' - 1']
		if substr("`vname'",1,2) == "i." {
			local categorical = 1
			local vname = subinstr("`vname'","i.","",1)
		}
		else local categorical = 0
		if substr("`prev_vname'",1,2) == "i." local prev_vname = subinstr("`prev_vname'","i.","",1)
		local coef_name: di coef[`i']
		local varlab: di varlab[`i']
		local val: di vallab[`i']
		local estim: di %8.2f estim[`i']
		local low: di %8.2f low[`i']
		local up: di %8.2f up[`i']
		local p: di %8.4f p[`i']

		if "`overall'" != "" {
			if "`overall'" != "" local op: di %8.4f op[`i']
			if `op' < 0.001 {
				local op_val = "< 0.001"
			}
			else local op_val: di "`op'"
		}		

		if "`vname'" != "`prev_vname'" & `categorical' == 1 { /* new categorical variable */
			/* baseline */
		    htput <TR ALIGN="center">
			htput <TD ROWSPAN="`lev_`vname''" ALIGN="center" VALIGN="top"> `lab_`vname'' </TD>
			htput <TD ALIGN="center"> `base_`vname'' </TD>
			if "`logscale'" == "" htput <TD> 0 </TD>
			else htput <TD> 1 </TD>
			htput <TD> - </TD>
			htput <TD> - </TD>
			if "`overall'" != "" {
				if `lev_`vname'' > 2 htput <TD ROWSPAN="`lev_`vname''"> `op_val' </TD>
				else htput <TD ROWSPAN="`lev_`vname''"> - </TD>
			}	
			htput </TR>
			
			/* current level */
			htput <TR ALIGN="center">
			htput <TD ALIGN="center"> `val' </TD>
			htput <TD> `estim' </TD>
			htput <TD> (`low'; `up') </TD>
			htput <TD> `p' </TD>
			htput </TR>
		} /* if */
		else {
			if `categorical' == 1 { /* more about the same categorical variable */
				htput <TR ALIGN="center">
				htput <TD ALIGN="center"> `val' </TD>
				htput <TD> `estim' </TD>
				htput <TD> (`low'; `up') </TD>
				htput <TD> `p' </TD>
				htput </TR>
			}
			else { /* new continuos variable */
				htput <TR ALIGN="center">
				htput <TD COLSPAN="2" ALIGN="center"> `varlab' `trans`coef_name'' </TD>
				htput <TD> `estim' </TD>
				htput <TD> (`low'; `up') </TD>
				htput <TD> `p' </TD>
				if "`overall'" != "" htput <TD> - </TD>
		       	htput </TR>
			}
		} /* else */
	} /* for */

   	htput <TR>
   	if "`overall'" != "" htput <TD COLSPAN="6">
	else htput <TD COLSPAN="5">
   	htput <i>Random part</i> </TD>
   	htput </TR>

	forvalues i=`= `nfix' + 1'(1)`= `nfix' + `nrnd'' {
		local varlab: di varlab[`i']
		local estim: di %8.2f estim[`i']
		local ee: di %8.2f ee[`i']
		if "`varlab'" != "Residual"  local between "Between-"
		else local between ""
	    htput <TR ALIGN="center">
		htput <TD COLSPAN="2"> `between'`varlab' Std. Dev. (<i>Std. Err.</i>)</TD>
		if "`overall'" != "" htput <TD COLSPAN="4"> 
		else htput <TD COLSPAN="3">
		htput `estim' (<i>`ee'</i>)</TD>
       	htput </TR>
	} /* for */
	htput </TABLE>
	if "`overall'" != "" htput `testype' <BR>
	
	/* intraclass correlations*/
	if "`intraclass'" != "" {
		htput <BR>
		htput <b>Intraclass correlations:</b><BR>

		use `estimates', replace
		keep if _n > `nfix' 

		if "`batch'" != "" {
			local _lab`batch': di varlab[1]
			local _`batch':di %3.2f estim[1]
			local _lab`id':di varlab[2]
			local _`id':di %3.2f estim[2]
			local _labresid:di varlab[3]
			local _resid:di %3.2f estim[3]

			htput <UL>
				/* for batch */
				local formula:di "`_lab`batch'' = `_`batch''<sup>2</sup>/(`_`batch''<sup>2</sup>+`_`id''<sup>2</sup>+`_resid'<sup>2</sup>)"
				local result: di %3.2f (`_`batch''^2)/((`_`batch''^2)+(`_`id''^2)+(`_resid'^2)) 
				htput <LI>for `formula' = `result'</LI>
				/* for id */
				local formula:di "`_lab`id'' = `_`id''<sup>2</sup>/(`_`batch''<sup>2</sup>+`_`id''<sup>2</sup>+`_resid'<sup>2</sup>)"
				local result: di %3.2f (`_`id''^2)/((`_`batch''^2)+(`_`id''^2)+(`_resid'^2)) 
				htput <LI>for `formula' = `result'</LI>
				/* for both */
				local formula:di "both `_lab`batch'' and `_lab`id'' = (`_`batch''<sup>2</sup>+`_`id''<sup>2</sup>)/(`_`batch''<sup>2</sup>+`_`id''<sup>2</sup>+`_resid'<sup>2</sup>)"
				local result: di %3.2f ((`_`batch''^2)+(`_`id''^2))/((`_`batch''^2)+(`_`id''^2)+(`_resid'^2)) 
				htput <LI>for `formula' = `result'</LI>
				/* for id within batch */
				local formula:di "`_lab`id'' for a given `_lab`batch''  = `_`id''<sup>2</sup>/(`_`id''<sup>2</sup>+`_resid'<sup>2</sup>)"
				local result: di %3.2f ((`_`id''^2))/((`_`id''^2)+(`_resid'^2)) 
				htput <LI>for `formula' = `result'</LI>
				/* for batch within id */
				local formula:di "`_lab`batch'' for a given `_lab`id''  = `_`batch''<sup>2</sup>/(`_`batch''<sup>2</sup>+`_resid'<sup>2</sup>)"
				local result: di %3.2f ((`_`batch''^2))/((`_`batch''^2)+(`_resid'^2)) 
				htput <LI>for `formula' = `result'</LI>
			htput</UL>
		}
		else {
			local _lab`id':di varlab[1]
			local _`id':di %3.2f estim[1]
			local _labresid:di varlab[2]
			local _resid:di %3.2f estim[2]
			htput <UL>
				/* for id */
				local formula:di "`_lab`id'' = `_`id''<sup>2</sup>/(`_`id''<sup>2</sup>+`_resid'<sup>2</sup>)"
				local result: di %3.2f (`_`id''^2)/((`_`id''^2)+(`_resid'^2)) 
				htput <LI>for `formula' = `result'</LI>
			htput</UL>
		}
	}

* return list information
	return local tabletype "htxtmixed"
	return local method = "`method'"
	return local depvar = "`dep'"
	if "`logscale'" != "" return local logscale "on"
	else return local logscale "off"
	return local var_inmod = subinstr("`= subinstr("`adjust'","i.","",.)'"," ",", ",.)
	return scalar N = `N'
 	return scalar n_subjects = `n_subjects'
	return scalar ll = `ll'
	return scalar chi2 = `chi2'
	return scalar df_m = `df_m'
	return scalar p = `p_mod'
	
/* restore data in memory */			
quietly use `data', replace

end
			
