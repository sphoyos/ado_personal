program define ht_streg,rclass
*! version 1.0 jan 2018 by SPH
*! 

st_is 2 analysis
syntax [varlist(default=empty fv)] [fw pw iw aw] [if] [in] /*
                */ [, CLuster(passthru) CMD Level(cilevel) /*
                */ Distribution(string) Robust  /* 
                */ FRailty(string) SHared(varname) noHEADer /*
                */ TIme TR noHR noSHow SCore(passthru) noCOEF /* 
                */ STrata(passthru) ANCillary(passthru) /*
                */ anc2(passthru) noCONstant VCE(passthru) /*
                */ FORCESHARED *]


local options=substr("`0'",strpos("`0'",",")+1,.)
disp in green "a `time'"
disp in green "b `nohr'"
disp in green "c `nohr'"

if "`time'" != "" local betas="Accerelated time"
if "`hr'"=="" local betas="Hazard Ratio"
if "`hr'"!="" local betas="Coefficients"


disp in yellow "`betas'"

streg `varlist', `options' nohr 
/* save data in memory */			
tempfile data
quietly save `data', replace


tokenize `varlist'
local k=0

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












/* restore data in memory */			
quietly use `data', replace
end
