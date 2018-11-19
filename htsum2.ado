program define htsum2
*! version 1.0 Oct 15 2000 by PJV
*!syntax varname [if] [in], [Head] [Close] [by(varlist min=1 max=1)]



version 6.0
syntax varlist(min=1 max=1) [if] [in], [Head] [CLose] /*
*/                          [by(varlist min=1 max=1)]
			

local varlab:variable label `varlist'
if `"`varlab'"' == "" {
	local varlab= "`varlist'"
}

local varform:format `varlist'

preserve
tempvar var   
quiet gen `var' = `varlist'


if "`head'" != "" {
	*/ Heading for first time head */
	htput <TABLE BORDER=1  CELLSPACING=0 CELLPADDING=2>
	htput <TR>
	htput <TH> Variable </TH>
	htput <TH> Obs </TH>
	htput <TH> Median</TH> 
	htput <TH> (Centile 25; Centile 75)</TH>
	htput <TH> Min </TH>
	htput <TH> Max </TH>
	htput </TR>
		
}
if "`by'" != "" {
	local yvarlab:variable label `by'
	local yvallab:value label `by'
	if `"`yvarlab'"' == "" {
		local yvarlab = "`by'"
	}
}

quietly summ `var' `if' `in', d
local xmed = r(p50)
local xmin = r(min)
local xmax = r(max)
local xn   = r(N)
local xp25 = r(p25)
local xp75 = r(p75)

local xmed :di `varform' `xmed'
local xmin :di `varform' `xmin'
local xmax :di `varform' `xmax'
local xp25 :di `varform' `xp25'
local xp75 :di `varform' `xp75'
local xci  :di "(`xp25'; `xp75')"
htput <TR>
htput <TD> `varlab' </TD>
htput <TD> `xn' </TD>
htput <TD ALIGN=RIGHT> `xmed' 
htput </TD>
htput <TD ALIGN=CENTER><I>`xci'</I></TD>
htput <TD ALIGN=RIGHT> `xmin' </TD>
htput <TD ALIGN=RIGHT> `xmax' </TD>
htput </TR>

if "`by'" != "" {
	tempvar n min max med p25 p75
	quietly collapse (count)`n'=`var' (min)`min'=`var' (max)`max'=`var' /*
	*/ (median)`med'=`var' (p25)`p25'=`var' (p75)`p75'=`var' `if' `in', by(`by')
	local xi = 1
	while `xi' <= _N {
		htput <TR>
		local xme : di `varform' `med'[`xi']
		local xci : di "(" `varform' `p25'[`xi'] "; " `varform'  `p75'[`xi'] ")"
		local xmin : di `varform' `min'[`xi']
		local xmax : di `varform' `max'[`xi']
		local xn : di `n'[`xi']	
		local xby = `by'[`xi']
		if "`yvallab'" == "" {
			local tl = `xby'
		}
		else {
			local tl: label `yvallab' `xby'	
		}

		htput  <TD> &nbsp;  &nbsp; &nbsp;  &nbsp; -In: `tl' </TD>
		htput <TD> `xn' </TD>
		htput <TD ALIGN=RIGHT> `xme' 
		htput </TD>
		htput <TD ALIGN=CENTER><I>`xci'</I></TD>
		htput <TD ALIGN=RIGHT> `xmin' </TD>
		htput <TD ALIGN=RIGHT> `xmax' </TD>
		htput </TR>
		local xi = `xi'+1
	}
}
if "`close'" != "" {
	htput </TABLE>
}

restore

end

