program define ht_tab1
*! version 1.0 July 31 2000 by JJ
*! version 1.1 Aug 23 2000 by PJ
*! version 2.0 Feb 18 2004 by EDL
*! this program makes a frequency table in HTML format
*! syntax varlist(min=1 max=1) [if] [in],  [Head] [CLose] [Tot]
*! head to put the header
*! close to finish

version 6.0
syntax varlist(min=1 max=1) [if] [in],  [Head] [CLose] [Tot] 


preserve
tokenize `varlist'
if "`if'" != "" {
	keep `if' 
}

tempvar flag c 
quietly gen `flag' = 1
quietly collapse  (sum) `flag', by(`varlist')
quietly summ `flag'
local tx = r(sum)
quietly gen `c' = `flag'/`tx'*100
local ti:variable label `varlist'
if "`ti'" == "" {
	local ti = "`varlist'"
}
local vallab: value label `varlist'
if "`head'" != "" {
	htput <TABLE BORDER=1 CELLSPACING=0 CELLPADDING=2>
	htput <TR>
	htput <TH> `ti' 
	htput <TH> N 
	htput <TH> %

	htput </TR> }
else {
	htput <TR>
	htput <TH> `ti' 
	htput <TH>  
	htput <TH> 
		htput </TR> }

local t = 0
local i = 1
while `i' <= _N {
	local val = `varlist'[`i']
	local n = `flag'[`i']
	local t = `t'+`flag'[`i']
	local p : di %4.1f `c'[`i']
	if "`vallab'"=="" {
		local di: di `val'
	}
	else {
		local di: label `vallab' `val'
	}
	htput <TR>
	htput <TD>`di'
	htput <TD ALIGN=RIGHT> `n'
	htput <TD ALIGN=RIGHT> `p'%
	htput </TR>
	local i = `i'+1
}

local vallab: value label `varlist'
if "`tot'" != "" {

	htput <TR>
	htput <TD ALIGN=RIGHT><i> total </i>
	htput <TD ALIGN=RIGHT><i> `t' </i>
	htput <TD ALIGN=RIGHT> &nbsp
	htput </TR>
}


if "`close'" != ""{
	htput </TABLE>

end
