program define htsum1
*! version 1.0 Oct 4 2000 by PJV & LQ & JJ
*!syntax varname [if] [in], [Head] [Geo] [CI] [CLose]
*!                          [Log] [Add(real 0)] [by(varlist min=1 max=1)]



version 6.0
syntax varlist(min=1 max=1) [if] [in], [Head] [Geo] [CI] [CLose]  /*
*/                          [Log] [Add(real 0)] [by(varlist min=1 max=1)]
			

local varlab:variable label `varlist'
if `"`varlab'"' == "" {
	local varlab= "`varlist'"
}

local varform:format `varlist'

preserve
tempvar var   

if "`log'" != "" {
	quiet gen `var' = log(`varlist'+`add')
}
else {
	quiet gen `var' = `varlist' +`add'
}


if "`head'" != "" {
	if "`geo'" != "" {
		global HTgeo = "on"
	}
	else {
		global HTgeo = "off"
	}
	if "`ci'" != "" {
		global HTci = "on"
	}
	else {
		global HTci = "off"
	}
		*/ Heading for first time head */
		htput <TABLE BORDER=1  CELLSPACING=0 CELLPADDING=2>
		htput <TR>
		htput <TH> Variable </TH>
		htput <TH> Obs </TH>
			if "$HTgeo" == "on" {
				htput <TH>Geometric<BR>Mean</TH> 
			} 
			else {
				htput <TH>Mean</TH> 
			}
			if "$HTci" == "on" {
				htput <TH>(95%CI)</TH>
			}
			else {
				htput <TH>SD</TH>
			}

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

quietly summ `var' `if' `in'
local xmean = r(mean)
local xmin = r(min)
local xmax = r(max)
local xn = r(N)
local xsd = r(sd)
local xup = `xmean'+(invt(`xn'-1,.95)*`xsd'/sqrt(`xn'))
local xlo = `xmean'-(invt(`xn'-1,.95)*`xsd'/sqrt(`xn'))

if "`log'" != "" {
	local xmean = exp(`xmean')
	local xmin = exp(`xmin')
	local xmax = exp(`xmax')
	local xsd = `xmean'*`xsd'
	local xup = exp(`xup')
	local xlo = exp(`xlo')
}

local xmean :di `varform' `xmean'
local xmin :di `varform' `xmin'
local xmax :di `varform' `xmax'
local xsd : di `varform' `xsd'
local xup: di `varform' `xup'
local xlo: di `varform' `xlo'
local xci: di "(`xlo'; `xup')"
htput <TR>
htput <TD> `varlab' </TD>
htput <TD ALIGN=RIGHT> `xn' </TD>
htput <TD ALIGN=RIGHT> `xmean' 
if "`log'" != "" & "$HTgeo" != "on" {
	htsupput Geometric mean
}
if "`log'" == "" & "$HTgeo" == "on" {
	htsupput Arithmetic mean
	}
htput </TD>
if "$HTci" == "on" {
	htput <TD ALIGN=RIGHT><I>`xci'</I></TD>
}
else {
	htput <TD ALIGN=RIGHT> <I> `xsd' </I></TD>
}
htput <TD ALIGN=RIGHT> `xmin' </TD>
htput <TD ALIGN=RIGHT> `xmax' </TD>
htput </TR>

if "`by'" != "" {
	tempvar n min max mean sd
	quietly collapse (count)`n'=`var' (min)`min'=`var' (max)`max'=`var' (mean)`mean'=`var' (sd)`sd'=`var' `if' `in', by(`by')
	tempvar lo up
	quietly gen `up' = `mean'+(invt(`n'-1,.95)*`sd'/sqrt(`n'))
	quietly gen `lo' = `mean'-(invt(`n'-1,.95)*`sd'/sqrt(`n'))
	if "`log'" != "" {
		qui replace `min' = exp(`min')	
		qui replace `max' = exp(`max')
		qui replace `mean' = exp(`mean')
		qui replace `sd' = `mean'*`sd'
		qui replace `up' = exp(`up')
		qui replace `lo' = exp(`lo')
	}
	local xi = 1
	while `xi' <= _N {
		htput <TR>
		local xme : di %5.2f `varform' `mean'[`xi']
		if "$HTci" == "on" {
		local xci : di "(" `varform' `lo'[`xi'] "; " `varform'  `up'[`xi'] ")"
		}
		else {
			local xsd : di %5.2f `varform' `sd'[`xi']	
		}	
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

		htput  <TD> &nbsp;  &nbsp; &nbsp;  &nbsp; -In: `yvarlab' =  `tl' </TD>
		htput <TD ALIGN=RIGHT> `xn' </TD>
		htput <TD ALIGN=RIGHT> `xme' 
			if "`log'" != "" & "$HTgeo" != "on" {
				htsupput Geometric mean
			}
			if "`log'" == "" & "$HTgeo" == "on" {
			htsupput Arithmetic mean
			}
		htput </TD>
		if "$HTci" == "on" {
			htput <TD ALIGN=RIGHT><I>`xci'</I></TD>
		}
		else {
			htput <TD ALIGN=RIGHT> <I> `xsd' </I></TD>
		}
		htput <TD ALIGN=RIGHT> `xmin' </TD>
		htput <TD ALIGN=RIGHT> `xmax' </TD>
		htput </TR>
		local xi = `xi'+1
	}
}
if "`close'" != "" {
	htput </TABLE>
	htsupwri
	macro drop HTgeo HTci HTline 
}

restore

end

