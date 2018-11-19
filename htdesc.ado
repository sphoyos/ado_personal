*! Version 1.0  Aug 4 de 1999 by JJAV
*! Version 2.0  Aug 4 de 2010 by JJAV
program define htdesc
version 6.0
syntax [varlist], [Notitle Table(string)]  [NOCases]

if `"`table'"' == "" {
	local table = "BORDER = 1 CELLSPACING=0 CELLPADDING=2"
}

unab varlist: `varlist'

if `"`nocases'"'=="" {

local nvar=wordcount("`varlist'")
local nobs=_N
}  

tokenize `varlist'
if "`title'" == "" {
	local labda : data label
	if `"`labda'"' != "" {
		htput<FONT color="#993366" > <b>`labda'</FONT  color size>  <p>
		htput <FONT color="#993366" > <b> $S_FN </FONT  color size> </b>
	}
	else {
		htput <FONT color="#993366" > $S_FN  </FONT  color size> 
	}
}	
htput <BR>
htput <FONT color="#993366"><b> N de variables: `nvar'</FONT  color> </b> <p>
htput <FONT color="#993366"><b> N de observaciones: `nobs'</FONT color></b>
htput <TABLE `table'>
htput <TR>
htput <TH>
htput Variable
htput <TH>
htput Type
htput <TH>
htput Format
htput <TH>
htput Description
htput <TH>
htput Label
htput </TR>
while "`1'" != "" {
	local desc: variable label `1'
	local labli: value label `1'
	local type: type `1'
	local format: format `1'
*	htput <TR STYLE="font-size:80%">
	htput <TR>
	htput <TD VALIGN=TOP >
	htput `1'
	htput <TD VALIGN=TOP>
	htput `type'
	htput <TD VALIGN=TOP>
	htput `format'
	htput <TD VALIGN=TOP>
	if "`desc'" != "" {
		htput `desc'
	}
	else {
		htput &nbsp;
	}
	htput <TD VALIGN=TOP>
	htput <KBD>
	if "`labli'" != "" {
		preserve
		sort `1'
		quietly by `1': keep if _n == 1 & `1' != .
		local j = 1
		local lin = 0
		while `j' <= _N {
		   if `j' > 10 {
			htput <BR> ... <i> etc </i>
			local j = _N+1
	            }
		    else {
			if `lin' > 0 {
			       htput <BR>
			}
			local a = `1'[`j']
			local b: label `labli' `a'
			htput `a': `b'
			local j = `j'+1
			local lin = `lin' +1
		   }
		}		
		restore
	}
	else {
		htput &nbsp;
	}
	htput </KBD>
	htput </TR>
	macro shift
}
htput </TABLE>
if `"`labda'"' != "" {
 htdi "File: " lower("$S_FN")
}

end 

