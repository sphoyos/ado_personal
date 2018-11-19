! update on quotes 20071130

program define htlist_tot
version 6.0
syntax [varlist] [if] [in] [, NODisplay Display NOLabel NOObs /*
*/ VArname NOVarlab /*
*/ Table(string) Align(string)]

preserve
if `"`table'"' == "" {
	local table = `"BORDER="1" CELLSPACING="0" CELLPADDING="2""'
}
local varal = "align=left"
local valal = "align=right"

if "`noobs'" == "" {
	tempvar n
	qui gen `n' = _n
}

if "`if'`in'" != "" {
	qui keep `if'  `in'
}

disp in green "`varlist'"

local varnum =wordcount( "`varlist'")




if "`nodispl'"  != "" & "`display'" != "" {
	noi di in red "nodisplay<->display error"
	error 101
}

local mode = "long"
if "`nodispl'" == "" & "`display'" == ""  & `varnum' <= 30 {
	local mode = "browse"

}
if "`nodispl'" != "" {
	local mode = "browse"
}

noi htput <TABLE `table'>
if "`mode'" == "long" {
	local cols = cond("`varname'"!="",1,0)+cond("`novarla'" == "", 1, 0)+1
	
	
	if `cols' == 1 {
		local valal = "ALIGN LEFT"
	}
	local i = 1 
	while `i' <= _N {
		if "`noobs'" == "" {
			noi htput <TR>
			noi htput <TD colspan=`cols'> Observation  `: di `n'[`i']'   </TD>
			noi htput </TR>
		}
		else{
			if `i' != 1 {
				noi htput <TR>
				noi htput <TD colspan=`cols'>&nbsp;</TD>
				noi htput </TR>
			}
		}

		tokenize "`varlist'"
		while "`1'" != "" {
		
			noi htput <TR>
			if "`varname'" != "" {			
				local x = "`1'"
				noi htput <TD `varal'>`x'</TD>
			}
			if "`novarla'" == "" {
				local x: variable label `1'
				if `"`x'"' == "" {
					local x = "`1'"
				}
				noi htput <TD `varal'>`x'</TD>
			}
			local x = `1'[`i']
			local type: type `1'
			if substr("`type'",1,3) == "str" {
				if `"`x'"' == "" {
					local x = "&nbsp;"
				}
				noi htput <TD `valal'>`x'</TD>
			}
			else {
				local y: label (`1') `x'
				if "`y'" != "`x'" & "`nolabel'" == "" {
					noi htput <TD `valal'>`y'</TD>
				}
				else {		
					local format: format `1'
					noi htput   <TD `valal'> `: di `format' `x''</TD>
				}
			}
			noi htput </TR>
			mac shift
		}
		local i = `i'+1
	}
}
else {
	if `varnum' == 1 {
		if "`align'" != "" {
			local valal = "ALIGN=`align'"
		}
		else {
			local valal = "ALIGN=RIGHT"
		}
	}
	else {
		local valal = "ALIGN=CENTER"
	}
	if "`novarlab'" == "" {
		noi htput <TR>
		if "`noobs'" == "" {
			noi htput <TH>&nbsp;</TH>
		}
		local k=0
		tokenize "`varlist'"
			
		 
		while "`1'" != "" {
	
			if "`novarlab'" == "" {
				local x: variable label `1'
				if `"`x'"' == "" {
					local x = "`1'"
				}
				noi htput <TH ALIGN=CENTER>`x'</TH>
			}
			
		mac shift			
		}		
		noi htput </TR>
	 }

	if "`varname'" != "" {
		htput <TR>
		if "`noobs'" == "" {
			noi htput <TH>&nbsp;</TH>
		}
		tokenize "`varlist'"
		while "`1'" != "" {
			local x = "`1'"
			noi htput <TH ALIGN=CENTER>`x'</TH>
			mac shift
		}
		htput </TR>
	}
	
	local i = 1 
	while `i' <= _N {
		htput <TR>
		if "`noobs'" == "" {
			noi htput <TD ALIGN=RIGHT>`i'</TD>
		}
	
		tokenize "`varlist'"
			local k=0
		while "`1'" != "" {
		 local k=`k'+1
		 if `k'==1  {
		   local valal = "ALIGN=LEFT"
		   }
		   else {
		   local valal = "ALIGN=CENTER"
		   }
	
			local x = `1'[`i']
			local type: type `1'
			if substr("`type'",1,3) == "str" {
				if `"`x'"' == "" {
					local x = "&nbsp;"
				}
				noi htput <TD `valal'>`x'</TD>
			}
			else {
				local y: label (`1') `x'
				if "`y'" != "`x'"  & "`nolabel'" == "" {
					noi htput <TD `valal'>`y'</TD>
				}
				else {		
					local format: format `1'
					noi htput  <TD `valal'> `:di `format' `x'' </TD>
				}
			}
	
			mac shift
		}
		htput </TR>
		local i = `i'+1
	}
}
noi htput </TABLE>
end
