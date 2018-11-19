*! This program put a superscript in HT format
*! and saves the content in a global var
*! version 1, by JJ Ago 2000
program define htsupput
version 6.0
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
* Note.. use htdi `"..."' _cont to attach close to the previous text!
htput <SUP>`return'</SUP>   
 
end
