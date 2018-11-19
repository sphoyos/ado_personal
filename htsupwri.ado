*! This program writes the content of superscripts
*! And clean the macros
*! version 1 by JJ AGO 2000
program define htsupwri
version 6.0
if `"$HTsup"' != "" {
	local i = 1
	while `i' <= $HTsup {
		local xtxt "HT`i'"
		local xtxt = `"$`xtxt'"'
		htput `i': `xtxt' <BR>
		macro drop HT`i'
		local i = `i'+1
	}
}
macro drop HTsup
end 
