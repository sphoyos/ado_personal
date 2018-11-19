
capture program drop do_fromstring
******************************************************************
* Programa per reduir el format de les variables string         *
******************************************************************
program define do_fromstring

 ds , has(type string)
local var_etiqueta "`r(varlist)'"

foreach var in  `var_etiqueta' {
tempvar len
cap drop `len'
replace `var'=trim(`var')
replace `var'= subinstr(`var',"º","_",.)
replace `var'= subinstr(`var',"ª","_",.)
*replace `var'= subinstr(`var',"á","a",.)
replace `var'= subinstr(`var',"Á","a",.)
replace `var'= subinstr(`var',"Ã¡","á",.)

*replace `var'= subinstr(`var',"à","a",.)
replace `var'= subinstr(`var',"À","a",.)
*replace `var'= subinstr(`var',"é","e",.)
replace `var'= subinstr(`var',"É","e",.)
replace `var'= subinstr(`var',"Ã¨","è",.)
replace `var'= subinstr(`var',"Ã©","é",.)
*replace `var'= subinstr(`var',"è","e",.)
replace `var'= subinstr(`var',"ÃƒÂ©","e",.)
replace `var'= subinstr(`var',"È","e",.)
*replace `var'= subinstr(`var',"í","i",.)
replace `var'= subinstr(`var',"Ã­","i",.)
replace `var'= subinstr(`var',"Ã­","i",.)
replace `var'= subinstr(`var',"ÃƒÂ","í",.)
replace `var'= subinstr(`var',"ƒÃ‚Â­","",.)

replace `var'= subinstr(`var',"í‚Ã‚Â","",.)
replace `var'= subinstr(`var',"Í","i",.)
*replace `var'= subinstr(`var',"ò","o",.)
replace `var'= subinstr(`var',"Ò","o",.)
replace `var'= subinstr(`var',"Ã³","ó",.)

*replace `var'= subinstr(`var',"ó","o",.)
replace `var'= subinstr(`var',"Ó","o",.)
*replace `var'= subinstr(`var',"ú","u",.)
replace `var'= subinstr(`var',"Ú","u",.)
replace `var'= subinstr(`var',"Ã_","ú",.)

replace `var'= subinstr(`var',"Ã±","ñ",.)




gen `len'=length(ltrim(rtrim(`var')))
qui summarize `len'
drop `len'
local l=r(max)+1
local l=min(`l',244)
local format : di "%`l's"
format `format' `var'
recast str`l' `var', force
}

end


