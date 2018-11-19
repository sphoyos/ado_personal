cap program drop catego_string_prg
*! Version 20 Nov 2013  by SPH
*! syntax:  "catego_string varlist"
*! Programa que serveix per preparar la sintaxis per convertir una variable texte a númerica i preparar etiquetes

program define catego_string_prg
version 13.1
syntax varlist (min=1 max=1), [save(string)] [Executa]
local var="`varlist'"

 local nomvar: var label `var' 
 if   `"`nomvar'"' == "" local nomvar = "`var'"

 levelsof `var'  , local(races)

tempname _mydo
cap file close prg_code
file open prg_code using `_mydo'.do, write replace


display  "rename  `var'  var_txt "
local linewrite: display  "rename  `var'  var_txt "
file write prg_code "`linewrite'" _n

display  " gen `var' =."
local linewrite: display  " gen `var' =."
file write prg_code "`linewrite'" _n
local g 1
foreach  race of local races {
*display  "replace `var' = `g'   if var_txt==" char(34) "`race'" char(34)
local linewrite="replace `var' = `g'   if var_txt==" + `"""' +"`race'"+ `"""'

file write prg_code `"`linewrite'"'  _n
 local ++g
}


local linewrite=  "label var  `var' " +`"""'+  "`nomvar'" +`"""' 
file write prg_code `"`linewrite'"' _n

local linewrite: display  " label define lab_`var'   /// "
file write prg_code `"`linewrite'"' _n

local g 1
foreach  race of local races {
display "`g'" `"""'  "`race'" `"""'  " ///" 
local linewrite: display "`g'" `"""'  "`race'" `"""'  " ///" 
file write prg_code `"`linewrite'"' _n
 local ++g
}

display  " , modify"
local linewrite: display  " , modify"
file write prg_code `"`linewrite'"' _n

display  " label val `var' lab_`var' "
local linewrite:display  " label val `var' lab_`var' "
file write prg_code `"`linewrite'"' _n
display  "order `var' , before(var_txt)"
local linewrite: display  "order `var' , before(var_txt)"
file write prg_code `"`linewrite'"' _n
display  "drop var_txt "
local linewrite: display  "drop var_txt "
file write prg_code `"`linewrite'"' _n

file close prg_code



do `_mydo'





end 

