cap program drop catego_string
program define catego_string
*! Version 20 Nov 2013  by SPH
*! syntax:  catego_string varlist 
*! Programa que serveix per preparar la sintaxis per convertir una variable texte a númerica i preparar etiquetes

version 11.2
syntax varlist (min=1 max=1) , [Num]
local var="`varlist'"

*** Guarda la etiqueta  de la variable
local nomvar:var label `var'
if `"`nomvar'"' =="" local nomvar="`var'"

*** Selecciona els nivells de la variable

qui levelsof `var'  , local(races)
 
*** Escriu en la pantalla la sintaxis. Caldra posar els nivells per si hi han repeticions

display "local var=" char(34)  "`var'" char(34) 
display "local varlab:  var lab "  char(96) "var"char(39) 
display  "rename  "  char(96) "var"char(39)  " "    char(96) "var"char(39) "_txt "
display  " gen  "  char(96) "var" char(39) "=."

if "`num'"=="" {

    foreach  race of local races {
	   display  "replace "  char(96) "var"char(39) " =    if " char(96) "var"char(39) "_txt=="char(34) "`race'"char(34)

     }
}
else {
local i=0

 foreach  race of local races {
	   local i=`i'+1
	   display  "replace "  char(96) "var"char(39) " = `i'   if " char(96) "var"char(39) "_txt=="char(34) "`race'"char(34)

     }
}


display  "label var  " char(96) "var"char(39) " " char(34) char(96) "varlab" char(39) char(34) 
display  " label define lab_" char(96) "var"char(39) "  /// "
if "`num'"=="" {
    foreach  race of local races {
	    display char(34) "`race'" char(34) " ///" 
     }

     
}
else {
local i=0
  foreach  race of local races {
        local i=`i'+1
	    display "`i'" char(34) "`race'" char(34) " ///" 
     }
}	 


display  " , modify"
display  " label val " char(96) "var" char(39) "   lab_" char(96) "var" char(39) 


display  " order " char(96) "var" char(39) " , before(" char(96) "var" char(39) "_txt)"

display  " tab "  char(96) "var" char(39) "_txt if  "char(96) "var" char(39) "==."
display  "drop " char(96) "var" char(39) "_txt "

end 

