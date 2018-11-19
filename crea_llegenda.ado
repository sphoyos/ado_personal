cap program drop crea_llegenda
*! Version 1  03Dec2013 by SPH
*! Crea llegenda per  gràfic d'una variable en programació
*! syntax varlist (nombre de la variable

program define crea_llegenda, rclass

syntax varlist (min=1 max=1)
local g 1
 
* Indentifica els nivells de la variable i si la variable te etiqueta de valors
qui levelsof `1', local(lev)
local lab : value label `1'
	
* Per a cada nivell de l'etiqueta selecciona l'etiqueta o el valor	
foreach val of local lev {
	if "`lab'"!="" {
		local k `:label `lab' `val''
	}
	else{
		local k=`val'
	}

	* Incrementa la llegenda afegint-la a la macro local m
 
	local m `m' lab(`g' "`k'")
    	local ++g
                
    }
 
* Torna l'etiqueta en la variable local de retorn r(txt)
		return  local  txt `m'

end   


