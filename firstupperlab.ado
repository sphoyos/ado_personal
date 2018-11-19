cap program drop firstupperlab
*! Version 1  03Dec2013 by SPH
*! Cambia la primera lletra de les etiquetes a majuscules
*! syntax varlist (nombre de la variable

program define firstupperlab, rclass

syntax varlist (min=1 max=1)


local g 1

local vallab : value label `1'

if "`vallab'"!="" {

 qui levelsof `1', local(lev)

      foreach val of local lev {
	    local k `:label `vallab' `val''
		
		local k= subinstr("`k'","á","�",.)
		local k= subinstr("`k'","é","�",.)
		local k= subinstr("`k'","í","�",.)
		local k= subinstr("`k'","ó","�",.)
		local k= subinstr("`k'","ú","�",.)
        local k= subinstr("`k'","Ó","�",.)
		local k =upper(substr("`k'",1,1))+ lower(substr("`k'",2,.)) 
		label define `vallab' `val' "`k'", modify
	  }	
}
end


