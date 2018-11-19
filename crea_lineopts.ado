cap program drop crea_lineopts
*! Version 1  03Dec2013 by SPH
*! Cree les opccions de linies per per  gràfic SPV d'una variable en programació
*! syntax varlist (nombre de la variable

program define crea_lineopts, rclass

syntax varlist (min=1 max=1)


* Indentifica els nivells de la variable i si la variable te etiqueta de valors
  qui levelsof `1'
*  Calcula el número de niveles de la variable hasta un màximo de 4
 local nlevel=min(wordcount(r(levels)), 4)

 ******Codigo para tipo y color de línea del gràfico

 local lineopts1=" plot1opts(lpattern("+ char(34) +"l" +char(34)+") lcolor("+char(34)+"153 52 137"+char(34)+")) "
 local lineopts2="plot2opts(lpattern("+ char(34) +"_" +char(34)+") lcolor("+char(34)+"237 66 243"+char(34)+")) "
 local lineopts3=" plot3opts(lpattern("+ char(34) +"-.." +char(34)+") lcolor("+char(34)+"206 123 27"+char(34)+")) "
 local lineopts4=" plot4opts(lpattern("+ char(34) +"." +char(34)+") lcolor("+char(34)+"12 18 192"+char(34)+")) "
	
	
* Per a cada nivell de genera el color de la linea
  forvalues j=1(1)`nlevel'{
  	local m `m' `lineopts`j''
    }
 * Torna l'etiqueta en la variable local de retorn r(txt)

 return  local  lopt `m'

end   



