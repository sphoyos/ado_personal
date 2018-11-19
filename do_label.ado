capture program drop do_label
******************************************************************
*  RUN LABEL DO FILES                                      *
******************************************************************
program define do_label
local curdir : pwd
local curdir= "`curdir'"+"\labels"  
local files : dir `"`curdir'"' files "*.do",  nofail
* Entre cometes cal posar el nom deles taules on estan els dos camps (numero, etiqueta)
foreach filename in  `files'  {

if strpos("`filename'","coments")==0  {
local name: disp "\$labels\\`filename'"
qui do  `name' 
}
}
end




