program define ht_kappa
*! Version 1  16 dic 2004, by SSB
*! Version html 2013
*! syntax varlist(min=2 max=2)

* This program produces kappa test
version 8.0
syntax varlist(min=2 max=2) [if] [in] [fweight] [, Absolute Wgt(string)]  [lang(string)]

if "`lang'"=="" local lang="cat"
   if "`lang'"=="cat"  {
	local tit1="Concordància"
	local tit2="Index de Concordància Observada"
	local tit3="Index de Concordància Esperat"
	local tit4="Index Kappa"
	local tit5="Valor p"
   }	
   if "`lang'"=="cast"  {
	local tit1="Concordancia"
	local tit2="Indice de Concordancia Observada"
	local tit3="Indice de Concordancia Esperada"
	local tit4="Indice Kappa"
	local tit5="Valor p"
   }	
      if "`lang'"=="eng"  {
	local tit1="Concordance"
	local tit2="Observed Concordance Index"
	local tit3="Expected Concordance Index"
	local tit4="Kappa Index"
	local tit5="p  Value"
   }	
   


quietly {
tokenize "`varlist'"
local var1 `1'
local var2 `2'
* di `"httab `var1' `var2' "'
 ht_tablacat_eng `var1' `var2', head close rowtotal coltotal symmetry color
di `"kap `var1' `var2' , `absolut'  wgt(`wgt')"'
kap `var1' `var2',`absolut'  wgt(`wgt')
local obs : di %3.2f r(prop_o) * 100 " %"
local esp : di %3.2f r(prop_e) * 100 " %"
local nkap : di %5.3f  r(kappa)
local nse = r(se)
local pval: di %6.4f 1-normal(r(z))
local nkap_sup: di %5.3f  r(kappa) + 1.96*r(se)
local nkap_inf: di %5.3f = r(kappa) - 1.96*r(se)

local collab: variable label `var2'
if "`collab'" == "" {
	local collab = "`var1'"
}


local coldef: value label `var2'
if "`coldef'" == "" {
	local coldef = "coldef"
	label def `coldef' 0 0, modify
	
}
local rowlab: variable label `var1'
if "`rowlab'" == "" {
	local rowlab = "`var2'"
}
local rowdef: value label `var1'
if "`rowdef'" == "" {
	local rowdef = "rowdef"
	label def `rowdef' 0 0, modify
}

htput <BR>

htput <TABLE BORDER="1" CELLSPACING="0" CELLPADDING="2" >
  htput <TR>
    htput <TH COLSPAN=2 > `tit1' `rowlab'  vs `collab'  </TH>
    
  htput </TR>
  htput <TR>
    htput <TD> `tit2' </TD>
    htput <TD ALIGN="right"> `obs'</TD>
  htput </TR>
  htput <TR>
    htput <TD> `tit3'</TD>
    htput <TD ALIGN="right"> `esp'</TD>
  htput </TR>
    htput <TR>
    htput <TD> `tit4'</TD>
    htput <TD ALIGN="right"> `nkap' [IC 95% `nkap_inf'; `nkap_sup' ] </TD>
  htput </TR>
   htput </TR>
    htput <TR>
    htput <TD> `tit5'</TD>
    htput <TD ALIGN="right"> `pval' </TD>
  htput </TR>
  
  
htput </TABLE>
}

end	



