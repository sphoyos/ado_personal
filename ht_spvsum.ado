*! Programa per resumen de supervivencia
*! By SPH 03/04/2013


program define ht_spvsum


syntax [if] [in] [, BY(varlist)] [color] [PVal(real -1)] [Outcome(string)] [Temp(string)]  [Eventos(string)] [Lang(string)] [Notest]





st_is 2 analysis

preserve

if "`if'`in'" != "" {
   qui keep `if'  `in'
}

local var_by_lab=""
 if "`by'" !="" {
  keep if `by'!=.
  local var_by_lab: variable label `by'
    if `"`var_by_lab'"' ==""  {
         local var_row_lab="`by'"
     } 
  local val_by_lab: value label `by'
  levelsof `by', local(bylevels) 
  local rowspan=wordcount("`bylevels'")+1
  sts test `by'
  local pval: di %9.4f  chiprob(r(df),r(chi2))
  if "`color'"!="" & `"`notest'"'==""  {
     if round(`pval',0.001) <=0.05 & `pval'!=-1 {
      local coloret= `"BGCOLOR="#FFCCCC""' 
     }
	else local coloret=""
  }
	else local coloret=""
 


}
   	 
if "`lang'"=="" local lang="cat"
if "`lang'"=="cast" local lang="esp"

if  "`lang'"=="cat" {
 local et_nsuj="N individus"
 local et_temp="Temps"
 local et_risc=" a risc"
 local seg="mitjana de seguiment"
 
   if "`outcome'"=="" {
	 local outcome="Taxa de incidencia"
	 }
	   if "`temp'"=="" {
	 local temp="Temps"
	 }
	   if "`eventos'"=="" {
	 local eventos="Events"
	 }
 }
 if  "`lang'"=="esp" {
 local et_nsuj="N sujetos"
 local et_temp="Tiempo"
 local et_risc=" a riesgo"
 local seg="promedio de seguimiento"
 
   if "`outcome'"=="" {
	 local outcome="Tasa de incidencia"
	 }
	   if "`temp'"=="" {
	 local temp="Tiempo"
	 }
	   if "`eventos'"=="" {
	 local eventos="Eventos"
	 }
 }
	 
	 
	 
   htput <TABLE BORDER=1 CELLSPACING =0 CELlPADDING=2>
   htput <TR>
   htput <TH VALIGN=CENTER ALIGN=CENTER `coloret' > `var_by_lab'</TH>
   htput <TH VALIGN=CENTER ALIGN=CENTER `coloret' > `et_nsuj' </TH>
   htput <TH VALIGN=CENTER ALIGN=CENTER `coloret' > N `eventos' </TH>
   htput <TH VALIGN=CENTER ALIGN=CENTER `coloret' > `outcome'*100 pers. `temp' <br> (I.C.95%)</TH>
   htput <TH VALIGN=CENTER ALIGN=CENTER  `coloret'> `temp' `et_risc'</TH>
   htput <TH VALIGN=CENTER ALIGN=CENTER  `coloret'> `temp' `seg'</TH>
   htput <TH VALIGN=CENTER ALIGN=CENTER `coloret' > `et_temp' Q<sub>25</sub></TH>
   htput <TH VALIGN=CENTER ALIGN=CENTER  `coloret'> `et_temp' Mediana (`temp')</TH>
   htput <TH VALIGN=CENTER ALIGN=CENTER `coloret' >`et_temp' Q<sub>75</sub></TH>
   if "`by'" !=""  & `"`notest'"'=="" {
   htput <TH VALIGN=CENTER ALIGN=CENTER `coloret' > P valor </TH>
   
   }
   htput </TR>

  if "`by'" !="" {
     local i=0
        foreach  b  in `bylevels'  {
			  local i=`i'+1
			  if "`val_by_lab'"=="" local etiq=`b'
			  else local etiq: label `val_by_lab' `b'
			  
			  htput <TR> 		
			  htput <TD ALIGN=left  `coloret'>   `etiq' </TD>
			   qui sum  _d  if `by'==`b'
			  local ndef: disp %8.0g r(sum)
			  qui stsum if `by'==`b'
			  local nsuj: disp %8.0g r(N_sub)
			  local ntasa: disp %5.2f r(ir)*100
			  local trisk: disp %8.2f r(risk)
			  local mtseg: disp %8.2f r(risk)/`nsuj'
			  local t25: disp %5.2f r(p25)
			  local t50: disp %5.2f r(p50)
			  local t75: disp %5.2f r(p75)
		
			  qui cii r(risk) `ndef', poisson
			  local cint:disp  "(" %5.2f r(lb)*100 ";" %5.2f r(ub)*100 ")"
			  
			  
				htput <TD VALIGN=CENTER ALIGN=CENTER `coloret'  > `nsuj' </TD>
			   htput <TD VALIGN=CENTER ALIGN=CENTER  `coloret'> `ndef'  </TD>
			   htput <TD VALIGN=CENTER ALIGN=CENTER `coloret'  > `ntasa' <br>`cint'  </TD>
			   htput <TD VALIGN=CENTER ALIGN=CENTER `coloret' > `trisk' </TD>
			   htput <TD VALIGN=CENTER ALIGN=CENTER `coloret' > `mtseg' </TD>
			   htput <TD VALIGN=CENTER ALIGN=CENTER `coloret' > `t25'   </TD>
			   htput <TD VALIGN=CENTER ALIGN=CENTER `coloret' > `t50'   </TD>
			   htput <TD VALIGN=CENTER ALIGN=CENTER `coloret' > `t75'   </TD>
               if "`by'" !=""  & `i'==1 & `"`notest'"'=="" {
               		if round(`pval',0.0001) < 0.0001     local   ptest : di  " <0.0001" 
					else local  ptest: di  %6.4f `pval'   
					htput <TD VALIGN=CENTER ALIGN=CENTER ROWSPAN= `rowspan' `coloret' > `ptest' </TD>
                 }
			  			  

			  htput </TR>
        }
   }
  
  htput <TR> 		
			  htput <TD ALIGN=left `coloret' >  Total </TD>
			  qui  sum  _d  
			  local ndef: disp %8.0g  r(sum)
			  qui stsum 
			  local nsuj: disp %8.0g r(N_sub)
			    local ntasa: disp %5.2f r(ir)*100
			  local trisk: disp %8.2f r(risk)
			  local mtseg: disp %8.2f r(risk)/`nsuj'
			  local t25: disp %5.2f r(p25)
			  local t50: disp %5.2f r(p50)
			  local t75: disp %5.2f r(p75)
			  cii r(risk) `ndef' , poisson
			  local cint:disp  "(" %5.2f r(lb)*100 ";" %5.2f r(ub)*100 ")"
				
			   htput <TD VALIGN=CENTER ALIGN=CENTER `coloret'  > `nsuj' </TD>
			   htput <TD VALIGN=CENTER ALIGN=CENTER  `coloret' > `ndef'  </TD>
			   htput <TD VALIGN=CENTER ALIGN=CENTER  `coloret' > `ntasa'  <br>`cint'  </TD>
			   htput <TD VALIGN=CENTER ALIGN=CENTER  `coloret' > `trisk' </TD>
			   htput <TD VALIGN=CENTER ALIGN=CENTER `coloret' > `mtseg' </TD>
			   htput <TD VALIGN=CENTER ALIGN=CENTER  `coloret' > `t25'   </TD>
			   htput <TD VALIGN=CENTER ALIGN=CENTER  `coloret' > `t50'   </TD>
			   htput <TD VALIGN=CENTER ALIGN=CENTER  `coloret' > `t75'   </TD>
			   htput </TR>
  
 
    htput </TABLE>
     
	restore
end 
exit
	
