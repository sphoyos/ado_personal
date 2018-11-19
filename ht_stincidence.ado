*! Programa per resumen de supervivencia
*! By SPH 03/04/2013


program define ht_stincidence


syntax [if] [in] [, BY(varlist)] 


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
     

   htput <TABLE BORDER=1 CELLSPACING =0 CELlPADDING=2>
   htput <TR>
   htput <TH VALIGN=CENTER ALIGN=CENTER `coloret' > `var_by_lab'</TH>
   htput <TH VALIGN=CENTER ALIGN=CENTER `coloret' >N eventos </TH>
   htput <TH VALIGN=CENTER ALIGN=CENTER `coloret' > Personas tiempoN eventos </TH>
   htput <TH VALIGN=CENTER ALIGN=CENTER `coloret' > Tasa incidencia*100 pers. año </TH>
   htput <TH VALIGN=CENTER ALIGN=CENTER  `coloret'> Tiempo a riesgo</TH>
   htput <TH VALIGN=CENTER ALIGN=CENTER `coloret' > Tiempo Q<sub>25</sub></TH>
   htput <TH VALIGN=CENTER ALIGN=CENTER  `coloret'> Tiempo Mediana</TH>
   htput <TH VALIGN=CENTER ALIGN=CENTER `coloret' > Tiempo Q<sub>75</sub></TH>
   if "`by'" !="" {
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
			  qui  sum  _d  if `by'==`b'
			  local ndef: disp %5.0g r(sum)
			  stsum if `by'==`b'
			  local nsuj: disp %5.0g r(N_sub)
			  local ntasa: disp %5.2f r(ir)*100
			  local trisk: disp %8.2f r(risk)
			  local t25: disp %5.2f r(p25)
			  local t50: disp %5.2f r(p50)
			  local t75: disp %5.2f r(p75)
			  
				htput <TD VALIGN=CENTER ALIGN=CENTER `coloret'  > `nsuj' </TD>
			   htput <TD VALIGN=CENTER ALIGN=CENTER  `coloret'> `ndef'  </TD>
			   htput <TD VALIGN=CENTER ALIGN=CENTER `coloret'  > `ntasa' </TD>
			   htput <TD VALIGN=CENTER ALIGN=CENTER `coloret' > `trisk' </TD>
			   htput <TD VALIGN=CENTER ALIGN=CENTER `coloret' > `t25'   </TD>
			   htput <TD VALIGN=CENTER ALIGN=CENTER `coloret' > `t50'   </TD>
			   htput <TD VALIGN=CENTER ALIGN=CENTER `coloret' > `t75'   </TD>
               if "`by'" !=""  & `i'==1{
               htput <TD VALIGN=CENTER ALIGN=CENTER ROWSPAN= `rowspan' `coloret' > `pval' </TD>
   
                 }
			  			  

			  htput </TR>
        }
   }
  
  htput <TR> 		
			  htput <TD ALIGN=left `coloret' >  Total </TD>
			  qui  sum  _d  
			  local ndef: disp %5.0g  r(sum)
			  stsum 
			  local nsuj: disp %5.0g r(N_sub)
			    local ntasa: disp %5.2f r(ir)*100
			  local trisk: disp %8.2f r(risk)
			  local t25: disp %5.2f r(p25)
			  local t50: disp %5.2f r(p50)
			  local t75: disp %5.2f r(p75)
			  
				htput <TD VALIGN=CENTER ALIGN=CENTER `coloret'  > `nsuj' </TD>
			   htput <TD VALIGN=CENTER ALIGN=CENTER  `coloret' > `ndef'  </TD>
			   htput <TD VALIGN=CENTER ALIGN=CENTER  `coloret' > `ntasa' </TD>
			   htput <TD VALIGN=CENTER ALIGN=CENTER  `coloret' > `trisk' </TD>
			   htput <TD VALIGN=CENTER ALIGN=CENTER  `coloret' > `t25'   </TD>
			   htput <TD VALIGN=CENTER ALIGN=CENTER  `coloret' > `t50'   </TD>
			   htput <TD VALIGN=CENTER ALIGN=CENTER  `coloret' > `t75'   </TD>
			   htput </TR>
  
 
    htput </TABLE>
     
	restore
end 
exit
	
