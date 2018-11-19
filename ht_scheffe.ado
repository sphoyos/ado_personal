*! version 1.2 - pbe - 9/29/08 - 1/30/08 - 4/10/06
*! version 1.2 - sph- 21/06/13
program define ht_scheffe
  version 6.0
  syntax varlist(max=1) [if/] [, Level(real 0.95) nu(real 0) den(real 0) mse(real 0) dv(varname numeric max=1) ]
  if "`e(cmd)'" ~= "anova" {
    error 301
  }

 
   if (`nu'==0 & `mse'~=0) | (`nu'~=0 & `mse'==0) {
     di in red "both nu and mse required"
     exit
   }
   tempname q mse2 qfh
   tempvar grp
   tokenize `varlist'
     egen `grp' = group(`1'), lname(aaa)
	
version 11.1
   levelsof `grp', local(gruplevels)
   
 
   disp in yellow "`gruplevels'" 
   
   
    foreach  g  in `gruplevels'  {
        disp in yellow "`g'" 
		local lab_`g': label aaa `g'
		disp in yellow  "`lab_`g''"
		}
	 version 6.0
	 
	 
	   
	 
     quietly summ `grp'
     local min = r(min)
     local max = r(max) 
     local p1  = `max' - `min'
     local p   = `p1' + 1
     if `nu' == 0 {
       local dfe = e(df_m)
       local mse2= e(rmse)^2
     }
     else {
       local dfe=`nu'
       local mse2 = `mse'
     }
	 
	 if `den' == 0 {
       local dfd = e(df_r)
	  }
     else {
       local dfd=`den'
      } 	   
 
 	 
     if "`dv'"=="" {
       local dv `e(depvar)'
     }
     local alpha = 1 - `level'
   local cv= sqrt((`dfe')* invF(`dfe',`dfd', `level'))
	 
     
      local hmean = 0
   local i = `min'
   while `i' <= `max' {
     if "`if'" ~= "" {
       quietly summ `dv' if `grp' == `i' & `if'
     }
     else quietly summ `dv' if `grp' == `i'
     local m`i' = r(mean)
     local n`i' = r(N)
          local hmean = `hmean' + 1/r(N)
     local i = `i' + 1
   }
      local hmean = `p'/`hmean'
	  
   display
   display in green "Comparaciones HSD para la variable `1'"
   display  in green "valor crítico para el test Scheffe(`alpha', `dfe', `dfd') = " `cv'
   display 
   display in green "                                       mean "
   display in green "grp vs grp       group means           dif    test Scheffe"
   display in green "-------------------------------------------------------"
    

   htput <TABLE BORDER=1 CELLSPACING =0 CELlPADDING=2>
   htput <TR>
   htput <TH VALIGN=CENTER ALIGN=CENTER COLSPAN =4  >  Comparaciones HSD para la variable `1'  </TH>
   htput </TR>
   htput <TR>
   local vc: disp  %9.4f  `cv' 
   htput <TH VALIGN=CENTER ALIGN=CENTER COLSPAN =4  > Valor crítico para el test Scheffe(`alpha', `dfe', `dfd') =  `vc'  </TH>
   htput </TR> 
    htput <TR>
	  htput <TH VALIGN=CENTER ALIGN=CENTER  >  Grup vs Grup  </TH>
	  htput <TH VALIGN=CENTER ALIGN=CENTER  > Medias  grupos </TH>
	  htput <TH VALIGN=CENTER ALIGN=CENTER  >Diferencia de Medias </TH>
	  htput <TH VALIGN=CENTER ALIGN=CENTER  >Test Scheffe </TH>
	      htput </TR>
   local ii = `min'
   local i  = `min'
   local j  = `min' + 1
   local s1 = `max' - 1
   while `i' <= `s1' {
   while `j' <= `max'  {
   local dif = abs(`m`i'' - `m`j'')
   local nn = (1/`n`i'') + (1/`n`j'')
   local sch = (`dif')/ sqrt(`mse2' *`nn')
 
   local sig " "
   local coloret ""
   if `sch' >= `cv' {
   local sig ="*" 
   local coloret=  `"BGCOLOR="#FDA9FF""'  
   }
   display in yellow  "`lab_`i''  " " vs " "  `lab_`j''""  " %9.4f `m`i'' /*
     */ "  " %9.4f `m`j'' "  " %10.4f `dif' in yellow %9.4f `sch'  in blue "`sig'"
	 
	 
	 local celda1 :disp " `lab_`i''   vs  `lab_`j''  " 
	 local celda2: disp %9.2f `m`i'' " / " %9.2f `m`j'' 
	 local celda3: disp  %10.2f `dif'
	 local celda4: disp %9.4f `sch'  
	     htput <TR>
		 
	   htput <TD VALIGN=CENTER ALIGN=CENTER `coloret' >  `celda1' </TD>
	   htput <TD VALIGN=CENTER ALIGN=CENTER `coloret'  >  `celda2' </TD>
	   htput <TD VALIGN=CENTER ALIGN=CENTER `coloret'  >  `celda3' </TD>
	   htput <TD VALIGN=CENTER ALIGN=CENTER `coloret' >  `celda4' <b> `sig' </b>   </TD>
	        htput </TR>
	 
   local j = `j' + 1
   }
   local ii = `ii' + 1
   local i = `ii'
   local j = `ii' + 1
   }
   quietly summ `1'
   if r(max) ~= `max' {
   display
   display in green "Note: the levels of `1' have been recoded."
   }
   
     htput </TABLE>
end
