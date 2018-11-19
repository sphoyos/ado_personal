cap drop ht_diagtest.ado
*! diagtest AT 1.1.0, 6 January 1999    (STB-56: sbe36)
* diagtest AT 1.0.0, 21 December 1998
* ht_diatgest AT 1.1.0 SPH April 2015

program define ht_diagtest,rclass
    version 13.1
	
syntax varlist(min=2 max=2 numeric) 	 [if] [in] [fweight], [Prev(passthru)]  [noTable]
	
	disp "***************************** `prev' ********************"
    tokenize "`varlist'"
    local test `1'
    local diag `2'

	local nomdiag:var label `diag'
    if "`nomdiag'"  =="" local nomvar="`diag'"
	local val_diag_lab: value label `diag'
	if  "`val_diag_lab'"=="" {
	  local val_diag_lab ="lab_`diag'"
	  levelsof `diag'
	  local levels=r(levels)
      foreach val in `levels' {
			label define `val_diag_lab' `val' "`val'", modify
		 }
      label val `diag' `val_diag_lab'
	 }
			
	 qui summ  `diag'
	 local lmax=r(max)
      local labdis `:label `val_diag_lab' `lmax''
 
	  
	  
    preserve
	tempvar touse
	mark `touse' `if' `in'
	markout `touse' `test' `diag'
	qui keep if `touse'

    *local z=invnorm((`level'/100)+((1-(`level'/100))/2))

    
	
	
	
	
	
	
    if "`table'"=="" { 	
  
		htput <H4><b> Sensitivity and Specificity for `nomdiag' </b> </H4>
		htput <BR>
	    ht_tablacat_eng  `test' `diag', head close rowtotal coltotal pcol  symmetry
		htput <BR>
		htput <b> <font color="#993489">True D defined as `nomdiag'~= `labdis' </font color></b>
		htput <BR>
 } 
  * 2x2 Table
    tabulate `test' `diag' [`weight'`exp'], matcell(T) `options'
	 
    local d=T[1,1]
    local c=T[1,2]
    local b=T[2,1]
    local a=T[2,2]    
    local n=`a'+`b'+`c'+`d'
* Sensitivity 
    local sens= (`a'/(`a'+`c'))
    qui cii (`a'+`c') (`a'), exact binomial
   	local li_sens=r(lb)
	local ls_sens=r(ub)
 * Especificity
	local esp= (`d'/(`b'+`d'))
    qui cii (`b'+`d') (`d') , exact binomial
   	local li_esp=r(lb)
	local ls_esp=r(ub)
	
	
	* % False Negative 
    local pfn= (`c'/(`a'+`c'))
    qui cii (`a'+`c') (`c'), exact binomial
   	local li_pfn=r(lb)
	local ls_pfn=r(ub)
 
 *  % false positive
	local pfp= (`b'/(`b'+`d'))
    qui cii (`b'+`d') (`b') , exact binomial
   	local li_pfp=r(lb)
	local ls_pfp=r(ub)
		
 *Likelihood ratios
	qui csi `a'  `b' `c' `d' , tb or
	local lrp = r(rr)
	local li_lrp = r(lb_rr)
	local ls_lrp = r(ub_rr)

	qui csi `c' `d' `a' `b' , tb
	local lrn = r(rr)
	local li_lrn = r(lb_rr)
	local ls_lrn = r(ub_rr)

  * Accuracy       	*	
	local pac = ((`a'+`d')/(`a'+`b'+`c'+`d'))
    qui cii (`a'+`b'+`c'+`d') (`a'+`d'), exact binomial
	local li_pac=r(lb)
	local ls_pac=r(ub)
	
	* NND
	local nnd= 1/ (`sens'+`esp' -1)
	
	* NNM
	local nnm= 1/ (1-`pac' )
	
	
			
    if "`prev'"=="" {
	
	  * Prevalence
	local prev=((`a'+`c')/(`a'+`b'+`c'+`d'))    
	qui cii (`a'+`b'+`c'+`d') (`a'+`c'), exact binomial
	local li_prev=r(lb)
	local ls_prev=r(ub)
	
	   * VPP
	   local vpp= (`a'/(`a'+`b'))
       qui cii     (`a'+`b') (`a') , exact binomial
       local li_vpp=r(lb)
	   local ls_vpp=r(ub)
	   
	   * VPN
	   local vpn=  (`d'/(`c'+`d'))
       qui cii     (`c'+`d') (`d') , exact binomial
       local li_vpn=r(lb)
	   local ls_vpn=r(ub)
	
	}
    else {

		local vpp = ((`sens'*`prev') / ((`sens'*`prev')+((1-`spec')*(1-`prev'))))
		local li_vpp=log(-1))
		local ls_vpp=log(-1))
		local vpn = ((`spec'*(1-`prev')) / (((1-`sens')*`prev')+(`spec'*(1-`prev'))))
		local li_vpn=log(-1))
		local ls_vpn=log(-1))
	}


   
	 
    if "`table'"=="" { 
		htput <BR>

	
		 htput <TABLE BORDER=1 CELLSPACING =0 CELlPADDING=2>
		  
	   
	   
		htput <TR>
		htput <TH VALIGN=CENTER ALIGN=LEFT > MEASURE </TH>
		htput <TH VALIGN=CENTER ALIGN=CENTER > FORMULA  </TH>
		htput <TH VALIGN=CENTER ALIGN=CENTER > VALUE </TH>
		htput <TH VALIGN=CENTER ALIGN=CENTER >95% Conf. Inter.  </TH>
		htput </TR>	
		
		htput <TR>
		htput <TD VALIGN=CENTER ALIGN=LEFT ><b> Sensitivity </b> </TD>
		htput <TD VALIGN=CENTER ALIGN=CENTER > Pr( +| D) </TD>
		 
		local VALUE: di %5.2f `sens'*100 "%"
		local CI: di %5.2f `li_sens'*100 "% ; " %5.2f `ls_sens'*100 "%"
		 
		htput <TD VALIGN=CENTER ALIGN=CENTER > `VALUE' </TD>
		htput <TD VALIGN=CENTER ALIGN=CENTER >`CI' </TD>
		htput </TR>	
		
		htput <TR>
		htput <TD VALIGN=CENTER ALIGN=LEFT > <b>Specificity  </b> </TD>
		htput <TD VALIGN=CENTER ALIGN=CENTER > Pr( -|~D)  </TD>
		  
		local VALUE: di %5.2f `esp'*100 "%"
		local CI: di %5.2f `li_esp'*100 "% ; "  %5.2f `ls_esp'*100 "%"
		
		htput <TD VALIGN=CENTER ALIGN=CENTER > `VALUE' </TD>
		htput <TD VALIGN=CENTER ALIGN=CENTER >`CI' </TD>
		htput </TR>	
		  
		htput <TR>
		htput <TD VALIGN=CENTER ALIGN=LEFT ><b> % False negative </b> </TD>
		htput <TD VALIGN=CENTER ALIGN=CENTER > Pr( -| D) </TD>
		 
		local VALUE: di %5.2f `pfn'*100 "%"
		local CI: di %5.2f `li_pfn'*100 "% ; "  %5.2f `ls_pfn'*100 "%"
		 
		htput <TD VALIGN=CENTER ALIGN=CENTER > `VALUE' </TD>
		htput <TD VALIGN=CENTER ALIGN=CENTER >`CI' </TD>
		htput </TR>	
		
		
		htput <TR>
		htput <TD VALIGN=CENTER ALIGN=LEFT ><b> % False positive </b> </TD>
		htput <TD VALIGN=CENTER ALIGN=CENTER > Pr( +|~D) </TD>
		 
		local VALUE: di %5.2f `pfp'*100 "%"
		local CI: di %5.2f `li_pfp'*100 "% ; " %5.2f `ls_pfp'*100 "%"
		 
		htput <TD VALIGN=CENTER ALIGN=CENTER > `VALUE' </TD>
		htput <TD VALIGN=CENTER ALIGN=CENTER >`CI' </TD>
		htput </TR>	
		
		htput <TR>
		htput <TD VALIGN=CENTER ALIGN=LEFT > <b>Positive predictive value  </b> </TD>
		htput <TD VALIGN=CENTER ALIGN=CENTER > Pr( D| +)  </TD>
		
		local VALUE: di %5.2f `vpp'*100 "%"
		local CI: di %5.2f `li_vpp'*100 "% ; "  %5.2f `ls_vpp'*100 "%"
		 
		htput <TD VALIGN=CENTER ALIGN=CENTER > `VALUE' </TD>
		htput <TD VALIGN=CENTER ALIGN=CENTER >`CI' </TD>
		htput </TR>	
		  
		  
		htput <TR>
		htput <TD VALIGN=CENTER ALIGN=LEFT > <b>Negative predictive value </b> </TD>
		htput <TD VALIGN=CENTER ALIGN=CENTER > Pr(~D| -)   </TD>
		
		local VALUE: di %5.2f `vpn'*100 "%"
		local CI: di %5.2f `li_vpn'*100 "% ; " %5.2f `ls_vpn'*100 "%"
		 
		htput <TD VALIGN=CENTER ALIGN=CENTER > `VALUE' </TD>
		htput <TD VALIGN=CENTER ALIGN=CENTER >`CI' </TD>
		htput </TR>	
		
		htput <TR>
		htput <TD VALIGN=CENTER ALIGN=LEFT > <b>Likelihood ratio(+)  </b> </TD>
		htput <TD VALIGN=CENTER ALIGN=CENTER >  Pr(+|D)/Pr(+|~D)   </TD>
		
		local VALUE: di %5.2f `lrp' ""		
		local CI: di %5.2f `li_lrp' " ; "  %5.2f `ls_lrp' 
		 
		htput <TD VALIGN=CENTER ALIGN=CENTER > `VALUE' </TD>
		htput <TD VALIGN=CENTER ALIGN=CENTER >`CI' </TD>
		htput </TR>	
		  
		  
		htput <TR>
		htput <TD VALIGN=CENTER ALIGN=LEFT > <b>Likelihood ratio(-)  </b> </TD>
		htput <TD VALIGN=CENTER ALIGN=CENTER >  Pr(-|D)/Pr(-|~D)   </TD>
		
		local VALUE: di %5.2f `lrn' 
		local CI: di %5.2f `li_lrn'  " ; "  %5.2f `ls_lrn' 
		 
		htput <TD VALIGN=CENTER ALIGN=CENTER > `VALUE' </TD>
		htput <TD VALIGN=CENTER ALIGN=CENTER >`CI' </TD>
		htput </TR>	
		  
		htput <TR>
		htput <TD VALIGN=CENTER ALIGN=LEFT  colspan=4>  </TD>
		htput </TR>
		 
		htput <TR>
		htput <TD VALIGN=CENTER ALIGN=LEFT > <b>Prevalence </b> </TD>
		htput <TD VALIGN=CENTER ALIGN=CENTER > Pr(D)    </TD>
		  
		local VALUE: di %5.2f `prev'*100 "%"
		local CI: di %5.2f `li_prev'*100 "% ; " %5.2f `ls_prev'*100 "%"
		 
		htput <TD VALIGN=CENTER ALIGN=CENTER > `VALUE' </TD>
		htput <TD VALIGN=CENTER ALIGN=CENTER >`CI' </TD>
		htput </TR>
		  
		htput <TR>
		htput <TD VALIGN=CENTER ALIGN=LEFT  colspan=4>  </TD>
		htput </TR>
		
		
		htput <TR>
		htput <TD VALIGN=CENTER ALIGN=LEFT > <b>Accuracy </b> </TD>
		htput <TD VALIGN=CENTER ALIGN=CENTER > Pr(A)    </TD>
		 
		local VALUE: di %5.2f `pac'*100 "%"
		local CI: di %5.2f `li_pac'*100 "% ; "  %5.2f `ls_pac'*100 "%"

		htput <TD VALIGN=CENTER ALIGN=CENTER > `VALUE' </TD>
		htput <TD VALIGN=CENTER ALIGN=CENTER >`CI' </TD>
		htput </TR>
		  
		  
		htput <TR>
		htput <TD VALIGN=CENTER ALIGN=LEFT > <b>Number need to diagnose</b> </TD>
		htput <TD VALIGN=CENTER ALIGN=CENTER > NND    </TD>
		
		local VALUE: di %5.2f `nnd'
		
		htput <TD VALIGN=CENTER ALIGN=CENTER > `VALUE' </TD>
		htput <TD VALIGN=CENTER ALIGN=CENTER >-</TD>
		htput </TR>
		  
		  
		htput <TR>
		htput <TD VALIGN=CENTER ALIGN=LEFT > <b>Number need to misdiagnose</b> </TD>
		htput <TD VALIGN=CENTER ALIGN=CENTER > NNM    </TD>
		
		local VALUE: di %5.2f `nnm'
			 
		htput <TD VALIGN=CENTER ALIGN=CENTER > `VALUE' </TD>
		htput <TD VALIGN=CENTER ALIGN=CENTER >-</TD>
		htput </TR>
		  
		  
		htput </TABLE> 


		htput <font size="1"> Interpretation: Positive Likelihood Ratio (LR+) <br>
		htput LR+ over 5 - 10: Significantly increases likelihood of the disease<br>
		htput LR+ between 0.2 to 5 (esp if close to 1): Does not modify the likelihood of the disease<br>
		htput LR+ below 0.1 - 0.2: Significantly decreases the likelihood of the disease<br>
		htput NND: Number of positive test neeed to diagnose 1 case. 
		htput NNM= Number of patients who need to be tested in order for one to be misdiagnosed by the test. 
		htput </font><BR>

	}
*/	
** Variables a retornar  en r()
return local sens=`sens'
return local esp=`esp'
return local pfn=`pfn'
return local pfp=`pfp'
return local lrp=`lrp'
return local lrn=`lrn'
return local vpp=`vpp'
return local vpn=`vpn'
return local pac=`pac'
return local nnd=`nnd'
return local nnm=`nnm'



end



