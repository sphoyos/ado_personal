***********************************************************************************
*!  Version 1.0  Novembre 2013: ht_cleaning
*! Programa per fer la detecció d'incongruencies amb sortida en html
*!Modificación de la función assertk con salida html  Version 0.1 Olga & SPH
*!syntax anything(name=asscond) [if], [by(varlist) Mess(string) Vars(varlist) Quest(string) Query(string)    noobs nod Prev Flag Next nol


cap program drop ht_cleaning_test
program define ht_cleaning_test


vers 8.0



*Updated 02/11/2009

syntax anything(name=asscond) [if], [by(varlist) Mess(string) Vars(varlist)  Quest(string) Query(string)   noobs nod Prev Flag Next nol 


preserve

/*1.Check options*/


if `"`l'"'~="" & (`"`mess'"'~=""|`"`vars'"'~=""|`"`obs'"'~=""|`"`d'"'~=""|`"`prev'"'~=""|`"`next'"'~="" /*[Olga] |`"`sep'"'~=""|`"`sepby'"'~=""*/){
	noi htput option nol may only be combined with by and flag options
	error 198

	}

/*2.Prepare local macros and temporary variables*/

if `"`if'"'~="" local ifcond `"if `if'"'
if `"`by'"'~="" local bypart `"by `by' ,sort : "'
local antiasscond `"!(`asscond')"'


tempvar _viol
tempvar _pre
tempvar _nex
tempvar _message
tempvar _nquery
tempvar strvariab
tempfile results

cap drop _assertflag
  * postfile tableerror  _nquery str80 _quest  str80 SubjID    str80  textvar  str180 _message  str80 action using `results',replace
/*3.Process if full output required (i.e. nol option not specified)*/

if "`l'"==""{

/* Mir si hay errores (incumplimentos de la condición que se debe de cumplir) */
`bypart' assert `asscond' `ifcond'

cap assert   fecha_diagnÓstico<.
scalar define errcode=_rc
disp "`errcode'"

if errcode~=0{ 
	`bypart' gen `_viol'=((`antiasscond') `if'
	if "`flag'"~="" {
	gen _assertflag=(`_viol'==1)
	}
	
	/*   Cuenta el número de errores */

	qui count if `_viol'==1 
	local numviols=r(N)
	/*   Identifica el número de query para  ponerla posteriormente en el título */
    gen _nquery =`query'
    qui  summarize _nquery if `_viol'==1

	  local numquery=r(max)
   
	
    qui {
    if "`prev'"~=""|"`next'"~=""	{
	gen `_pre'=0
	gen `_nex'=0
	if "`prev'"~="" `bypart' replace `_pre'=1 if (`_viol'[_n+1]==1)
	if "`next'"~="" `bypart' replace `_nex'=1 if (`_viol'[_n-1]==1)
	replace `_viol'=1 if (`_viol'==1|`_pre'==1|`_nex'==1)
	drop `_pre'
	drop `_nex'
	}
	 }
	
	
      
    gen _quest=`"`quest'"'
	gen _message= `"`mess'"'
	gen action=""
    gen  _numquery= `numquery'
	*gen textvar=" "
	*local i=0
	
	

   * foreach variab of varlist `vars'{
	*local i=`i'+1	

  *if `i'>1{
   

   
   

	*noi di in yellow  `variab'
	*local varname: var lab `variab'
     
	*cap drop strvariab
	*cap decode `variab', gen (strvariab)
   * if _rc==108 gen strvariab= `variab'

	*if _rc==182 {
	
	* gen strvariab=string(`variab')
	* replace strvariab=string(`variab',"%d" ) if regexm(`"`variab'"',"_date") 
	* }

	
	*replace textvar = textvar +`"`varname'"'+"="+strvariab+"; "
	*}
    *}
	

	noi htput
	noi htput <B><PRE><H2>Nº Query  `numquery' </H2></PRE>`mess' (`numviols' obs)</B><BR><BR> 
	

	/*[Olga] if `"`sepby'"'=="" noi htlist `vars' if `_viol==1', `d' `obs' sep(`sep')
	else noi htlist  _quest  _nquery `vars' _message action if `_viol==1', `d' `obs' sepby(`sepby')*/

	noi htlist _quest `vars' _message action  if `_viol==1', `d' `obs' 
	* noi htlist  _numquery _quest SubjID textvar _message action if  `_viol==1', `d' `obs' 
    noi li  _numquery _quest    _message action if  `_viol==1', `d' `obs' 
	local N=_N
	
	*forvalues k =1(1)`N' {
	
	*if `_viol'[`k']==1{
	

	
	*qui  post tableerror (_nquery[`k']) (_quest[`k']) (SubjID[`k'])  (textvar[`k']) (_message[`k']) (action[`k'])
	 
	* }
	*     }
	
	*htput <BR><BR>
	*drop `_viol'

} /* of if errcode not equal 0*/


if errcode==0{ 

	if "`flag'"~=""{
	gen _assertflag=0
	}
	 
}
} /* of if l equals "" */


/*4.Process if minimal output required (i.e. nol option was specified)*/

if "`l'"~=""{

cap `bypart' assert `asscond' `ifcond'
scalar define errcode=_rc
if errcode~=0{

	if `"`if'"'~="" `bypart' gen `_viol'=((`antiasscond') & (`if'))
	else `bypart' gen `_viol'= (`antiasscond')

	qui count if `_viol'
	local numviols = r(N)

	noi htput
	noi htput `Assertion `asscond' `ifcond' produced ' `numviols'  contradictions.
	noi htput

	if "`flag'"~="" {
	gen _assertflag=(`_viol'==1)
	}
	drop `_viol'

}/* of if errcode not equal 0*/


if errcode==0{ 

	if "`flag'"~=""{
	gen _assertflag=0
	}
	 
}
}/* of if l not equal "" */

cap scalar drop errcode

*postclose  tableerror
*use `results',clear 
*compress
*save `saving',replace

restore
end

