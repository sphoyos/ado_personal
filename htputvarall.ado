*! Version 1.0 Dec 2005 by LQ
*! Version 2.0 Dec 2005 by LQ, corregido problema de missings
*! Version 3.0 Apr 21 2008 by LQ, Monte Carlo permutation tests 
*! Version 4.0 Oct 07 2008 by LQ, options for regression 
*! Version 5.0 Nov 28 2008 by LQ, option adjust 


*! syntax [if] [in], 	reg(string)	Regression command (regress, logistic, cox, ...)
*!		dep(varname)	Dependent variable
*!		indep(string)	Independent variables (remember i. before qualitative variables)
*!		[rr(string)]	RR string 
*!		[per(string)]	Per increment string (the same for all quantitative variables in the list)
*!		[trans(string)]	Transformation of coef exp2 log2 VE (the same for all quantitative variables in the list)
*!		[multi]		Multivariate model
*!		[PERMute(int)]  Number of Monte Carlo permutation tests                  
*!	        [color]	        Use colored cells when p-values are <= 0.05, it only works with head option
*!		[options(string)] Options for regression command 
*!		[adjust(string)] Variables for adjusted effects 
*!		[note(string)] Add footnote 

program define htputvarall
version 9.1
syntax [if] [in], reg(string) [dep(varname)] indep(string) [rr(string)] [trans(string)] [per(string)] [multi] [PERMute(integer 0)] [color] [options(string)] [adjust(string)] [note(string)]

* write ado-file to estimate models
tempname _mymodel
file open prg_code using `_mymodel'.ado, write replace
	file write prg_code "program define `_mymodel', rclass" _n
	file write prg_code "version 9.1" _n
	file write prg_code "syntax [if] [in], cmd(string) [dep(string)] indep(string) [opt(string)] [adj(string)]" _n

	file write prg_code "xi: `""cmd""' `""dep""' `""indep""' `""adj""' `""if""' `""in""', `""opt""'" _n
	file write prg_code "tokenize " `"""' "`""indep""'" `"""' _n

	file write prg_code "while " `"""' "`""1""'" `"""' "!=" `"""'`"""'" {" _n _tab(1)
	file write prg_code "if substr(" `"""' "`""1""'" `"""' ",1,2) ==" `"""' "i." `"""' " {"  _n _tab(2)
	file write prg_code "local var = subinstr(" `"""' "`""1""'" `"""' "," `"""' "i." `"""' "," `"""'`"""' ",.)" _n _tab(2)
	file write prg_code "local var9 = substr(" `"""' "`""var""'" `"""' ",1,9)" _n _tab(2)
	file write prg_code "local dummy " `"""' "_I`""var9""'_*" `"""' _n _tab(2)
	file write prg_code "testparm `""dummy""'" _n _tab(2)
	file write prg_code "local p = r(p)" _n _tab(2)
	file write prg_code "return scalar p_`""var""'=`""p""'" _n _tab(1)
	file write prg_code "}" _n _tab(1)
	file write prg_code "else {" _n _tab(2)
	file write prg_code "testparm `""1""'" _n _tab(2)
	file write prg_code "local p = r(p)" _n _tab(2)
	file write prg_code "return scalar p_`""1""'=`""p""'" _n _tab(1)
	file write prg_code "}" _n
	file write prg_code "mac shift" _n
	file write prg_code "}" _n
	file write prg_code "end" _n
	file close prg_code
discard

* begin procedure htputvarall
tokenize "`indep'"

local deplab:variable label `dep'

* remove i. from the categorical variables 
* and create list of parameters for permute in multi option
local i=1
if ("`multi'" != "" & `permute' != 0) {
	local mult_params ""
}
while "``i''"!="" {
local v`i'=subinstr("``i''","i.","",1)
if ("`multi'" != "" & `permute' != 0) {
	local mult_params "`mult_params' `v`i''=r(p_`v`i'')"
}
local i=`i'+1
}

* adjust option for permutation
if "`adjust'" != "" local adj "adj(`adjust')"
else local adj ""

********************* crude/adjusted effects
if "`multi'" == "" {

* table head
if "`1'"!="" {
	
	count
	global HTtot `r(N)'

	* permute before model estimation
	if `permute' != 0 {
 
	version 9.1: permute `dep' `v1'=r(p_`v1'), reps(`permute') seed(20080416) left: `_mymodel' `if' `in', cmd(`reg') dep(`dep') indep(`1') opt(`options') `adj'
	matrix p = r(p)
	matrix ci = r(ci)
	}
	
	* model estimation	
	if "`1'"=="`v1'" {
		* continuous variable
		quietly xi:`reg' `dep' `1' `adjust' `if' `in', `options'
		count if e(sample)
		global HTmitot `r(N)'
		quietly testparm `1'
		local p=r(p)
	}
	else {
		* categorical variable
		quietly xi:`reg' `dep' `1' `adjust' `if' `in', `options'
		count if e(sample)
		global HTmitot `r(N)'
		local sub= substr("`v1'",1,9)
		local c="_I`sub'*"
		quietly testparm `c'
		local p=r(p)
	}

	if `permute' != 0 {
		local pcor:di p[1,colnumb("p","`v1'")]
		local lpcor:di ci[rownumb("ci","ll"),colnumb("ci","`v1'")]
		local upcor:di ci[rownumb("ci","ul"),colnumb("ci","`v1'")]

		local perm_results `"pcor(`pcor') lpcor(`lpcor') upcor(`upcor') metcor(Monte Carlo permutation test (`permute' random permutations))"'
	}	
	* making-up the table
	if "`2'"!="" {

		* more variables
		if "`1'"=="`v1'" {
		* continuous variable	
		htputvar `v1', head rr(`rr') define coef(`1') strata(1) trans(`trans') pval(`p') per(`per') `perm_results' `color'
		}
		else {
		* categorical variable

		preserve
		drop if `v1'==.
		sort `v1'
		tempvar aux
		gen `aux'=1
		collapse `aux', by("`v1'")
		local cat=_N

		local val= `v1'[1]	
		htputvar `v1', head rr(`rr') define strata(`cat') coef(baseline) val(`val') trans(`trans') pval(`p') `perm_results' `color'

		local k=2
		while `k'<=`cat' {
		local sub= substr("`v1'",1,9)
		local val= `v1'[`k']	
		local c="_I`sub'_`val'"
		htputvar `v1', strata(`cat') coef("`c'") val(`val') trans(`trans') `color'
		local k=`k'+1
		}
		restore
		}

	}
	else {
		* no more variables
		if "`1'"=="`v1'" {
		* continuous variable
		htputvar `v1', head rr(`rr') define coef(`1') strata(1) trans(`trans') pval(`p') per(`per') close `perm_results' `color'
		}
		else {
		* categorical variable

		preserve
		drop if `v1'==.
		sort `v1'
		tempvar aux
		gen `aux'=1
		collapse `aux', by("`v1'")
		local cat=_N

		local val= `v1'[1]
		htputvar `v1', head rr(`rr') define strata(`cat') coef(baseline) val(`val') trans(`trans') pval(`p') `perm_results' `color'

		local k=2
		while `k'<`cat' {
		local sub= substr("`v1'",1,9)
		local val= `v1'[`k']	
		local c="_I`sub'_`val'"
		htputvar `v1', strata(`cat') coef("`c'") val(`val') trans(`trans') `color'
		local k=`k'+1
		}
		local sub= substr("`v1'",1,9)
		local val= `v1'[`cat']	
		local c="_I`sub'_`val'"
		local val= `v1'[`cat']	
		htputvar `v1', strata(`cat') coef("`c'") val(`val') trans(`trans') `color' close 
		restore
		}
	}
}

* rest of the table
local i=2
local j=3
while "``i''"!="" {

	* permute before model estimation
	if `permute' != 0 {
	version 9.1: permute `dep' `v`i''=r(p_`v`i''), reps(`permute') seed(20080416) left: `_mymodel' `if' `in', cmd(`reg') dep(`dep') indep(``i'') opt(`options') `adj'
	matrix p = r(p)
	matrix ci = r(ci)
	}

	* model estimation	
	if "``i''"=="`v`i''" {
		* continuous variable
		quietly xi:`reg' `dep' ``i'' `adjust' `if' `in', `options'
		count if e(sample)
		global HTmitot `r(N)'
		quietly testparm ``i''
		local p=r(p)
	}
	else {
		* categorical variable
		quietly xi:`reg' `dep' ``i'' `adjust' `if' `in', `options'
		count if e(sample)
		global HTmitot `r(N)'
		local sub= substr("`v`i''",1,9)
		local c="_I`sub'*"
		quietly testparm `c'
		local p=r(p)
	}

	if `permute' != 0 {
		local pcor:di p[1,colnumb("p","`v`i''")]
		local lpcor:di ci[rownumb("ci","ll"),colnumb("ci","`v`i''")]
		local upcor:di ci[rownumb("ci","ul"),colnumb("ci","`v`i''")]

		local perm_results `"pcor(`pcor') lpcor(`lpcor') upcor(`upcor') metcor(Monte Carlo permutation test (`permute' random permutations))"'
	}

	* making-up the table
	if "``j''"!="" {
		* more variables
		if "``i''"=="`v`i''" {
		* continuous variable	
		htputvar `v`i'', define coef(``i'') strata(1) trans(`trans') pval(`p') per(`per') `perm_results' `color'
		}
		else {
		* categorical variable

		preserve
		drop if `v`i''==.
		sort `v`i''
		tempvar aux
		gen `aux'=1
		collapse `aux', by("`v`i''")
		local cat=_N

		local val= `v`i''[1]
		htputvar `v`i'', define strata(`cat') coef(baseline) val(`val') trans(`trans') pval(`p') `perm_results' `color'

		local k=2
		while `k'<=`cat' {
		local sub= substr("`v`i''",1,9)
		local val= `v`i''[`k']	
		local c="_I`sub'_`val'"
		htputvar `v`i'', strata(`cat') coef("`c'") val(`val') trans(`trans') `color'
		local k=`k'+1
		}
		restore
		}
	}
	else {
		* no more variables
		if "``i''"=="`v`i''" {
		* continuous variable
		htputvar `v`i'', define coef(``i'') trans(`trans') strata(1) pval(`p') per(`per') close `perm_results' `color'
		}
		else {
		* categorical variable

		preserve
		drop if `v`i''==.
		sort `v`i''
		tempvar aux
		gen `aux'=1
		collapse `aux', by("`v`i''")
		local cat=_N

		local val= `v`i''[1]
		htputvar `v`i'', define strata(`cat') coef(baseline) val(`val') trans(`trans') pval(`p') `perm_results' `color'

		local k=2
		while `k'<`cat' {
		local sub= substr("`v`i''",1,9)
		local val= `v`i''[`k']	
		local c="_I`sub'_`val'"
		htputvar `v`i'', strata(`cat') coef("`c'") val(`val') trans(`trans') `color'
		local k=`k'+1
		}
		local sub= substr("`v`i''",1,9)
		local val= `v`i''[`cat']	
		local c="_I`sub'_`val'"
		local val= `v`i''[`cat']	
		htputvar `v`i'', strata(`cat') coef("`c'") val(`val') trans(`trans') `color' close
		restore
		}
	}

local i=`i'+1
local j=`j'+1
}

}

********************* multivariate effects (multi option)
else {

* table head
if "`1'"!="" {

	* permute before model estimation
	if `permute' != 0 {
	version 9.1: permute `dep' `mult_params', reps(`permute') seed(20080416) left: `_mymodel' `if' `in', cmd(`reg') dep(`dep') indep(`indep') opt(`options') `adj'
	matrix p = r(p)
	matrix ci = r(ci)
	}

	* model estimation	
	if "`1'"=="`v1'" {
		* continuous variable
		quietly xi:`reg' `dep' `indep' `adjust' `if' `in', `options'
		count if e(sample)
		global HTmitot `r(N)'
		global HTtot $HTmitot
		htput Number of obs = $HTmitot

		quietly testparm `1'
		local p=r(p)
	}
	else {
		* categorical variable
		quietly xi:`reg' `dep' `indep' `adjust' `if' `in', `options'
		count if e(sample)
		global HTmitot `r(N)'
		global HTtot $HTmitot
		htput Number of obs = $HTmitot

		local sub= substr("`v1'",1,9)
		local c="_I`sub'*"
		quietly testparm `c'
		local p=r(p)
	}


	if `permute' != 0 {
		local pcor:di p[1,colnumb("p","`v1'")]
		local lpcor:di ci[rownumb("ci","ll"),colnumb("ci","`v1'")]
		local upcor:di ci[rownumb("ci","ul"),colnumb("ci","`v1'")]

		local perm_results `"pcor(`pcor') lpcor(`lpcor') upcor(`upcor') metcor(Monte Carlo permutation test (`permute' random permutations))"'
	}	

	* making-up the table
	if "`2'"!="" {
		* more variables
		if "`1'"=="`v1'" {
		* continuous variable	
		htputvar `v1', head rr(`rr') define coef(`1') trans(`trans') strata(1) pval(`p') per(`per') `perm_results' `color'
		}
		else {
		* categorical variable

		preserve
		drop if `v1'==.
		sort `v1'
		tempvar aux
		gen `aux'=1
		collapse `aux', by("`v1'")
		local cat=_N

		local val= `v1'[1]	
		htputvar `v1', head rr(`rr') define strata(`cat') coef(baseline) val(`val') trans(`trans') pval(`p') `perm_results' `color'

		local k=2
		while `k'<=`cat' {
		local sub= substr("`v1'",1,9)
		local val= `v1'[`k']	
		local c="_I`sub'_`val'"
		htputvar `v1', strata(`cat') coef("`c'") val(`val') trans(`trans') `color'
		local k=`k'+1
		}
		restore
		}
	}
	else {
		* no more variables
		if "`1'"=="`v1'" {
		* continuous variable
		htputvar `v1', head rr(`rr') define coef(`1') trans(`trans') strata(1) pval(`p') per(`per') close `perm_results' `color'
		}
		else {
		* categorical variable

		preserve
		drop if `v1'==.
		sort `v1'
		tempvar aux
		gen `aux'=1
		collapse `aux', by("`v1'")
		local cat=_N

		local val= `v1'[1]
		htputvar `v1', head rr(`rr') define strata(`cat') coef(baseline) val(`val') trans(`trans') pval(`p') `perm_results' `color'

		local k=2
		while `k'<`cat' {
		local sub= substr("`v1'",1,9)
		local val= `v1'[`k']	
		local c="_I`sub'_`val'"
		htputvar `v1', strata(`cat') coef("`c'") val(`val') trans(`trans') `color'
		local k=`k'+1
		}
		local sub= substr("`v1'",1,9)
		local val= `v1'[`cat']	
		local c="_I`sub'_`val'"
		local val= `v1'[`cat']	
		htputvar `v1', strata(`cat') coef("`c'") val(`val') trans(`trans') `color' close
		restore
		}
	}
}

* rest of the table
local i=2
local j=3
while "``i''"!="" {

	* model estimation	
	if "``i''"=="`v`i''" {
		* continuous variable
		quietly xi:`reg' `dep' `indep' `adjust' `if' `in', `options'
		quietly testparm ``i''
		local p=r(p)
	}
	else {
		* categorical variable
		quietly xi:`reg' `dep' `indep' `adjust' `if' `in', `options'
		local sub= substr("`v`i''",1,9)
		local c="_I`sub'*"
		quietly testparm `c'
		local p=r(p)
	}

	if `permute' != 0 {
		local pcor:di p[1,colnumb("p","`v`i''")]
		local lpcor:di ci[rownumb("ci","ll"),colnumb("ci","`v`i''")]
		local upcor:di ci[rownumb("ci","ul"),colnumb("ci","`v`i''")]

		local perm_results `"pcor(`pcor') lpcor(`lpcor') upcor(`upcor') metcor(Monte Carlo permutation test (`permute' random permutations))"'
	}

	* making-up the table
	if "``j''"!="" {
		* more variables
		if "``i''"=="`v`i''" {
		* continuous variable	
		htputvar `v`i'', define coef(``i'') trans(`trans') strata(1) pval(`p') per(`per') `perm_results' `color'
		}
		else {
		* categorical variable

		preserve
		drop if `v`i''==.
		sort `v`i''
		tempvar aux
		gen `aux'=1
		collapse `aux', by("`v`i''")
		local cat=_N

		local val= `v`i''[1]
		htputvar `v`i'', define strata(`cat') coef(baseline) val(`val') trans(`trans') pval(`p') `perm_results' `color'

		local k=2
		while `k'<=`cat' {
		local sub= substr("`v`i''",1,9)
		local val= `v`i''[`k']	
		local c="_I`sub'_`val'"
		htputvar `v`i'', strata(`cat') coef("`c'") val(`val') trans(`trans') `color'
		local k=`k'+1
		}
		restore
		}
	}
	else {
		* no more variables
		if "``i''"=="`v`i''" {
		* continuous variable
		htputvar `v`i'', define coef(``i'') trans(`trans') strata(1) pval(`p') per(`per') close `perm_results' `color'
		}
		else {
		* categorical variable

		preserve
		drop if `v`i''==.
		sort `v`i''
		tempvar aux
		gen `aux'=1
		collapse `aux', by("`v`i''")
		local cat=_N

		local val= `v`i''[1]
		htputvar `v`i'', define strata(`cat') coef(baseline) val(`val') trans(`trans') pval(`p') `perm_results' `color'

		local k=2
		while `k'<`cat' {
		local sub= substr("`v`i''",1,9)
		local val= `v`i''[`k']	
		local c="_I`sub'_`val'"
		htputvar `v`i'', strata(`cat') coef("`c'") val(`val') trans(`trans') `color'
		local k=`k'+1
		}
		local sub= substr("`v`i''",1,9)
		local val= `v`i''[`cat']	
		local c="_I`sub'_`val'"
		local val= `v`i''[`cat']	
		htputvar `v`i'', strata(`cat') coef("`c'") val(`val') trans(`trans') `color' close
		restore
		}
	}

local i=`i'+1
local j=`j'+1
}

}

* add note for adjust variables
if "`adjust'" != "" {
	local adjnote ""
	tokenize "`adjust'"

	local i = 1
	while "``i''"!="" {
		local j = `i' + 1
		local var = subinstr("``i''","i.","",1)
		local lab: variable label `var'
		if "`lab'" == "" local lab "`var'"
		if "`adjnote'" == "" local adjnote "Adjusted by `lab'"
		else {
			if "``j''" != "" local adjnote "`adjnote', `lab'"
			else local adjnote "`adjnote' and `lab'"
		}
		local i = `i' + 1	
	}
htput `adjnote'<BR>
}

* add additional note
htput `note'

macro drop HTtot HTmitot

erase `_mymodel'.ado
end
