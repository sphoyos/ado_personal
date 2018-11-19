*! version 1.2.11 27apr2015 by alexis dot dinno at pdx dot edu
*! perform Dunn's test of multiple comparisons using rank sums

********************************************************************************
* Syntax:  dunntest varname [if exp] [in range] , by(groupvar) ma(method) 
*          [ nokwallis nolabel wrap rmc level(#) ]

program define ht_dunntest

  if int(_caller())<8 {
    di in r "dunntest- does not support this version of Stata." _newline
    di as txt "Requests for a version compatible with versions of STATA earlier than v8 are "
    di as txt "untenable since I do not have access to the software." _newline 
    di as txt "All requests are welcome and will be considered."
    exit
  }
   else dunntest8 `0'
end


program define dunntest8, rclass sort
  version 8
  syntax varname [if] [in], BY(varname) [ma(string) NOKWALLIS NOLABEL WRAP RMC /*
*/               level(cilevel)]
 

  * Validate nolabel and labels
  local labelname : value label `by'
  if "`labelname'" == "" { 
    di as res _newline "Warning: by() values are unlabeled, option nolabel implicit" _newline
    local nolabel = "nolabel"
    }
    
        

  * Validate ma
  if (lower("`ma'") == "" ) {
    local ma = "none"
    }
  if (lower("`ma'") != "none" & lower("`ma'") != "bonferroni" & lower("`ma'") != "sidak" & lower("`ma'") != "holm" & lower("`ma'") != "hs" & lower("`ma'") != "hochberg" & lower("`ma'") != "bh" & lower("`ma'") != "by") {
    noi: di as err "option ma() must be one of none, bonferroni, sidak, hs, hochberg, bh, or by"
    exit 198
    }
  if (lower("`ma'")=="none") {
    local Name = "No adjustment"
    }
  if (lower("`ma'")=="bonferroni") {
    local Name = "Bonferroni"
    }
  if (lower("`ma'")=="sidak") {
    local Name = "Sid{c a'}k"
    }
  if (lower("`ma'")=="holm") {
    local Name = "Holm"
    }
  if (lower("`ma'")=="hs") {
    local Name = "Holm-Sid{c a'}k"
    }
  if (lower("`ma'")=="hochberg") {
    local Name = "Hochberg"
    }
  if (lower("`ma'")=="bh") {
    local Name = "Benjamini-Hochberg"
    }
  if (lower("`ma'")=="by") {
    local Name = "Benjamini-Yekuteili"
    }
  
  if (lower("`nokwallis'")=="") {
    kwallis `varlist' `if' `in', by(`by')
    }
  if "`nolabel'" == "" {
    preserve
      qui: egen __blocks = group(`by') `if' `in'
      qui: tab __block `if' `in', nofreq
      local b = r(r)
      mata: st_vlload("`labelname'", values = ., text = "")
      mata: st_local("testvalue", strofreal(length(values)))
      if ("`testvalue'"!="`b'") {
        di as res _newline "Warning: by() values are incompletely labeled, option nolabel implicit" _continue
        local nolabel = "nolabel"
        }
      restore
    }
  if (lower("`nokwallis'")!="") {
    qui: kwallis `varlist' `if' `in', by(`by')
    }
    local chi2  = r(chi2)
    local df = r(df)
    local chi2_adj = r(chi2_adj)

  preserve
    qui: drop if `varlist' == .
    qui: drop if `by' == .
    if "`if'" != "" {
      qui: keep `if'
      }
    if  "`in'" != "" {
      qui: keep `in'
      }
    generate __order = _n
    di
    egen ranks = rank(`varlist')
    by `by', sort: egen __meanrank = sum(ranks)
    qui: tab __meanrank
    egen __group = group(`by')
    su __group, meanonly
    local k = r(max)
    drop __group
    local m = `k'*(`k'-1)/2
    if (c(matsize) < `m') {
      set matsize `m'
      }
    local N = r(N)
    egen __group = group(`by')
    su __group, meanonly
    forvalues i = 1/`r(max)' {
      qui: tabstat ranks if __group == `i', s(mean) save
      local meanrank`i' = el(r(StatTot),1,1)
      qui: count if __group == `i'
      local n`i' = r(N)
      }
    local var_mA = `N'*(`N'+1)/12

    by ranks, sort: gen __totalrankvalues = _n == 1 
    qui: count if __totalrankvalues
    local totalrankvalues = r(N)

    by ranks, sort: egen __uniques = max(_n)
    by ranks, sort: gen __counts = _n
    qui: count if __uniques==1
    local uniques = r(N)

    local var_mB = 0
    local nminusone = `N' - 1
    forvalues i = 1/`nminusone' {
      if (__counts[`i']==1 & __counts[`i'+1]==2) {
        qui: count if ranks == ranks[`i'] 
        local nties = r(N)
        local var_mB = `var_mB' + (((`nties'^3) - `nties')/(12*(`N'-1)))
        }
      }
    drop __totalrankvalues __uniques
    qui: sort __order

    di _newline as txt %~76s "Dunn's Pairwise Comparison of `varlist' by `by'"
    di as txt %~76s "(`Name')"
    local colhead = ""
    local kminusone = `k'-1
    capture confirm numeric variable `by'
    if (!_rc) {
      local groupisnum = 1
      }
     else {
      local groupisnum = 0
      }
    qui: levelsof `by'
    local groupvalues = r(levels)
    matrix Z = J(1,`m',0)
    matrix P = J(1,`m',0)
    forvalues i = 2/`k' {
      local iminusone = `i'-1
      forvalues j = 1/`iminusone' {
        if "`rmc'" == "" {
          local z = (`meanrank`j''-`meanrank`i'')/sqrt((`var_mA' - `var_mB')*(1/`n`j'' + 1/`n`i''))
          }
        if "`rmc'" != "" {
          local z = (`meanrank`i''-`meanrank`j'')/sqrt((`var_mA' - `var_mB')*(1/`n`j'' + 1/`n`i''))
          }
        local p = 1 - normal(abs(`z'))
        * Static multiple comparisons adjustments
        if (lower("`ma'") == "bonferroni") {
          local p = min(1,`p'*`m')
          }
        if (lower("`ma'") == "sidak") {
          local p = min(1,1 - (1-`p')^`m')
          }
        local index = ((`i'-2)*(`i'-1)/2) + `j'
        matrix Z[1,`index'] = `z'
        matrix P[1,`index'] = `p'
        }
      }
    * Sequential multiple comparrisons adjustments
    quietly {
      local alpha = (100 - `level')/100
      matrix Reject = J(1,`m',0)
      if (lower("`ma'")=="holm") {
        mata: Ps = st_matrix("P")\range(1,`m',1)'\rangen(0,0,`m')'
        mata: Psort = sort(Ps',1)'
        forvalues i = 1/`m' {
          local adjust = `m'+1-`i'
          mata: Psort[1,`i'] = min((1,Psort[1,`i'] :*`adjust'))
          if (`i' == 1) {
            mata: Psort[3,`i'] = Psort[1,`i'] <= `alpha'/2
            }
          if (`i' > 1) {
            mata: Psort[3,`i'] = (Psort[1,`i'] <= `alpha'/2) & Psort[3,`i'-1] != 0
            }
          }
        mata: Psort = sort(Psort',2)'
        mata: st_matrix("P",Psort[1,])
        mata: st_matrix("Reject",Psort[3,])
        }
      if (lower("`ma'")=="hs") {
        mata: Ps = st_matrix("P")\range(1,`m',1)'\rangen(0,0,`m')'
        mata: Psort = sort(Ps',1)'
        forvalues i = 1/`m' {
          local adjust = `m'+1-`i'
          mata: Psort[1,`i'] = min((1,(1 - ((1 - Psort[1,`i']) ^`adjust'))))
          if (`i' == 1) {
            mata: Psort[3,`i'] = Psort[1,`i'] <= `alpha'/2
            }
          if (`i' > 1) {
            mata: Psort[3,`i'] = (Psort[1,`i'] <= `alpha'/2) & Psort[3,`i'-1] != 0
            }
          }
        mata: Psort = sort(Psort',2)'
        mata: st_matrix("P",Psort[1,])
        mata: st_matrix("Reject",Psort[3,])
        }
      if (lower("`ma'")=="hochberg") {
        mata: Ps = st_matrix("P")\range(1,`m',1)'\rangen(0,0,`m')'
        mata: Psort = sort(Ps',-1)'
        forvalues i = 1/`m' {
          local adjust = `i'
          mata: Psort[1,`i'] = min((1,Psort[1,`i'] :*`adjust'))
          if (`i' == 1) {
            mata: Psort[3,`i'] = Psort[1,`i'] <= `alpha'/2
            }
          if (`i' > 1) {
            mata: Psort[3,`i'] = (Psort[1,`i'] <= `alpha'/2) | Psort[3,`i'-1] == 1
            }
          }
        mata: Psort = sort(Psort',2)'
        mata: st_matrix("P",Psort[1,])
        mata: st_matrix("Reject",Psort[3,])
        }
      if (lower("`ma'")=="bh") {
        mata: Ps = st_matrix("P")\range(1,`m',1)'\rangen(0,0,`m')'
        mata: Psort = sort(Ps',-1)'
        forvalues i = 1/`m' {
          local adjust = (`m'/(`m'+1-`i'))
          mata: Psort[1,`i'] = min((1,Psort[1,`i'] :*`adjust'))
          if (`i' == 1) {
            mata: Psort[3,`i'] = Psort[1,`i'] <= `alpha'/2
            }
          if (`i' > 1) {
            mata: Psort[3,`i'] = (Psort[1,`i'] <= `alpha'/2) | Psort[3,`i'-1] == 1
            }
          }
        mata: Psort = sort(Psort',2)'
        mata: st_matrix("P",Psort[1,])
        mata: st_matrix("Reject",Psort[3,])
        }
      if (lower("`ma'")=="by") {
        mata: Ps = st_matrix("P")\range(1,`m',1)'\rangen(0,0,`m')'
        mata: Psort = sort(Ps',-1)'
        local adjustB = 0
        forvalues i=1/`m' {
          local C = `C' + 1/`i'
          }
        forvalues i = 1/`m' {
          local adjust = (`m'/(`m'+1-`i')) * `C'
          mata: Psort[1,`i'] = min((1,Psort[1,`i'] :* `adjust' ))
          if (`i' == 1) {
            mata: Psort[3,`i'] = Psort[1,`i'] <= `alpha'/2
            }
          if (`i' > 1) {
            mata: Psort[3,`i'] = (Psort[1,`i'] <= `alpha'/2) | Psort[3,`i'-1] == 1
            }
          }
        mata: Psort = sort(Psort',2)'
        mata: st_matrix("P",Psort[1,])
        mata: st_matrix("Reject",Psort[3,])
        }
      }

********************************************************************************
*** OUTPUT
********************************************************************************
  * Need to determine how many tables (reps) to output
  local reps = int((`k'-1)/6)
  local laststart = `k' - mod(`k',6) + 1
  local kminusone = `k' - 1

  * Replication loop for >7 groups, no wrap
  if ("`wrap'"=="") {
    if (`k' > 7) {
      forvalues rep = 1/`reps' {
        local colstart = (6*`rep')-5
        local colstop  = 6*`rep'
        _dunntestheader `by' `colstart' `colstop' "`nolabel'" `groupisnum' "`rmc'"
        * Table body
        local start = `colstart'+1
        forvalues i = `start'/`k' {
          local colstop = min(`i'-1,6*`rep')
          _dunntestztable `by' `i' Z `colstart' `colstop' "`nolabel'" `groupisnum' 
          if (`i' < `k') {
            _dunntestptable `i' P `colstart' `colstop' Reject 0
            }
           else {
            _dunntestptable `i' P `colstart' `colstop' Reject 1
            }
          }
        }
      * End of table
      if (`laststart' < `k') {
        _dunntestheader `by' `laststart' `kminusone' "`nolabel'" `groupisnum' "`rmc'"
        * Table body
        local start = `laststart'+1
        forvalues i = `start'/`k' {
          _dunntestztable `by' `i' Z `laststart' `kminusone' "`nolabel'" `groupisnum' 
          if (`i' < `k') {
            _dunntestptable `i' P `laststart' `kminusone' Reject 0
            }
           else {
            _dunntestptable `i' P `laststart' `kminusone' Reject 1
            }
          }
        }
      }

    * Replication loop for <=7 groups
    if (`k' <= 7) {
      _dunntestheader `by' 1 `kminusone' "`nolabel'" `groupisnum' "`rmc'"
      * Table body
      forvalues i = 2/`k' {
        local colstop  = `i'-1
        _dunntestztable `by' `i' Z 1 `colstop' "`nolabel'" `groupisnum' 
        if (`i' < `k') {
          _dunntestptable `i' P 1 `colstop' Reject 0
          }
         else {
          _dunntestptable `i' P 1 `colstop' Reject 1
          }
        }
      }
    }

  * Replication loop for >7 groups, with wrap
  if ("`wrap'"=="wrap") {
    _dunntestheader `by' 1 `kminusone' "`nolabel'" `groupisnum' "`rmc'"
    * Table body
    forvalues i = 2/`k' {
      local colstop  = `i'-1
      _dunntestztable `by' `i' Z 1 `colstop' "`nolabel'" `groupisnum' 
      if (`i' < `k') {
        _dunntestptable `i' P 1 `colstop' Reject 0
        }
       else {
        _dunntestptable `i' P 1 `colstop' Reject 1
        }
      }
    }

  restore

  * Saves
  ret matrix P        = P
  ret matrix Z        = Z
  ret scalar df       = `df'
  ret scalar chi2_adj = `chi2_adj'

end

program define _dunntestheader
/*
    This program displays Dunn's test table headers.

    Syntax:
            _dunntestheader groupvar colstart colstop nolabel groupisnum
*/
  args groupvar colstart colstop nolabel groupisnum rmc

  if "`rmc'" == "" {
    di as txt "Col Mean-{c |}"
    di as txt "Row Mean {c |}" _continue
    }
   else {
    di as txt "Row Mean-{c |}"
    di as txt "Col Mean {c |}" _continue
    }
  qui: levelsof `groupvar'
  local groupvalues = r(levels)
  forvalues col = `colstart'/`colstop' {
    if (lower("`nolabel'")=="") {
      local labelname : value label `groupvar'
      if ("`labelname'") != "" {
        local vallab = substr("`:label `labelname' `:word `col' of `groupvalues'''",1,8)
        local pad = 8-length("`vallab'")
        local colhead = "{dup `pad': }`vallab'"
        if ("`vallab'" == "") {
          local colhead = ""
          local nolabel = "nolabel"
          }
        }
       else {
        local nolabel = "nolabel"
        }
      }
    if (lower("`nolabel'")=="nolabel") {
      if (`groupisnum' == 0) {
        local pad = max(0,8-length(substr(`groupvar'[`col'],1,8)))
        local colhead = "   {dup `pad': }"+ substr(`groupvar'[`col'],1,8)
        }
      if (`groupisnum' == 1) {
        local pad = max(0,8-length(substr(word("`groupvalues'",`col'),1,8)))
        local colhead = "   {dup `pad': }"+ substr(word("`groupvalues'",`col'),1,8)
        }
      }
    di as txt "   " %8s "`colhead'" _continue
    }
  di  
  local separatorlength = 10 + 11*(`colstop'-`colstart')+1
  di as txt "{hline 9}{c +}{hline `separatorlength'}"
      
  end
  
program define _dunntestztable
/*
    This program displays Dunn's test z values.

    Syntax:
            _dunntestztable groupvar index Z colstart colstop nolabel groupisnum
*/
  args groupvar index Z colstart colstop nolabel groupisnum

  qui: levelsof `groupvar'
  local groupvalues = r(levels)

  * Validate labels
    if (lower("`nolabel'")=="") {
     local testlab: label (`groupvar') `colstart' 8, strict
     if ("`testlab'" == "") {
       local nolabel = "nolabel"
       }
     }

  * Row headers
  if (lower("`nolabel'")=="") {
    local labelname : value label `groupvar'
    if ("`labelname'") != "" {
      local vallab = substr("`:label `labelname' `:word `index' of `groupvalues'''",1,8)
      di as txt %8s "`vallab'" " {c |}"   _continue
      }
     else {
      local nolabel = "nolabel"
      }
    }
  if (lower("`nolabel'")=="nolabel") {
    if (`groupisnum' == 0) {
      di as txt %8s substr(`groupvar'[`index'],1,8) " {c |}"   _continue
      }
    if (`groupisnum' == 1) {
      di as txt %8s substr(word("`groupvalues'",`index'),1,8) " {c |}"   _continue
      }
    }

  * Table z entries
  forvalues i = `colstart'/`colstop' {
    di as res "  " %9.6f el(matrix(`Z'),1,((`index'-2)*(`index'-1))/2 + `i') _continue
    }
  di
  end


program define _dunntestptable
/*
    This program displays Dunn's test p values.

    Syntax:
            _dunntestptable index P colstart colstop Reject last
*/
  args index P colstart colstop Reject last

  * Blank row header
    di as txt "         {c |}" _continue

  * Table p entries
  forvalues i = `colstart'/`colstop' {
    if ( el(`Reject',1,((`index'-2)*(`index'-1))/2 + `i') == 0) {
      di as res "     " %6.4f el(matrix(`P'),1,((`index'-2)*(`index'-1))/2 + `i') _continue
      }
     else {
      di as res "     {ul on}" %6.4f el(matrix(`P'),1,((`index'-2)*(`index'-1))/2 + `i') "{ul off}" _continue
      }
    }
  di

  * Close out with another blank row header
  if (`last' == 0) {
    di as txt "         {c |}"
    }
  end
