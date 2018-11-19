
capture program drop ht_concord
*! version 2.1.6 18jun1998  TJS & NJC  STB-45 sg84.1
*  version 2.0.2  6mar1998  TJS & NJC  STB-43 sg84

program define ht_concord
 version 5.0

* Syntax help

 if "`1'" == "" {
   di _n in gr "Syntax is:" _n
   di in wh "  concord" in gr " var1 var2 [weight] [" _c
   di in wh "if" in gr " exp] [" in wh "in" in gr " range]"
   di in gr "         [ " in wh ", s" in gr "ummary" in wh " g" _c
   di in gr "raph" in wh "(c" in gr "cc|" in wh "l" in gr "oa" _c
   di in wh ") snd(" in gr "sndvar[" in wh ", replace" in gr "]" in wh ")"
   di in wh "             noref reg by(" in gr "byvar" in wh ") l" _c
   di in gr "evel" in wh "(" in gr "level" in wh ") " _c
   di in gr "graph_options ]"
   exit
 }

* Setup

 local if "opt"
 local in "opt"
 local weight  "fweight opt"
 local options "Summary Graph(str) Level(real 95) BY(str)"
 local options "`options' noREF REG SND(str) *"
 local varlist "req ex min(2) max(2)"
 parse "`*'"
 parse "`varlist'", parse(" ")

* Set temporary names

 tempvar d db dll dul m touse byg kk bylabel
 tempname dv zv k xb yb sx2 sy2 r rp sxy p u sep z zp
 tempname t set ll ul llt ult zt ztp sd1 sd2 sl

* Set up wgt and what to use

 if "`weight'" != "" { local wgt "[`weight'`exp']" }
 mark `touse' `if' `in' `wgt'
 markout `touse' `varlist'

* Generate CI z-value and label from Level()

 if `level' < 1 { local level = `level' * 100 }
 scalar `zv' = -1 * invnorm((1 - `level' / 100) / 2)
 local rl = `level'
 local level : di %7.0g `level'
 local level = ltrim("`level'")

* Generate BY groups

 if "`by'" != "" { confirm var `by' }
 sort `touse' `by'
 qui by `touse' `by': gen byte `byg' = _n == 1 if `touse'
 if "`by'" != "" { qui gen `kk' = _n if `byg' == 1 }
 qui replace `byg' = sum(`byg')
 local byn = `byg'[_N]

* Generate `by' labels -- if required

 if "`by'" != "" {
   capture decode `by', gen(`bylabel')
   if _rc != 0 {
     local type : type `by'
     qui gen `type' `bylabel' = `by'
   }
 }

 if "`graph'" != "" {local graph = lower(substr("`graph'",1,1))}

* Do calculations

 local j = 1
 while `j' <= `byn' {  /* start of loop for each `by' group */
   di
   if "`by'" != "" {
     sort `kk'
     di in bl "--> `by' == " `bylabel'[`j']
     local byl : di "--> `by' == " `bylabel'[`j']
   }

* LOA (Bland & Altman) calculations

   qui gen  `d'   = `2' - `1'
   qui summ `d' if `byg' == `j' `wgt'
   qui gen  `db'  = _result(3)
   scalar   `dv'  = _result(4)
   qui gen  `dll' = `db' - `zv' * sqrt(`dv')
   qui gen  `dul' = `db' + `zv' * sqrt(`dv')
   qui gen  `m'   = (`1' + `2') / 2
   if "`reg'" == "reg" {
     tempvar fit
     qui regr `d' `m' if `byg' == `j' `wgt'
     qui predict `fit'
   }

* Concordance calculations

   qui summ `1' if `byg' == `j' `wgt'
   scalar `k'   = _result(1)
   scalar `yb'  = _result(3)
   scalar `sy2' = _result(4) * (`k' - 1) / `k'
   scalar `sd1' = sqrt(_result(4))

   qui summ `2' if `byg' == `j' `wgt'
   scalar `xb'  = _result(3)
   scalar `sx2' = _result(4) * (`k' - 1) / `k'
   scalar `sd2' = sqrt(_result(4))

   qui corr `1' `2' if `byg' == `j' `wgt'
   scalar `r'   = _result(4)
   scalar `sl'  = sign(`r') * `sd1' / `sd2'
   #delimit ;
   scalar `rp'  = min(tprob(_result(1) - 2, _result(4) *
     sqrt(_result(1) -  2) / sqrt(1 - _result(4)^2)) ,1) ;
   #delimit cr
   scalar `sxy' = `r' * sqrt(`sx2' * `sy2')
   scalar `p'   = 2 * `sxy' / (`sx2' + `sy2' + (`yb' - `xb')^2)
   scalar `u'   = (`yb' - `xb') / (`sx2' * `sy2')^.25

* --- variance, test, and CI for asymptotic normal approximation

   #delimit ;
   scalar `sep' = sqrt(((1 - ((`r')^2)) * (`p')^2 * (1 -
     ((`p')^2)) / (`r')^2 + (4 * (`p')^3 * (1 - `p') * (`u')^2
     / `r') - 2 * (`p')^4 * (`u')^4 / (`r')^2 ) / (`k' - 2));
   #delimit cr
   scalar `z'  = `p' / `sep'
   scalar `zp' = 2 * (1 - normprob(abs(`z')))
   scalar `ll' = `p' - `zv' * `sep'
   scalar `ul' = `p' + `zv' * `sep'

* --- statistic, variance, test, and CI for inverse hyperbolic
*      tangent transform to improve asymptotic normality

   scalar `t'   = ln((1 + `p') / (1 - `p')) / 2
   scalar `set' = `sep' / (1 - ((`p')^2))
   scalar `zt'  = `t' / `set'
   scalar `ztp' = 2 * (1 - normprob(abs(`zt')))
   scalar `llt' = `t' - `zv' * `set'
   scalar `ult' = `t' + `zv' * `set'
   scalar `llt' = (exp(2 * `llt') - 1) / (exp(2 * `llt') + 1)
   scalar `ult' = (exp(2 * `ult') - 1) / (exp(2 * `ult') + 1)

* Print output

   di in gr "Concordance correlation coefficient (Lin, 1989)" _n
   di in gr " rho_c   SE(rho_c)   Obs    [" _c
   if index("`level'",".") {
       di in gr %6.1f `level' "% CI  ]     P        CI type"
   }
   else { di in gr "   `level'% CI   ]     P        CI type" }
   di in gr _dup(63) "-"
   di in ye %6.3f `p' %10.3f `sep' %8.0f `k' %10.3f `ll' _c
   di in ye %7.3f `ul' %9.3f `zp' in gr "   asymptotic"
   di in ye _dup(24) " " %10.3f `llt' %7.3f `ult' %9.3f `ztp' _c
   di in gr "  z-transform"

   di _n in gr "Pearson's r =" in ye %7.3f `r' _c
   di    in gr "  Pr(r = 0) =" in ye %6.3f `rp' _c
   di    in gr "  C_b = rho_c/r =" in ye %7.3f `p' / `r'
   di    in gr "Reduced major axis:   Slope = " in ye %9.3f `sl' _c
   di    in gr "   Intercept = " in ye %9.3f `yb'-`xb'*`sl'

   local dl  = length("Difference (`2' - `1')")
   local sk1 = 32 - `dl'
   local sk  = min(int(`sk1' / 2), 3)
   local sk1 = `sk1' - `sk'
   di _n in gr _sk(`sk') "Difference (`2' - `1')" _sk(`sk1') _c
   if index("`level'", ".") {
     di in gr %6.1f `level' "% Limits Of Agreement"
   }
   else { di in gr "   `level'% Limits Of Agreement" }
   di in gr "   Average    Std. Dev.             (Bland & Altman, 1986)"
   di in gr _dup(63) "-"
   di in ye %10.3f `db' %12.3f sqrt(`dv') _c
   di in ye "            " %11.3f `dll' %11.3f `dul'

   if "`summary'" != "" { summ `1' `2' if `byg' == `j' `wgt' }

   
 *** print   ht_output *******
   local level="95"
   
 htput <TABLE BORDER=1 CELLSPACING =0 CELlPADDING=2>
 htput <TR>
 
 htput <TH VALIGN=CENTER ALIGN=CENTER COLSPAN =6  > Concordance correlation coefficient (Lin, 1989) </TH>
 htput </TR>
 
 htput <TR>
 htput <TH ALIGN=CENTER> rho_c </TH>
 htput <TH ALIGN=CENTER> SE(rho_c) </TH>
 htput <TH ALIGN=CENTER> Obs  <T/H>
  if index("`level'",".") {
     local ic:  di  %6.1f "[" `level' "% CI ]"
   }
   else { 
   local ic: di  " [ `level'% CI ]"
   }
 htput <TH ALIGN=CENTER> `ic' </TH>
 htput <TH ALIGN=CENTER> P value </TH>
 htput <TH ALIGN=CENTER> CI type </TH>
 htput </TR>	  

 htput <TR>
 
 
 local p1: di %6.3f `p'
 local p2: di %10.3f `sep'
 local p3: di %8.0f `k'
 local p4: di "[" %10.3f `ll' ";"  %7.3f `ul' "]"
 local p5: di %9.3f `zp'
 
 htput <TD ALIGN=CENTER> `p1' </TD> 
 htput <TD ALIGN=CENTER> `p2' </TD> 
 htput <TD ALIGN=CENTER > `p3' </TD> 
 htput <TD ALIGN=CENTER>  `p4' </TD> 
 htput <TD ALIGN=CENTER> `p5' </TD> 
 htput <TD> asymptotic </TD> 
 htput </TR>
  
  htput <TR>
 
 


 local p4: di "[" %10.3f `llt' ";"  %7.3f `ult' "]"
 local p5: di %9.3f `ztp'
 
 htput <TD ALIGN=CENTER>  </TD> 
 htput <TD ALIGN=CENTER>  </TD> 
 htput <TD ALIGN=CENTER>  </TD> 
 htput <TD ALIGN=CENTER>  `p4' </TD> 
 htput <TD ALIGN=CENTER > `p5' </TD>  
 htput <TD ALIGN=CENTER> z-transform </TD> 
 htput </TR>
 
 htput <TR>
 htput <TD colspan =6> </TD>
 htput </TR>
 
 htput <TR>
 local p1:di  "Pearson's r ="  %7.3f `r' 
 local p2: di "Pr(r = 0) ="  %6.3f `rp' 
 local p3: di "C_b = rho_c/r ="  %7.3f `p' / `r'
 htput <TD colspan=2 ALIGN=CENTER > `p1'  </TD> 
 htput <TD colspan=2 ALIGN=CENTER > `p2' </TD> 
 htput <TD colspan=2 ALIGN=CENTER> `p3' </TD> 
 htput </TR>
 
 htput <TR>
 local p1:di  "Reduced major axis:   Slope = " %9.3f `sl' 
 local p2:di     "   Intercept = "  %9.3f `yb'-`xb'*`sl'
 htput <TD colspan=3 ALIGN=CENTER> `p1'  </TD> 
 htput <TD colspan=3 ALIGN=CENTER> `p2' </TD> 
 htput </TR>
 
 htput <TR>
 htput <TD colspan =6>  </TD>
 htput </TR>
 
 htput <TR>
 local p1: di  _sk(`sk') "Difference (`2' - `1')" _sk(`sk1') 
 if index("`level'", ".") {
   local p2:  di %6.1f `level' "% Limits Of Agreement"
   }
   else {  local p2:di "   `level'% Limits Of Agreement" }
 
 htput <TH colspan=4 ALIGN=CENTER> `p1'</TH>
 htput <TH colspan=2 ALIGN=CENTER > `p2' </TH>
  htput </TR>
 
 htput <TR>
 htput <TH colspan=2> Average</TH>
 htput <TH colspan=2> Std. Dev. </TH>
 htput <TH colspan=2>  (Bland & Altman, 1986) </TH>
 htput </TR>
 
 htput <TR>
 local p1: di %10.3f `db'
 local p2: di %12.3f sqrt(`dv')
 local p3: di  %11.3f `dll' %11.3f `dul'
 
 htput <TD colspan=2>`p1' </TD>
 htput <TD colspan=2> `p2' </TD>
 htput <TD colspan=2> `p3'</TDH>
 htput </TR>
 
 htput </TABLE>
 
 
 
* setup local options for passing to graph routines

   if "`byl'"   != "" { local byls byl("`byl'") }
   if "`snd'"   != "" { local snds snd("`snd'") }
   if "`level'" != "" { local levs level("`level'") }

* loa graphs

   if "`graph'" == "l" {
     #delimit ;
     gphloa `1' `2' `dll' `db' `dul' `d' `m' `byg'
       `fit' `wgt', j(`j') byn(`byn') `byls' `reg' `ref'
       `snds' `levs' `options' ;
     #delimit cr
   }

* ccc graphs

   local sll=`sl'
   local xbl=`xb'
   local ybl=`yb'
   if "`graph'" == "c" {
     #delimit ;
     gphccc `1' `2' `byg' `wgt', j(`j') xb(`xbl')
        yb(`ybl') sl(`sll') byn(`byn') `byls' `options' ;
     #delimit cr
   }

   if `byn' > 1 { capture drop `d' `db' `dll' `dul' `m' }
   local j = `j' + 1
 } /* end of loop for each `by' group */

* Save key values

 if `byn' == 1 {
   global S_1 = `k'
   global S_2 = `p'
   global S_3 = `sep'
   global S_4 = `ll'
   global S_5 = `ul'
   global S_6 = `llt'
   global S_7 = `ult'
   global S_8 = `p' / `r'
   global S_9 = `db'
   global S_10 = sqrt(`dv')
   global S_11 = `dll'
   global S_12 = `dul'
 }
end


program define gphloa
 version 5.0
*-----------------------------------------------------
* loa graphs
* ----------------------------------------------------
 local weight "fweight opt"
 local varlist "req ex min(2)"
 #delimit ;
 local options "J(int 1) BYN(int 1) BYL(str) REG noREF SND(str)
   Pen(str) Connect(str) Symbol(str) Level(real 95) L2title(str)
   T1title(str) T2title(str) XLAbel(str) YLAbel(str) B2title(str)
   SAving(str) *" ;
 #delimit cr

 parse "`*'"
 parse "`varlist'", parse(" ")

 tempvar dll db dul d m byg Z Zv Psubi touse2
 local dll `3'
 local db  `4'
 local dul `5'
 local d   `6'
 local m   `7'
 local byg `8'
 if "`reg'" == "reg" {
   tempvar fit
   local fit `9'
 }
 if "`weight'" != "" { local wgt "[`weight'`exp']" }

* Graph options for loa plot

 if "`reg'" == "reg" & length("`connect'") < 5 { local fc "l" }

 if "`connect'" == "" { local connect "co(lll.`fc')" }
 else {
   local lll = length("`connect'")
   if `lll' == 1 { local connect "co(`connect'l`connect'.`fc')" }
   if `lll' == 2 { local connect = /*
    */ "co(`connect'" + substr("`connect'",1,1) + ".`fc')" }
   if `lll' == 3 { local connect "co(`connect'.`fc')" }
   if `lll' >= 4  { local connect "co(" + /*
    */ substr("`connect'",1,4) + "`fc')" }
 }

 if "`reg'" == "reg" & length("`symbol'") < 5 { local fc "." }
 if "`symbol'" == "" { local symbol "sy(...o`fc')" }
 else {
   local lll = length("`symbol'")
   if `lll' == 1 { local symbol "sy(`symbol'.`symbol'o`fc')" }
   if `lll' == 2 { local symbol = /*
    */ "sy(`symbol'" + substr("`symbol'",1,1) + "o`fc')" }
   if `lll' == 3 { local symbol "sy(`symbol'o`fc')" }
   if `lll' >= 4 { local symbol = /*
    */ "sy(" + substr("`symbol'",1,4) + "`fc')" }
 }

 if "`reg'" == "reg" & length("`pen'") < 5 { local fc "4" }
 if "`pen'" == "" { local pen "pe(3532`fc')" }
 else {
   local lll = length("`pen'")
   if `lll' == 1 { local pen "pe(`pen'5`pen'2`fc')" }
   if `lll' == 2 { local pen = /*
    */ "pe(`pen'" + substr("`pen'",1,1) + "2`fc')" }
   if `lll' == 3 { local pen "pe(`pen'2`fc')" }
   if `lll' >= 4 { local pen = /*
    */ "pe(" + substr("`pen'",1,4) + "`fc')" }
 }

 if "`t1title'" == "" { local t1title = "`level'% Limits Of Agreement" }
 else if "`t1title'" == "." { /* "." means blank it out */
   local t1title "   "
 }
 local t1title t1("`t1title'")

 if "`xlabel'" != "" { local xlabel "xlabel(`xlabel')" }
 else if index("`options'","xla") == 0 {local xlabel "xla" }

 if "`ylabel'" != "" { local ylabel "ylabel(`ylabel')" }
 else if index("`options'","yla") == 0 {local ylabel "yla" }

 if "`t2title'" == "" {
   if "`byl'" != "" { local t2title t2("`byl'") }
 }     /* "." means blank it out */
 else if "`t2title'" == "." { local t2title "   " }
 else { local t2title t2("`t2title'") }

 local l2t "`l2title'"
 if "`l2title'" == "" { local l2title l2t("Difference of `2' and `1'") }
 else { local l2title l2t("`l2title'") }

 if "`b2title'" == "" { local b2title b2t("Mean of `2' and `1'") }
 else { local b2title b2t("`b2title'") }

 if "`ref'" != "noref" { local refline "yli(0)" }

* graph loa plot

 #delimit ;
 gr `dll' `db' `dul' `d' `fit' `m' if `byg' == `j'
    `wgt', `pen' `connect' `symbol' `xlabel' `ylabel' `refline'
    `xscale' `yscale' `l1title' `l2title' `b2title' `t1title'
    `t2title' `saving' `options' ;
 #delimit cr
 more

* normal prob plot 
* note: logic pilfered from qnorm

 mark `touse2' if `byg' == `j'
 qui {
   gsort -`touse2' `d'
   gen `Psubi' = sum(`touse2')
   replace `Psubi' = cond(`touse2'==0,.,`Psubi'/(`Psubi'[_N] + 1))
   sum `d' if `touse2' == 1, detail
   gen `Zv' = (`d' - _result(3)) / sqrt(_result(4))
   gen `Z'  = invnorm(`Psubi')
   label var `Z' "Standard Normal Deviate"
}
 local l2title ""
 if "`l2t'" != "" { local l2title l2t("`l2t'") }
 #delimit ;
 graph `Z' `Zv' `d' if `byg' == `j' `wgt', `t2title'
       t1("Normal plot for differences") xla c(.l) gap(3)
       s(oi) yla(-2,-1,0,1,2) b2("Difference of `2' and `1'")
       `l2title' `b1title' `options' ;
 #delimit cr

* save snd var?

 if "`snd'" != "" {
   local c = index("`snd'",",")
   if `c' != 0 { local snd = substr("`snd'",1,`c'-1) + substr("`snd'",`c'+1, .) }
   local snd1 : word 1 of `snd'
   if `byg' == 1 {
     local replace : word 2 of `snd'
     if "`replace'" == "replace" { capture drop `snd1' }
     capture confirm new var `snd1'
     if _rc == 0 {qui gen `snd1' = .}
     else {
       local rc = _rc
       di in re "`snd1' exists. Use 'replace' option: snd(snd_var, replace)."
       exit `rc'
     }
   }
   local snd `snd1'
   qui replace `snd' = `Z' if `byg' == `j'
   label var `snd' "standard normal deviate"
 }

 if `byn' > 1 { more }
end


program define gphccc
 version 5.0
*-----------------------------------------------------
* ccc graph
* ----------------------------------------------------
 local weight "fweight opt"
 local varlist "req ex min(2)"
 #delimit ;
 local options "J(integer 1) XB(real 0) YB(real 0) SL(real 0)
   BYN(integer 1) BYL(str) Pen(str) Connect(str) Symbol(str)
   L1title(str) T1title(str) T2title(str) XLAbel(string)
   YLAbel(string) SAving(str) *" ;
 #delimit cr

 parse "`*'"
 parse "`varlist'", parse(" ")

 tempvar byg rmaxis
 tempname ysca yloc xsca xloc xmin xstep
 local byg   `3'

 if "`weight'" != "" { local wgt "[`weight'`exp']" }

* Graph options for concordance plot

 if "`connect'" == "" { local connect "co(.l.)" }
 else {
   local lll = length("`connect'")
   if `lll' == 1 { local connect "co(`connect'l.)" }
   else if `lll' == 2 { local connect "co(`connect'.)" }
   else {
     local connect = "co(" + substr("`connect'",1,2) + ".)"
   }
 }

 if "`symbol'" == "" { local symbol "sy(o.i)" }
 else {
   local lll = length("`symbol'")
   if `lll' == 1 { local symbol "sy(`symbol'.i)" }
   else if `lll' == 2 { local symbol "sy(`symbol'i)" }
   else {
     local symbol = "sy(" + substr("`symbol'",1,2) + "i)"
   }
 }

 if "`pen'" == "" { local pen "pe(221)" }
 else {
   local lll = length("`pen'")
   if `lll' == 1 { local pen "pe(`pen'`pen'1)" }
   else if `lll' == 2 { local pen "pe(`pen'1)" }
   else { local pen = "pe(" + substr("`pen'",1,2) + "1)" }
 }

 if "`l1title'" == "" {
   local l1title : variable label `1'
   if "`l1title'" == "" { local l1title "`1'" }
 }
 local l1title l1("`l1title'")

 if "`t1title'" == "" {
   local t1title = "Note: Data must" +                /*
 */  " overlay dashed line for perfect concordance"
 }
 else if "`t1title'" == "." { /* "." means blank it out */
   local t1title "   "
 }
 local t1title t1("`t1title'")

 if "`t2title'" == "." { local t2title "" }
 else if "`t2title'" != "" { local t2title t2("`t2title'")}

 if "`xlabel'" != "" { local xlabel "xlabel(`xlabel')" }
 else if index("`options'","xla") == 0 {local xlabel "xla" }

 if "`ylabel'" != "" { local ylabel "ylabel(`ylabel')" }
 else if index("`options'","yla") == 0 {local ylabel "yla" }

 if "`saving'" != "" { local saving ", saving(`saving')" }

* Graph concordance plot

 qui gen `rmaxis' = `sl' * (`2' - `xb') + `yb'

 gph open `saving'
 #delimit ;
 graph `1' `rmaxis' `2' `2' if `byg' == `j'
       `wgt', `connect' `symbol' `t1title' `l1title'
       `t2title' `xlabel' `ylabel' sort `pen' `options';
 #delimit cr
 scalar `ysca'  = _result(5)
 scalar `yloc'  = _result(6)
 scalar `xsca'  = _result(7)
 scalar `xloc'  = _result(8)
 scalar `xmin'  = _result(3)
 scalar `xstep' = (_result(4) - `xmin') / 50
 local r1 = _result(2) * `ysca' + `yloc'
 local c1 = `xmin'     * `xsca' + `xloc'
 gph pen 1
 gph text `r1' `c1' 0 -1 `byl'
 local ig = 0
 while `ig' < 49 {
   local r1 = (`xmin' +  `ig'      * `xstep') * `ysca' + `yloc'
   local c1 = (`xmin' +  `ig'      * `xstep') * `xsca' + `xloc'
   local r2 = (`xmin' + (`ig' + 1) * `xstep') * `ysca' + `yloc'
   local c2 = (`xmin' + (`ig' + 1) * `xstep') * `xsca' + `xloc'
   gph line `r1' `c1' `r2' `c2'
   local ig = `ig' + 2
 }
 gph close

 if `byn' > 1 { more }
end
