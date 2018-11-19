
program define regicor_calc, rclass

*! version 1.1 Diciembre 2012 by SPH
*! Este programa calcula la una tabla con las variables con el indiccor del regicor


syntax varlist(min=8 max=8) 


cap drop gir_all


*******************************************************************************.
*******************************************************************************.
** la base de datos tiene que tener las siguientes variables:
* `sex', 0 = mujer, 1 = hombre.
* `age', entre 35 i 74 años.
* `coltot', colesterol total.
* `hdl', colesterol `hdl'.
* `tas', pressión arterial sistólica.
* `tad', pressión arterial diastólica.
* `diab', 0 = no , 1 = si.
* `smoke', 0 = no, 1 si.
*******************************************************************************.
*******************************************************************************.

tokenize `varlist'
        local sex = "`1'"
        local age = "`2'"
		local coltot = "`3'"
		local hdl = "`4'"
		local tas = "`5'"
		local tad = "`6'"
		local diab = "`7'"
		local smoke = "`8'"

		
		
        tempvar c_160
		tempvar c160_199
		tempvar c200_239
		tempvar c240_279
		tempvar c280_
		tempvar h_35
		tempvar h35_44
		tempvar h45_49
		tempvar h50_59
		tempvar h60_
		tempvar bp_opti
		tempvar bp_norm
		tempvar bp_high
		tempvar bp_1
		tempvar bp_2


*********** recodificaciones previas --> categorización de COLESTEROL TOTAL, `hdl', y BP (BLOOD PRESSURE) **************************.


gen byte `c_160'= (`coltot'<160  ) if !missing(`coltot') 
gen byte `c160_199'= (`coltot'>=160 & `coltot'<200) if !missing(`coltot')
gen byte `c200_239'= (`coltot'>=200 & `coltot'<240) if !missing(`coltot')
gen byte `c240_279'= (`coltot'>=240 & `coltot'<280) if !missing(`coltot')
gen byte `c280_'= (`coltot'>=280) if !missing(`coltot')



gen byte `h_35'= (`hdl'<35  ) if !missing(`hdl')
gen byte `h35_44'= (`hdl'>=35 & `hdl'<45   ) if !missing(`hdl') 
gen byte `h45_49'= (`hdl'>=45 & `hdl'<50   ) if !missing(`hdl') 
gen byte `h50_59'= (`hdl'>=50 & `hdl'<60   ) if !missing(`hdl') 
gen byte `h60_'= (`hdl'>=60 ) if !missing(`hdl') 

gen `bp_opti' = .
gen `bp_norm'= .
gen `bp_high' = .
gen `bp_1' = .
gen `bp_2' = .
 

replace `bp_opti' = 1 if (`tad' < 80 & `tas' <  120) & (tas!=. & tad!=.) 
replace `bp_high' = 0 if (`tad' < 80 & `tas' <  120) & (tas!=. & tad!=.) 
replace `bp_1' = 0 if (`tad' < 80 & `tas' <  120) & (tas!=. & tad!=.) 
replace `bp_2' = 0 if (`tad' < 80 & `tas' <  120) & (tas!=. & tad!=.) 

replace `bp_opti' = 0 if (`tad' < 80 & `tas' >= 120 & `tas' <=  129) & (tas!=. & tad!=.) 
replace `bp_high' = 0 if (`tad' < 80 & `tas' >= 120 & `tas' <=  129) & (tas!=. & tad!=.) 
replace `bp_1' = 0 if (`tad' < 80 & `tas' >= 120 & `tas' <=  129) & (tas!=. & tad!=.) 
replace `bp_2' = 0 if (`tad' < 80 & `tas' >= 120 & `tas' <=  129) & (tas!=. & tad!=.) 

replace `bp_opti' = 0 if (`tad' < 80 & `tas' >= 130 & `tas' <=  139) & (tas!=. & tad!=.) 
replace `bp_high' = 1 if (`tad' < 80 & `tas' >= 130 & `tas' <=  139) & (tas!=. & tad!=.) 
replace `bp_1' = 0 if (`tad' < 80 & `tas' >= 130 & `tas' <=  139) & (tas!=. & tad!=.) 
replace `bp_2' = 0 if (`tad' < 80 & `tas' >= 130 & `tas' <=  139) & (tas!=. & tad!=.) 

replace `bp_opti' = 0 if (`tad' < 80 & `tas' >= 140 & `tas' <=  159) & (tas!=. & tad!=.) 
replace `bp_high' = 0 if (`tad' < 80 & `tas' >= 140 & `tas' <=  159) & (tas!=. & tad!=.) 
replace `bp_1' = 1 if (`tad' < 80 & `tas' >= 140 & `tas' <=  159) & (tas!=. & tad!=.) 
replace `bp_2' = 0 if (`tad' < 80 & `tas' >= 140 & `tas' <=  159) & (tas!=. & tad!=.) 

replace `bp_opti' = 0 if (`tad' < 80 & `tas' >= 160) & (tas!=. & tad!=.) 
replace `bp_high' = 0 if (`tad' < 80 & `tas' >= 160) & (tas!=. & tad!=.) 
replace `bp_1' = 0 if (`tad' < 80 & `tas' >= 160) & (tas!=. & tad!=.) 
replace `bp_2' = 1 if (`tad' < 80 & `tas' >= 160) & (tas!=. & tad!=.) 

replace `bp_opti' = 0 if (`tad' >= 80 & `tad' <= 84 & `tas' <  120) & (tas!=. & tad!=.) 
replace `bp_high' = 0 if (`tad' >= 80 & `tad' <= 84 & `tas' <  120) & (tas!=. & tad!=.) 
replace `bp_1' = 0 if (`tad' >= 80 & `tad' <= 84 & `tas' <  120) & (tas!=. & tad!=.) 
replace `bp_2' = 0 if (`tad' >= 80 & `tad' <= 84 & `tas' <  120) & (tas!=. & tad!=.) 

replace `bp_opti' = 0 if (`tad' >= 80 & `tad' <= 84 & `tas' >= 120 & `tas' <=  129) & (tas!=. & tad!=.) 
replace `bp_high' = 0 if (`tad' >= 80 & `tad' <= 84 & `tas' >= 120 & `tas' <=  129) & (tas!=. & tad!=.) 
replace `bp_1' = 0 if (`tad' >= 80 & `tad' <= 84 & `tas' >= 120 & `tas' <=  129) & (tas!=. & tad!=.) 
replace `bp_2' = 0 if (`tad' >= 80 & `tad' <= 84 & `tas' >= 120 & `tas' <=  129) & (tas!=. & tad!=.) 

replace `bp_opti' = 0 if (`tad' >= 80 & `tad' <= 84 & `tas' >= 130 & `tas' <=  139) & (tas!=. & tad!=.) 
replace `bp_high' = 1 if (`tad' >= 80 & `tad' <= 84 & `tas' >= 130 & `tas' <=  139) & (tas!=. & tad!=.) 
replace `bp_1' = 0 if (`tad' >= 80 & `tad' <= 84 & `tas' >= 130 & `tas' <=  139) & (tas!=. & tad!=.) 
replace `bp_2' = 0 if (`tad' >= 80 & `tad' <= 84 & `tas' >= 130 & `tas' <=  139) & (tas!=. & tad!=.) 

replace `bp_opti' = 0 if (`tad' >= 80 & `tad' <= 84 & `tas' >= 140 & `tas' <=  159) & (tas!=. & tad!=.) 
replace `bp_high' = 0 if (`tad' >= 80 & `tad' <= 84 & `tas' >= 140 & `tas' <=  159) & (tas!=. & tad!=.) 
replace `bp_1' = 1 if (`tad' >= 80 & `tad' <= 84 & `tas' >= 140 & `tas' <=  159) & (tas!=. & tad!=.) 
replace `bp_2' = 0 if (`tad' >= 80 & `tad' <= 84 & `tas' >= 140 & `tas' <=  159) & (tas!=. & tad!=.) 

replace `bp_opti' = 0 if (`tad' >= 80 & `tad' <= 84 & `tas' >= 160) & (tas!=. & tad!=.) 
replace `bp_high' = 0 if (`tad' >= 80 & `tad' <= 84 & `tas' >= 160) & (tas!=. & tad!=.) 
replace `bp_1' = 0 if (`tad' >= 80 & `tad' <= 84 & `tas' >= 160) & (tas!=. & tad!=.) 
replace `bp_2' = 1 if (`tad' >= 80 & `tad' <= 84 & `tas' >= 160) & (tas!=. & tad!=.) 

replace `bp_opti' = 0 if (`tad' >= 85 & `tad' <= 89 & `tas' <  120) & (tas!=. & tad!=.) 
replace `bp_high' = 1 if (`tad' >= 85 & `tad' <= 89 & `tas' <  120) & (tas!=. & tad!=.) 
replace `bp_1' = 0 if (`tad' >= 85 & `tad' <= 89 & `tas' <  120) & (tas!=. & tad!=.) 
replace `bp_2' = 0 if (`tad' >= 85 & `tad' <= 89 & `tas' <  120) & (tas!=. & tad!=.) 

replace `bp_opti' = 0 if (`tad' >= 85 & `tad' <= 89 & `tas' >= 120 & `tas' <=  129) & (tas!=. & tad!=.) 
replace `bp_high' = 1 if (`tad' >= 85 & `tad' <= 89 & `tas' >= 120 & `tas' <=  129) & (tas!=. & tad!=.) 
replace `bp_1' = 0 if (`tad' >= 85 & `tad' <= 89 & `tas' >= 120 & `tas' <=  129) & (tas!=. & tad!=.) 
replace `bp_2' = 0 

replace `bp_opti' = 0 if (`tad' >= 85 & `tad' <= 89 & `tas' >= 130 & `tas' <=  139) & (tas!=. & tad!=.) 
replace `bp_high' = 1 if (`tad' >= 85 & `tad' <= 89 & `tas' >= 130 & `tas' <=  139) & (tas!=. & tad!=.) 
replace `bp_1' = 0 if (`tad' >= 85 & `tad' <= 89 & `tas' >= 130 & `tas' <=  139) & (tas!=. & tad!=.)
replace `bp_2' = 0 if (`tad' >= 85 & `tad' <= 89 & `tas' >= 130 & `tas' <=  139) & (tas!=. & tad!=.) 

replace `bp_opti' = 0 if (`tad' >= 85 & `tad' <= 89 & `tas' >= 140 & `tas' <=  159) & (tas!=. & tad!=.) 
replace `bp_high' = 0 if (`tad' >= 85 & `tad' <= 89 & `tas' >= 140 & `tas' <=  159) & (tas!=. & tad!=.) 
replace `bp_1' = 1 if (`tad' >= 85 & `tad' <= 89 & `tas' >= 140 & `tas' <=  159) & (tas!=. & tad!=.) 
replace `bp_2' = 0 if (`tad' >= 85 & `tad' <= 89 & `tas' >= 140 & `tas' <=  159) & (tas!=. & tad!=.) 

replace `bp_opti' = 0 if (`tad' >= 85 & `tad' <= 89 & `tas' >= 160) & (tas!=. & tad!=.) 
replace `bp_high' = 0 if (`tad' >= 85 & `tad' <= 89 & `tas' >= 160) & (tas!=. & tad!=.) 
replace `bp_1' = 0 if (`tad' >= 85 & `tad' <= 89 & `tas' >= 160) & (tas!=. & tad!=.) 
replace `bp_2' = 1 if (`tad' >= 85 & `tad' <= 89 & `tas' >= 160) & (tas!=. & tad!=.) 

replace `bp_opti' = 0 if (`tad' >= 90 & `tad' <= 99 & `tas' <  120) & (tas!=. & tad!=.) 
replace `bp_high' = 0 if (`tad' >= 90 & `tad' <= 99 & `tas' <  120) & (tas!=. & tad!=.) 
replace `bp_1' = 1 if (`tad' >= 90 & `tad' <= 99 & `tas' <  120) & (tas!=. & tad!=.) 
replace `bp_2' = 0 if (`tad' >= 90 & `tad' <= 99 & `tas' <  120) & (tas!=. & tad!=.) 

replace `bp_opti' = 0 if (`tad' >= 90 & `tad' <= 99 & `tas' >= 120 & `tas' <=  129) & (tas!=. & tad!=.) 
replace `bp_high' = 0 if (`tad' >= 90 & `tad' <= 99 & `tas' >= 120 & `tas' <=  129) & (tas!=. & tad!=.) 
replace `bp_1' = 1 if (`tad' >= 90 & `tad' <= 99 & `tas' >= 120 & `tas' <=  129) & (tas!=. & tad!=.) 
replace `bp_2' = 0 if (`tad' >= 90 & `tad' <= 99 & `tas' >= 120 & `tas' <=  129) & (tas!=. & tad!=.) 

replace `bp_opti' = 0 if (`tad' >= 90 & `tad' <= 99 & `tas' >= 130 & `tas' <=  139) & (tas!=. & tad!=.) 
replace `bp_high' = 0 if (`tad' >= 90 & `tad' <= 99 & `tas' >= 130 & `tas' <=  139) & (tas!=. & tad!=.) 
replace `bp_1' = 1 if (`tad' >= 90 & `tad' <= 99 & `tas' >= 130 & `tas' <=  139) & (tas!=. & tad!=.) 
replace `bp_2' = 0 if (`tad' >= 90 & `tad' <= 99 & `tas' >= 130 & `tas' <=  139) & (tas!=. & tad!=.) 

replace `bp_opti' = 0 if (`tad' >= 90 & `tad' <= 99 & `tas' >= 140 & `tas' <=  159) & (tas!=. & tad!=.) 
replace `bp_high' = 0 if (`tad' >= 90 & `tad' <= 99 & `tas' >= 140 & `tas' <=  159) & (tas!=. & tad!=.) 
replace `bp_1' = 1 if (`tad' >= 90 & `tad' <= 99 & `tas' >= 140 & `tas' <=  159) & (tas!=. & tad!=.) 
replace `bp_2' = 0 if (`tad' >= 90 & `tad' <= 99 & `tas' >= 140 & `tas' <=  159) & (tas!=. & tad!=.) 

replace `bp_opti' = 0 if (`tad' >= 90 & `tad' <= 99 & `tas' >= 160) & (tas!=. & tad!=.) 
replace `bp_high' = 0 if (`tad' >= 90 & `tad' <= 99 & `tas' >= 160) & (tas!=. & tad!=.) 
replace `bp_1' = 0 if (`tad' >= 90 & `tad' <= 99 & `tas' >= 160) & (tas!=. & tad!=.) 
replace `bp_2' = 1 if (`tad' >= 90 & `tad' <= 99 & `tas' >= 160) & (tas!=. & tad!=.) 


replace `bp_opti' = 0 if (`tad' >= 100 & `tas' <  120) & (tas!=. & tad!=.) 
replace `bp_high' = 0 if (`tad' >= 100 & `tas' <  120) & (tas!=. & tad!=.) 
replace `bp_1' = 0 if (`tad' >= 100 & `tas' <  120) & (tas!=. & tad!=.) 
replace `bp_2' = 1 if (`tad' >= 100 & `tas' <  120) & (tas!=. & tad!=.) 

replace `bp_opti' = 0 if (`tad' >= 100 & `tas' >= 120 & `tas' <=  129) & (tas!=. & tad!=.) 
replace `bp_high' = 0 if (`tad' >= 100 & `tas' >= 120 & `tas' <=  129) & (tas!=. & tad!=.) 
replace `bp_1' = 0 if (`tad' >= 100 & `tas' >= 120 & `tas' <=  129) & (tas!=. & tad!=.) 
replace `bp_2' = 1 if (`tad' >= 100 & `tas' >= 120 & `tas' <=  129) & (tas!=. & tad!=.) 

replace `bp_opti' = 0 if (`tad' >= 100 & `tas' >= 130 & `tas' <=  139) & (tas!=. & tad!=.) 
replace `bp_high' = 0 if (`tad' >= 100 & `tas' >= 130 & `tas' <=  139) & (tas!=. & tad!=.) 
replace `bp_1' = 0 if (`tad' >= 100 & `tas' >= 130 & `tas' <=  139) & (tas!=. & tad!=.) 
replace `bp_2' = 1 if (`tad' >= 100 & `tas' >= 130 & `tas' <=  139) & (tas!=. & tad!=.) 

replace `bp_opti' = 0 if (`tad' >= 100 & `tas' >= 140 & `tas' <=  159) & (tas!=. & tad!=.) 
replace `bp_high' = 0 if (`tad' >= 100 & `tas' >= 140 & `tas' <=  159) & (tas!=. & tad!=.) 
replace `bp_1' = 0 if (`tad' >= 100 & `tas' >= 140 & `tas' <=  159) & (tas!=. & tad!=.) 
replace `bp_2' = 1 if (`tad' >= 100 & `tas' >= 140 & `tas' <=  159) & (tas!=. & tad!=.) 

 replace `bp_opti' = 0 if (`tad' >= 100 & `tas' >= 160) & (tas!=. & tad!=.)
replace `bp_high' = 0 if (`tad' >= 100 & `tas' >= 160) & (tas!=. & tad!=.) 
replace `bp_1' = 0 if  (`tad' >= 100 & `tas' >= 160) & (tas!=. & tad!=.) 
 replace `bp_2' = 1 if (`tad' >= 100 & `tas' >= 160) & (tas!=. & tad!=.)

replace `bp_norm'=1 if (`bp_opti'+`bp_high'+`bp_1'+`bp_2'==0) 
replace `bp_norm'=0 if (`bp_opti'+`bp_high'+`bp_1'+`bp_2'==1)





***********************************************************************************************************************************************************************
*** Cálculo del riego de IAM (mortal o no) o Angina (riesgo a 10 anys) ***
***********************************************************************************************************************************************************************.

tempvar l_chol
tempvar g_chol
tempvar a_chol
tempvar b_chol



* Men.
gen `l_chol' = (0.04826*`age') ///
                              - (0.65945*`c_160') + (0.17692* `c200_239') +(0.50539* `c240_279') + (0.65713* `c280_')  /// 
                             + (0.49744* `h_35') + (0.24310* `h35_44') - (0.05107* `h50_59') - (0.48660*`h60_') ///
                              - (0.00226 * `bp_opti') + (0.28320 *  `bp_high') + (0.52168 * `bp_1') + (0.61859 * `bp_2') ///
                             + (0.42839 * `diab') + (0.52337* `smoke')  if (`sex' == 1)

* Women.
 replace `l_chol' = (0.33766*`age')  - (0.00268 * (`age')^2)  ///
                              - (0.26138* `c_160') + (0.20771*`c200_239') + (0.24385 * `c240_279') + (0.53513* `c280_') /// 
                             + (0.84312* `h_35') + (0.37796* `h35_44') + (0.19785* `h45_49') - (0.42951*`h60_') ///
                              - (0.53363 * `bp_opti') - (0.06773 *  `bp_high') + (0.26288 * `bp_1') + (0.46573 * `bp_2') ///
                              + (0.59626 * `diab') + (0.29246* `smoke') if (`sex' ==0)

							  
							  

*function at means of covariates (predictor linial).
 gen `g_chol' =  3.489 if (`sex' == 1)
replace  `g_chol' = 10.279 if (`sex' == 0)

gen `a_chol' = `l_chol' - `g_chol'
gen  `b_chol' = exp(`a_chol')


gen gir_all =  (1 - (1-0.049)^`b_chol') if (`sex' == 1)
replace gir_all =  (1 - (1-0.022)^`b_chol') if (`sex' == 0)


replace gir_all=gir_all*100
end

