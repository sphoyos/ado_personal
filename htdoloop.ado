*! htdo.ado 3.01  ABR 16 2008 Bug in register graph fixed (LQ)
*! htdo.ado 3     ABR 03 2008 Removed the time stamp (JJAV)
*! htdo.ado 2.04  NOV 13 2007 Updated to version 9 and 10 (LQ)
*! htdo.ado 2.03  AGO 04 2005 Adding suport for colorer do and log files
*! htdo.ado 2.02  AGO 26 2004 Remove support for tables control but keep graphs control
*! htdo.ado 2.01  AGO 20 2004 Fix version 7 definition of programs
*! htdo.ado 2.00  JUN 15 2004 New concept to enable registy of tables and graphs
*! htdo.ado 1.04  SEP 30 2002 No HTML is save in document.dta if HTMLOUT is set to no
*! htdo.ado 1.0.3 SEP 09 2002 Bug in comparison of filenames fixed
*! htdo.ado 1.0.2 AGO 31 2002 Without DOS window to rename files 
*! htdo.ado 1.0.1
*! by JJAV AGO 21 2002
*! doanl and document file for HT
*! starts logs and htm files
*! requires _setup.do file with directories
*! REFRACTOR to improve functionality over version 8




program define htdoepi
	syntax,  chapter(string) item(string) [Hosp(string)]  [Com(string)] [Ghosp(string)]
version 11.2
	clear
	
	/*
		Run setup to define global macros and common things
	*/
	cap confirm file _setup.do
	if _rc == 0 {
		cap quietly run _setup
	}
	cap quietly htclose 
	set more 1
	clear
	
	
	
	if  length("`chapter'")!=3 {
		noi di in red "El n�mero de chapter debe tener 3 digitos"
		error(1)
	}	
	
	
	if  length("`item'")!=2 {
		noi di in red "El n�mero de itemp debe tener 2 digitos"
		error(1)
	}	
	/*  GENERA L'ESTRUCTURA DE DIRECTORIS SEGONS SIGA HOSPITAL, COMUNITAT O GLOBAL */
	/*  GENERA VARIABLES GLOBALS DE DIRECTORI/$dir_res , N� CENTRO $sdir           */
    /*	NOMBRE CENTRO o CCCAA $description, SELECCIO DE CASOS $if_selec*/
	
   quietly do $labels\global_labels.do
	global dir_res= "$htm"
	
	if "`hosp'"!="" {
	cap mkdir "$htm/hospitals"
	global dir_res= "$htm/hospitals/"+"`hosp'"
	global sdir="`hosp'"
	global if_sele "if hospitalid==`hosp'"
	global tipoana="hospitalid"
	global tipoan="hosp"
	preserve 
	use  "$dta\pri\Hospital.dta"
	quietly count if hospitalid==`hosp'
	
	if r(N)==0{
	noi di in red "El hospital no existe. Verifica el n�mero"
		error(1)
	}
	else
	{
	keep if hospitalid==`hosp'
	local nom_hosp:  disp trim( nombre_hospital[1])
	global description = "`nom_hosp'"
	noi di in red "$description"
	
	}
	restore 
	}
	
	if "`com'"!="" {
		cap mkdir "$htm/comunitat"
	global sdir="`com'"
	global dir_res =   "$htm/comunitat/"+"`com'"
	global if_sele "if comunidad==`com'"
	global tipoana="comunidad"
	global tipoan="com"
	preserve 
	use  "$dta\pri\Hospital.dta"
	quietly count if comunidad==`com'
	
	if r(N)==0{
	noi di in red "La comunidad no existe. Verifica el n�mero"
		error(1)
	}
	else
	{
	global description: disp  "`: label(comunidad) `com''" 
	noi di in red "$description"
	
	}
	restore 
	
	}
	
	
	if "`ghosp'"!="" {
	
	global sdir="`ghosp'"
	global dir_res =   "$htm/hospitals/"+"`ghosp'"
	global if_sele "if ghosp==`ghosp'"
	global tipoana="ghosp"
	global tipoan="hosp"
	preserve 
	use  "$dta\pri\Hospital.dta"
	quietly count if ghosp==`ghosp'
	
	if r(N)==0{
	noi di in red "El grupo hospitalario no existe. Verifica el n�mero"
		error(1)
	}
	else
	{
	global description: disp  "`: label(ghosp) `ghosp''" 
	noi di in red "$description"
	
	}
	restore 
	
	}
	
	
	if "`hosp'"=="" & "`com'"==""  & "`ghosp'"=="" {
	global dir_res= "$htm/Epine_Epps"
	local hosp="Epine_EPPS"
    global sdir="`hosp'"
	global description "EPINE EPPS"
	global tipoana="total"
	global tipoan="total"
	global if_sele "if hospitalid!=591 "
	 }
	
	
    disp in yellow "`sdir'"
    disp in yellow "`chapter'"
	disp in yellow "$dir_res"
	disp in yellow "$if_sele"
	cap mkdir  "$dir_res"
	
	if _rc==0 {
	
	 copy "$htm/capsalera_epine.png"   "$dir_res/capsalera_epine.png"   
	 copy "$htm/semp2.png"   "$dir_res/semp2.png"   
	 copy "$htm/estilos.css"   "$dir_res/estilos.css"   
	 }
	 cap mkdir "$dir_res/log"
	 cap mkdir "$dir_res/gph"
	 cap mkdir "$dir_res/dta"
	 cap mkdir "$dir_res/do"
     cap mkdir "$dir_res/res_${tipoan}_$sdir"
	 
		_htdo8epi `chapter' `item'  
end

program define _htdo8epi
cap log close
	version 8.2
	args chapter item
	
	local curdir : pwd
	if "$log" == "" { 
		global log : pwd
	}
	if "$gph" == "" {
		global gph : pwd
	}
	if "$dta" == "" {
		global dta : pwd
	}
	if "$htm" == "" {
		global htm : pwd
	}

	******************************************************************
	* Obtain the filename                                            *
	******************************************************************

	local files : dir `"`pwd'"' files "`chapter'_`item'*.do",  nofail
	tokenize `"`files'"'
	if (`"`2'"' != "" ) {
		noi di in red "Filename is not unique!!"
		while `"`1'"' != "" {
			noi di in yellow `"`1'""
			mac shift
			error 1
		}	
	}
	if (`"`1'"' == "" ) {
		noi di in red "File `chapter'_`item'*.do not found!!"
		error 1
	}
	
	local filename = `"`1'"'

	******************************************************************
	* Removes the extension of the filename                                            *
	******************************************************************
	local atdot = length(`"`filename'"')-index(reverse(`"`filename'"'),".")
	local fileshort = substr(`"`filename'"',1,`atdot')

	******************************************************************
	* Process file and determines the propierties according docver   *
	******************************************************************
	tempname f1
	file open `f1' using `"`filename'"', read
	file read `f1' line
	if index(`"`line'"',"docver:") == 0 {
		noi di in red "Not found 'docver:' directive "
		error(1) 
	}
	local version = trim(substr(`"`line'"',index(`"`line'"',"docver:")+7,.))
	local foundversion = 0
	if `"`version'"' == "1.0" {
    	_process_v1 `"`chapter'"' `"`item'"' `"`f1'"' `"`filename'"' `"`xlog'"'
	}
	else if  `"`version'"' == "2.0" {
  	  _process_v2_epi `"`chapter'"' `"`item'"' `"`f1'"' `"`filename'"' `"`xlog'"'
	}
	else {
		noi di in red "docver `version' not supported!"
		error 1
	}
end



program  define _process_v2_epi
args chapter item f1 filename
	version 8.2

	* reads four more lines: title, html, log and comments
	local foundversion = 1
	file  read `f1' line
	if index(`"`line'"',"title:") == 0 {
		noi di in red "Not found 'title:' directive"
		error(1)
	}
	global HTtitle = trim(substr(`"`line'"',index(`"`line'"',"title:")+6,.))

	file  read `f1' lined
	if index(`"`lined'"',"html:") != 0 {
		local htmlyesno = trim(substr(`"`lined'"',index(`"`lined'"',"html:")+5	,.))
	}

	file  read `f1' linee
	if index(`"`linee'"',"log:") != 0 {
		local  logyesno= trim(substr(`"`linee'"',index(`"`linee'"',"log:")+4	,.))
	}

	file  read `f1' linec
	if index(`"`linec'"',"comment:") != 0 {
		local htcomment = trim(substr(`"`linec'"',index(`"`linec'"',"comment:")+8	,.))
	}
	file close `f1'

	/* check invalid characters in filenames and replaces with "_" */
	local menuname
	local menuname = subinstr(`"$HTtitle"'," ","_",.)
	local menuname = subinstr(`"`menuname'"',"\","_",.)
	local menuname = subinstr(`"`menuname'"',"/","_",.)
  
    local ndir="$sdir"
	
	/* names for logfile, htfile and do files and signatures*/
	local  htlogfile =lower(`"`ndir'_`chapter'_`item'_`menuname'.log"')
	if "`htmlyesno'" == "yes" {
	   
		local  htfile  =lower(`"`ndir'_`chapter'_`item'_`menuname'.html"')
   		} 
	else {
		local htfile = ""
	}

	if "`logyesno'" == "yes" {
		local  htlogfile  =lower(`"`ndir'_`chapter'_`item'_`menuname'.log"')
	} 
	else {
		local htlogfile = ""
	}
	
	
		
	global HTprog  =  lower(`"`chapter'_`item'_`menuname'"')+".do"

* HTprog se pierde con el htclose
local htprog = `"$HTprog"'
	
	if lower(`"$HTprog"') != lower(`"`filename'"') {
	noi di in yellow  `" $HTprog != `filename' "'
		copy `"`filename'"' `"$HTprog"'
		erase `"`filename'"'
	}


	/* test macros and global variables */

	/*
	noi di in red "chapter: `chapter'"
	noi di in red "item: `item'"
	noi di in red "title: $HTtitle "
	noi di in red "html: `htfile' "
	noi di in red "log: `htlogfile'"
	noi di in red "prg: $HTprog "
	*/

	/* Documentation of the do file */


	*******************************************************
	* Open database document or creates it if do not exist *
	********************************************************
	local htdoc = "$dir_res/dta/reg_do_files.dta"
	cap confirm file `"`htdoc'"'
	if _rc > 0 {
		local newfile = "YES"
		quietly set obs 1
		quietly gen chapter = .
	    quietly gen item = .
		quietly gen sdir="."
		quietly gen str1 title = "."
		quietly gen str1 html = "."
		quietly gen str1 log = "."
		quietly gen str1 created = "."
		quietly gen str1 comments = "."

	}
	else {
		quietly use `"`htdoc'"'
	}

	local change = 0
	
	quietly count if chapter == `chapter' & item == `item'
	if r(N) == 0 {
		if "`newfile'" != "YES" {
		    local xobs = _N+1
		    quietly set obs `xobs'
		}		 
		quietly replace chapter = `chapter' if chapter == .
		quietly replace item = `item' if item == .
		quietly replace sdir= "$sdir" if chapter == `chapter' & item == `item'
		quietly replace created =`"$S_DATE - $S_TIME"' if chapter == `chapter' & item == `item'
noi di in red "NEW OBSB"
		local change = 1
	}

	quietly count if chapter == `chapter' & item == `item' & trim(title) == trim(`"`menuname'"')
	if r(N) == 0 {
		quietly replace title = `"`menuname'"' if chapter == `chapter' & item == `item'
noi di in red "MENUNAME"		
		local change = 1
	}
	
	quietly count if chapter == `chapter' & item == `item' & trim(html) == trim(`"`htmlyesno'"')
	if r(N) == 0 {
		quietly replace html = `"`htmlyesno'"' if chapter == `chapter' & item == `item'
noi di in red "HTML"
		local change = 1
	}

	quietly count if chapter == `chapter' & item == `item' & trim(log) == trim(`"`logyesno'"')
	if r(N) == 0 {
		quietly replace log = `"`logyesno'"' if chapter == `chapter' & item == `item'
noi di in red "LOG"
		local change = 1
	}

	quietly count if chapter == `chapter' & item == `item' & trim(comments) == trim(`"`htcomment'"')
	if r(N) == 0 {
		quietly replace comments = `"`htcomment'"' if chapter == `chapter' & item == `item'
noi di in red "COMMENT"
		local change = 1
	}

	if (`change' > 0 ) {
		quietly save `"`htdoc'"', replace
	}
	quietly drop *

	/* Documentation of the graphs*/
	*******************************************************
	* Open tables database or creates it do not exist     *
	*******************************************************
	local htgraph = "$dir_res/$dta/reg_graphs.dta"
	cap confirm file `"`htgraph'"'
	if _rc > 0 {
		local newfile = "YES"
		quietly set obs 1
		quietly gen chapter = .
	    quietly gen item = .
	    quietly gen num = .
		quietly gen str1 title = "."
		quietly gen str1 file = "."
		quietly save `"`htgraph'"', replace
	}
	macro drop XHTGraph_*
	global XHTGraph_n = 0

	

	/*****************************************/
	/* Open log file for this analysis       */
	/* in case a previous one was not opened */
	/*****************************************/
	if `"`xlog'"' == "" & `"`logyesno'"'== "yes" {
		quietly log using `"$dir_res/log/`htlogfile'"', replace text
	}


	/*****************************************/
	/* Open HTML output                      */ 
	/* in case a htmlout = "yes"             */
	/*****************************************/
	if "`htmlyesno'" == "yes" {
		htopen using `"$dir_res/`htfile'"', replace
		/* Not that htopen puts the <!DOCTYPE> and <HTML> tags by default */
		htput <HEAD>
		htput <TITLE> $HTtitle </TITLE>
		if "$stylesheet" != "" {
			htput <LINK REL="stylesheet" HREF="$stylesheet" TYPE="text/css">
		}
		htput </HEAD>
		htput <BODY>
	}


	/*****************************************/
	/* Executes the program                  */
	/*****************************************/
	display in yellow ".* $HTprog run on $S_DATE at $S_TIME"


	capture noisily do `"$HTprog"'
	local myrc = _rc
	tempfile borrar
	cap save `borrar', replace

	/*****************************************/
	/* Close HTML output                     */
	/* in case a htmlyesno = "yes"           */
	/*****************************************/


	if "`htmlyesno'" == "yes" {
	*	htput <DIV CLASS=sign><HR>Last update $S_DATE, 
		if (`"$colorer"' == "") {
	
	*		htput by <a href="do/$HTprog.txt">$HTprog.txt</a>
			if `"`xlog'"'== "" & `"`logyesno'"'== "yes" {
	*			htput . Log file in  <a href="log/`htlogfile'.txt">`htlogfile'.txt</a>
			}
		}
		else {
		
	*		htput by <a href="do/$HTprog.html">$HTprog.html</a>
			if `"`xlog'"'== "" & `"`logyesno'"'== "yes" {
			
			
			
	*			htput . Log file in  <a href="log/`htlogfile'.html">`htlogfile'.html</a>
			}

		}
	*	htput </DIV>
		htput </BODY>
		htclose
	}


	/*****************************************/
	/* Close LOG file open for this output   */
	/* in case a logyesno = "yes" you should */
	/* log file should be closed             */
	/*****************************************/
	if `"`xlog'"' == `""' & `"`logyesno'"'== `"yes"' {
		cap log close
	}


	/* Copy the  log file under the html tree */
	if `"`logyesno'"' == "yes" & `myrc' == 0 {
		if (`"$colorer"' == "") {

			quietly  copy `"$dir_res/log/`htlogfile'"' `"$dir_res/log/`htlogfile'.txt"', replace
			
		}
		else {
			disp in red  "Colorer  $colorer"
		   local cmd = `"! $colorer -h -t"text" -dc $ilogtype  -o$dir_res/log/`htlogfile'.html $dir_res/log/`htlogfile'  "'
		   
			`cmd'
		}

	}

	/*********************************************/
	/* Close HTLM output if not properly closed  */
	/* Probably not necessary in this program    */
	/*********************************************/
	
	if `"$HTfile"' != "" {
		/* HT not properly closed.. */
		cap htclose
	}

	/*********************************************/
	/* Copy the program to the HTML output       */
	/* if rc_ 0                                  */
	/*********************************************/
	
	 if (`myrc' == 0 ) {
		/*Copy the program file under the html tree*/
		if (`"$colorer"' == "") {
			quietly copy  `"`htprog'"'   `"$dir_res/do/`htprog'.txt"', replace
		}
		else {
		   local cmd =`"! $colorer -h -dc $idotype -o$dir_res/do/`htprog'.html `htprog'  "'
		   `cmd'
		}
	}

    /**************************/
    /* Update the reg_graphs  */
    /**************************/
	preserve
	local gchange = 0
	use `htgraph', clear

	if $XHTGraph_n > 0 {
		local i = 1
		while `i' <= $XHTGraph_n {
			local title :di  "$" "XHTGraph_`i'"
			local xfile : di "$" "XHTGfile_`i'"
			quietly count if chapter == `chapter' &  item == `item' & num == `i'

			if r(N) == 0 { 
				quietly replace item = `item' if chapter == .
				quietly replace num = `i' if chapter == .
				quietly replace chapter = `chapter' if chapter == .
				local gchange = 1
				local newobs = _N+1
				quietly set obs `newobs'

			}

			quietly count if chapter == `chapter' &  item == `item' & num == `i' & trim(title) == trim(`"`title'"') & trim(file) == trim("`xfile'")

			if r(N) == 0 {			
				quietly replace title = `"`title'"' if chapter ==`chapter' & item == `item' &  num == `i'
				quietly replace file = "`xfile'" if chapter ==`chapter' & item == `item' &  num == `i'
				local gchange = 1
			}
			local i = `i' +1
		}
	}

	quietly count if chapter == `chapter' &  item == `item' & num > $XHTGraph_n
	if r(N) > 0 {
		quietly drop if chapter == `chapter' &  item == `item' & num > $XHTGraph_n
		local gchange = 1
	}
	
	if `gchange' > 0 { 
		quietly save `htgraph', replace
	}
	
	restore

	macro drop XHTGraph_*
	
	/*********************************************/
	/* Return errorcode to the system            */
	/*********************************************/

	exit `myrc'

end
