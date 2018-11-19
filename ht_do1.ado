program define ht_do1
	args chapter item

	clear
	
	/*	**************************************************************
		Run setup to define global macros and common things
	*/	**************************************************************
	
	cap confirm file _setup.do
	if _rc == 0 {
		cap quietly run _setup
	}
	cap quietly htclose 
	set more 1
	clear

	/* **************************************************************
 		Normal use is for two parameters but in 
		case of a complete filename in one parameter 
		it check first if match the pattern
	*/ **************************************************************
	
	local digits = "0123456789"
	local d1 = substr("`chapter'",1,1)
	local d2 = substr("`chapter'",2,1)
	local d4 = substr("`chapter'",4,1)
	local d5 = substr("`chapter'",5,1)

	* index() function will work only if version is less than 9

		if "`item'" == "" & strpos("`digits'","`d1'") & strpos("`digits'","`d2'")  & strpos("`digits'","`d4'")  & strpos("`digits'","`d5'")  {
			local item = substr("`chapter'",4,2)
			local chapter=substr("`chapter'",1,2)
		}
		else {
			if `"`item'"' == "" {
			do "`chapter'"
			exit
			}
		}
		 
	/* **************************************************************
 		 Execute do file once chapter & item are recognized
	*/ **************************************************************
	
	_htdo8 `chapter' `item'
	
end

     /* **************************************************************
 		 Program _htdo8 to run . args chapter and item
	*/ **************************************************************
	

program define _htdo8

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
	* Obtain the filename  & check for uniqueness                                            *
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
	* Process file and determines the properties according docver   *
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
	
	if  `"`version'"' == "2.0" {
  	  _process_v2 `"`chapter'"' `"`item'"' `"`f1'"' `"`filename'"' `"`xlog'"'
	}
	else {
		noi di in red "docver `version' not supported!"
		error 1
	}
end


	******************************************************************
	* Program file to process data 
	******************************************************************
	

program  define _process_v2
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

	/* names for logfile, htfile and do files and signatures*/
	local  htlogfile =lower(`"`chapter'_`item'_`menuname'.log"')
	if "`htmlyesno'" == "yes" {
		local  htfile  =lower(`"`chapter'_`item'_`menuname'.html"')
	} 
	else {
		local htfile = ""
	}

	if "`logyesno'" == "yes" {
		local  htlogfile  =lower(`"`chapter'_`item'_`menuname'.log"')
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
	local htdoc = "$dta/reg_do_files.dta"
	cap confirm file `"`htdoc'"'
	if _rc > 0 {
		local newfile = "YES"
		quietly set obs 1
		quietly gen chapter = .
	    quietly gen item = .
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
		quietly saveold `"`htdoc'"', replace
	}
	quietly drop *

	/* Documentation of the graphs*/
	*******************************************************
	* Open tables database or creates it do not exist     *
	*******************************************************
	local htgraph = "$dta/reg_graphs.dta"
	cap confirm file `"`htgraph'"'
	if _rc > 0 {
		local newfile = "YES"
		quietly set obs 1
		quietly gen chapter = .
	    quietly gen item = .
	    quietly gen num = .
		quietly gen str1 title = "."
		quietly gen str1 file = "."
		quietly saveold `"`htgraph'"', replace
	}
	macro drop XHTGraph_*
	global XHTGraph_n = 0

	

	/*****************************************/
	/* Open log file for this analysis       */
	/* in case a previous one was not opened */
	/*****************************************/
	if `"`xlog'"' == "" & `"`logyesno'"'== "yes" {
		quietly log using `"$log/`htlogfile'"', replace text
	}


	/*****************************************/
	/* Open HTML output                      */ 
	/* in case a htmlout = "yes"             */
	/*****************************************/
	if "`htmlyesno'" == "yes" {
		htopen using `"$htm/`htfile'"', replace
		/* Not that htopen puts the <!DOCTYPE> and <HTML> tags by default */
		htput <HEAD>
		htput <TITLE> $HTtitle </TITLE>
		if "$stylesheet" != "" {
			htput <LINK REL="stylesheet" HREF="$stylesheet" TYPE="text/css">
			
		}
		htput <script src="toc.js" type="text/javascript"></script>
		htput </HEAD>
		htput <BODY onload="generateTOC(document.getElementById('toc'));">
	}


	/*****************************************/
	/* Executes the program                  */
	/*****************************************/
	display in yellow ".* $HTprog run on $S_DATE at $S_TIME"


	capture noisily do `"$HTprog"'
	local myrc = _rc
	tempfile borrar
	cap saveold `borrar', replace

	/*****************************************/
	/* Close HTML output                     */
	/* in case a htmlyesno = "yes"           */
	/*****************************************/
	

	if "`htmlyesno'" == "yes" {
		htput <DIV CLASS=sign><HR>Last update $S_DATE, 
		if (`"$colorer"' == "") {
			htput by <a href="do/$HTprog.txt">$HTprog.txt</a>
			if `"`xlog'"'== "" & `"`logyesno'"'== "yes" {
				htput . Log file in  <a href="log/`htlogfile'.txt">`htlogfile'.txt</a>
			}
		}
		else {
			htput by <a href="do/$HTprog.html">$HTprog.html</a>
			if `"`xlog'"'== "" & `"`logyesno'"'== "yes" {
				htput . Log file in  <a href="log/`htlogfile'.html">`htlogfile'.html</a>
			}

		}
		htput </DIV>
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
			quietly copy `"$log/`htlogfile'"' `"$htm/log/`htlogfile'.txt"', replace
		}
		else {
		   local cmd = `"! $colorer -h -t"text" -dc $ilogtype  -o$htm/log/`htlogfile'.html $log/`htlogfile'  "'
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
			quietly copy  `"`htprog'"'   `"$htm/do/`htprog'.txt"', replace
		}
		else {
		   local cmd =`"! $colorer -h -dc $idotype -o$htm/do/`htprog'.html `htprog'  "'
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
		quietly saveold `htgraph', replace
	}
	
	restore

	macro drop XHTGraph_*
	
	/*********************************************/
	/* Return errorcode to the system            */
	/*********************************************/

	exit `myrc'

end
