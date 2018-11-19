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


program define htdo
	args chapter item

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

	/*
 		Normal use is for two parameters but in 
		case of a complete filename in one parameter 
		it check first if match the pattern
	*/
	local digits = "0123456789"
	local d1 = substr("`chapter'",1,1)
	local d2 = substr("`chapter'",2,1)
	local d4 = substr("`chapter'",4,1)
	local d5 = substr("`chapter'",5,1)

	* index() function will work only if version is less than 9

	if (_caller() < 9) {
		if "`item'" == "" & index("`digits'","`d1'") & index("`digits'","`d2'")  & index("`digits'","`d4'")  & index("`digits'","`d5'")  {
			local item = substr("`chapter'",4,2)
			local chapter=substr("`chapter'",1,2)
		}
		else {
			if `"`item'"' == "" {
			do "`chapter'"
			exit
			}
		}
	}
	else {
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
	}	 
	if ( _caller() < 8) {
		_htdo7 `chapter' `item'	
	}
	else {
		_htdo8 `chapter' `item'
	}
end

program define _htdo7
	version 7.0
	args chapter item
		noi di in yellow  "Running htdo7 with `chapter' `item'
		
	****************************************************
	* Save previous log status and close it            *
	* A new log is required to print the directory     *
	* to obtain the filename                           *
	****************************************************
	quietly log
	local logname = r(filename)
	if "`logname'" == "." {
	        local xlog = ""
		local xlogon = ""
		local xlogty = ""
		}
	else {
		local xlog = r(filename)
		local xlogon = r(status)
		local xlogty = r(type)
	}	
    if `"`xlog'"' !=  "" {
            quietly log close
    }
	local xlogli: set linesize

	****************************************************
	* List directory with match pattern                *
	****************************************************
	tempfile directory
	quietly log using `directory', text
	dir `chapter'_`item'*.do
	
	****************************************************
	* close log and reopen previous logfile            *
	****************************************************
	quietly log close
	set linesize `xlogli'
	if `"`xlog'"' != "" {
	   quietly log using `"`xlog'"', append `xlogty'
	}

	*************************************************************************
	* Import directory and proccess it - define propierties of the filename *
	* Specific version for windows output...                                 *
	*************************************************************************

	capture {
	quietly infile str10 size str10 datestr str10 time str80 filename using `directory', clear
	quietly compress
	quietly gen date = date(datestr,"mdy",2070)
	quietly format date %dd/m/cy
	}	
	if _rc != 0 {
		noi di in red "Cannot read the directory"
		error(_rc)
	}	

	if _N == 0 | (_n == 1 & filename[1] == ""){
		noi di in red "Cannot find any matching name"
		error(1)
	}
	
	if _N > 1 {
		noi di in red "Multiple matchs. Cannot resolve the name"
		error(1)

	}

	local filename = filename[1]
	local filedate : di %dd_m_CY date[1]
	local filetime = time[1]
	clear

	cap confirm file `"`filename'"'
	if _rc != 0 {
		noi di in red "Cannot find the file"
		error(_rc)
	}

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
	else {
		noi di in red "docver `version' not supported!"
		error 1
	}

end

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
  	  _process_v2 `"`chapter'"' `"`item'"' `"`f1'"' `"`filename'"' `"`xlog'"'
	}
	else {
		noi di in red "docver `version' not supported!"
		error 1
	}
end



program define _process_v1
args chapter item f1 filename xlog

	/* reads three more lines: title, htmlout, and comments */
	local foundversion = 1
	file  read `f1' line
	if index(`"`line'"',"title:") == 0 {
		noi di in red "Not found 'title:' directive"
		error(1)
	}
	global HTtitle = trim(substr(`"`line'"',index(`"`line'"',"title:")+6,.))

	file  read `f1' lined
	if index(`"`lined'"',"htmlout:") != 0 {
		local htmlout = trim(substr(`"`lined'"',index(`"`lined'"',"htmlout")+8	,.))
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
	local  htlogfile =`"`chapter'_`item'_`menuname'.log"'
	if "`htmlout'" == "yes" {
		local  htfile  =`"`chapter'_`item'_`menuname'.html"'
	}
	global HTprog  = `"`chapter'_`item'_`menuname'.do"'

	/* rename file with the title found in docver */
	if `"$HTprog"' != `"`filename'"' {
		copy `"`filename'"' `"$HTprog"'
		erase `"`filename'"'
	}
	/* test macros and global variables */
	
	/*	
	noi di in red "chapter: `chapter'"
	noi di in red "item: `item'"
	noi di in red "title: $HTtitle "
	noi di in red "html: `htfile' "
	noi di in red "log:  `htlogfile'"
	noi di in red "prg: $HTprog "
	*/

	/* Documentation of the do file */

	*******************************************************
	* Open database document or creates it if do not exist *
	********************************************************
	local htdoc = "$dta/document.dta"
	cap confirm file `"`htdoc'"'
	if _rc > 0 {
		local newfile = "YES"
		quietly set obs 1
		quietly gen chapter = .
	    quietly gen item = .
		quietly gen str1 title = "."
		quietly gen str1 program = "."
		quietly gen str1 output = "."
		quietly gen str1 created = "."
		quietly gen str1 comments = "."
	}
	else {
		quietly use `"`htdoc'"'
	}
	local change = 0

	quietly count if chapter == `chapter' & item == `item'
	if r(N) == 0 {
	   /* new record! */
		if "`newfile'" != "YES" {
		    local xobs = _N+1
		    quietly set obs `xobs'
		}		
		quietly replace chapter = `chapter' if chapter == .
		quietly replace item = `item' if item == .
		quietly replace created = `"`filedate'"' if chapter == `chapter' & item == `item'
		local change = 1
	}

	/* old record. Only change if there is a change in the names */
	quietly count if chapter == `chapter' & item == `item' & trim(title) == trim(`"$HTtitle"')
	if r(N) != 1 {
		quietly replace title = `"$HTtitle"' if chapter == `chapter' & item == `item'
		local change = 1
	}
	quietly count if chapter == `chapter' & item == `item' & trim(program) == trim(`"$HTprog"')
	if r(N) != 1 {
		quietly replace program = `"$HTprog"' if chapter == `chapter' & item == `item'
		local change = 1
	}
	quietly count if chapter == `chapter' & item == `item' & trim(output) == trim(`"`htfile'"')
	if r(N) != 1 {
		quietly replace output = `"`htfile'"' if chapter == `chapter' & item == `item'
		local change = 1
	}
	quietly count if chapter == `chapter' & item == `item' & trim(comments) == trim(`"`htcomment'"')
	if r(N) != 1 {
		quietly replace comments = `"`htcomment'"' if chapter == `chapter' & item == `item'
		local change = 1
	}
	
	if `change' > 0  {
		quietly saveold `"`htdoc'"', replace
		quietly drop *
	}
	/*****************************************/
	/* Open log file for this analysis       */
	/* in case a previous one was not opened */
	/*****************************************/
	if `"`xlog'"' == "" {
		quietly log using `"$log/`htlogfile'"', replace text
	}


	/*****************************************/
	/* Open HTML output                      */
	/* in case a htmlout = "yes"             */
	/*****************************************/
	if "`htmlout'" == "yes" {
		htopen using `"$htm/`htfile'"', replace
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
	cap saveold `borrar', replace

	/*****************************************/
	/* Close HTML output                     */
	/* in case a htmlout = "yes"             */
	/*****************************************/
	if "`htmlout'" == "yes" {
		htput <DIV CLASS=sign><HR>Last update $S_DATE <BR> by $HTprog </DIV>
		htput </BODY>
		htclose
	}


	/*****************************************/
	/* close log file for this analysis      */
	/* in case a previous one was not opened */
	/*****************************************/
	if `"`xlog'"' == "" {
		quietly log close
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
	/* Return errorcode to the system            */
	/*********************************************/

	exit `myrc'

end


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
	
	/*********************************************/
	/*********************************************/
	/*********************************************/
	/*********************************************/
	
	local menuname= subinstr("`menuname'","á","a",.)
    local menuname= subinstr("`menuname'","Á","a",.) 
    local menuname= subinstr("`menuname'","à","a",.)
    local menuname= subinstr("`menuname'","À","a",.)
    local menuname= subinstr("`menuname'","é","e",.)
    local menuname= subinstr("`menuname'","É","e",.)
    local menuname= subinstr("`menuname'","è","e",.)
    local menuname= subinstr("`menuname'","È","e",.)
    local menuname= subinstr("`menuname'","í","i",.)
    local menuname= subinstr("`menuname'","Í","i",.)
    local menuname= subinstr(`"`menuname'"',"ò","o",.)
    local menuname= subinstr(`"`menuname'"',"Ò","o",.)
    local menuname= subinstr("`menuname'","ó","o",.)
    local menuname= subinstr("`menuname'","Ó","o",.)
    local menuname= subinstr("`menuname'","ú","u",.)
    local menuname= subinstr("`menuname'","Ú","u",.)
	local menuname=lower("`menuname'")
	
    /*********************************************/
	/*********************************************/
	/*********************************************/
	
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
		quietly gen str1 titlemenu="."
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
		quietly replace titlemenu="$HTtitle"   if chapter == `chapter' & item == `item'
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

		local date_prog=subinstr("$S_DATE"," ","_",.)+"_"+subinstr("$S_TIME",":","_",.)	
	    *_`date_prog'
		
	/* Copy the  log file under the html tree */
	if `"`logyesno'"' == "yes" & `myrc' == 0 {
		if (`"$colorer"' == "") {
		
			if "`chapter'"!="00" {
			quietly copy  `"$log/`htlogfile'"' `"$htm/log/ver_`date_prog'_`htlogfile'.txt"', replace
					}
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
		
		local date_prog=subinstr("$S_DATE"," ","_",.)+"_"+subinstr("$S_TIME",":","_",.)
	    *_`date_prog'
		
		if (`"$colorer"' == "") {
		if "`chapter'"!="00" {
			quietly copy  `"`htprog'"'   `"$htm/do/ver_`date_prog'_`htprog'.txt"', replace
			}
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
