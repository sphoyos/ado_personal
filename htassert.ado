program define htassert
*! version 1.0 by JJAV 22 Feb 2001, Ifakara
syntax, Title(string) Expr(string) List(varlist) [NOStop]

cap assert (`expr')
if _rc == 0 | _rc == 9 {
	if _rc == 9 {
		htput <BR/><B>`title'<B><BR/>
		htlist `list' if ! (`expr')
		if "`nostop'" == ""{
			error 9
		}
	}
}
else {
	error _rc
}
end
