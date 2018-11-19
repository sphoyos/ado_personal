program define htsign
*! Version 1.0 Aug 16 1999
*! This program closes and sign html ouptut 
*! Note: Uses sign class which should be defined elsewhere

version 6.0
syntax, file(string)
htput <DIV CLASS=sign><HR>Last update $S_DATE <BR> by `file'</DIV>

end
