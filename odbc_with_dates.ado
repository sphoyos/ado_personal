
****** programa per passar dates amb odbc**********************


program define odbc_with_dates
    version 11.1
    syntax [varlist], table(string) [*]

    if ("`varlist'" == "") {
        local varlist _all
    }
    else {
        // no op
    }

    local date_variable_list
    foreach variable of varlist `varlist' {
        local variable_format : format `variable'
        if (strpos("`variable_format'", "%t") == 1) {
            local date_variable_list `date_variable_list' `variable'
        }
        else {
            // continue
        }
    }

    preserve
version 11.1 
    quietly tostring `date_variable_list', force replace ///
        usedisplayformat

    odbc insert `varlist', table("`table'") create `options'

    foreach variable  in `date_variable_list' {
        odbc exec("ALTER TABLE `table' ALTER COLUMN `variable' DATETIME"), ///
           `options'
   }

    restore
end



