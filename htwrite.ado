*! write text in html format.
*! 
*! text should be between doble quotes (") and if it contains doble quotes 
*! within itself should be between macro quotes (`"....."')
*! 
*! text can contain html marks, as font or paragraph indicators 
*! (<P ALIGN="justify"></P> for justify, <B> for bold text, <i> for italian font...)
*! 
*! Version 1.0 30 APR 2009 by LQ
*! Version 1.1 08 FEB 2010 by LQ

*! Version 2.0 09 FEB 2010 by LQ, new manage for quotes
*! syntax anything(everything), [linesize(real -1)]

program htwrite
*version 8

syntax anything(everything), [linesize(real -1)]

	tempname length text mac1 mac2 quotyn quotini quotend quotinitxt quotendtxt
	local `length' = 0

	gettoken `text': anything
	gettoken `mac1' `mac2': `text', qed(`quotyn')

	while `"``mac1''"' != "" {
		if ``quotyn'' == 1 {
			local `quotini' = 1
			gettoken 1 2: `mac1'
			while "`1'" != "" {
				local `quotend' = cond("`2'" == "", 1, 0)
				local `quotinitxt' = cond(``quotini'' == 1, `"""', "")
				local `quotendtxt' = cond(``quotend'' == 1, `"""', "")
				
				if (`linesize' != -1) & (``length'' + ``quotini'' + length("`1'") + ``quotend'' > `linesize') {
					htput <BR>
					local `length' = 0
				}
				htput ``quotinitxt''`1'``quotendtxt''
				if `linesize' != -1 local `length' = ``length'' + length("`1'") + ``quotini'' + ``quotend'' + 1
				local `quotini' = 0
				gettoken 1 2: 2
			}
		}	
		else {
			if (`linesize' != -1) & (``length'' + length(`"``mac1''"') > `linesize') {
				htput <BR>
				local `length' = 0
			}
			htput ``mac1''
			if `linesize' != -1 local `length' = ``length'' + length(`"``mac1''"') + 1
		}
		gettoken `mac1' `mac2': `mac2', qed(`quotyn')
	}

end

