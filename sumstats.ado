//! version 1.2 31DEC2019 Benjamin Daniels bbdaniels@gmail.com

// sumstats - Stata module to produce tables of summary statistics

cap prog drop sumstats
prog def sumstats

version 15.1 // Necessary for putexcel syntax

syntax anything using/ [aw fw],   ///
  stats(string asis)              ///
  [replace]

cap mat drop stats_toprint
qui {

// Separate into variable lists

  parenParse `anything'

// Initialize output Excel file

	putexcel set "`using'" , `replace'

	// Stats headers
	local col = 1
	foreach stat in `stats' {
		local ++col
		local theCol : word `col' of `c(ALPHA)'
		putexcel `theCol'1 = "`stat'" , bold
	}

// Loop over groups writing statistics and if-conditions

	local theRow = 1
	forvalues i = 1/`r(nStrings)' {
		local ++theRow

		// Catch if-condition if any, else print full sample
		if regexm("`r(string`i')'"," if ") {
			local ifcond = substr(`"`r(string`i')'"',strpos(`"`r(string`i')'"'," if ")+4,.)
			local justvars = substr(`"`r(string`i')'"',1,strpos(`"`r(string`i')'"'," if "))
			local ifcond = `"Subsample: `ifcond'"'
		}
		else {
      local ifcond "Full Sample"
      local justvars = "`r(string`i')'"
    }
		putexcel A`theRow' = `"`ifcond'"', bold

		// Get statistics
		local ++theRow
		qui tabstat  `r(string`i')'  ///
			[`weight'`exp'] ///
			, s(`stats') save
			mat a = r(StatTotal)'
			putexcel B`theRow' = matrix(a) , nformat(number_d2)

		// Get variable labels
		local varRow = `theRow'
		foreach var in `justvars' {
			local theLabel : var label `var'
			putexcel A`varRow' = "`theLabel'"
			local ++varRow
		}
	local theRow = `theRow' + `=rowsof(a)'
	}

// Finalize

	putexcel close

// end qui
}
di "Summary statistics output to {browse `using'}"
end

// Program to parse on parenthesis
cap prog drop parenParse
program def parenParse , rclass

  syntax anything

  local N = length(`"`anything'"')

  local x = 0
  local parCount = 0

  // Run through string
  forv i = 1/`N' {
    local char = substr(`"`anything'"',`i',1) // Get next character

    // Increment unit and counter when encountering open parenthesis
    if `"`char'"' == "(" {
      if `parCount' == 0 {
        local ++x // Start next item when encountering new block
      }
      else {
        local string`x' = `"`string`x''`char'"'
      }
      local ++parCount
    }
    // Otherwise de-increment counter if close parenthesis
    else if `"`char'"' == ")" {
      local --parCount
      if `parCount' != 0 local string`x' = `"`string`x''`char'"'
    }
    // Otherwise add character to string block
    else {
      local string`x' = `"`string`x''`char'"'
    }
  }

  // Return strings to calling program
  return scalar nStrings = `x'
  forv i = 1/`x' {
    return local string`i' = `"`string`i''"'
  }

end
// End

// Have a lovely day!
