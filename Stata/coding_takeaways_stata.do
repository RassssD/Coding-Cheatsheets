
/*******************************************************************************
IMPORTING VARIABLES VIA EXCEL
*******************************************************************************/

// List of variables to search for - based on excel sheet, takes in all variables in the "Variable" column
import excel using "Variables.xlsx", sheet("Variables") firstrow clear
levelsof Variable, clean
global list_allvars "`r(levels)'"




/*******************************************************************************
TRY / IF ERROR THEN
*******************************************************************************/

cap {do_something}

if _rc != 0 {
    di "Error"
    cap {do_something_else}
}



/*******************************************************************************
ESTTAB - MULTI-LINE COLUMN TITLES
*******************************************************************************/

estpost sum vars
esttab, mtitles("One line" "\shortstack{First Line\\Second Line}")


/*******************************************************************************
ESTTAB - SPAN MULTIPLE COLS WITH LINE
*******************************************************************************/

// Pattern: Put a number for each col, 1 if new group, 0 if same as previous
// The span prefix... does the line part

estpost sum vars
esttab, ///
    mgroups("Group 1" "Group 2", pattern(1 1 0 0) span prefix(\multicolumn{@span}{c}{) suffix(}) erepeat(\cmidrule(lr){@span}))


/*******************************************************************************
ESTTAB - FORMAT NUMBERS
*******************************************************************************/

estpost sum vars
esttab, cells("mean(fmt(2))")

/*******************************************************************************
ESTTAB - MATHS IN LINE
*******************************************************************************/

estpost sum vars
esttab, mtitles("Eq $\alpha=2$")


/*******************************************************************************
ESTTAB - NICE DESC STATS
*******************************************************************************/

estpost sum vars
esttab using "path.text", ///
	cells("mean(fmt(2))") t(3) label replace nonumbers mtitles("Col1" "Col2") collabels(, none)




/*******************************************************************************
GRAPHING - STYLING
*******************************************************************************/

line y x, lstyle(width(thick))




/*******************************************************************************
EVENT STUDY - USING RELATIVE TIME VAR
*******************************************************************************/

gen rel_time = abs_time - treat_time

sum rel_time
local min_rel_time = `r(min)'

gen rel_time_new = rel_time - $min_rel_time + 1

// Get the number of dummies and loop over them to give them the right label
unique rel_time
local n_rel_time_dummies = `r(unique)'

foreach counter of numlist 1(1)`n_rel_time_dummies' {
	
	local rct_counter = `counter' + `min_rel_time' - 1
	
	// Recode values iteratively
	cap drop rel_time_temp
	recode rel_time_new (`counter'=`counter' "`rct_counter'"), gen(rel_time_temp)
	drop rel_time_new
	rename rel_time_temp rel_time_new
}

// Find the dummy to use as base, and where to draw the event line
local dummy_exclude = $analysis_wave - 1
local event_xline = $analysis_wave

// Regression - uses the excluded one as base
eststo: reghdfe ///
	`outcome_var' ib(`dummy_exclude').rel_time_new
estimates store ests

coefplot ests, yline(0, lcolor(black)) vertical xline(`event_xline') graphregion(color(white)) xtitle("Relative Time") ytitle("Coefficient") title("Outcome") keep(*.rel_time_new) recast(connected) ciopts(recast(rcap)) omitted labels nokey baselevels



