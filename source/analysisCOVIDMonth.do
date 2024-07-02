/* AnalysisSchoolClosure.do         DanielPailanir          yyyy-mm-dd:2021-12-17
*----|----1----|----2----|----3----|----4----|----5----|----6----|----7----|----8
  -- Write to Daniel requesting fix to apertura
*/
clear all
set more off

*-------------------------------------------------------------------------------
*--- (0) Global and some details
*-------------------------------------------------------------------------------
global ROOT "C:/Users/danie/OneDrive/Escritorio/Research/SchoolClosureViolence/"
global GRA "$ROOT/results/graphs"
global OUT "$ROOT/results/reg"
global LOG "$ROOT/results/log"

if `"`c(username)'"'=="damian"|`"`c(username)'"'=="dcc213" {
    global ROOT "/home/`c(username)'/investigacion/2022/childrenSchools"
    global GRA "$ROOT/replication/results/graphs"
    global OUT "$ROOT/replication/results/reg"
    global LOG "$ROOT/replication/results/log"
}
global DAT "$ROOT/replication/data"


set scheme plotplainblind, permanently
graph set window fontface "Times New Roman"

*-------------------------------------------------------------------------------
*-- (1) Triple difference event study and individual (closing)
*-------------------------------------------------------------------------------
use "$DAT/SchoolClosure_Final_RR_age.dta", clear

foreach var of varlist rate_* {
    replace `var' = 0 if `var'==.
}
gen monthtoclose = tm - tm(2020m3)
gen mes = month(dofm(tm))

local r1s rate_5_10 rate_6_8  rate_6_8
local r2s rate_0_4  rate_3_5  rate_0_4

local controls caseRate pcr positivity quarantine
tokenize `r2s'
foreach r1 of local r1s {
    local r2 `1'
    gen tripleDiff = (`r1' - `r2')

    #delimit ;
    eventdd tripleDif i.mes i.comuna `controls' [aw=populationyoung],
    timevar(monthtoclose) cluster(comuna) lags(10) leads(14) accum
    endpoints_op(ms(Dh) mc(midblue)) coef_op(ms(Dh) mc(midblue))
    ci(rarea, color(gs10%50))
    graph_op(legend(off) xtitle("Calendar Month", size(medlarge)) 
             ytitle("Criminal Complaints per 100,000", size(medlarge)) 
             ylabel(#6,format(%9.0f) labsize(medlarge))
             xlabel(-14 "{&le} Jan 2019" -10 "May 2019" -5 "Oct 2019" 
                    0 "Mar 2020" 5 "Aug 2020" 10 "{&ge} Jan 2021",
                    labsize(medium) angle(15)  ));
    graph export "$GRA/eventdd_controls_DDD_close_`r1'_`r2'.pdf", replace;
    
    eventdd `r1' i.mes i.comuna `controls' [aw=populationyoung],
    timevar(monthtoclose) cluster(comuna) lags(10) leads(14) accum
    endpoints_op(ms(Dh) mc(midblue)) coef_op(ms(Dh) mc(midblue))
    ci(rarea, color(gs10%50))
    graph_op(legend(off) xtitle("Calendar Month", size(medlarge)) 
             ytitle("Criminal Complaints per 100,000", size(medlarge)) 
             ylabel(#6,format(%9.0f) labsize(medlarge))
             xlabel(-14 "{&le} Jan 2019" -10 "May 2019" -5 "Oct 2019" 
                    0 "Mar 2020" 5 "Aug 2020" 10 "{&ge} Jan 2021",
                    labsize(medium) angle(15)));
    graph export "$GRA/eventdd_controls_DD_close_FD1_`r1'.pdf", replace;
    
    eventdd `r2' i.mes i.comuna `controls' [aw=populationyoung],
    timevar(monthtoclose) cluster(comuna) lags(10) leads(14) accum
    endpoints_op(ms(Dh) mc(midblue)) coef_op(ms(Dh) mc(midblue))
    ci(rarea, color(gs10%50))
    graph_op(legend(off) xtitle("Calendar Month", size(medlarge)) 
             ytitle("Criminal Complaints per 100,000", size(medlarge)) 
             ylabel(#6,format(%9.0f) labsize(medlarge))
             xlabel(-14 "{&le} Jan 2019" -10 "May 2019" -5 "Oct 2019" 
                    0 "Mar 2020" 5 "Aug 2020" 10 "{&ge} Jan 2021",
                    labsize(medium) angle(15)));
    graph export "$GRA/eventdd_controls_DD_close_FD2_`r2'.pdf", replace;
    #delimit cr
    macro shift
    drop tripleDif
}


*------------------------------------------------------------------------------*
*--- (2) Triple difference event study and individual (opening)
*------------------------------------------------------------------------------*
use $DAT/SchoolClosure_Final_RR.dta, clear

xtset comuna week
gen timeToClose = week-63
keep if year>=2019

*time to open schools
bys comuna (week): egen minOpen = min(week) if SchoolOpen_i==1
bys comuna: egen openStart = min(minOpen)
gen timeToOpen = week-openStart
drop if timeToClose<0

local controls caseRate pcr positivity quarantine p_comunal_m p_comunal_h

gcollapse (first) populationyoung (mean) `controls' SchoolOpen_i, by(month comuna)
ren month tm

merge 1:1 comuna tm using "$DAT/rate_by_age.dta", keep(1 3) nogen
foreach var of varlist rate_* {
    replace `var' = 0 if `var'==.
}


replace SchoolOpen_i=1 if SchoolOpen_i>0
bys comuna (tm): egen minOpen = min(tm) if SchoolOpen_i==1
bys comuna: egen openStart = min(minOpen)
gen timeToOpen = tm-openStart
gen mes = month(dofm(tm))

local r1s rate_5_10 rate_6_8  rate_6_8
local r2s rate_0_4  rate_3_5  rate_0_4

tokenize `r2s'
foreach r1 of local r1s {
    local r2 `1'
    gen tripleDiff = (`r1' - `r2')

    #delimit ;
    eventdd tripleDif i.mes i.comuna `controls' [aw=populationyoung],
    timevar(timeToOpen) cluster(comuna) lags(14) leads(10) accum
    endpoints_op(ms(Dh) mc(midblue)) coef_op(ms(Dh) mc(midblue))
    ci(rarea, color(gs10%50))
    graph_op(legend(off) xtitle("Months Relative to Schools Reopening") 
             ytitle("Criminal Complaints per 100,000")
             ylabel(-10(5)10,format(%9.0f))
             xlabel(-10 "{&le} -10" -8 "-8" -6 "-6" -4 "-4" -2 "-2" 
                    0 "0" 2 "2" 4 "4" 6 "6" 8 "8" 10 "10" 12 "12" 14 "{&ge} 14"));
    graph export "$GRA/eventdd_controls_DDD_open_`r1'_`r2'.pdf", replace;
    
    eventdd `r1' i.mes i.comuna `controls' [aw=populationyoung],
    timevar(timeToOpen) cluster(comuna) lags(14) leads(10) accum
    endpoints_op(ms(Dh) mc(midblue)) coef_op(ms(Dh) mc(midblue))
    ci(rarea, color(gs10%50))
    graph_op(legend(off) xtitle("Months Relative to Schools Reopening") 
             ytitle("Criminal Complaints per 100,000")
             ylabel(-10(5)10,format(%9.0f))
             xlabel(-10 "{&le} -10" -8 "-8" -6 "-6" -4 "-4" -2 "-2" 
                    0 "0" 2 "2" 4 "4" 6 "6" 8 "8" 10 "10" 12 "12" 14 "{&ge} 14"));
    graph export "$GRA/eventdd_controls_DD_open_FD1_`r1'.pdf", replace;
    
    eventdd `r2' i.mes i.comuna `controls' [aw=populationyoung],
    timevar(timeToOpen) cluster(comuna) lags(14) leads(10) accum
    endpoints_op(ms(Dh) mc(midblue)) coef_op(ms(Dh) mc(midblue))
    ci(rarea, color(gs10%50))
    graph_op(legend(off) xtitle("Months Relative to Schools Reopening") 
             ytitle("Criminal Complaints per 100,000")
             ylabel(-10(5)10,format(%9.0f))
             xlabel(-10 "{&le} -10" -8 "-8" -6 "-6" -4 "-4" -2 "-2" 
                    0 "0" 2 "2" 4 "4" 6 "6" 8 "8" 10 "10" 12 "12" 14 "{&ge} 14"));
    graph export "$GRA/eventdd_controls_DD_open_FD2_`r2'.pdf", replace;         
    #delimit cr
    drop tripleDif
    macro shift
}

