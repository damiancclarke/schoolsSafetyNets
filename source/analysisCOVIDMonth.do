/* AnalysisSchoolClosure.do         DanielPailanir          yyyy-mm-dd:2021-12-17
*----|----1----|----2----|----3----|----4----|----5----|----6----|----7----|----8


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


/*
*------------------------------------------------------------------------------*
*--- (1) Triple difference event study closing
*------------------------------------------------------------------------------*
use "$DAT/SchoolClosure_Final_RR_01102024.dta", clear
foreach var of varlist privado vulnerable prioritario {
    bys comuna (month): replace `var'=`var'[_n-1] if `var'==.
}

keep if year>=2019
local controls   caseRate pcr positivity quarantine p_comunal_m p_comunal_h privado vulnerable prioritario
local level      by(month comuna)
local population Poblacion* populationyoung
gcollapse (first) `population' (sum) caso* (mean) `controls' SchoolClose, `level'

ren month tm
gen monthtoclose = tm - tm(2020m3)
gen mes = month(dofm(tm))

local a1 0 5  5
local a2 4 10 17
tokenize `a2'

foreach age1 of local a1 {
    local age2 `1'
    dis "`age1' to `age2'"
    local caserange
    local poprange
    foreach aa of numlist `age1'(1)`age2' {
        local caserange `caserange' casoEdad`aa'  
        local poprange  `poprange'  Poblacion`aa' 
    }
    egen caso_`age1'_`age2' = rowtotal(`caserange')
    egen pop_`age1'_`age2'  = rowtotal(`poprange')
    gen  rate_`age1'_`age2'  = caso_`age1'_`age2'/pop_`age1'_`age2'*100000
    macro shift
}



local r1s rate_5_10 rate_5_17  
local r2s rate_0_4  rate_0_4   

tokenize `r2s'
local controls caseRate pcr positivity quarantine

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
*--- (2) Triple difference event study (opening)
*------------------------------------------------------------------------------*
use $DAT/SchoolClosure_Final_RR_01102024.dta, clear
foreach var of varlist privado vulnerable prioritario {
    bys comuna (month): replace `var'=`var'[_n-1] if `var'==.
}

xtset comuna week
gen timeToClose = week-63
keep if year>=2019
drop if timeToClose<0

local controls   caseRate pcr positivity quarantine p_comunal_m p_comunal_h privado vulnerable prioritario
local level      by(month comuna)
local population Poblacion* populationyoung
gcollapse (first) `population' (sum) caso* (mean) `controls' SchoolOpen_i, `level'
ren month tm

replace SchoolOpen_i=1 if SchoolOpen_i>0
bys comuna (tm): egen minOpen = min(tm) if SchoolOpen_i==1
bys comuna: egen openStart = min(minOpen)
gen timeToOpen = tm-openStart
gen mes = month(dofm(tm))

local a1 0 5  5
local a2 4 10 17
tokenize `a2'

foreach age1 of local a1 {
    local age2 `1'
    dis "`age1' to `age2'"
    local caserange
    local poprange
    foreach aa of numlist `age1'(1)`age2' {
        local caserange `caserange' casoEdad`aa' 
        local poprange  `poprange'  Poblacion`aa' 
    }
    egen caso_`age1'_`age2' = rowtotal(`caserange')
    egen pop_`age1'_`age2'  = rowtotal(`poprange')
    gen  rate_`age1'_`age2'  = caso_`age1'_`age2'/pop_`age1'_`age2'*100000
    macro shift
}


local r1s rate_5_10 rate_5_17  
local r2s rate_0_4  rate_0_4   
tokenize `r2s'
local controls   caseRate pcr positivity quarantine  p_comunal_m p_comunal_h privado vulnerable prioritario

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
*/

*------------------------------------------------------------------------------*
*--- (3) Comparison
*------------------------------------------------------------------------------*
use "$DAT/SchoolClosure_Final_RR_01102024.dta", clear
foreach var of varlist privado vulnerable prioritario {
    bys comuna (month): replace `var'=`var'[_n-1] if `var'==.
}
keep if year>=2019

local a1 0 5  5
local a2 4 10 17
tokenize `a2'
foreach age1 of local a1 {
    local age2 `1'
    dis "`age1' to `age2'"
    local caserange
    local poprange
    foreach aa of numlist `age1'(1)`age2' {
        local caserange `caserange' casoEdad`aa' 
        local poprange  `poprange'  Poblacion`aa' 
    }
    egen caso_`age1'_`age2' = rowtotal(`caserange')
    egen pop_`age1'_`age2'  = rowtotal(`poprange')
    gen  rate_`age1'_`age2'  = caso_`age1'_`age2'/pop_`age1'_`age2'*100000
    macro shift
}

local controls   quarantine caseRate pcr positivity  



local r1s rate_5_10 rate_5_17  
local r2s rate_0_4  rate_0_4   

tokenize `r2s'
local depvars SchoolClose2 SchoolOpen_i
local close SchoolOpen_i
local open  SchoolClose2
foreach r1 of local r1s {
    local r2 `1'
    gen tripleDiff = (`r1' - `r2')

    eststo: reghdfe `r2' `depvars' [aw=populationyoung], absorb(w comuna) cluster(comuna)
    eststo: reghdfe `r1' `depvars' [aw=populationyoung], absorb(w comuna) cluster(comuna)
    local Close0 = _b[`open']
    local Open0  = _b[`close']

    eststo: reghdfe `r2' `depvars' `controls' [aw=populationyoung], absorb(w comuna) cluster(comuna)
    eststo: reghdfe `r1' `depvars' `controls' [aw=populationyoung], absorb(w comuna) cluster(comuna)
    local Close1 = _b[`open']
    local Open1  = _b[`close']
    estadd scalar ratioClose = `Close1'/`Close0'
    estadd scalar ratioOpen  = `Open1'/`Open0'



    eststo: reghdfe tripleDiff `depvars' `controls' [aw=populationyoung], absorb(w comuna) cluster(comuna)
    local CloseTD = _b[`open']
    local OpenTD  = _b[`close']
    estadd scalar ratioClose = `CloseTD'/`Close1'
    estadd scalar ratioOpen  = `OpenTD'/`Open1'

    esttab est1 est2 est3 est4 est5, keep(`depvars') stats(N ratioClose ratioOpen)
    #delimit ;
    esttab est1 est2 est3 est4 est5 using "$OUT/tripleDiff_`r1'_`r2'.tex", 
    b(%-9.3f) se(%-9.3f) noobs keep(`depvars') nonotes nogaps mlabels(, none)
    stats(N ratioClose ratioOpen, fmt(%9.0gc %05.3f %05.3f)
          label("\\ Observations" "Coefficient Ratio (Close)" "Coefficient Ratio (Open)"))
    nonumbers style(tex) starlevel ("*" 0.10 "**" 0.05 "***" 0.01) 
    fragment replace noline label;
    #delimit cr
    estimates clear
    drop tripleDiff
}


//Month

local controls   caseRate pcr positivity quarantine p_comunal_m p_comunal_h privado vulnerable prioritario
local level      by(month comuna)
local population Poblacion* populationyoung
gcollapse (first) `population' (sum) caso* (mean) `controls' SchoolOpen_i SchoolClose2, `level'
ren month tm

replace SchoolOpen_i=1 if SchoolOpen_i>0
replace SchoolClose =1  if SchoolClose >0
tab SchoolOpen_i SchoolClose
exit

bys comuna (tm): egen minOpen = min(tm) if SchoolOpen_i==1
bys comuna: egen openStart = min(minOpen)
gen timeToOpen = tm-openStart
gen mes = month(dofm(tm))

local a1 0 5  5
local a2 4 10 17
tokenize `a2'

foreach age1 of local a1 {
    local age2 `1'
    dis "`age1' to `age2'"
    local caserange
    local poprange
    foreach aa of numlist `age1'(1)`age2' {
        local caserange `caserange' casoEdad`aa' 
        local poprange  `poprange'  Poblacion`aa' 
    }
    egen caso_`age1'_`age2' = rowtotal(`caserange')
    egen pop_`age1'_`age2'  = rowtotal(`poprange')
    gen  rate_`age1'_`age2'  = caso_`age1'_`age2'/pop_`age1'_`age2'*100000
    macro shift
}




local a1 0 5  5
local a2 4 10 17
tokenize `a2'
foreach age1 of local a1 {
    local age2 `1'
    dis "`age1' to `age2'"
    local caserange
    local poprange
    foreach aa of numlist `age1'(1)`age2' {
        local caserange `caserange' casoEdad`aa' 
        local poprange  `poprange'  Poblacion`aa' 
    }
    egen caso_`age1'_`age2' = rowtotal(`caserange')
    egen pop_`age1'_`age2'  = rowtotal(`poprange')
    gen  rate_`age1'_`age2'  = caso_`age1'_`age2'/pop_`age1'_`age2'*100000
    macro shift
}

local controls   quarantine caseRate pcr positivity



local r1s rate_5_10 rate_5_17  
local r2s rate_0_4  rate_0_4   

tokenize `r2s'
local depvars SchoolClose SchoolOpen_i
local close SchoolOpen_i
foreach r1 of local r1s {
    local r2 `1'
    gen tripleDiff = (`r1' - `r2')

    eststo: reghdfe `r2' `depvars' [aw=populationyoung], absorb(w comuna) cluster(comuna)
    eststo: reghdfe `r1' `depvars' [aw=populationyoung], absorb(w comuna) cluster(comuna)
    local Close0 = _b[SchoolClose]
    local Open0  = _b[`close']

    eststo: reghdfe `r2' `depvars' `controls' [aw=populationyoung], absorb(w comuna) cluster(comuna)
    eststo: reghdfe `r1' `depvars' `controls' [aw=populationyoung], absorb(w comuna) cluster(comuna)
    local Close1 = _b[SchoolClose]
    local Open1  = _b[`close']
    estadd scalar ratioClose = `Close1'/`Close0'
    estadd scalar ratioOpen  = `Open1'/`Open0'



    eststo: reghdfe tripleDiff `depvars' `controls' [aw=populationyoung], absorb(w comuna) cluster(comuna)
    local CloseTD = _b[SchoolClose]
    local OpenTD  = _b[`close']
    estadd scalar ratioClose = `CloseTD'/`Close1'
    estadd scalar ratioOpen  = `OpenTD'/`Open1'

    esttab est1 est2 est3 est4 est5, keep(`depvars') stats(N ratioClose ratioOpen)
    estimates clear
    drop tripleDiff
}




local controls caseRate pcr positivity quarantine p_comunal_m p_comunal_h

gcollapse (first) population* (mean) `controls' SchoolOpen_i, by(month comuna)
ren month tm

merge 1:1 comuna tm using "$DAT/rate_by_age.dta", keep(1 3) nogen

foreach var of varlist rate_* {
    replace `var' = 0 if `var'==.
}
gen tripleDiff = (rate_5_10 - rate_0_4)
gen SchoolClose = tm>=tm(2020m3)&SchoolOpen_i==0
replace SchoolOpen_i=1 if SchoolOpen_i>0
gen mes = month(dofm(tm))

eststo: reghdfe rate_0_4 SchoolClose SchoolOpen_i  [aw=populationyoung], absorb(mes comuna)
eststo: reghdfe rate_5_10 SchoolClose SchoolOpen_i [aw=populationyoung], absorb(mes comuna)
local Close0 = _b[SchoolClose]
local Open0  = _b[SchoolOpen_i]
eststo: reghdfe rate_0_4 SchoolClose SchoolOpen_i `controls' [aw=populationyoung], absorb(mes comuna)
eststo: reghdfe rate_5_10 SchoolClose SchoolOpen_i `controls' [aw=populationyoung], absorb(mes comuna)

local Close1 = _b[SchoolClose]
local Open1  = _b[SchoolOpen_i]


eststo: reghdfe tripleDiff SchoolClose SchoolOpen_i `controls' [aw=populationyoung], absorb(mes comuna)
local CloseTD = _b[SchoolClose]
local OpenTD  = _b[SchoolOpen_i]

dis `CloseTD'/`Close1'
dis `OpenTD'/`Open1'

dis `Close1'/`Close0'
dis `Open1'/`Open0'


esttab est1 est2 est3 est4 est5, keep(SchoolClose SchoolOpen_i)
estimates clear

*-------------------------------------------------------------------------------
*-- (1) Triple difference event study and individual (closing)
*-------------------------------------------------------------------------------
use "$DAT/SchoolClosure_Final_RR_age_28092024.dta", clear

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
use $DAT/SchoolClosure_Final_RR_01102024.dta, clear

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
local va caso6 caso7 caso8 caso9 caso10 caso11 caso12 caso13 caso14 caso15 caso16 caso17
egen total6_17 = rowtotal(`va')
egen pop6_17   = rowtotal(Poblacion6-Poblacion17)
gen rate_6_17 = total6_17/pop6_17*100000

replace SchoolOpen_i=1 if SchoolOpen_i>0
bys comuna (tm): egen minOpen = min(tm) if SchoolOpen_i==1
bys comuna: egen openStart = min(minOpen)
gen timeToOpen = tm-openStart
gen mes = month(dofm(tm))

local r1s rate_5_10 rate_6_8  rate_6_8 rate_6_17
local r2s rate_0_4  rate_3_5  rate_0_4 rate_0_4

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

