/* strikes.do                        DP/DC                 yyyy-mm-dd:2024-06-06
----|----1----|----2----|----3----|----4----|----5----|----6----|----7----|----8

*/

vers 18
clear all
set more off
cap log close

*-------------------------------------------------------------------------------
*-- (0) Global and some details
*-------------------------------------------------------------------------------
global ROOT "C:/Users/danie/OneDrive/Escritorio/Research/SchoolClosureViolence/"
global ROOT "~/investigacion/2022/childrenSchools"

global DAT "$ROOT/replication/data"
global GRA "$ROOT/replication/results/graphs"
global OUT "$ROOT/replication/results/reg"
global LOG "$ROOT/replication/results/log"

set scheme plotplainblind, permanently
graph set window fontface "Times New Roman"


*-------------------------------------------------------------------------------
*-- (1) Set-up
*-------------------------------------------------------------------------------
use $DAT/Asistencia/asistencia_2011_2022.dta, clear
gen month = month(date)
gen year = year(date)
gen fecha = ym(year, month)
format %tm fecha
*keep if fecha<=tm(2019m12)

gen rateInatt_all = no_asiste_med/(no_asiste_med+asiste_med)
gen rateInatt_sin = no_asiste_med_srep/(no_asiste_med_srep+asiste_med_srep)


preserve
keep if year<2015
drop if year==2012
collapse (sum) asiste_bas asiste_med, by(date)
gen doy=doy(date)
gen yr2011=year(date)==2011
//drop weekends
drop if asiste_med<150000
collapse asiste_* , by(doy yr2011)

local l1 lwidth(thick) lcolor(red%60)  mc(red%60)  ms(Oh)
local l2 lwidth(thick) lcolor(blue%60) mc(blue%60) ms(Sh)
drop if doy>=258&doy<=265
drop if doy<70
drop if doy>365
replace asiste_med=asiste_med/1000
replace asiste_bas=asiste_bas/1000
#delimit ;
twoway connected asiste_med doy if yr2011 ==1, `l1'
||     connected asiste_med doy if yr2011 ==0, `l2'
xline(152, lcolor(gs5%80)) xlabel(60(30)360, labsize(medium))
ylabel(, format(%12.0gc) labsize(medium)) 
xtitle("Day of the year", size(medlarge))
ytitle("Secondary School Attendance (1000s)", size(medlarge))
legend(order(1 "Attendance 2011" 2 "Attendance 2013-2014")
pos(1) ring(0)) ;
#delimit cr
graph export "$GRA/attendanceTrendsStrikes.pdf", replace

#delimit ;
twoway connected asiste_bas doy if yr2011 ==1, `l1'
||     connected asiste_bas doy if yr2011 ==0, `l2'
xline(152, lcolor(gs5%80)) xlabel(60(30)360, labsize(medium))
ylabel(, format(%12.0gc) labsize(medium)) 
xtitle("Day of the year", size(medlarge))
ytitle("Primary School Attendance (1000s)", size(medlarge))
legend(order(1 "Attendance 2011" 2 "Attendance 2013-2014")
pos(1) ring(0)) ;
#delimit cr
graph export "$GRA/attendanceTrendsStrikes_Primary.pdf", replace
restore


collapse (mean) rateInatt*, by(com_cod fecha month year)

tab year

gen during = (fecha>=tm(2011m6) & fecha<=tm(2011m12))
foreach group in all sin {
    gen rateInatt08_11_`group'    = rateInatt_`group' if fecha==tm(2011m8)
    gen rateInatt08_12_`group'    = rateInatt_`group' if fecha==tm(2012m8)
    gen rateInatt08_other_`group' = rateInatt_`group' if month==8&year>2012
    bys com_cod: egen strike_`group'    =mean(rateInatt08_11_`group')
    bys com_cod: egen strikePost_`group'=mean(rateInatt08_other_`group')
    gen strikeD_`group' = strike_`group'-strikePost_`group'
    gen strikeXduring_`group' = strike_`group'*during
    gen strikedXduring_`group'= strikeD_`group'*during
}
ren (com_cod fecha) (comuna tm)

#delimit ;
local newcodes 16101 16102 16202 16203 16302 16103 16104 16204 16303 16105
         16106 16205 16107 16201 16206 16301 16304 16108 16305 16207 16109;
#delimit cr
tokenize `newcodes'
foreach number of numlist 1(1)21 {
    local oldcode = 8400+`number'
    dis "Old code is `oldcode'.  New code is `1'"
    replace comuna = `oldcode' if comuna==`1'
    macro shift
}
levelsof comuna

tempfile dasistencia
save `dasistencia'



//unimos con data mensual//
use "$DAT/SchoolClosure_Final_RR.dta", clear
xtset comuna week
format month %tm 
gen mm = month(monday)

collapse (sum) caso* (first) population* (mean) mm, by(month comuna)
ren month tm
#delimit ;
local newcodes 16101 16102 16202 16203 16302 16103 16104 16204 16303 16105
         16106 16205 16107 16201 16206 16301 16304 16108 16305 16207 16109;
#delimit cr
tokenize `newcodes'
foreach number of numlist 1(1)21 {
    local oldcode = 8400+`number'
    dis "Old code is `oldcode'.  New code is `1'"

    replace comuna = `oldcode' if comuna==`1'
    macro shift
}
merge 1:1 comuna tm using `dasistencia'
//Merge is good for time.
// But 22 comunas with no colegios and so no strike measure


gen rate = caso/populationyoung*100000
gen strikeperiod = (tm>=tm(2011m4) & tm<=tm(2011m12))
gen popSecondary = population4+population5
gen popPrimary   = population1+population2+population3
gen rateSecondary   = (caso4+caso5)/popSecondary*100000
gen rateVSecondary  = (casoV4+casoV5)/popSecondary*100000
gen rateSASecondary = (casoSA4+casoSA5)/popSecondary*100000
gen rateAllSecondary = (caso4+caso5+casoV4+casoV5+casoSA4+casoSA5)/popSecondary*100000

gen ratePrimary   = (caso1+caso2+caso3)/popPrimary*100000
gen rateVPrimary  = (casoV1+casoV2+casoV3)/popPrimary*100000
gen rateSAPrimary = (casoSA1+casoSA2+casoSA3)/popPrimary*100000
gen rateAllPrimary =(caso1+caso2+caso3+casoV1+casoV2+casoV3+casoSA1+casoSA2+casoSA3)/popPrimary*100000

gen rateDif   = rateSecondary-ratePrimary
gen rateVDif  = rateVSecondary-rateVPrimary
gen rateSADif = rateSASecondary-rateSAPrimary
gen rateAllDiff = rateAllSecondary-rateAllPrimary

*-------------------------------------------------------------------------------
*-- (2) Descriptives
*-------------------------------------------------------------------------------
preserve
bys comuna: egen strikevar = mean(strikeD_sin)
count
sum strikevar, d
local limit = r(mean)
local limit = 0.15
gen highStrike=0
replace highStrike = 1 if strikevar>`limit' & strikevar!=.
*replace highStrike = 2 if strikevar>r(p50) & strikevar!=.
*replace highStrike = 3 if strikevar>r(p75) & strikevar!=.
gen time3 = floor(tm/3)
gen rateAll = rateSASec+rateVSec+rateSec
collapse strikeXduring_all rateSec [aw=populationyoung], by(time3 highStrike)

#delimit ;
twoway line rate time3 if time3<=214&highStrike==0, lwidth(thick) lcolor(blue)
    || line rate time3 if time3<=214&highStrike==1, lwidth(thick) lcolor(red)
legend(order(1 "Low Strike" 2 "High Strike"))
xline(205, lcolor(red)) xline(208, lcolor(red));
#delimit cr
graph export "$GRA/strikeDescriptive_3m.pdf", replace
restore

preserve
bys comuna: egen strikevar = mean(strikeD_sin)
count
sum strikevar, d
local limit=0.15
gen highStrike=0
replace highStrike = 1 if strikevar>`limit' & strikevar!=.

gen time3 = floor(tm/3)
collapse strikeXduring_all rateSecon [aw=populationyoung], by(tm highStrike)

#delimit ;
twoway line rateS tm if tm<=tm(2013m12)&highStrike==0, lwidth(thick) lcolor(blue%70)
    || line rateS tm if tm<=tm(2013m12)&highStrike==1, lwidth(thick) lcolor(red%70)
legend(order(1 "Low Strike Exposure" 2 "High Strike Exposure") pos(1) ring(0))
ytitle("Rate of Criminal Complaints", size(medlarge)) xtitle("Month", size(medlarge))
xlabel(, labsize(medium)) ylabel(, labsize(medium)) 
xline(617, lcolor(red)) xline(623, lcolor(red))
text(68 620 "Strike") text(66 620 "Period");
#delimit cr
graph export "$GRA/strikeDescriptive.pdf", replace
drop strikeX
reshape wide rateS, i(tm) j(highStrike)
gen diff=rateSecondary1-rateSecondary0

#delimit ;
twoway line diff tm if tm<=tm(2013m12), lwidth(thick) lcolor(blue%64)
ytitle("{&Delta} Reports (High-Low Strike Exposure)", size(medlarge))
xtitle("Month", size(medlarge)) xlabel(, labsize(medium)) 
ylabel(, labsize(medium)) yline(0, lcolor(black%30) lpattern(dash))
xline(617, lcolor(red)) xline(623, lcolor(red))
text(19 620 "Strike") text(17.5 620 "Period");
#delimit cr
graph export "$GRA/strikeDescriptiveDiff.pdf", replace
restore

#delimit ;
hist strike_sin, scheme(plotplainblind) color(red%70) 
xlabel(, format(%04.2f) labsize(medium))
ylabel(, labsize(medium))
xtitle("Strike Intensity", size(medlarge))
ytitle("Density", size(medlarge));
#delimit cr
graph export "$GRA/strikeIntensity.pdf", replace


*-------------------------------------------------------------------------------
*-- (3) Model
*-------------------------------------------------------------------------------
local conts popS
local conts
local wt    [aw=popS]
local opts  abs(comuna tm) cluster(comuna)

replace strikeXduring_sin=0  if tm<tm(2011m1)
replace strikedXduring_sin=0 if tm<tm(2011m1)

drop if tm>tm(2017m12)


reghdfe rateSecondary strikeXduring_all  `conts' `wt', abs(comuna tm) cluster(comuna)
reghdfe rateSecondary strikedXduring_all `conts' `wt', abs(comuna tm) cluster(comuna)
sum strike_sin
local sd=r(sd)

eststo: reghdfe rateAllSecondary strikeXduring_sin  `conts' `wt', `opts'
sum rateAllSecondary if e(sample)==1
estadd scalar mean=r(mean)
estadd scalar effect = _b[strikeXduring_sin]*`sd'

eststo: reghdfe rateSecondary    strikeXduring_sin  `conts' `wt', `opts'
sum rateSecondary if e(sample)==1
estadd scalar mean=r(mean)
estadd scalar effect = _b[strikeXduring_sin]*`sd'

eststo: reghdfe rateSASecondary  strikeXduring_sin  `conts' `wt', `opts'
sum rateSASecondary if e(sample)==1
estadd scalar mean=r(mean)
estadd scalar effect = _b[strikeXduring_sin]*`sd'

eststo: reghdfe rateVSecondary   strikeXduring_sin  `conts' `wt', `opts'
sum rateVSecondary if e(sample)==1
estadd scalar mean=r(mean)
estadd scalar effect = _b[strikeXduring_sin]*`sd'

//Difference value
sum strikeD_sin
local sd=r(sd)
eststo: reghdfe rateAllSecondary strikedXduring_sin `conts' `wt', `opts'
sum rateAllSecondary if e(sample)==1
estadd scalar mean=r(mean)
estadd scalar effect = _b[strikedXduring_sin]*`sd'

eststo: reghdfe rateSecondary    strikedXduring_sin `conts' `wt', `opts'
sum rateSecondary if e(sample)==1
estadd scalar mean=r(mean)
estadd scalar effect = _b[strikedXduring_sin]*`sd'

eststo: reghdfe rateSASecondary  strikedXduring_sin `conts' `wt', `opts'
sum rateSASecondary if e(sample)==1
estadd scalar mean=r(mean)
estadd scalar effect = _b[strikedXduring_sin]*`sd'

eststo: reghdfe rateVSecondary   strikedXduring_sin `conts' `wt', `opts'
sum rateVSecondary if e(sample)==1
estadd scalar mean=r(mean)
estadd scalar effect = _b[strikedXduring_sin]*`sd'

esttab est1 est2 est3 est4 est5 est6 est7 est8
lab var strikeXduring_sin "Strike Intensity $\times$ Strike"
#delimit ;
estout est1 est2 est3 est4 using "$OUT/strikesMain.tex", replace
cells(b(fmt(a3) star) se(par fmt(a3))) keep(strikeXduring_sin) style(tex)
stats(N r2 effect mean, fmt(%12.0gc %04.2f %05.2f %05.2f )
      labels("\\ Observations" "R-squared" "Scaled Effect" "Mean Dep.\  Var."))
starlevels(* 0.10 ** 0.05 *** 0.01) label
mlabels(none) collabels(none);
#delimit cr


lab var strikedXduring_sin "Alternative Strike Intensity $\times$ Strike"
#delimit ;
estout est5 est6 est7 est8 using "$OUT/strikesAlt.tex", replace
cells(b(fmt(a3) star) se(par fmt(a3))) keep(strikedXduring_sin) style(tex)
stats(N r2 effect mean, fmt(%12.0gc %04.2f %05.2f %05.2f)
      labels("\\ Observations" "R-squared" "Scaled Effect" "Mean Dep.\  Var."))
starlevels(* 0.10 ** 0.05 *** 0.01) label
mlabels(none) collabels(none);
#delimit cr
estimates clear


//Summary statistics
gen strikeIntense = strike_sin
replace strikeIntense = 0 if strike_sin==.

lab var rateAllSecondary "All Violence"
lab var rateSecondary    "Intrafamily Violence"
lab var rateSASecondary  "Sexual Abuse"
lab var rateVSecondary   "Rape"
lab var strikeperiod     "Strike Period"
lab var strikeIntense    "Strike Intensity"
lab var popSecondary     "Population of Secondary Students"

#delimit ;
local PA rateAllS rateSec rateSASe rateVS strikeperiod strikeIntense popSecondary;
estpost sum `PA' if e(sample)==1;
estout using "$OUT/summaryStrikes.tex", replace label  mlabels(,none) 
collabels(,none) cells("count() mean(fmt(2)) sd(fmt(2)) min(fmt(2)) max(fmt(2))") 
style(tex);
#delimit cr


local wt    [aw=popP]
eststo: reghdfe rateAllPrimary strikeXduring_sin  `conts' `wt', `opts'
eststo: reghdfe ratePrimary    strikeXduring_sin  `conts' `wt', `opts'
eststo: reghdfe rateSAPrimary  strikeXduring_sin  `conts' `wt', `opts'
eststo: reghdfe rateVPrimary   strikeXduring_sin  `conts' `wt', `opts'

eststo: reghdfe rateAllPrimary strikedXduring_sin `conts' `wt', `opts'
eststo: reghdfe ratePrimary    strikedXduring_sin `conts' `wt', `opts'
eststo: reghdfe rateSAPrimary  strikedXduring_sin `conts' `wt', `opts'
eststo: reghdfe rateVPrimary   strikedXduring_sin `conts' `wt', `opts'

esttab est1 est2 est3 est4 est5 est6 est7 est8
estimates clear



sum strike_sin
exit
local sd=r(sd)
local wt    [aw=popS]
eststo: reghdfe rateAllDif strikeXduring_sin  `conts' `wt', `opts'
sum rateAllDif if e(sample)==1
estadd scalar mean=r(mean)
estadd scalar effect = _b[strikeXduring_sin]*`sd'

eststo: reghdfe rateDif    strikeXduring_sin  `conts' `wt', `opts'
sum rateDif if e(sample)==1
estadd scalar mean=r(mean)
estadd scalar effect = _b[strikeXduring_sin]*`sd'

eststo: reghdfe rateSADif  strikeXduring_sin  `conts' `wt', `opts'
sum rateSADif if e(sample)==1
estadd scalar mean=r(mean)
estadd scalar effect = _b[strikeXduring_sin]*`sd'

eststo: reghdfe rateVDif   strikeXduring_sin  `conts' `wt', `opts'
sum rateVDif if e(sample)==1
estadd scalar mean=r(mean)
estadd scalar effect = _b[strikeXduring_sin]*`sd'

eststo: reghdfe rateAllDif strikedXduring_sin `conts' `wt', `opts'
eststo: reghdfe rateDif    strikedXduring_sin `conts' `wt', `opts'
eststo: reghdfe rateSADif  strikedXduring_sin `conts' `wt', `opts'
eststo: reghdfe rateVDif   strikedXduring_sin `conts' `wt', `opts'

esttab est1 est2 est3 est4 est5 est6 est7 est8
lab var strikeXduring_sin "Strike Intensity $\times$ Strike"
#delimit ;
estout est1 est2 est3 est4 using "$OUT/strikesTripleDiff.tex", replace
cells(b(fmt(a3) star) se(par fmt(a3))) keep(strikeXduring_sin) style(tex)
stats(N r2 effect mean, fmt(%12.0gc %04.2f %05.2f %05.2f )
      labels("\\ Observations" "R-squared" "Scaled Effect" "Mean Dep.\  Var."))
starlevels(* 0.10 ** 0.05 *** 0.01) label
mlabels(none) collabels(none);
#delimit cr

estimates clear

