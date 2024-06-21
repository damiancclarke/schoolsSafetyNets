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


/*
//Agregamos sistema//
preserve
local f "https://raw.githubusercontent.com/Daniel-Pailanir/Cuarentenas/master/Cuarentenas.csv"
import delimited using `f', clear encoding(UTF-8)
keep comuna nombre

replace nombre = ustrto(ustrnormalize(nombre, "nfd"), "ascii", 2)
replace nombre = lower(nombre)

replace nombre = "cabo de hornos" if nombre=="cabo de hornos          "
replace nombre = "calera de tango" if nombre=="calera  de tango"
replace nombre = "independencia" if nombre=="inpendencia"
replace nombre = "isla de pascua" if nombre=="isla de  pascua"
replace nombre = "porvenir" if nombre=="porvenir                "
replace nombre = "punta arenas" if nombre=="punta arenas            "
replace nombre = "quinta de tilcoco" if nombre=="quinta de  tilcoco"
replace nombre = "san jose de maipo" if nombre=="san jose  de maipo"
replace nombre = "san pedro de la paz" if nombre=="san pedro de  la paz"
replace nombre = "natales" if nombre=="natales                 "

tempfile dname
save `dname'


import excel using "$DAT/ANEXO_1721.xlsx", clear sheet(Ordenados) first
reshape long m@, i(comuna) j(time)
format %tm time
gen aux = m if comuna=="Total"
destring aux, replace
gegen total = min(aux), by(time)
drop aux
drop if comuna=="Total"

gen ingresos = m
replace ingresos = "999" if ingresos=="<10"
destring ingresos, replace
replace ingresos = 0 if ingresos==.
replace ingresos = . if ingresos==999

gegen comunas_parcial = count(total) if ingresos==. , by(time)

ipolate ingresos time, gen(ingresos_ip) by(comuna)
replace ingresos_ip = 1 if ingresos_ip>=0 & ingresos_ip<=5 & comunas_parcial!=.
replace ingresos_ip = 8 if ingresos_ip>10 & ingresos_ip!=. & comunas_parcial!=.
replace ingresos_ip = round(ingresos_ip, 1)-3 if ingresos_ip>5 & ingresos_ip<10 & comunas_parcial!=.
replace ingresos_ip=1 if ingresos_ip==.

ren comuna nombre

replace nombre = ustrto(ustrnormalize(nombre, "nfd"), "ascii", 2)
replace nombre = lower(nombre)

replace nombre = "cabo de hornos" if nombre=="cabo de hornos (ex navarino)"
replace nombre = "hijuela" if nombre=="hijuelas"
replace nombre = "la calera" if nombre=="calera"
merge m:1 nombre using `dname', keep(1 3) nogen
rename time tm
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
tempfile system
save `system'
restore

merge 1:1 comuna tm using `system', gen(_mergeSystem)

gen rateIngresos = ingresos_ip/populationyoung*100000
*/
a

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
local limit = r(mean)
local limit=0.15
gen highStrike=0
replace highStrike = 1 if strikevar>`limit' & strikevar!=.
*replace highStrike = 2 if strikevar>r(p50) & strikevar!=.
*replace highStrike = 3 if strikevar>r(p75) & strikevar!=.
gen time3 = floor(tm/3)
collapse strikeXduring_all rateSecon [aw=populationyoung], by(tm highStrike)

#delimit ;
twoway line rateS tm if tm<=tm(2013m12)&highStrike==0, lwidth(thick) lcolor(blue%70)
    || line rateS tm if tm<=tm(2013m12)&highStrike==1, lwidth(thick) lcolor(red%70)
legend(order(1 "Low Strike Exposure" 2 "High Strike Exposure") pos(1) ring(0))
ytitle("Rate of Criminal Complaints")
xline(617, lcolor(red)) xline(623, lcolor(red))
text(68 620 "Strike") text(66 620 "Period");
#delimit cr
graph export "$GRA/strikeDescriptive.pdf", replace
drop strikeX
reshape wide rateS, i(tm) j(highStrike)
gen diff=rateSecondary1-rateSecondary0
#delimit ;
twoway line diff tm if tm<=tm(2013m12), lwidth(thick) lcolor(blue%64)
ytitle("{&Delta} Reports (High-Low Strike Exposure)")
yline(0, lcolor(black%30) lpattern(dash))
xline(617, lcolor(red))
xline(623, lcolor(red))
text(19 620 "Strike") text(17.5 620 "Period");
#delimit cr
graph export "$GRA/strikeDescriptiveDiff.pdf", replace
restore

#delimit ;
hist strike_sin, scheme(plotplainblind) color(red%70) xlabel(, format(%04.2f))
xtitle("Strike Intensity");
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
//replace strikeXduring_sin = 0 if strikeXduring_sin==.


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

//Missing strikevar is either 2011, 2020, or 2021, or no students
bys comuna: egen strikevar = mean(strike_sin)
drop if strikevar==.
gen timeToStrike = (tm-tm(2011m6))

foreach num of numlist 0(1)12 {
    gen lead=timeToStrike == -`num'
    gen lead`num'XIntensity = lead*strikevar
    
    gen lag=timeToStrike == `num'
    gen lag`num'XIntensity = lag*strikevar
    drop lag lead
}
gen lead= timeToStrike<-12
gen lead13XIntensity = lead*strikevar
gen lag= timeToStrike>12
gen lag13XIntensity = lag*strikevar

drop lead0X lead lag

gen rateAll = rateSecondary+rateSASecondary+rateVSecondary

*reghdfe rateSecondary    lead* lag*  `conts' `wt', `opts'
*reghdfe rateSASecondary  lead* lag*  `conts' `wt', `opts'
*reghdfe rateVSecondary   lead* lag*  `conts' `wt', `opts'
#delimit ;
constraint 1 lead13XIntensity+lead12XIntensity+lead11XIntensity+
             lead10XIntensity+lead9XIntensity+lead8XIntensity
             lead7XIntensity+lead6XIntensity+lead5XIntensity
             lead4XIntensity+lead3XIntensity+lead2XIntensity
             lead1XIntensity=0;
#delimit cr
cnsreg rateAllSecondary  lead* lag* popSecondary `conts' i.comuna i.tm `wt', cluster(comuna) constraints(1)
gen EST = .
gen TIME = _n-14 in 1/27
gen LB = .
gen UB = .
foreach num of numlist 1(1)13 {
    if `num'==2 {
        replace EST = 0 if TIME==-2
        replace LB = 0 if TIME==-2
        replace UB = 0 if TIME==-2
    }
    else {
        replace EST=_b[lead`num'XIntensity] if TIME==-`num'
        replace LB =_b[lead`num'XIntensity]-1.64*_b[lead`num'XIntensity] if TIME==-`num'
        replace UB =_b[lead`num'XIntensity]+1.64*_b[lead`num'XIntensity] if TIME==-`num'
    }
}
foreach num of numlist 0(1)13 {
    replace EST=_b[lag`num'XIntensity] if TIME==`num'
    replace LB =_b[lag`num'XIntensity]-1.64*_b[lag`num'XIntensity] if TIME==`num'
    replace UB =_b[lag`num'XIntensity]+1.64*_b[lag`num'XIntensity] if TIME==`num'
}
#delimit ;
twoway rcap LB UB TIME, 
   || scatter EST TIME, mc(blue%80)
legend(order(2 "Point Estimate" 1 "90% CI") pos(6) rows(1))
xlabel(-13 "> 1 year" -12(2)12 13 "> 1 year", angle(55)) yline(0) xline(-0.5)
xtitle("Time to Strike") ytitle("Rate of Complaints");
#delimit cr
graph export "$GRA/strikeEvent_VIF.pdf", replace
exit

reghdfe rateDif          lead* lag*  `conts' `wt', `opts'
