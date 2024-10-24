clear all
set more off

*-------------------------------------------------------------------------------
*Global and some details
*-------------------------------------------------------------------------------
global ROOT "C:/Users/danie/OneDrive/Escritorio/Research/SchoolClosureViolence/"
global ROOT "/home/damian/investigacion/2022/childrenSchools/replication"

global DAT "$ROOT/data"
global GRA "$ROOT/results/graphs"
global OUT "$ROOT/results/reg"
global LOG "$ROOT/results/log"

set scheme plotplainblind, permanently
graph set window fontface "Times New Roman"

*-------------------------------------------------------------------------------
* Event Study
*-------------------------------------------------------------------------------
insheet using "$DAT/covid/vacunacion_comuna_fecha_all_2daDosis_fagonza.csv", clear
reshape long v, i(region cod_region comuna cod_comuna poblacion) j(d)
//Drop "desconocido" comunas (all vaccines known)
drop if v==.
rename v vaccines
bys comuna (d): gen day = dmy(23,12,2020)+_n
gen weekday = dow(day)
gen weekNum = ceil(((day-3)/7)-3182)
gen monday = day if weekday==2
collapse (min) monday (sum) vaccines (mean) poblacion, by(comuna cod_comuna weekNum)
rename comuna comunaName
rename cod_comuna comuna
drop weekNum
drop if monday==.
tempfile vaccines
save `vaccines'


    
use $DAT/SchoolClosure_Final_RR.dta, clear
merge 1:1 comuna monday using `vaccines', keep(1 3)
drop _merge
replace vaccines=0 if vaccines == .

global controls caseRate pcr positivity quarantine movilidad2 p_comunal_mujer p_comunal_hom vaccines

xtset comuna week
gen timeToClose = week-63
keep if year>=2019
format month %tm 

gen mm = month(monday)

gcollapse (mean) $controls timeToClose mm, by(month comuna)
ren month time

tempfile dclose
save `dclose'


use $DAT/SchoolClosure_Final_RR.dta, clear
merge 1:1 comuna monday using `vaccines', keep(1 3)
drop _merge
replace vaccines=0 if vaccines == .

xtset comuna week
gen timeToClose = week-63
keep if year>=2019

bys comuna (week): egen minOpen = min(week) if SchoolOpen_i==1
bys comuna: egen openStart = min(minOpen)
gen timeToOpen = week-openStart
drop if timeToClose<0

global controls caseRate pcr positivity quarantine movilidad2 p_comunal_mujer p_comunal_hom vaccines

gcollapse (mean) $controls SchoolOpen_i, by(month comuna)
ren month time

tempfile dopen
save `dopen'


*------------------------------------------------------------------------------*
*comuna
*------------------------------------------------------------------------------*
import delimited using "https://raw.githubusercontent.com/Daniel-Pailanir/Cuarentenas/master/Cuarentenas.csv", clear encoding(UTF-8)
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

*------------------------------------------------------------------------------*
*sename
*------------------------------------------------------------------------------*
*global dat "C:\Users\danie\OneDrive\Escritorio\Research\SchoolClosureViolence\data\"


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

ren m ingresos_original
order comuna nombre time ingresos*
drop comunas_

gen year = year(dofm(time))
global pop "C:\Users\danie\OneDrive\Escritorio\Trabajo\Population"
merge m:1 comuna year using "$DAT/populationINE_5-18.dta", keep(1 3) nogen

*------------------------------------------------------------------------------*
*analisis
*------------------------------------------------------------------------------*
preserve

merge 1:1 time comuna using `dclose', //keep(1 3)
gen rate = ingresos_ip/population*100000

gen monthtoclose = time - tm(2020m3)
gen mes = month(dofm(time))
keep if time>=tm(2016m1)

foreach var of varlist $controls {
    replace `var'=0 if `var'==.
}
gen region = floor(comuna/1000)

#delimit ;
eventdd rate i.mes#i.region i.comuna $controls [aw=populationyoung],
timevar(monthtoclose) cluster(comuna) lags(10) leads(14) accum
endpoints_op(ms(Dh) mc(midblue)) coef_op(ms(Dh) mc(midblue))
graph_op(legend(off) xtitle("Calendar Time", size(medlarge)) 
         ytitle("Number of Children per 100,000", size(medlarge))
         ylabel(#6, format(%9.0f) labsize(medium))
         xlabel(-14 "{&le} Jan 2020" -10 "May 2019" -5 "Oct 2019" 
		0 "Mar 2020" 5 "Aug 2020" 10 "{&ge} Jan 2021",
                angle(45) labsize(medium))
         legend(order(1 "Point Estimate" 1 "95% CI") pos(1) ring(0)
                rows(1)));
#delimit cr
graph export "$GRA/eventdd_controls_close_SENAME.eps", replace


#delimit ;
eventdd rate i.mes#i.region i.comuna [aw=populationyoung],
timevar(monthtoclose) cluster(comuna) lags(24) leads(24) accum
endpoints_op(ms(Dh) mc(midblue)) coef_op(ms(Dh) mc(midblue))
graph_op(legend(off) xtitle("Months Relative to Schools Close") 
         ytitle("Number of Children per 100,000")
         ylabel(#6,format(%9.0f))
         xlabel(-24 "{&le} -24" -18 "-18" -12 "-12" -6 "-6" -3 "-3" 
		 "0" 0 "0" 3 "3" 6 "6" 12 "12" 18 "18" 24 "{&ge} 24"));
#delimit cr
graph export "$GRA/eventdd_Nocontrols_close_SENAME.eps", replace

restore

merge 1:1 time comuna using `dopen', //keep(1 3)
gen rate = ingresos_ip/population*100000

bys comuna (time): egen minOpen = min(time) if SchoolOpen_i==1
bys comuna: egen openStart = min(minOpen)
gen timeToOpen = time-openStart


gen mes = month(dofm(time))
gen region = floor(comuna/1000)
keep if time>=tm(2016m1)
foreach var of varlist $controls {
    replace `var'=0 if `var'==.
}

replace quarantine=ceil(quarantine)
#delimit ;
local controls i.quarantine#c.caseRate i.quarantine#c.pcr
               i.quarantine#c.positivity
               i.quarantine#c.movilidad2 SchoolOpen_i
               p_comunal_mujer p_comunal_hom vaccines;

eventdd rate i.comuna i.mes `controls'  [aw=populationyoung],
timevar(timeToOpen) cluster(comuna) lags(14) leads(10) accum baseline(-2)
endpoints_op(ms(Dh) mc(midblue)) coef_op(ms(Dh) mc(midblue))
graph_op(xtitle("Months Relative to Schools Reopening", size(medlarge)) 
         ytitle("Number of Children per 100,000", size(medlarge)) legend(off) 
         xlabel(-10 "{&le} -10" -5 "-5" 0 "0" 5 "5" 10 "10" 14 "{&ge} 14",
                labsize(medium))
         ylabel(#6,format(%9.0f) labsize(medium)));
#delimit cr
graph export "$GRA/eventdd_controls_open_SENAME.eps", replace

#delimit ;
eventdd rate i.mes i.comuna [aw=populationyoung],
timevar(timeToOpen) cluster(comuna) lags(14) leads(10) accum
endpoints_op(ms(Dh) mc(midblue)) coef_op(ms(Dh) mc(midblue))
graph_op(legend(off) xtitle("Months Relative to Schools Reopening") 
         ytitle("Number of Children per 100,000")
         ylabel(#6,format(%9.0f)))
;
#delimit cr
graph export "$GRA/eventdd_Nocontrols_open_SENAME.eps", replace



#delimit ;
eventdd caseRate i.mes i.comuna [aw=populationyoung],
timevar(timeToOpen) cluster(comuna) lags(14) leads(10) accum
endpoints_op(ms(Dh) mc(midblue)) coef_op(ms(Dh) mc(midblue))
graph_op(legend(off) xtitle("Months Relative to Schools Reopening") 
         ytitle("Number of Children per 100,000")
         ylabel(#6,format(%9.0f)))
;
#delimit cr
graph export "$GRA/eventdd_caseRate.eps", replace










