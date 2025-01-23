/* analysisAuxiliary.do          damiancclarke             yyyy-mm-dd:2024-07-01
----|----1----|----2----|----3----|----4----|----5----|----6----|----7----|----8



*/

vers 16
clear all
set more off
cap log close


*-------------------------------------------------------------------------------
*--- (0) locations
*-------------------------------------------------------------------------------
global ROOT "/home/`c(username)'/investigacion/2022/childrenSchools/replication"
global DAT "$ROOT/data"
global OUT "$ROOT/results/graphs/auxiliary"
global LOG "$ROOT/log"

cap mkdir "$OUT"
log using "$LOG/analysisAuxiliary.txt", text replace



*-------------------------------------------------------------------------------
*--- (1) Worldwide
*-------------------------------------------------------------------------------
import delimited using "$DAT/international/school-closures-covid", clear
ren day date
gen year=substr(date, 1, 4)
gen month=substr(date, 6, 2)
gen day=substr(date, 9, 2)
destring year month day, replace
gen d=mdy(month, day, year)
format d %d

gen c=1
collapse (sum) c, by(d school_closures)

*0 no measure
*1 recommended
*2 Required (only at some levels)
*3 Required (all levels)

reshape wide c, i(d) j(school_closures)

forval i=1/3 {
	replace c`i'=0 if c`i'==.
}

*nos quedamos hasta la ultima fecha con "todos"
keep if d<=22872

#delimit ;
tw line c3 d, lw(1pt) || line c2 d, lw(1pt) 
|| line c1 d, lw(1pt) || line c0 d, lw(1pt) lp(-......) ||, 
	xlabel(#12, angle(45)) xtitle("")
	legend(order(1 "Required (all levels)"
	             2 "Required (only at some levels)"
	             3 "Recommended" 
				 4 "No measure" ) 
	pos(12) col(2));
graph export "$OUT/countries_all.eps", replace;
#delimit cr


*-------------------------------------------------------------------------------
*--- (2) Attendance by age
*-------------------------------------------------------------------------------
use "$DAT/CASEN/CASEN2017_extract.dta", clear
keep if edad<20

gen asiste_0_4  =e3==1 if edad<5&e3!=.
gen asiste_5_10 =e3==1 if edad>=5&edad<10&e3!=.
gen asiste_0_1  =e3==1 if edad<1&e3!=.

foreach num of numlist 1(1)18 {
    local e2=`num'+1
    gen asiste_`num'_`e2' = e3==1 if edad>=`num'&edad<`e2'&e3!=.
}


collapse (mean) asiste* [pw=expc], by(comuna)
#delimit ;
hist asiste_0_4, ytitle(, size(large)) color(purple%60)
xtitle("Formal Education Attendance Rate 0-4 years", size(large))
xlabel(, labsize(medlarge) format(%03.1f)) ylabel(, labsize(medlarge));
#delimit cr
graph export "$OUT/attendance_0_4.eps", replace

#delimit ;
hist asiste_5_10, ytitle(, size(large))  color(purple%60)
xtitle("Formal Education Attendance Rate 5-10 years", size(large))
xlabel(, labsize(medlarge)  format(%03.1f)) ylabel(, labsize(medlarge));
#delimit cr
graph export "$OUT/attendance_5_10.eps", replace



*-------------------------------------------------------------------------------
*--- (3) Opening and development internationally
*-------------------------------------------------------------------------------
clear all
import excel using "$DAT/international/UNESCO_school_closures_database.xlsx", firstrow

rename CountryID countrycode
merge m:1 countrycode using "$DAT/international/GDPpc"
drop _merge
merge m:1 countrycode using "$DAT/international/pop"
bys countrycode: gen n=_n
encode Region2, gen(Region)

keep if yr2019<100000
colorpalette viridis, n(7)

graph set eps fontface "Times New Roman"
graph set window fontface "Times New Roman"
#delimit ;
twoway scatter Weeksfullyclose yr2019 if n==1 [aw=Enrolment],
  colorvar(Region) colordiscrete colorlist(`r(p)')
coloruseplegend
plegend(order(- "Region" 7 "SSA" 6 "Asia C/S" 5 "Asia E/SE"
              4 "LAC" 3 "Europe/NA" 2 "Oceania"  1 "MENA"))
||   qfit Weeksfullyclose yr2019 if n==1, lcolor(gs12%40) lpattern(solid)
ytitle("Weeks Fully Closed") xtitle("GDP per capita (2019)") legend(off)
scheme(stcolor);
graph export $OUT/schoolCloseGDP.eps, replace;
#delimit cr

*-------------------------------------------------------------------------------
*--- (4) Attendance under-reporting tests: currently commented out as data v large
*-------------------------------------------------------------------------------
foreach month in Octubre Noviembre {
    import delimited "$DAT/attendance/Asistencia_`month'_2018.csv", encoding(ISO-8859-1) clear
    keep if cod_ense==110
    keep if cod_grado==4
    bys mrun: gen nreps=_N
    bys mrun rbd: gen nreps2 = _N
    drop if (dia31==-3 & nreps>1 & dias_asistidos==0)
    drop if (dia30==-3 & nreps>1 & dias_asistidos==0)
    drop if (dia29==-3 & nreps>1 & dias_asistidos==0) 
    drop nreps
    drop nreps2
    bys mrun: gen nreps=_N
    bys mrun rbd: gen nreps2 = _N
    
    foreach num of numlist 1(1)31 {
        cap gen attendance_`month'_`num' = dia`num'==1 if dia`num'==1|dia`num'==2
    }
    
    collapse attendance_`month'_*
    gen a=1
    reshape long attendance_`month'_, i(a) j(date)
    drop a
    tempfile `month'Attendance
    save ``month'Attendance'
}
append using `OctubreAttendance'
keep if attendance_O!=. | attendance_N!=.
gen month = 11 if attendance_Noviembre_ !=.
replace month = 10 if attendance_Octubre_ !=.
sort month date

//drop Saturdays
drop if (date==10|date==17|date==24)&month==11
drop if (date==6 |date==13|date==20|date==27)&month==10
egen attendance = rowmin(attendance*)
gen timeToTest = _n-25

gen attendanceLower = 0.86
gen attendanceUpper = 0.95

#delimit ;
twoway rarea attendanceLower attendanceUpper timeToTest  if timeToTest >= -1 & timeToTest <= 1, color(gs12%70)
|| connected attendance timeToTest, lwidth(thick) lcolor(purple) lpattern(solid) mcolor(purple)
ylabel(0.86(0.02)0.94, format(%5.2f) labsize(medium)) legend(off)
xlabel(-20 "Fri Oct 5" -15 "Fri Oct 12 " -10 "Mon Oct 22" -5 "Mon Oct 29"
       0 "Wed Nov 7" 5 "Wed Nov 14" 10 "Wed Nov 21" 15 "Wed Nov 28",  labsize(medium) alternate)
xtitle("") ytitle("Attendance Rate", size(medlarge))
text(0.88 0 "SIMCE Test Day", place(e));
graph export "$OUT/attendanceSIMCEtime.pdf", replace;
#delimit cr


import delimited "$DAT/attendance/Asistencia_Noviembre_2018.csv", encoding(ISO-8859-1) clear
keep if cod_ense==110
keep if cod_grado==4

//se elimina mrun repetido, sin asistencia y retirado del colegio
bys mrun: gen nreps=_N
bys mrun rbd: gen nreps2 = _N
drop if (dia31==-3 & nreps>1 & dias_asistidos==0)
drop if (dia30==-3 & nreps>1 & dias_asistidos==0)
drop if (dia29==-3 & nreps>1 & dias_asistidos==0) 
drop nreps
drop nreps2
bys mrun: gen nreps=_N
bys mrun rbd: gen nreps2 = _N

gen asistencia_simceleng=1 if dia7==1
gen asistencia_simcemat=1 if dia6==1
gen inasistencia_simceleng=1 if dia7==2
gen inasistencia_simcemat=1 if dia6==2
gen repetido_leng=1 if nreps>1 & dia7==1
gen repetido_mat=1 if nreps>1 & dia6==1
gen repetido2_leng=1 if nreps2>1 & dia7==1
gen repetido2_mat=1 if nreps2>1 & dia6==1

bys mrun repetido_leng : gen nreps_leng=_N
bys mrun repetido_mat : gen nreps_mat=_N
order rbd mrun nreps nreps2 repetido_leng nreps_leng repetido2_leng repetido_mat nreps_mat repetido2_mat
drop if nreps_mat>1 & repetido_mat==1
drop if nreps_leng>1 & repetido_leng==1

#delimit ;
collapse (count) asistencia_simceleng asistencia_simcemat inasistencia_simceleng 
inasistencia_simcemat repetido_leng repetido_mat repetido2_leng repetido2_mat, by(rbd);
#delimit cr

merge 1:1 rbd using "$DAT/attendance/asistencia_simce42018.dta"

keep if _merge==3
drop _merge

drop if asistencia_simceleng==0 & nalu_lect4b_rbd>0
drop if asistencia_simcemat==0 & nalu_mate4b_rbd >0
local lstyle lcolor(red%50) lpattern(solid) lwidth(thick)
#delimit ;
twoway scatter asistencia_simcemat nalu_mate4b_rbd, mc(black%80) ms(Oh)
||  line asistencia_simcemat asistencia_simcemat, sort `lstyle'
ytitle("Reported Test Day Attendance") xtitle("Observed Test-takers")
legend(off);
graph export "$OUT/attendance_2018_simce4mate.pdf", replace;
#delimit cr

log close