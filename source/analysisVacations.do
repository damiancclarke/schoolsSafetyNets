/* analysisVacations.do          damiancclarke             yyyy-mm-dd:2024-09-10
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
global OUT "$ROOT/results/graphs/vacations"
global TAB "$ROOT/results/reg"
global LOG "$ROOT/results/log"

cap mkdir "$OUT"

log using "$LOG/analysisVacations.txt", text replace

*-------------------------------------------------------------------------------
*--- (1) Open data
*-------------------------------------------------------------------------------
use "$DAT/denuncias_fechas_clean.dta"

*-------------------------------------------------------------------------------
*--- (2) Descriptives by calendar time
*-------------------------------------------------------------------------------
preserve
collapse (sum) caso, by(date)
twoway line caso date
graph export "$OUT/casesByDate.pdf", replace
gen day = day(date)
twoway line caso date if day!=1
graph export "$OUT/casesByDateNoFirst.pdf", replace

gen year = year(date)
twoway line caso date if day!=1&year==2018
graph export "$OUT/casesByDateNoFirst_2018.pdf", replace
restore

preserve
collapse (sum) caso, by(mes año)
gen date = año+(mes-1)/12
sort date
generate upper=2000
#delimit ;
local barcall upper date if inrange(date, 2009.917, 2010.0833) |
                            inrange(date, 2010.917, 2011.0833) |
                            inrange(date, 2011.917, 2012.0833) |
                            inrange(date, 2012.917, 2013.0833) |
                            inrange(date, 2013.917, 2014.0833) |
                            inrange(date, 2014.917, 2015.0833) |
                            inrange(date, 2015.917, 2016.0833) |
                            inrange(date, 2016.917, 2017.0833) |
                            inrange(date, 2017.917, 2018.0833) |
                            inrange(date, 2018.917, 2019.0833) |
                            inrange(date, 2019.917, 2020.0833),
                            bcolor(gs12%60) base(500) barwidth(0.25);

local winter xline(2010.6) xline(2011.6) xline(2012.6) xline(2013.6)                            
             xline(2014.6) xline(2015.6) xline(2016.6) xline(2017.6)
             xline(2018.6) xline(2019.6);
twoway bar `barcall' 
 ||  line caso date, lcolor(red%60) lwidth(medthick) lpattern(solid)
 xlabel(2010(2)2022) legend(order(2 "Reported Cases" 1 "Summer Vacation") pos(1) ring(0) rows(1))
 ylabel(, format(%7.0gc) labsize(medium)) ytitle("Reported Violence Against Children", size(medlarge))
 xlabel(, labsize(medium)) xtitle("Date", size(medlarge));
graph export "$OUT/casesByDateMonth.pdf", replace;
#delimit cr
restore

gen young = edad<=4
preserve
collapse (sum) caso, by(mes año young)
bys año young: egen yearmean = mean(caso)
gen casoRestandard = (caso-yearmean) 

gen date = año+(mes-1)/12
sort date
generate upper=300
#delimit ;
local barcall upper date if inrange(date, 2009.917, 2010.0833) |
                            inrange(date, 2010.917, 2011.0833) |
                            inrange(date, 2011.917, 2012.0833) |
                            inrange(date, 2012.917, 2013.0833) |
                            inrange(date, 2013.917, 2014.0833) |
                            inrange(date, 2014.917, 2015.0833) |
                            inrange(date, 2015.917, 2016.0833) |
                            inrange(date, 2016.917, 2017.0833) |
                            inrange(date, 2017.917, 2018.0833) |
                            inrange(date, 2018.917, 2019.0833) |
                            inrange(date, 2019.917, 2020.0833) ,
                            bcolor(gs14%60) base(-300) barwidth(0.25);

local winter xline(2010.6) xline(2011.6) xline(2012.6) xline(2013.6)                            
             xline(2014.6) xline(2015.6) xline(2016.6) xline(2017.6)
             xline(2018.6) xline(2019.6);

twoway bar `barcall'
 ||   line casoR date if young==0&date<2020, lwidth(thick) lcolor(red%60) lpattern(solid)
 ||   line casoR date if young==1&date<2020, lwidth(thick) lcolor(blue%60) lpattern(solid)
 yline(0) legend(order(2 "5-18 years" 3 "0-4 years" 1 "Summer Vacation") pos(1) ring(0) rows(1))
 ylabel(-300(150)300, labsize(medium)) ytitle("Deviation from Yearly Average", size(medlarge))
 xlabel(, labsize(medium)) xtitle("Date", size(medlarge));
graph export "$OUT/casesByDateMonth_age.pdf", replace;
#delimit cr
restore

*-------------------------------------------------------------------------------
*--- (3) Descriptives by relative time
*-------------------------------------------------------------------------------
replace fin_clases_mes=12 if fin_clases_mes==.
foreach year of numlist 2011(1)2021 {
    gen summerleave  = dmy(fin_clases_dia,fin_clases_mes,`year') if año==`year'
    bys region:  egen summer_leave_`year' = min(summerleave)
    gen summerreturn = dmy(inicio_clases_dia,inicio_clases_mes,`year')  if año==`year'
    bys region:  egen summer_return_`year' = min(summerreturn)
    gen winterleave  = dmy(inicio_vacaciones_dia,inicio_vacaciones_mes,`year')  if año==`year' 
    bys region:  egen winter_leave_`year' = min(winterleave)
    gen winterreturn = dmy(fin_vacaciones_dia,fin_vacaciones_mes,`year')  if año==`year'
    bys region:  egen winter_return_`year' = min(winterreturn)
    drop summerleave summerreturn winterleave winterreturn
}
local weekendShift=0
foreach year of numlist 2011(1)2021 {
    gen time_to_summer_leave_`year' = date-summer_leave_`year'+`weekendShift'
    gen time_to_summer_return_`year'= date-summer_return_`year'
    gen time_to_winter_leave_`year' = date-winter_leave_`year'+`weekendShift'
    gen time_to_winter_return_`year'= date-winter_return_`year'
}
// SUMMER LEAVE -- needs 2011 and then remove all pre-2012 because no 2010 dates
preserve
gen timeToNearestSummerLeave = .
gen exposureYearSummerLeave = .
foreach year of numlist 2012(1)2021 {
    local lyear = `year'-1
    gen test1 = time_to_summer_leave_`lyear'
    gen test2 = time_to_summer_leave_`year'
    local clastyear abs(test1)< abs(test2) & test1!=. & test2!=. & year==`year'
    local cthisyear abs(test1)>=abs(test2) & test1!=. & test2!=. & year==`year'
    replace timeToNearestSummerLeave = time_to_summer_leave_`lyear' if `clastyear'
    replace timeToNearestSummerLeave = time_to_summer_leave_`year' if `cthisyear'
    replace exposureYearSummerLeave = `lyear' if `clastyear'
    replace exposureYearSummerLeave = `year' if `cthisyear'
    drop test1 test2
}


drop if year<2012&year>2020
//drop if edad<4
collapse (sum)  caso, by(timeToNearestSummerLeave year)
gen weeks = floor(timeToNearestSummerLeave/7)
collapse (sum) caso, by(weeks year)
collapse       caso, by(weeks)
#delimit ;
twoway connected caso weeks if weeks>-20&weeks<14, 
lcolor(blue%70) lwidth(thick)  ms(Sh) mcolor(blue%70)
xtitle("Weeks Until Summer Vacations", size(medlarge)) 
xlabel(-20(5)10, labsize(medlarge)) 
ytitle("Reported Violence Against Children", size(medlarge))
xline(-1, lcolor(red)) ylabel(, format(%9.0gc) labsize(medlarge));
#delimit cr
graph export "$OUT/casesAroundSummerClose.pdf", replace
tempfile summerClose
save `summerClose'
restore


// SUMMER RETURN
preserve
gen timeToNearestSummerReturn = .
gen exposureYearSummerReturn = .
foreach year of numlist 2012(1)2020 {
    local nyear = `year'+1
    gen test1 = time_to_summer_return_`year'
    gen test2 = time_to_summer_return_`nyear'
    local cthisyear abs(test1)< abs(test2) & test1!=. & test2!=. & year==`year'
    local cnextyear abs(test1)>=abs(test2) & test1!=. & test2!=. & year==`year'
    replace timeToNearestSummerReturn = time_to_summer_return_`year'  if `cthisyear'
    replace timeToNearestSummerReturn = time_to_summer_return_`nyear' if `cnextyear'
    replace exposureYearSummerReturn = `year'  if `cthisyear'
    replace exposureYearSummerReturn = `nyear' if `cnextyear'
    drop test1 test2
}

drop if year<2011&year>2018
//drop if edad<4
collapse (sum) caso, by(timeToNearestSummerReturn year)
gen weeks = floor(timeToNearestSummerReturn/7)
collapse (sum) caso, by(weeks year)
collapse       caso, by(weeks)
#delimit ;
twoway connected caso weeks if weeks>-20&weeks<15, 
lcolor(blue%70) lwidth(thick) ms(Sh) mcolor(blue%70)
xtitle("Weeks Until Beginning of School", size(medlarge)) 
xlabel(-20(5)10, labsize(medlarge)) 
ytitle("Reported Violence Against Children", size(medlarge))
xline(-1, lcolor(red)) ylabel(, format(%9.0gc) labsize(medlarge));
#delimit cr
graph export "$OUT/casesAroundSummerReturn.pdf", replace
tempfile summerReturn
save `summerReturn'
restore


// WINTER LEAVE -- needs 2011 and then remove all pre-2012 because no 2010 dates
preserve
gen timeToNearestWinterLeave = .
gen exposureYearWinterLeave = .
foreach year of numlist 2012(1)2021 {
    local lyear = `year'-1
    gen test1 = time_to_winter_leave_`lyear'
    gen test2 = time_to_winter_leave_`year'
    local clastyear abs(test1)< abs(test2) & test1!=. & test2!=. & year==`year'
    local cthisyear abs(test1)>=abs(test2) & test1!=. & test2!=. & year==`year'
    replace timeToNearestWinterLeave = time_to_winter_leave_`lyear' if `clastyear'
    replace timeToNearestWinterLeave = time_to_winter_leave_`year' if `cthisyear'
    replace exposureYearWinterLeave = `lyear' if `clastyear'
    replace exposureYearWinterLeave = `year' if `cthisyear'
    drop test1 test2
}

drop if year<2012&year>2020
//drop if edad<4
collapse (sum) caso, by(timeToNearestWinterLeave year)
gen weeks = floor(timeToNearestWinterLeave/7)
collapse (sum) caso, by(weeks year)
collapse       caso, by(weeks)
#delimit ;
twoway connected caso weeks if weeks>-20&weeks<14, 
lcolor(blue%70) lwidth(thick)  ms(Sh) mcolor(blue%70)
xtitle("Weeks Until Winter Vacations", size(medlarge)) 
xlabel(-20(5)10, labsize(medlarge)) 
ytitle("Reported Violence Against Children", size(medlarge))
xline(-1, lcolor(red)) ylabel(, format(%9.0gc) labsize(medlarge));
#delimit cr
graph export "$OUT/casesAroundWinterClose.pdf", replace
tempfile winterClose
save `winterClose'
restore


// SUMMER RETURN
preserve
gen timeToNearestWinterReturn = .
gen exposureYearWinterReturn = .
foreach year of numlist 2012(1)2020 {
    local nyear = `year'+1
    gen test1 = time_to_winter_return_`year'
    gen test2 = time_to_winter_return_`nyear'
    local cthisyear abs(test1)< abs(test2) & test1!=. & test2!=. & year==`year'
    local cnextyear abs(test1)>=abs(test2) & test1!=. & test2!=. & year==`year'
    replace timeToNearestWinterReturn = time_to_winter_return_`year'  if `cthisyear'
    replace timeToNearestWinterReturn = time_to_winter_return_`nyear' if `cnextyear'
    replace exposureYearWinterReturn = `year'  if `cthisyear'
    replace exposureYearWinterReturn = `nyear' if `cnextyear'
    drop test1 test2
}

drop if year<2011&year>2018
drop if edad<4
collapse (sum) caso, by(timeToNearestWinterReturn year)
gen weeks = floor(timeToNearestWinterReturn/7)
collapse (sum) caso, by(weeks year)
collapse       caso, by(weeks)
#delimit ;
twoway connected caso weeks if weeks>-20&weeks<15, lcolor(blue%70) lwidth(thick)  ms(Sh) mcolor(blue%70)
xtitle("Weeks Until Return From Winter Vacations", size(medlarge)) 
xlabel(-20(5)10, labsize(medlarge)) 
ytitle("Reported Violence Against Children", size(medlarge))
xline(-1, lcolor(red)) ylabel(, format(%9.0gc) labsize(medlarge));
#delimit cr
graph export "$OUT/casesAroundWinterReturn.pdf", replace
tempfile winterReturn
save `winterReturn'
restore

*-------------------------------------------------------------------------------
*--- (4) Attendance -- set up time to vacations
*-------------------------------------------------------------------------------
#delimit ;
local c1 8401  8402  8403  8404  8405  8406  8407  8408  8409  8410  8411  8412  
         8413  8414 8415  8416  8417  8418  8419  8420  8421;
local c2 16101 16102 16202 16203 16302 16103 16104 16204 16303 16105 16106 16205 
         16107 16201 16206 16301 16304 16108 16305 16207 16109;
#delimit cr

//systematise comuna numbers for changed codes
preserve
use "$DAT/Asistencia/asistencia_2011_2022.dta", clear
tokenize `c1'
foreach c of local c2 {
    replace com_cod=`1' if com_cod==`c'
    macro shift
}
tempfile attendance
save `attendance'
restore


local d1 fin_vacaciones_dia fin_vacaciones_mes inicio_clases_dia inicio_clases_mes
local d2 inicio_vacaciones_dia inicio_vacaciones_mes fin_clases_dia fin_clases_mes
drop if edad<=4
gen casoPrimary   = edad>=6&edad<=13
gen casoSecondary = edad>=14&edad<=17

collapse `d1' `d2' region (sum) caso casoPrimary casoSecondary, by(comuna date) 
gen  year = year(date)
drop if year==2010 | year>2020
rename comuna com_cod

//systematise comuna numbers for changed codes
tokenize `c1'
foreach c of local c2 {
    replace com_cod=`1' if com_cod==`c'
    macro shift
}
drop year

merge 1:1 com_cod date using `attendance', gen(_mergeAttendance)
tab _mergeAttendance
gen year = year(date)
drop if year==2022
gen region2 = floor(com_cod/1000)
// fill in vacation days for days when no violence reported so merge==1
foreach var of varlist `d1' `d2' {
    // fill in with region as vacations set by region
    bys region2 year: egen temp = max(`var')
    //replace temp = round(temp)
    replace `var'=temp if `var'==.
    drop temp
}
egen asiste_bas_med = rowtotal(asiste_bas asiste_med) 

gen año = year
//Now generate relative times
replace fin_clases_mes=12 if fin_clases_mes==.
foreach year of numlist 2011(1)2021 {
    gen summerleave  = dmy(fin_clases_dia,fin_clases_mes,`year') if año==`year'
    bys region:  egen summer_leave_`year' = min(summerleave)
    gen summerreturn = dmy(inicio_clases_dia,inicio_clases_mes,`year')  if año==`year'
    bys region:  egen summer_return_`year' = min(summerreturn)
    gen winterleave  = dmy(inicio_vacaciones_dia,inicio_vacaciones_mes,`year')  if año==`year' 
    bys region:  egen winter_leave_`year' = min(winterleave)
    gen winterreturn = dmy(fin_vacaciones_dia,fin_vacaciones_mes,`year')  if año==`year'
    bys region:  egen winter_return_`year' = min(winterreturn)
    drop summerleave summerreturn winterleave winterreturn
}
foreach year of numlist 2011(1)2021 {
    gen time_to_summer_leave_`year' = date-summer_leave_`year'
    gen time_to_summer_return_`year'= date-summer_return_`year'
    gen time_to_winter_leave_`year' = date-winter_leave_`year'
    gen time_to_winter_return_`year'= date-winter_return_`year'
}

// SUMMER LEAVE -- needs 2011 and then remove all pre-2012 because no 2010 dates
gen timeToNearestSummerLeave = .
gen exposureYearSummerLeave = .
foreach year of numlist 2012(1)2020 {
    local lyear = `year'-1
    gen test1 = time_to_summer_leave_`lyear'
    gen test2 = time_to_summer_leave_`year'
    local clastyear abs(test1) < abs(test2) & year==`year'
    local cthisyear abs(test2) < abs(test1) & year==`year'
    replace timeToNearestSummerLeave = time_to_summer_leave_`lyear' if `clastyear'
    replace timeToNearestSummerLeave = time_to_summer_leave_`year'  if `cthisyear'
    sum timeToNearestSummerLeave
    replace exposureYearSummerLeave = `lyear' if `clastyear'
    replace exposureYearSummerLeave = `year' if `cthisyear'
    drop test1 test2
}

// SUMMER RETURN
gen timeToNearestSummerReturn = .
gen exposureYearSummerReturn = .
foreach year of numlist 2012(1)2020 {
    local nyear = `year'+1
    gen test1 = time_to_summer_return_`year'
    gen test2 = time_to_summer_return_`nyear'
    local cthisyear abs(test1) < abs(test2) & year==`year'
    local cnextyear abs(test2) < abs(test1) & year==`year'
    replace timeToNearestSummerReturn = time_to_summer_return_`year'  if `cthisyear'
    replace timeToNearestSummerReturn = time_to_summer_return_`nyear' if `cnextyear'
    sum timeToNearestSummerReturn
    replace exposureYearSummerReturn = `year'  if `cthisyear'
    replace exposureYearSummerReturn = `nyear' if `cnextyear'
    drop test1 test2
}

// WINTER LEAVE -- needs 2011 and then remove all pre-2012 because no 2010 dates
gen timeToNearestWinterLeave = .
gen exposureYearWinterLeave = .
foreach year of numlist 2012(1)2021 {
    local lyear = `year'-1
    gen test1 = time_to_winter_leave_`lyear'
    gen test2 = time_to_winter_leave_`year'
    local clastyear abs(test1) < abs(test2) & year==`year'
    local cthisyear abs(test2) < abs(test1) & year==`year'
    replace timeToNearestWinterLeave = time_to_winter_leave_`lyear' if `clastyear'
    replace timeToNearestWinterLeave = time_to_winter_leave_`year' if `cthisyear'
    sum timeToNearestWinterLeave
    replace exposureYearWinterLeave = `lyear' if `clastyear'
    replace exposureYearWinterLeave = `year' if `cthisyear'
    drop test1 test2
}

// WINTER RETURN
gen timeToNearestWinterReturn = .
gen exposureYearWinterReturn = .
foreach year of numlist 2012(1)2020 {
    local nyear = `year'+1
    gen test1 = time_to_winter_return_`year'
    gen test2 = time_to_winter_return_`nyear'
    local cthisyear abs(test1) < abs(test2) & year==`year'
    local cnextyear abs(test2) < abs(test1) & year==`year'
    replace timeToNearestWinterReturn = time_to_winter_return_`year'  if `cthisyear'
    replace timeToNearestWinterReturn = time_to_winter_return_`nyear' if `cnextyear'
    sum timeToNearestWinterReturn
    replace exposureYearWinterReturn = `year'  if `cthisyear'
    replace exposureYearWinterReturn = `nyear' if `cnextyear'
    drop test1 test2
}

*-------------------------------------------------------------------------------
*--- (5) Graphs relative to vacations
*-------------------------------------------------------------------------------
local times weeks>-15&weeks<15
local graphVar asiste_bas_med
local graphVar asiste

//SUMMER LEAVE
preserve
drop if year<2012&year>2019

collapse (sum) asiste*, by(timeToNearestSummerLeave year)
gen weeks = floor(timeToNearestSummerLeave/7)
collapse (sum) asiste*, by(weeks year)
collapse       asiste*, by(weeks)
foreach var of varlist asiste* {
    replace `var'=`var'/1000
}

#delimit ;
twoway connected asiste_bas_med weeks if weeks>-20&weeks<14, 
lcolor(blue%70) lwidth(thick)  ms(Sh) mcolor(blue%70)
xtitle("Weeks Until Summer Vacations", size(medlarge)) 
xlabel(-20(5)10, labsize(medlarge)) 
ytitle("Attendance Rate", size(medlarge))
xline(-1, lcolor(red)) ylabel(, format(%9.0gc) labsize(medlarge));
#delimit cr
graph export "$OUT/attendanceAroundSummerClose.pdf", replace

merge 1:1 weeks using `summerClose'
#delimit ;
twoway connected `graphVar' weeks if `times', 
    lcolor(blue%70) lwidth(thick)  ms(Sh) mcolor(blue%70)
||    connected caso weeks if `times', 
    lcolor(red%70) lwidth(thick)  ms(Oh) mcolor(red%70) yaxis(2)
xtitle("Weeks Until Summer Vacations", size(medlarge)) 
xlabel(-15(5)15, labsize(medlarge)) 
ytitle("Total Reported Attendance Days (1000s)", size(medlarge))
ytitle("Reported Cases of Violence Against Children", size(medlarge) axis(2))
xline(-1, lcolor(red)) ylabel(, format(%12.0gc) labsize(medlarge))
ylabel(, format(%12.0gc) labsize(medlarge) axis(2))
legend(order(1 "Attendance" 2 "Reports") pos(1) ring(0) rows(1));
#delimit cr
graph export "$OUT/bothAroundSummerClose.pdf", replace
restore

// SUMMER RETURN
preserve
drop if year<2011&year>2018
collapse (sum) asiste*, by(timeToNearestSummerReturn year)
gen weeks = floor(timeToNearestSummerReturn/7)
collapse (sum) asiste*, by(weeks year)
collapse       asiste*, by(weeks)
foreach var of varlist asiste* {
    replace `var'=`var'/1000
}

#delimit ;
twoway connected asiste_bas_med weeks if weeks>-20&weeks<15, 
lcolor(blue%70) lwidth(thick) ms(Sh) mcolor(blue%70)
xtitle("Weeks Until Beginning of School", size(medlarge)) 
xlabel(-20(5)10, labsize(medlarge)) 
ytitle("Attendance Rate", size(medlarge))
xline(-1, lcolor(red)) ylabel(, format(%9.0gc) labsize(medlarge));
#delimit cr
graph export "$OUT/attendanceAroundSummerReturn.pdf", replace

merge 1:1 weeks using `summerReturn'
#delimit ;
twoway connected `graphVar' weeks if `times', 
    lcolor(blue%70) lwidth(thick) ms(Sh) mcolor(blue%70)
||    connected caso  weeks if `times', 
    lcolor(red%70) lwidth(thick) ms(Oh) mcolor(red%70) yaxis(2)
xtitle("Weeks Until Beginning of School", size(medlarge)) 
xlabel(-15(5)15, labsize(medlarge)) 
ytitle("Total Reported Attendance Days (1000s)", size(medlarge))
ytitle("Reported Cases of Violence Against Children", size(medlarge) axis(2))
xline(-1, lcolor(red)) ylabel(, format(%12.0gc) labsize(medlarge))
ylabel(, format(%12.0gc) labsize(medlarge) axis(2))
legend(order(1 "Attendance" 2 "Reports") pos(1) ring(0) rows(1));
#delimit cr
graph export "$OUT/bothAroundSummerReturn.pdf", replace
restore

// WINTER LEAVE
preserve
drop if year<2012&year>2020
collapse (sum) asiste*, by(timeToNearestWinterLeave year)
gen weeks = floor(timeToNearestWinterLeave/7)
collapse (sum) asiste*, by(weeks year)
collapse       asiste*, by(weeks)
foreach var of varlist asiste* {
    replace `var'=`var'/1000
}

#delimit ;
twoway connected asiste weeks if weeks>-20&weeks<14, 
lcolor(blue%70) lwidth(thick)  ms(Sh) mcolor(blue%70)
xtitle("Weeks Until Winter Vacations", size(medlarge)) 
xlabel(-20(5)10, labsize(medlarge)) 
ytitle("Attendance Rate", size(medlarge))
xline(-1, lcolor(red)) ylabel(, format(%9.0gc) labsize(medlarge));
#delimit cr
graph export "$OUT/attendanceAroundWinterClose.pdf", replace

merge 1:1 weeks using `winterClose'
#delimit ;
twoway connected `graphVar' weeks if `times', 
  lcolor(blue%70) lwidth(thick)  ms(Sh) mcolor(blue%70)
||     connected caso   weeks if `times',
  lcolor(red%70) lwidth(thick) ms(Oh) mcolor(red%70) yaxis(2)
xtitle("Weeks Until Winter Vacations", size(medlarge)) 
xlabel(-15(5)15, labsize(medlarge)) 
ytitle("Total Reported Attendance Days (1000s)", size(medlarge))
ytitle("Reported Cases of Violence Against Children", size(medlarge) axis(2))
xline(-1, lcolor(red)) ylabel(, format(%12.0gc) labsize(medlarge))
ylabel(, format(%9.0gc) labsize(medlarge) axis(2))
legend(order(1 "Attendance" 2 "Reports") pos(5) ring(0) rows(2));
#delimit cr
graph export "$OUT/bothAroundWinterClose.pdf", replace
restore


// WINTER RETURN
preserve
drop if year<2011&year>2018
collapse (sum) asiste*, by(timeToNearestWinterReturn year)
gen weeks = floor(timeToNearestWinterReturn/7)
collapse (sum) asiste*, by(weeks year)
collapse       asiste*, by(weeks)
foreach var of varlist asiste* {
    replace `var'=`var'/1000
}


list if abs(weeks)<25
#delimit ;
twoway connected asiste weeks if weeks>-20&weeks<15, 
  lcolor(blue%70) lwidth(thick)  ms(Sh) mcolor(blue%70)
xtitle("Weeks Until Return From Winter Vacations", size(medlarge)) 
xlabel(-20(5)10, labsize(medlarge)) 
ytitle("Attendance Rate", size(medlarge))
xline(-1, lcolor(red)) ylabel(, format(%9.0gc) labsize(medlarge));
#delimit cr
graph export "$OUT/attendanceAroundWinterReturn.pdf", replace

merge 1:1 weeks using `winterReturn'
#delimit ;
twoway connected `graphVar' weeks if `times', 
  lcolor(blue%70) lwidth(thick)  ms(Sh) mcolor(blue%70)
||     connected caso   weeks if `times',
  lcolor(red%70) lwidth(thick) ms(Oh) mcolor(red%70) yaxis(2)
xtitle("Weeks Until Return From Winter Vacations", size(medlarge)) 
xlabel(-15(5)15, labsize(medlarge)) 
ytitle("Total Reported Attendance Days (1000s)", size(medlarge))
ytitle("Reported Cases of Violence Against Children", size(medlarge) axis(2))
xline(-1, lcolor(red)) ylabel(, format(%12.0gc) labsize(medlarge))
ylabel(, format(%12.0gc) labsize(medlarge) axis(2))
legend(order(1 "Attendance" 2 "Reports") pos(5) ring(0) rows(2));
#delimit cr
graph export "$OUT/bothAroundWinterReturn.pdf", replace
restore


*-------------------------------------------------------------------------------
*--- (6) Models
*-------------------------------------------------------------------------------
keep if year>2011&year<2019
gen postVacations1 = timeToNearestWinterReturn>=0 & timeToNearestWinterReturn <=2
replace postVacations1 = 1 if timeToNearestSummerReturn>=0 & timeToNearestSummerReturn <=2
gen postVacations2 = timeToNearestWinterReturn>=3 & timeToNearestWinterReturn <=5
replace postVacations2 = 1 if timeToNearestSummerReturn>=3 & timeToNearestSummerReturn <=5

#delimit ;
local c1 8401  8402  8403  8404  8405  8406  8407  8408  8409  8410  8411  8412  
         8413  8414 8415  8416  8417  8418  8419  8420  8421;
local c2 16101 16102 16202 16203 16302 16103 16104 16204 16303 16105 16106 16205 
         16107 16201 16206 16301 16304 16108 16305 16207 16109;
#delimit cr
tokenize `c1'
foreach c of local c2 {
    replace com_cod=`c' if com_cod==`1'
    macro shift
}
merge m:1 com_cod year using "$DAT/populationYear", gen(_mergePop)
drop if year <2012|year>2018

egen maxattend=rowtotal(asiste no_asiste)
bys com_cod year: egen students = max(maxattend)

gen attendance = asiste/students
replace attendance = 0 if attendance==.
gen dow = dow(date)
replace attendance = . if dow==6|dow==0

replace caso = 0 if caso==.
gen denuncias = caso/populationyoung*100000

gen monday = dow(date)==1
gen weekDate = date if monday==1
bys com_cod (date): replace weekDate = weekDate[_n-1] if weekDate==.
collapse timeToNearest* attendance denuncias populationyoung (sum) caso, by(com_cod weekDate) 
gen denuncias2 = caso/populationyoung*100000
gen year = year(weekDate)


foreach t in SummerLeave SummerReturn WinterLeave WinterReturn {
    gen weeksTo`t'=floor(timeToNearest`t'/7)
    gen lead10`t'=weeksTo`t'<-10
    foreach num of numlist 9(-1)1 {
        gen lead`num'`t'=weeksTo`t'==-`num'
        gen lag`num'`t'=weeksTo`t'== `num'
    }
    drop lead1`t'
    gen lag0`t'=weeksTo`t'==0
    gen lag10`t'=weeksTo`t'>10&weeksTo`t'!=.
}




local BW = 5
// All year in term
gen inTerm = timeToNearestSummerLeave < 0 &timeToNearestWinterReturn > 5
replace inTerm = 1 if timeToNearestWinterLeave < 0 & timeToNearestSummerReturn > 5
// In term but not 5 weeks pre-vacation
gen inTerm1 = timeToNearestSummerLeave < -5 &timeToNearestWinterReturn > 5
replace inTerm1 = 1 if timeToNearestWinterLeave < -5 & timeToNearestSummerReturn > 5
// In vacations
gen vacations = timeToNearestSummerLeave>=0 & timeToNearestSummerReturn<=0
replace vacations = 1 if timeToNearestWinterLeave>=0 & timeToNearestWinterReturn<=0
// Pre-vacations
gen preVacations = timeToNearestWinterLeave>=-5 & timeToNearestWinterLeave <0
replace preVacations = 1 if timeToNearestSummerLeave>=-5 & timeToNearestSummerLeave <0
// Post-vacations
gen postVacations = timeToNearestWinterReturn>=0 & timeToNearestWinterReturn <=5
replace postVacations = 1 if timeToNearestSummerReturn>=0 & timeToNearestSummerReturn <=5

gen cons=1

local xvars1 vacations postVacations
local xvars2 vacations inTerm1 postVacations
local wt     [aw=populationyoung]
local opts   absorb(com_cod) cluster(com_cod)

eststo: reghdfe attendance `xvars1' `wt', `opts'
sum attendance `wt' if e(sample)==1&vacations==0&postVacations==0
estadd scalar mean=r(mean)
eststo: reghdfe denuncias2 `xvars1' `wt' if e(sample)==1, `opts'
sum denuncias2 `wt' if e(sample)==1&vacations==0&postVacations==0
estadd scalar mean=r(mean)

eststo: reghdfe attendance `xvars2' `wt', `opts'
sum attendance `wt' if e(sample)==1&vacations==0&postVacations==0
estadd scalar mean=r(mean)
eststo: reghdfe denuncias2 `xvars2' `wt' if e(sample)==1, `opts'
sum denuncias2 `wt' if e(sample)==1&vacations==0&postVacations==0
estadd scalar mean=r(mean)

gen date = doy(weekDate)
gen weekYear = floor(date/7)
local opts   absorb(com_cod weekYear) cluster(com_cod)
eststo: reghdfe attendance `xvars2' `wt', `opts'
sum attendance `wt' if e(sample)==1&vacations==0&postVacations==0
estadd scalar mean=r(mean)
eststo: reghdfe denuncias2 `xvars2' `wt' if e(sample)==1, `opts'
sum denuncias2 `wt' if e(sample)==1&vacations==0&postVacations==0
estadd scalar mean=r(mean)


lab var vacations     "Vacation Period"
lab var postVacations "Post-Vacation Period"
lab var inTerm1       "Term (Non Pre-Vacations)"
lab var cons          "Term"

#delimit ;
esttab est1 est2 est3 est4 est5 est6 using "$TAB/vacationsCalc.tex",
b(%-9.3f) se(%-9.3f) noobs nonotes nogaps 
stats(N mean, fmt(%9.0gc %05.3f) 
      label("\\ Observations" "Dep.\ Var.\ Mean (Term Time)"))
nonumbers style(tex) starlevel ("*" 0.10 "**" 0.05 "***" 0.01) 
fragment replace noline label mlabels(, none);
#delimit cr
estimates clear


