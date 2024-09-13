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
global LOG "$ROOT/results/log"

cap mkdir "$OUT"

log using "$LOG/analysisVacations.txt", text replace

*-------------------------------------------------------------------------------
*--- (1) Open data and variable generation
*-------------------------------------------------------------------------------
use "$DAT/denuncias_fechas.dta"

local dmonths 31 28 31 30 31 30 31 31 30 31 30 31
tokenize `dmonths'
gen ndays = .
foreach num of numlist 1(1)12 {
    replace ndays = ``num'' if mes==`num'
}
replace ndays = 29 if mes==2 & year==2012
replace ndays = 29 if mes==2 & year==2016
gen daysFromEnd = day-ndays
// comparing day 1 to day 2, there are (24083-5418)/24083% cases brought forward
//thus, re-assign 77.5% of these cases to lags
// in day -1, there is 
tab day


//Date impute
set seed 121316
gen reassign = runiform() if day == 1
replace date = date-1 if reassign<0.13
replace date = date-2 if reassign<0.18&reassign>=0.13
replace date = date-3 if reassign<0.22&reassign>=0.18
replace date = date-4 if reassign<0.25&reassign>=0.22
replace date = date-5 if reassign<0.28&reassign>=0.25
replace date = date-6 if reassign<0.31&reassign>=0.28
replace date = date-7 if reassign<0.34&reassign>=0.31
replace date = date-8 if reassign<0.37&reassign>=0.34
replace date = date-9 if reassign<0.40&reassign>=0.37
replace date = date-10 if reassign<0.42&reassign>=0.40
replace date = date-11 if reassign<0.44&reassign>=0.42
replace date = date-12 if reassign<0.46&reassign>=0.44
replace date = date-13 if reassign<0.48&reassign>=0.46
replace date = date-14 if reassign<0.50&reassign>=0.48
replace date = date-15 if reassign<0.52&reassign>=0.50
replace date = date-16 if reassign<0.54&reassign>=0.52
replace date = date-17 if reassign<0.56&reassign>=0.54
replace date = date-18 if reassign<0.58&reassign>=0.56
replace date = date-19 if reassign<0.60&reassign>=0.58
replace date = date-20 if reassign<0.62&reassign>=0.60
replace date = date-21 if reassign<0.64&reassign>=0.62
replace date = date-22 if reassign<0.66&reassign>=0.64
replace date = date-23 if reassign<0.68&reassign>=0.66
replace date = date-24 if reassign<0.70&reassign>=0.68
replace date = date-25 if reassign<0.72&reassign>=0.70
replace date = date-26 if reassign<0.74&reassign>=0.72
replace date = date-27 if reassign<0.75&reassign>=0.74
replace date = date-28 if reassign<0.76&reassign>=0.75
replace date = date-29 if reassign<0.77&reassign>=0.76

drop dia day mes año
gen day = day(date)
gen mes = month(date)
gen año = year(date)
drop if año<2010
tab day

/*
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
foreach year of numlist 2011(1)2021 {
    gen time_to_summer_leave_`year' = date-summer_leave_`year'+2
    gen time_to_summer_return_`year'= date-summer_return_`year'
    gen time_to_winter_leave_`year' = date-winter_leave_`year'+2
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
drop if edad<4
collapse (sum)  caso, by(timeToNearestSummerLeave)
gen weeks = floor(timeToNearestSummerLeave/7)
collapse (sum) caso, by(weeks)
#delimit ;
twoway connected caso weeks if weeks>-20&weeks<14, lcolor(blue%70) lwidth(thick)  ms(Sh) mcolor(blue%70)
xtitle("Weeks Until Summer Vacations", size(medlarge)) 
xlabel(-20(5)10, labsize(medlarge)) 
ytitle("Reported Violence Against Children", size(medlarge))
xline(-1, lcolor(red)) ylabel(, format(%9.0gc) labsize(medlarge));
#delimit cr
graph export "$OUT/casesAroundSummerClose.pdf", replace
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
drop if edad<4
collapse (sum)  caso, by(timeToNearestSummerReturn)
gen weeks = floor(timeToNearestSummerReturn/7)
collapse (sum) caso, by(weeks)
#delimit ;
twoway connected caso weeks if weeks>-20&weeks<15, lcolor(blue%70) lwidth(thick) ms(Sh) mcolor(blue%70)
xtitle("Weeks Until Beginning of School", size(medlarge)) 
xlabel(-20(5)10, labsize(medlarge)) 
ytitle("Reported Violence Against Children", size(medlarge))
xline(-1, lcolor(red)) ylabel(, format(%9.0gc) labsize(medlarge));
#delimit cr
graph export "$OUT/casesAroundSummerReturn.pdf", replace
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
drop if edad<4
collapse (sum)  caso, by(timeToNearestWinterLeave)
gen weeks = floor(timeToNearestWinterLeave/7)
collapse (sum) caso, by(weeks)
#delimit ;
twoway connected caso weeks if weeks>-20&weeks<14, lcolor(blue%70) lwidth(thick)  ms(Sh) mcolor(blue%70)
xtitle("Weeks Until Winter Vacations", size(medlarge)) 
xlabel(-20(5)10, labsize(medlarge)) 
ytitle("Reported Violence Against Children", size(medlarge))
xline(-1, lcolor(red)) ylabel(, format(%9.0gc) labsize(medlarge));
#delimit cr
graph export "$OUT/casesAroundWinterClose.pdf", replace
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
collapse (sum)  caso, by(timeToNearestWinterReturn)
gen weeks = floor(timeToNearestWinterReturn/7)
collapse (sum) caso, by(weeks)
#delimit ;
twoway connected caso weeks if weeks>-20&weeks<15, lcolor(blue%70) lwidth(thick)  ms(Sh) mcolor(blue%70)
xtitle("Weeks Until Return From Winter Vacations", size(medlarge)) 
xlabel(-20(5)10, labsize(medlarge)) 
ytitle("Reported Violence Against Children", size(medlarge))
xline(-1, lcolor(red)) ylabel(, format(%9.0gc) labsize(medlarge));
#delimit cr
graph export "$OUT/casesAroundWinterReturn.pdf", replace
restore

*-------------------------------------------------------------------------------
*--- (4) Models 
*-------------------------------------------------------------------------------
*/
#delimit ;
local c1 8401  8402  8403  8404  8405  8406  8407  8408  8409  8410  8411  8412  8413  8414
         8415  8416  8417  8418  8419  8420  8421;
local c2 16101 16102 16202 16203 16302 16103 16104 16204 16303 16105 16106 16205 16107 16201
         16206 16301 16304 16108 16305 16207 16109;
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


collapse (sum) caso, by(comuna date) 
gen  year = year(date)
drop if year==2010 | year>=2020
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


//Now collapse to week

//Now generate relative times
exit