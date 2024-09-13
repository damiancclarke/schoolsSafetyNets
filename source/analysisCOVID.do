/* AnalysisCOVID.do                  DanielPailanir          yyyy-mm-dd:2021-12-17
*----|----1----|----2----|----3----|----4----|----5----|----6----|----7----|----8
This was previously:
 childrenSchools/source/AnalysisSchoolClosure


*/
*sysdir set PLUS "C:\Users\danie\ado\plus"
clear all
set more off

*-------------------------------------------------------------------------------
* (0) Globals and some details
*-------------------------------------------------------------------------------
global ROOT "C:/Users/danie/OneDrive/Escritorio/Research/SchoolClosureViolence/"
local name `=c(username)'
global ROOT "/home/`name'/investigacion/2022/childrenSchools/replication/"

global DAT "$ROOT/data"
global GRA "$ROOT/results/graphs"
global OUT "$ROOT/results/reg"
global LOG "$ROOT/results/log"

set scheme plotplainblind, permanently
graph set window fontface "Times New Roman"
cap log close
log using "$LOG/analysisCOVID.txt", text replace

local data SchoolClosure_Final_RR_05082024.dta

use $DAT/`data'

preserve
collapse privado vulnerable prioritario [aw=populationyoung], by(week)

colorpalette viridis, n(3) nograph reverse
local c1 `"`r(p1)'"'
local c2 `"`r(p2)'"'
local c3 `"`r(p3)'"'
local l1 lwidth(medthick) lcolor("`c1'") mc("`c1'")
local l2 lwidth(medthick) lcolor("`c2'") mc("`c2'") ms(s)
local l3 lwidth(medthick) lcolor("`c3'") mc("`c3'") ms(dh)

#delimit ;
twoway connected privado week  if week>=90, `l1' 
 || connected vulnerable week  if week>=90, `l2' lpattern(dash)
 || connected prioritario week if week>=90, `l3' lpattern(longdash)
ytitle("Proportion of Schools", size(medlarge)) scheme(white_cividis)
ylabel(#10, format(%9.1f) labsize(medium))
xtitle("Week", size(medlarge))
xlabel(90 "15 Sep, 2020" 100 "24 Nov, 2020" 110 "2 Feb, 2021"
       120 "13 Apr, 2021" 130 "22 Jun, 2021" 140 "31 Aug, 2021"
       150 "9 Nov, 2021")
legend(order(1 "Private Schools" 2 "Municipal Schools" 3 "Priority Students")
       pos(1) rows(1) ring(0));
graph export "$GRA/schooolsReturn.eps", replace;
#delimit cr
restore


*-------------------------------------------------------------------------------
*-- (XX) Goodman Bacon [new ado -- need to fix]
*-------------------------------------------------------------------------------
/*
use $DAT/`data', clear
xtset comuna week
local cond "if year>=2019 [aw=populationyoung]"
local opt2 "cluster(comuna) abs(comuna)"
areg rate SchoolClose2 SchoolOpen_i i.w quarantine caseRate pcr positivity `cond', `opt2'
areg rate SchoolClose2 SchoolOpen_i i.w quarantine caseRate pcr positivity interno2 externo2 `cond', `opt2'
keep if week<154 & year>=2019

foreach var of varlist rate rateSA rateV {
    qui xtreg `var' SchoolClose2 SchoolOpen_i i.w, fe cluster(comuna)
    bacondecomp `var' SchoolOpen_i, ddetail
    twowayfeweights `var' comuna w SchoolOpen_i, type(feTR)
    graph export $GRA/Bacon_`var'.eps, replace
}
*/


*-------------------------------------------------------------------------------
*--- (1) Descriptives
*-------------------------------------------------------------------------------
/*
insheet using "20200108_Asistencia_noviembre_2019_20191215_PUBL.csv", delim(";") clear
replace asis_promedio = subinstr(asis_promedio, ",", ".", .)    
destring asis_promedio, replace
hgen yearbirth = substr(fec_nac_alu, 1,4)
destring yearbirth, replace
gen age = 2020-yearbirth

*drop if asis_promedio == 0
gen attend05  = asis_promedio if age>=0&age<=5
gen attend613 = asis_promedio if age>=6&age<=13
gen attend1417 = asis_promedio if age>=14&age<=17
gen attendAll  = asis_promedio 

collapse attend*, by(cod_com_alu nom_com_alu)
rename cod_com_alu comuna
save attendance, replace
*/


use $DAT/`data', clear
merge m:1 comuna using "$DAT/attendance", gen(_mergeAttend)
drop if _mergeAttend==2

lab var attendAll      "Baseline Attendance"
lab var SchoolClose2   "School Closure"
lab var SchoolOpen_i   "School Reopening (Binary)"
lab var prop_schools_i "School Reopening (Continuous)"
lab var Attendance1    "Attendance (1 day)"
lab var Attendance2    "Attendance (1-5 days)"
lab var Attendance3    "Attendance (6-10 days)"
lab var Attendance4    "Attendance (10+ days)"

#delimit ;
local PA rate rateVIF3 rateVIF2 rateVIF1 rateSA rateV;
estpost sum `PA' if year>=2019;
estout using "$OUT/summaryPA.tex", replace label  mlabels(,none) collabels(,none)
cells("count() mean(fmt(2)) sd(fmt(2)) min(fmt(2)) max(fmt(2))") style(tex);

local PB SchoolClose2 SchoolOpen_i prop_schools_i Attendance1 Attendance2
Attendance3 Attendance4 attendAll;
estpost sum `PB' if year>=2019;
estout using "$OUT/summaryPB.tex", replace label mlabels(,none) collabels(,none)
cells("count() mean(fmt(2)) sd(fmt(2)) min(fmt(2)) max(fmt(2))") style(tex);

local PC quarantine caseRate pcr positivity populationyoung;
estpost sum `PC' if year>=2019;
estout using "$OUT/summaryPC.tex", replace label mlabels(,none) collabels(,none)
cells("count() mean(fmt(2)) sd(fmt(2)) min(fmt(2)) max(fmt(2))") style(tex);
#delimit cr

local ccd if monday==22614&Attendance1<1
local lc lcolor(gs12) fcolor(pink%30)
local sc mc(black) ms(Oh)
#delimit ;
twoway lfitci Attendance1 attendAll `ccd', `lc' ||
      scatter Attendance1 attendAll `ccd' [aw=populationyoung], `sc'
legend(order(1 "Scatter" 2 "Linear Fit" 3 "95% CI") pos(1) rows(1))
ylabel(, format(%04.2f) labsize(medium)) 
xlabel(, format(%04.2f) labsize(medium))
ytitle("Post-pandemic attendance", size(medlarge))
xtitle("Pre-pandemic attendance", size(medlarge))
text(0.7 0.88 "{&rho}=0.363", placement(e))
text(0.635 0.92 "(p<0.001)", placement(e) size(small));
graph export "$GRA/attendanceComp.pdf", replace;
#delimit cr



gen pop_underQ=population //create population under quarantine
replace pop_underQ=0 if quarantine==0
local svar caso* VIF* quarantine pop_underQ population
collapse (sum) `svar' (mean) monday prop_schools_i, by(week)
gen prop_pop=pop_underQ/population //create proportion of population under Q
replace prop_schools_i=1 if week<=63
format prop_pop %9.1f
gen g2=VIF_2+VIF_3
gen g3=g2+VIF_1

#delimit ;
local c "if monday>21556&week<157";

twoway area VIF_3 monday `c', color(%50) 
       || rarea VIF_3 g2 monday `c', color(%50) 
       || rarea g2 g3 monday `c', color(%60)
       || line caso monday `c', lc(black) lp(solid)
ylabel(0(50)250) xlabel(#13, angle(45)) xline(21989 22144, lc(red))
ytitle("Formal reporting violence") xtitle("")
legend(order(3 "Psychological" 2 "Minor injuries" 1 "Serious injuries") 
pos(12) col(4));
graph export "$GRA/VIFreport_byClass.pdf", replace;

twoway line casoSA monday if monday>21556 & week<154, xlabel(#13, angle(45)) 
xtitle("") xline(21989, lc(red)) xline(22144, lc(red)) ylabel(0(25)150)
ytitle("Formal reporting Sexual Abuse");
graph export "$GRA/SAbusereport.eps", replace;

twoway line casoV monday if monday>21556 & week<154, xlabel(#13, angle(45)) 
xtitle("") xline(21989 22144, lc(red)) ytitle("Formal reporting Rape")
ylabel(0(5)30);
graph export "$GRA/Rapereport.eps", replace;


twoway (line quarantine monday if monday>21556, yaxis(1) 
              ytitle("Municipalities under quarantine", axis(1) size(medsmall)))
	   (line prop_pop monday if monday>21556, yaxis(2) lc(blue)
              ytitle("Proportion under quarantine", axis(2) size(medsmall) 
              orientation(rvertical))),
xlabel(#13, angle(45)) xline(21989 22144, lc(red))
legend(order(1 "Municipalities" 2 "Population") col(2) pos(11) ring(0) colg(1pt) 
bm(zero) keyg(.8pt) size(small) region(lcolor(gs8))) xtitle("");
graph export "$GRA/quarantine.eps", replace;

twoway (line prop_schools_i monday if monday<21990&week>0)
       (line prop_schools_i monday if monday>=21990&monday<22265&week>0, 
        lp(solid) lc(black))
	   (line prop_schools_i monday if monday>=22265&monday<=22343&week>0, 
         lp(shortdash) lc(black))
       (line prop_schools_i monday if monday>=22344&week>0, lp(solid) lc(black)), 
ylabel(, format(%9.1f)) xlabel(#13, angle(45) format(%td)) xline(21989, lc(red)) 
xline(22144, lc(red)) xtitle("") ytitle("Proportion of schools") legend(off);
graph export "$GRA/prop_school.eps", replace;
#delimit cr


preserve
*COVID cases graph
// Originally from now removed github: 
// local GIT "https://raw.githubusercontent.com/MinCiencia/Datos-COVID19/master/output/"
import delimited "$DAT/TotalesNacionales.csv", clear
keep fecha-v660

forvalues i=2/660 {
	local varlabel : var label v`i'
	scalar def f`i'="`varlabel'"
}

keep if fecha=="Casos nuevos totales" | fecha=="Fallecidos"
reshape long v@, i(fecha) j(date)
encode fecha, gen(tipo)
drop fecha
reshape wide v, i(date) j(tipo)
ren v1 NewTotalCases
ren v2 TotalDeaths

gen time=f2 if date==2
forvalues i=3/660 {
	replace time=f`i' if date==`i'
}

gen year=substr(t, 1, 4)
gen month=substr(t, 6, 2)
gen day=substr(t, 9, 2)

destring year, replace
destring month, replace
destring day, replace
gen t=mdy(month, day, year)
drop time month day year
format t %d
tsset t
gen Deaths=TotalDeaths-L.TotalDeaths

set obs 660
replace t=21550 in 660
replace NewTotalCases=0 in 660
replace Deaths=0 in 660
sort t

#delimit ;
twoway (line NewTotalCases t, yaxis(1) ytitle("Total Cases", 
        axis(1) size(medsmall)))
       (line Deaths t, yaxis(2) lc(blue) ytitle("Total Deaths", 
        axis(2) size(medsmall) orientation(rvertical))),
xlabel(#13, angle(45)) xline(21989, lc(red)) xline(22144, lc(red))
legend(order(1 "Total Cases" 2 "Total Deaths") pos(1) ring(0) col(2) 
colg(1pt) bm(zero) keyg(.8pt) size(small) region(lcolor(gs8))) xtitle("");
graph export "$GRA/COVID.eps", replace;
#delimit cr
restore


*-------------------------------------------------------------------------------
*--- (2) Fixed effect tables
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


// Extend back to Jan 2019 (all 0) for standardised plot
sum monday
local newobs = (r(min)-dmy(1,1,2019))/7
count
local oldobs = r(N)
local totobs = `oldobs'+`newobs'
set obs `totobs'
sort monday
replace monday = (_n-`oldobs')*7+21550 if monday==.
replace vaccines = 0 if vaccines==.
format monday  %td
format vaccines %12.0gc
//Figure 2(d)
collapse (sum) vaccines, by(monday)
keep if monday<=22646
replace vaccines = vaccines/1000
#delimit ;
twoway line vaccines monday,
ytitle("Number of vaccines administered (1000s)", size(medlarge))
lwidth(thick) ylabel(, labsize(medium))
xlabel(#13, angle(45) labsize(medium))
xline(21989, lc(red)) xline(22144, lc(red))
xtitle("");
graph export "$GRA/vaccines.eps", replace;
#delimit cr

use $DAT/`data', clear
merge m:1 comuna using "$DAT/attendance", gen(_mergeAttend)
drop if _mergeAttend==2
merge 1:1 comuna monday using `vaccines', keep(1 3)
drop _merge
replace vaccines=0 if vaccines == .

gen cod_com_rbd = comuna
preserve
use "$DAT/doc_asist_2016_22.dta", clear

#delimit ;
local c1 8401  8402  8403  8404  8405  8406  8407  8408  8409  8410  8411  8412  8413  8414
         8415  8416  8417  8418  8419  8420  8421;
local c2 16101 16102 16202 16203 16302 16103 16104 16204 16303 16105 16106 16205 16107 16201
         16206 16301 16304 16108 16305 16207 16109;
#delimit cr
tokenize `c1'
foreach c of local c2 {
    replace cod_com_rbd=`c' if cod_com_rbd==`1'
    macro shift
}
tempfile asistentes
save `asistentes'
restore
merge m:1 cod_com_rbd year using `asistentes', gen(_mergeAsistente)
// drops only 2022
keep if _mergeAs==1|_mergeAs==3

gen rateDocentes   = docentes  /(population1+population2+population3+population4+population5)*1000
gen rateAsistentes = asistentes/(population1+population2+population3+population4+population5)*1000
sum rateDocentes rateAsistentes


gen rAB = rateAsistentes if year==2019
bys comuna: egen rateAsistentesBaseline=mean(rAB)

gen eAB = experiencia_asistente_ee if year==2019
bys comuna: egen experienceAsistentesBaseline=mean(eAB)


local ccd if monday==22614&rateAsistentes<=100
local lc lcolor(gs12) fcolor(pink%30)
local sc mc(black) ms(Oh)

#delimit ;
twoway lfitci rateAsistentes rateAsistentesBaseline `ccd', `lc' ||
      scatter rateAsistentes rateAsistentesBaseline `ccd' [aw=populationyoung], `sc'
legend(order(1 "Scatter" 2 "Linear Fit" 3 "95% CI") pos(1) rows(1))
ylabel(, format(%4.0f) labsize(medium)) xlabel(, format(%4.0f) labsize(medium))
ytitle("Post-pandemic School Assistant Coverage", size(medlarge))
xtitle("Pre-pandemic School Assistant Coverage", size(medlarge))
text(50 65 "{&rho}=0.918", placement(e))
text(46 66 "(p<0.001)", placement(e) size(small));
graph export "$GRA/asistentesComp.pdf", replace;

local ccd if monday==22614&experienceAsistentesBaseline<=9;
twoway lfitci experiencia_asistente_ee experienceA `ccd', `lc' ||
      scatter experiencia_asistente_ee experienceA `ccd' [aw=populationyoung], `sc'
legend(order(1 "Scatter" 2 "Linear Fit" 3 "95% CI") pos(1) rows(1))
ylabel(, format(%4.0f) labsize(medium)) xlabel(, format(%4.0f) labsize(medium))
ytitle("Post-pandemic School Assistant Experience", size(medlarge))
xtitle("Pre-pandemic School Assistant Experience", size(medlarge))
text(5.6 7.4 "{&rho}=0.936", placement(e))
text(5   7.5 "(p<0.001)", placement(e) size(small));
graph export "$GRA/asistentesExpComp.pdf", replace;
#delimit cr


gen empB = p_comunal_m if year==2019
bys comuna: egen empleoBaseline=mean(empB)
local ccd if monday==22614&empleoBaseline<0.8
#delimit ;
twoway lfitci p_comunal_m empleoBaseline `ccd', `lc' ||
      scatter p_comunal_m empleoBaseline `ccd' [aw=populationyoung], `sc'
legend(order(1 "Scatter" 2 "Linear Fit" 3 "95% CI") pos(1) rows(1))
ylabel(, format(%04.2f) labsize(medium)) xlabel(, format(%04.2f) labsize(medium))
ytitle("Post-pandemic Formal Employment Rate", size(medlarge))
xtitle("Pre-pandemic Formal Employment Rate", size(medlarge))
text(0.38 0.4 "{&rho}=0.956", placement(e))
text(0.35 0.404 "(p<0.001)", placement(e) size(small));
graph export "$GRA/employmentComp.pdf", replace;
#delimit cr



exit
xtset comuna week
local cond "if year>=2019 [aw=populationyoung]"
local opt2 "cluster(comuna) abs(comuna)"

egen zmob = std(movilidad2)

local c1 i.w
local c2 `c1' quarantine caseRate pcr positivity p_comunal_m p_comunal_h vaccines 
local c3 `c2' privado vulnerable prioritario
//replace controls as baseline value for pre-opening period

foreach var of varlist privado vulnerable prioritario {
    bys comuna (week): replace `var'=`var'[_n-1] if `var'==.
}

*-------------------------------------------------------------------------------
*--- (a) Main Table
*-------------------------------------------------------------------------------
local varr rate rateSA rateV
foreach v of local varr {
    if "`v'"=="rate"   local en V
    if "`v'"=="rateSA" local en SA
    if "`v'"=="rateV"  local en R

    *for outcome mean
    qui sum `v' if week<=61
    local mean=`r(mean)'

    foreach cont of numlist 1(1)3 {
        eststo `en'_`cont'_d: areg `v' SchoolClose2 SchoolOpen_i   `c`cont'' `cond', `opt2'
        test _b[SchoolClose2]=_b[SchoolOpen_i]
        estadd scalar pval = r(p)
        estadd scalar mean = `mean'

        eststo `en'_`cont'_c: areg `v' SchoolClose2 prop_schools_i `c`cont'' `cond', `opt2'
        test _b[SchoolClose2]=_b[prop_schools_i]
        estadd scalar pval = r(p)
        estadd scalar mean = `mean'
    }
}
*PANEL A
#delimit ;
esttab V_1_d V_2_d V_3_d SA_1_d SA_2_d SA_3_d R_1_d R_2_d R_3_d, 
       keep(SchoolClose2 SchoolOpen_i) b(%-9.4f) se(%-9.4f) noobs;
esttab V_1_d V_2_d V_3_d SA_1_d SA_2_d SA_3_d R_1_d R_2_d R_3_d 
       using "$OUT/panelA.tex", b(%-9.3f) se(%-9.3f) noobs
       keep(SchoolClose2 SchoolOpen_i) nonotes nogaps mlabels(, none)
       stats(pval N mean, fmt(%04.3f %9.0gc %05.3f)
           label("\\ Test of $\beta=\gamma$ (p-value)" "Observations" "Baseline Mean"))
       nonumbers style(tex) starlevel ("*" 0.10 "**" 0.05 "***" 0.01) 
       fragment replace noline label;
#delimit cr

*PANEL B
#delimit ;
esttab V_1_c V_2_c V_3_c SA_1_c SA_2_c SA_3_c R_1_c R_2_c R_3_c, 
       keep(SchoolClose2 prop_schools_i) b(%-9.4f) se(%-9.4f) noobs;
esttab V_1_c V_2_c V_3_c SA_1_c SA_2_c SA_3_c R_1_c R_2_c R_3_c 
       using "$OUT/panelB.tex", b(%-9.3f) se(%-9.3f) noobs
       stats(pval N mean, fmt(%04.3f %9.0gc %05.3f)
           label("\\ Test of $\beta=\gamma$ (p-value)" "Observations" "Baseline Mean"))
       keep(SchoolClose2 prop_schools_i) nonotes nogaps mlabels(, none) 
       nonumbers style(tex) starlevel ("*" 0.10 "**" 0.05 "***" 0.01) 
       fragment replace noline label;
#delimit cr
estimates clear



*-------------------------------------------------------------------------------
*--- (b) Unadjusted
*-------------------------------------------------------------------------------
local cond "if year>=2019 [aw=populationyoung]"
local opt2 "cluster(comuna) abs(comuna)"


local varr rateSA_old rateV_old
foreach v of local varr {
    if "`v'"=="rateSA_old" local en SA
    if "`v'"=="rateV_old"  local en R

    *for outcome mean
    qui sum `v' if week<=61
    local mean=`r(mean)'

    foreach cont of numlist 1(1)3 {
        eststo `en'_`cont'_d: areg `v' SchoolClose2 SchoolOpen_i   `c`cont'' `cond', `opt2'
        test _b[SchoolClose2]=_b[SchoolOpen_i]
        estadd scalar pval = r(p)
        estadd scalar mean = `mean'

        eststo `en'_`cont'_c: areg `v' SchoolClose2 prop_schools_i `c`cont'' `cond', `opt2'
        test _b[SchoolClose2]=_b[prop_schools_i]
        estadd scalar pval = r(p)
        estadd scalar mean = `mean'
    }
}
*PANEL A
#delimit ;
esttab SA_1_d SA_2_d SA_3_d R_1_d R_2_d R_3_d
       using "$OUT/panelA_unadj.tex", b(%-9.3f) se(%-9.3f) noobs
       keep(SchoolClose2 SchoolOpen_i) nonotes nogaps mlabels(, none)
       stats(pval N mean, fmt(%04.3f %9.0gc %05.3f)
           label("\\ Test of $\beta=\gamma$ (p-value)" "Observations" "Baseline Mean"))
       nonumbers style(tex) starlevel ("*" 0.10 "**" 0.05 "***" 0.01) 
       fragment replace noline label;
#delimit cr

*PANEL B
#delimit ;
esttab SA_1_c SA_2_c SA_3_c R_1_c R_2_c R_3_c
       using "$OUT/panelB_unadj.tex", b(%-9.3f) se(%-9.3f) noobs
       stats(pval N mean, fmt(%04.3f %9.0gc %05.3f)
           label("\\ Test of $\beta=\gamma$ (p-value)" "Observations" "Baseline Mean"))
       keep(SchoolClose2 prop_schools_i) nonotes nogaps mlabels(, none) 
       nonumbers style(tex) starlevel ("*" 0.10 "**" 0.05 "***" 0.01) 
       fragment replace noline label;
#delimit cr
estimates clear


*-------------------------------------------------------------------------------
*--- (c) No vacation
*-------------------------------------------------------------------------------
local cond "if year>=2019 [aw=populationyoung]"
local opt2 "cluster(comuna) abs(comuna)"

preserve
gen m=month(monday)
drop if m==1 |m==2

local varr rate rateSA rateV
foreach v of local varr {
    if "`v'"=="rate"   local en V
    if "`v'"=="rateSA" local en SA
    if "`v'"=="rateV"  local en R

    *for outcome mean
    qui sum `v' if week<=61
    local mean=`r(mean)'

    foreach cont of numlist 1(1)3 {
        eststo `en'_`cont'_d: areg `v' SchoolClose2 SchoolOpen_i   `c`cont'' `cond', `opt2'
        test _b[SchoolClose2]=_b[SchoolOpen_i]
        estadd scalar pval = r(p)
        estadd scalar mean = `mean'

        eststo `en'_`cont'_c: areg `v' SchoolClose2 prop_schools_i `c`cont'' `cond', `opt2'
        test _b[SchoolClose2]=_b[prop_schools_i]
        estadd scalar pval = r(p)
        estadd scalar mean = `mean'
    }
}
*PANEL A
#delimit ;
esttab V_1_d V_2_d V_3_d SA_1_d SA_2_d SA_3_d R_1_d R_2_d R_3_d 
       using "$OUT/panelA_novacation.tex", b(%-9.3f) se(%-9.3f) noobs
       keep(SchoolClose2 SchoolOpen_i) nonotes nogaps mlabels(, none)
       stats(pval N mean, fmt(%04.3f %9.0gc %05.3f)
           label("\\ Test of $\beta=\gamma$ (p-value)" "Observations" "Baseline Mean"))
       nonumbers style(tex) starlevel ("*" 0.10 "**" 0.05 "***" 0.01) 
       fragment replace noline label;
#delimit cr

*PANEL B
#delimit ;
esttab V_1_c V_2_c V_3_c SA_1_c SA_2_c SA_3_c R_1_c R_2_c R_3_c 
       using "$OUT/panelB_novacation.tex", b(%-9.3f) se(%-9.3f) noobs
       stats(pval N mean, fmt(%04.3f %9.0gc %05.3f)
           label("\\ Test of $\beta=\gamma$ (p-value)" "Observations" "Baseline Mean"))
       keep(SchoolClose2 prop_schools_i) nonotes nogaps mlabels(, none) 
       nonumbers style(tex) starlevel ("*" 0.10 "**" 0.05 "***" 0.01) 
       fragment replace noline label;
#delimit cr
estimates clear
restore


*-------------------------------------------------------------------------------
*--- (d) By cause
*-------------------------------------------------------------------------------
local varr rateVIF1 rateVIF2 rateVIF3
foreach v of local varr {
    if "`v'"=="rateVIF1" local en V1
    if "`v'"=="rateVIF2" local en V2
    if "`v'"=="rateVIF3" local en V3

    *for outcome mean
    qui sum `v' if week<=61
    local mean=`r(mean)'

    foreach cont of numlist 1(1)3 {
        eststo `en'_`cont'_d: areg `v' SchoolClose2 SchoolOpen_i   `c`cont'' `cond', `opt2'
        test _b[SchoolClose2]=_b[SchoolOpen_i]
        estadd scalar pval = r(p)
        estadd scalar mean = `mean'

        eststo `en'_`cont'_c: areg `v' SchoolClose2 prop_schools_i `c`cont'' `cond', `opt2'
        test _b[SchoolClose2]=_b[prop_schools_i]
        estadd scalar pval = r(p)
        estadd scalar mean = `mean'
    }
}
*PANEL A
#delimit ;
esttab V3_1_d V3_2_d V3_3_d V2_1_d V2_2_d V2_3_d V1_1_d V1_2_d V1_3_d 
       using "$OUT/panelA_bycause.tex", b(%-9.3f) se(%-9.3f) noobs
       keep(SchoolClose2 SchoolOpen_i) nonotes nogaps mlabels(, none)
       stats(pval N mean, fmt(%04.3f %9.0gc %05.3f)
           label("\\ Test of $\beta=\gamma$ (p-value)" "Observations" "Baseline Mean"))
       nonumbers style(tex) starlevel ("*" 0.10 "**" 0.05 "***" 0.01) 
       fragment replace noline label;
#delimit cr

*PANEL B
#delimit ;
esttab V3_1_c V3_2_c V3_3_c V2_1_c V2_2_c V2_3_c V1_1_c V1_2_c V1_3_c 
       using "$OUT/panelB_bycause.tex", b(%-9.3f) se(%-9.3f) noobs
       stats(pval N mean, fmt(%04.3f %9.0gc %05.3f)
           label("\\ Test of $\beta=\gamma$ (p-value)" "Observations" "Baseline Mean"))
       keep(SchoolClose2 prop_schools_i) nonotes nogaps mlabels(, none) 
       nonumbers style(tex) starlevel ("*" 0.10 "**" 0.05 "***" 0.01) 
       fragment replace noline label;
#delimit cr
estimates clear

exit

*-------------------------------------------------------------------------------
*--- (3) Attendance analysis
*-------------------------------------------------------------------------------
local varr rate rateSA rateV
local cond "if year>=2019 [aw=populationyoung]"
local opt2 "cluster(comuna) abs(comuna)"
local indvar1 SchoolClose2 SchoolOpen_i
local indvar2 SchoolClose2 prop_schools_i

rename OpenAttendance1 W1
rename propOpenAttendance1 W2
gen OpenAttendance1 = attendAll*SchoolOpen_i
gen propOpenAttendance1 = attendAll*prop_schools_i

lab var SchoolClose2        "School Closure"
lab var SchoolOpen_i        "School Reopening"
lab var prop_schools_i      "School Reopening"
lab var OpenAttendance1     "School Reopening $\times$ Attendance"
lab var propOpenAttendance1 "School Reopening $\times$ Attendance"
*/
/*
foreach v of local varr {
    if "`v'"=="rate" local en V
    if "`v'"=="rateSA" local en SA
    if "`v'"=="rateV" local en R
    *for outcome mean
    qui sum `v' if week<=61 & year>=2019 [aw=populationyoung]
    local `en'_mean=`r(mean)'
    local `en'_mean : display %9.3f ``en'_mean'
    qui sum attendAll if SchoolOpen_i>0, d
    local p25=r(p25)
    local p50=r(p50)
    local p75=r(p75)
    local p90=r(p90)
    xtset comuna week
    local cond2 "if year>=2019 & Attendance1!=. [aw=populationyoung]"
    local cond2 "if year>=2019 & attendAll!=. [aw=populationyoung]"
    local opt2 "cluster(comuna) abs(comuna)"

    *(1) no controls
    eststo `en'_1_att1_d: reg `v' `indvar1' `cond2', cluster(comuna)
    local `en'_1_att1_d_N=e(N)
    eststo `en'_1_att1_c: reg `v' `indvar2' `cond2', cluster(comuna)
    local `en'_1_att1_c_N=e(N)
    *add attendance interaction: Binary Open
    eststo `en'_1_att2_d: reg `v' `indvar1' OpenAttendance1 `cond', cluster(comuna)
    local Attp25_1d = _b[SchoolOpen_i]+_b[OpenAttendance1]*`p25'
    local Attp50_1d = _b[SchoolOpen_i]+_b[OpenAttendance1]*`p50'
    local Attp75_1d = _b[SchoolOpen_i]+_b[OpenAttendance1]*`p75'
    local Attp90_1d = _b[SchoolOpen_i]+_b[OpenAttendance1]*`p90'
    local `en'_1_att2_d_N=e(N)
    *add attendance interaction: Continuous Open
    eststo `en'_1_att2_c: reg `v' `indvar2' propOpenAttendance1 `cond', cluster(comuna)
    local Attp25_1c = _b[prop_schools_i]+_b[propOpenAttendance1]*`p25'
    local Attp50_1c = _b[prop_schools_i]+_b[propOpenAttendance1]*`p50'
    local Attp75_1c = _b[prop_schools_i]+_b[propOpenAttendance1]*`p75'
    local Attp90_1c = _b[prop_schools_i]+_b[propOpenAttendance1]*`p90'
    local `en'_1_att2_c_N=e(N)

    *(2) week and comuna fe
    eststo `en'_2_att1_d: areg `v' `indvar1' i.w `cond2', `opt2'
    local `en'_2_att1_d_N=e(N)
    eststo `en'_2_att1_c: areg `v' `indvar2' i.w `cond2', `opt2'
    local `en'_2_att1_c_N=e(N)
    *add attendance interaction: Binary Open
    eststo `en'_2_att2_d: reg `v' `indvar1' OpenAttendance1 i.w  `cond', `opt2'
    local Attp25_2d = _b[SchoolOpen_i]+_b[OpenAttendance1]*`p25'
    local Attp50_2d = _b[SchoolOpen_i]+_b[OpenAttendance1]*`p50'
    local Attp75_2d = _b[SchoolOpen_i]+_b[OpenAttendance1]*`p75'
    local Attp90_2d = _b[SchoolOpen_i]+_b[OpenAttendance1]*`p90'
    local `en'_2_att2_d_N=e(N)
    *add attendance interaction: Continuous Open
    eststo `en'_2_att2_c: reg `v' `indvar2' propOpenAttendance1 i.w  `cond', `opt2'
    local Attp25_2c = _b[prop_schools_i]+_b[propOpenAttendance1]*`p25'
    local Attp50_2c = _b[prop_schools_i]+_b[propOpenAttendance1]*`p50'
    local Attp75_2c = _b[prop_schools_i]+_b[propOpenAttendance1]*`p75'
    local Attp90_2c = _b[prop_schools_i]+_b[propOpenAttendance1]*`p90'
    local `en'_2_att2_c_N=e(N)

    *(3) week and comuna fe, quarantine control and COVID controls
    local controls quarantine caseRate pcr positivity
    eststo `en'_3_att1_d: areg `v' `indvar1' `controls' i.w `cond2', `opt2'
    local `en'_3_att1_d_N=e(N)
    eststo `en'_3_att1_c: areg `v' `indvar2' `controls' i.w `cond2', `opt2'
    local `en'_3_att1_c_N=e(N)
    *add attendance interaction: Binary Open
    eststo `en'_3_att2_d: reg `v' `indvar1' OpenAttendance1 `controls' i.w `cond', `opt2'
    local Attp25_3d = _b[SchoolOpen_i]+_b[OpenAttendance1]*`p25'
    local Attp50_3d = _b[SchoolOpen_i]+_b[OpenAttendance1]*`p50'
    local Attp75_3d = _b[SchoolOpen_i]+_b[OpenAttendance1]*`p75'
    local Attp90_3d = _b[SchoolOpen_i]+_b[OpenAttendance1]*`p90'
    local `en'_3_att2_d_N=e(N)
    *add attendance interaction: Continuous Open
    eststo `en'_3_att2_c: reg `v' `indvar2' propOpenAttendance1 `controls' i.w `cond', `opt2'
    local Attp25_3c = _b[prop_schools_i]+_b[propOpenAttendance1]*`p25'
    local Attp50_3c = _b[prop_schools_i]+_b[propOpenAttendance1]*`p50'
    local Attp75_3c = _b[prop_schools_i]+_b[propOpenAttendance1]*`p75'
    local Attp90_3c = _b[prop_schools_i]+_b[propOpenAttendance1]*`p90'
    local `en'_3_att2_c_N=e(N)

    foreach t in d c {
        forval j=1/3 {
            local Attp25_`j'`t' : display %09.3f `Attp25_`j'`t''
            local Attp50_`j'`t' : display %09.3f `Attp50_`j'`t''
            local Attp75_`j'`t' : display %09.3f `Attp75_`j'`t''
            local Attp90_`j'`t' : display %09.3f `Attp90_`j'`t''
            local `en'_`j'_att1_`t'_N: display %9.0fc ``en'_`j'_att1_`t'_N'
            local `en'_`j'_att2_`t'_N: display %9.0fc ``en'_`j'_att2_`t'_N'
            dis "Finished `j', `t'"
        }
    }
    local a25_`en' : display %05.3f `Attp25_3d'
    local a50_`en' : display %05.3f `Attp50_3d'
    local a75_`en' : display %05.3f `Attp75_3d'
    local a90_`en' : display %05.3f `Attp90_3d'
    dis "``en'_3_att1_d_N'"
    local N_`en'   ``en'_3_att1_d_N'
    local m_`en'   : display %05.3f ``en'_mean'
    
    foreach t in d c {
        #delimit ;
        local R25Att`t' "Reopening Effect at Percentile 25 of Attendance && `Attp25_1`t''
        && `Attp25_2`t'' && `Attp25_3`t''";
        local R50Att`t' "Reopening Effect at Percentile 50 of Attendance && `Attp50_1`t''
        && `Attp50_2`t'' && `Attp50_3`t''";
        local R75Att`t' "Reopening Effect at Percentile 75 of Attendance && `Attp75_1`t''
        && `Attp75_2`t'' && `Attp75_3`t''";
        local R90Att`t' "Reopening Effect at Percentile 90 of Attendance && `Attp90_1`t''
        && `Attp90_2`t'' && `Attp90_3`t''";
        local mn "Baseline Mean    & ``en'_mean' & ``en'_mean' & ``en'_mean'
        & ``en'_mean' & ``en'_mean' & ``en'_mean'";
        local obs`t' "Observations & ``en'_1_att1_`t'_N' & ``en'_1_att2_`t'_N'
        & ``en'_2_att1_`t'_N' & ``en'_2_att2_`t'_N' & ``en'_3_att1_`t'_N' & ``en'_3_att2_`t'_N'";
        #delimit cr
    }

    *export tex file: dummy
    file open ff  using "$OUT/panelA_attendance1_extra_`en'.tex", write replace
    file write ff "`R25Attd'   \\" _n
    file write ff "`R50Attd'   \\" _n
    file write ff "`R75Attd'   \\" _n
    file write ff "`R90Attd'   \\" _n
    file write ff "`mn'        \\" _n
    file write ff "`obsd'      \\" _n
    file close ff

    *export tex file: continuous
    file open ff  using "$OUT/panelB_attendance1_extra_`en'.tex", write replace
    file write ff "`R25Attc'   \\" _n
    file write ff "`R50Attc'   \\" _n
    file write ff "`R75Attc'   \\" _n
    file write ff "`R90Attc'   \\" _n
    file write ff "`mn'        \\" _n
    file write ff "`obsc'      \\" _n
    file close ff

    *PANEL A
    #delimit ;
    local rg1 `en'_1_att1_d `en'_1_att2_d `en'_2_att1_d `en'_2_att2_d
    `en'_3_att1_d `en'_3_att2_d;
    esttab `rg1', keep(SchoolClose2 SchoolOpen_i OpenAttendance1) b(%-9.4f)
    se(%-9.4f) noobs;
    esttab `rg1' using "$OUT/panelA_attendance1_`en'.tex", b(%-9.3f) se(%-9.3f)
    noobs keep(SchoolClose2 SchoolOpen_i OpenAttendance1) mlabels(, none) nogaps
    nonumbers style(tex) starlevel ("*" 0.10 "**" 0.05 "***" 0.01) nonotes
    fragment replace noline label;

    *PANEL B;
    local rg2 `en'_1_att1_c `en'_1_att2_c `en'_2_att1_c `en'_2_att2_c
    `en'_3_att1_c `en'_3_att2_c;
    esttab `rg2', keep(SchoolClose2 prop_schools_i propOpenAttendance1) b(%-9.4f)
    se(%-9.4f) noobs;
    esttab `rg2' using "$OUT/panelB_attendance1_`en'.tex", b(%-9.3f) se(%-9.3f)
    noobs keep(SchoolClose2 prop_schools_i propOpenAttendance1) nonotes nogaps
    mlabels(, none) nonumbers style(tex) starlevel ("*" 0.10 "**" 0.05 "***" 0.01)
    fragment replace noline label;
    #delimit cr
}
local e V_3_att1_d V_3_att2_d SA_3_att1_d SA_3_att2_d R_3_att1_d R_3_att2_d
#delimit ;
esttab `e' using "$OUT/attendanceInt.tex", b(%-9.3f) se(%-9.3f)
noobs keep(SchoolClose2 SchoolOpen_i OpenAttendance1) mlabels(, none) nogaps
nonumbers style(tex) starlevel ("*" 0.10 "**" 0.05 "***" 0.01) nonotes
fragment replace noline label;

local dd "Reopening Effect at Percentile";
local bb "Baseline Mean";
local oo "Observations";
local R25 "`dd' 25 of Attendance && `a25_V' && `a25_SA' && `a25_R'";
local R50 "`dd' 50 of Attendance && `a50_V' && `a50_SA' && `a50_R'";
local R75 "`dd' 75 of Attendance && `a75_V' && `a75_SA' && `a75_R'";
local R90 "`dd' 90 of Attendance && `a90_V' && `a90_SA' && `a90_R'";
local mn  "`bb' & `m_V' & `m_V' & `m_SA' & `m_SA' & `m_R' & `m_R'";
local obs "`oo' & `N_V' & `N_V' & `N_SA' & `N_SA' & `N_R' & `N_R'";
#delimit cr

file open ff  using "$OUT/attendanceInt_extra.tex", write replace
file write ff "`R25'   \\" _n
file write ff "`R50'   \\" _n
file write ff "`R75'   \\" _n
file write ff "`R90'   \\" _n
file write ff "`mn'    \\" _n
file write ff "`obs'   \\" _n
file close ff
*/


local varr rate rateSA rateV
local cond "if year>=2019 [aw=populationyoung]"
local opt2 "cluster(comuna) abs(comuna)"
local indvar1 SchoolClose2 SchoolOpen_i
local indvar2 SchoolClose2 prop_schools_i

gen development = subinstr(IDC, ",", ".", 1)
destring development, replace
gen bE              = p_comunal_m if year < 2020 & year>=2018
bys comuna: egen baseEmployment = mean(bE)
gen bS              = rateAsistentes if year < 2020 & year>=2018
bys comuna: egen baseSupport = mean(bS)


egen ZAtt= std(attendAll)
egen ZDev= std(development)
egen ZEmp= std(baseEmployment)
egen ZSup= std(baseSupport)

gen OpenAtt         = ZAtt*SchoolOpen_i
gen OpenDev         = ZDev*SchoolOpen_i
gen OpenEmp         = ZEmp*SchoolOpen_i
gen OpenSup         = ZSup*SchoolOpen_i

lab var SchoolClose2 "School Closure"
lab var SchoolOpen_i "School Reopening"
lab var OpenAtt      "School Reopening$\times$ Attendance (Z)"
lab var OpenDev      "School Reopening$\times$ Development (Z)"
lab var OpenEmp      "School Reopening$\times$ Employment (Z)"
lab var OpenSup      "School Reopening$\times$ Support (Z)"
//school support system
//
estimates clear
foreach v of local varr {
    local controls quarantine caseRate pcr positivity
    eststo: reg `v' `indvar1' OpenAtt `controls' i.w `cond', `opt2'
    eststo: reg `v' `indvar1' OpenDev `controls' i.w `cond', `opt2'    
    eststo: reg `v' `indvar1' OpenEmp `controls' i.w `cond', `opt2'
    eststo: reg `v' `indvar1' OpenSup `controls' i.w `cond', `opt2'
    
    eststo: reg `v' `indvar1' OpenAtt OpenDev OpenEmp OpenSup `controls' i.w `cond', `opt2'

    #delimit ;
    esttab est1 est2 est3 est4 est5 
    using "$OUT/interactionsMun_`v'.tex", style(tex)  replace
    keep(`indvar1' OpenAtt OpenDev OpenEmp OpenSup) b(%-9.3f) se(%-9.3f)
    nonotes nogaps mlabels(, none) nonumbers
    starlevel ("*" 0.10 "**" 0.05 "***" 0.01)
    fragment  noline label;
    #delimit cr
    estimates clear
}


egen totals = rowtotal(caso casoSA casoV)
gen  totalRate = totals/populationyoung*100000
local controls quarantine caseRate pcr positivity
eststo: reg totalRate  `indvar1' OpenAtt `controls' i.w `cond', `opt2'
sum totalRate if year<2020
estadd scalar mean=r(mean) 
eststo: reg totalRate  `indvar1' OpenDev `controls' i.w `cond', `opt2'    
sum totalRate if year<2020
estadd scalar mean=r(mean) 
eststo: reg totalRate  `indvar1' OpenEmp `controls' i.w `cond', `opt2'
sum totalRate if year<2020
estadd scalar mean=r(mean) 
eststo: reg totalRate  `indvar1' OpenSup `controls' i.w `cond', `opt2'
sum totalRate if year<2020
estadd scalar mean=r(mean) 


eststo: reg totalRate  `indvar1' OpenAtt OpenDev OpenEmp OpenSup `controls' i.w `cond', `opt2'
sum totalRate if year<2020
estadd scalar mean=r(mean) 

lab var SchoolClose2 "School Closure"
lab var SchoolOpen_i "School Reopening"
lab var OpenAtt      "School Reopening$\times$ Baseline Attendance (Z)"
lab var OpenDev      "School Reopening$\times$ Baseline Development (Z)"
lab var OpenEmp      "School Reopening$\times$ Baseline Employment (Z)"
lab var OpenSup      "School Reopening$\times$ Baseline Support (Z)"

#delimit ;
esttab est1 est2 est3 est4 est5 using "$OUT/interactionsMunTot.tex",
style(tex)  replace
keep(`indvar1' OpenAtt OpenDev OpenEmp OpenSup) b(%-9.3f) se(%-9.3f)
nonotes nogaps mlabels(, none) nonumbers
starlevel ("*" 0.10 "**" 0.05 "***" 0.01)
stats(mean N, fmt(%4.2f %10.0gc) label("\\ Baseline Mean" "Observations"))
fragment  noline label;
#delimit cr
estimates clear


exit 
*/
*-----------------------------------------------------------------------------
*-- (4a) Event Studies -- closure
*-------------------------------------------------------------------------------
use $DAT/`data', clear

cap gen vacation = w>=1&w<=10
cap gen winter   = w==27|w==28|w==29
xtset comuna week
gen timeToClose = week-63
local cond "if year>=2019 [aw=populationyoung]"
qui tab w, gen(w)
local cond [aw=populationyoung]
local cond if year>2016 [aw=populationyoung]
local varr rate rateSA rateV
local base w1-w51 

//Compare with caseRate
gen lead60 = timeToClose<=-60
foreach num of numlist 59(-1)2 {
    gen lead`num' = timeToClose == -`num'
}
foreach num of numlist 0(1)19 {
    gen lag`num' = timeToClose == `num'

}
gen lag20 = timeToClose>=20

areg caseRate lead* lag*, absorb(comuna) 
gen TIME = _n-61 in 1/80
gen BETA = .
gen LB   = .
gen UB   = .
local j=1
foreach l of numlist 60(-1)2 {
    qui replace BETA = _b[lead`l'] in `j'
    qui replace LB   = _b[lead`l']-1.96*_se[lead`l'] in `j'
    qui replace UB   = _b[lead`l']+1.96*_se[lead`l'] in `j'
    local ++j
}
qui replace BETA = 0 in `j'
qui replace LB   = 0 in `j'
qui replace UB   = 0 in `j'
local ++j
foreach l of numlist 0(1)20 {
    qui replace BETA = _b[lag`l'] in `j'
    qui replace LB   = _b[lag`l']-1.96*_se[lag`l'] in `j'
    qui replace UB   = _b[lag`l']+1.96*_se[lag`l'] in `j'
    local ++j
}
twoway rarea LB UB TIME, color(gs10%50) || scatter BETA TIME, mc(red%70)
graph export "$GRA/eventCOVID_close.pdf", replace


//Compare with quaratine
areg quarantine lead* lag*, absorb(comuna) 
gen BETAq = .
gen LBq   = .
gen UBq   = .
local j=1
foreach l of numlist 60(-1)2 {
    qui replace BETAq = _b[lead`l'] in `j'
    qui replace LBq   = _b[lead`l']-1.96*_se[lead`l'] in `j'
    qui replace UBq   = _b[lead`l']+1.96*_se[lead`l'] in `j'
    local ++j
}
qui replace BETAq = 0 in `j'
qui replace LBq   = 0 in `j'
qui replace UBq   = 0 in `j'
local ++j
foreach l of numlist 0(1)20 {
    qui replace BETAq = _b[lag`l'] in `j'
    qui replace LBq   = _b[lag`l']-1.96*_se[lag`l'] in `j'
    qui replace UBq   = _b[lag`l']+1.96*_se[lag`l'] in `j'
    local ++j
}
twoway rarea LBq UBq TIME, color(gs10%50) || scatter BETAq TIME, mc(red%70)
graph export "$GRA/eventQuarantine_close.pdf", replace


/*
//xtitle("Weeks Relative to Schools Close")
//xlabel(-60 "{&le} 15/01/2019" -50 "-50" -40 "-40" -30 "-30"
//-20 "-20" -15 "-15" -10 "-10" -5 "-5" 0 "0" 5 "5" 10 "10"
//15 "15" 20 "{&ge} 20")
local yl = 4
foreach v of local varr {
    if `"`v'"'=="rate" {
        local b=-1
        local leads 60(-1)2
        local yl -4(1)4
    }
    else {
        local b=-3
        local leads 60(-1)4 2 1
    }
    if `"`v'"'=="rateSA" local yl -3(1)3
    if `"`v'"'=="rateV"  local yl -1(1)2    
    
    *(2) week and comuna fe
    #delimit ;
    eventdd `v' i.comuna `base' `cond', baseline(`b')
    timevar(timeToClose) cluster(comuna) lags(20) leads(60) accum
    endpoints_op(ms(Dh) mc(midblue)) coef_op(ms(Dh) mc(midblue))
    graph_op(legend(off)  ylabel(#10, format(%9.1f) labsize(medium))
             ytitle("Criminal Complaints per 100,000", size(medlarge)) 
             xlabel(-60 "{&le} 15 Jan 19" -50 "20 Mar 19" -40 "4 Jun 19"
                    -30 "13 Aug 19" -20 "22 Oct 19" -10 "31 Dec 19"
                    0 "10 Mar 20" 10 "19 May 20"
                    20 "{&ge} 28 Jul 20", angle(45) labsize(medium))
             xtitle(""));
    graph export "$GRA/eventdd_noControls_`v'.eps", replace;
    #delimit cr
    //Comparison event study
    gen BETAv = .
    gen LBv   = .
    gen UBv   = .
    local j=1
    foreach l of numlist `leads' {
        qui replace BETAv = _b[lead`l'] in `j'
        qui replace LBv   = _b[lead`l']-1.96*_se[lead`l'] in `j'
        qui replace UBv   = _b[lead`l']+1.96*_se[lead`l'] in `j'
        local ++j
    }
    qui replace BETAv = 0 in `j'
    qui replace LBv   = 0 in `j'
    qui replace UBv   = 0 in `j'
    local ++j
    foreach l of numlist 0(1)20 {
        qui replace BETAv = _b[lag`l'] in `j'
        qui replace LBv   = _b[lag`l']-1.96*_se[lag`l'] in `j'
        qui replace UBv   = _b[lag`l']+1.96*_se[lag`l'] in `j'
        local ++j
    }
    #delimit ;
    twoway rarea LB UB TIME, color(gs10%50) 
    || scatter BETA TIME, mc(red%70)
    || rcap LBv UBv TIME, color(black%50)
    || scatter BETAv TIME, ms(Dh) mc(midblue) 
    ylabel(`yl', labsize(medium)) ytitle("Rate per 100,000", size(medlarge))
    xline(-1, lcolor(black)) yline(0, lcolor(black%70) lpattern(solid))
    xlabel(-60 "{&le} 15 Jan 2019" -50 "20 Mar 2019" -40 "4 Jun 2019"
           -30 "13 Aug 2019" -20 "22 Oct 2019" -10 "31 Dec 2019"
           0 "10 Mar 2020" 10 "19 May 2020" 20 "{&ge} 28 Jul 2020", angle(45))
    xtitle("Calendar Time", size(medlarge))
    legend(order(4 "Criminal Complaints" 3 "95% CI" 2 "COVID cases" 1 "95% CI")
           pos(1) ring(0) rows(1));
    #delimit cr
    graph export "$GRA/eventJoint_close_`v'.pdf", replace
    drop BETAv UBv LBv 
    
    *(3) week and comuna fe, quarantine control
    #delimit ;
    eventdd `v' i.comuna `base' quarantine `cond',  baseline(`b')
    timevar(timeToClose) cluster(comuna) lags(20) leads(60) accum
    endpoints_op(ms(Dh) mc(midblue)) coef_op(ms(Dh) mc(midblue))
    graph_op(legend(off)  ylabel(#10, format(%9.1f) labsize(medium))
             ytitle("Criminal Complaints per 100,000", size(medlarge)) 
             xlabel(-60 "{&le} 15 Jan 19" -50 "20 Mar 19" -40 "4 Jun 19"
                    -30 "13 Aug 19" -20 "22 Oct 19" -10 "31 Dec 19"
                    0 "10 Mar 20" 10 "19 May 20"
                    20 "{&ge} 28 Jul 20", angle(45) labsize(medium))
             xtitle(""));
    graph export "$GRA/eventdd_QuarantineControls_`v'.eps", replace;
    #delimit cr

    *(4) COVID controls, quarantine control
    local controls caseRate pcr positivity quarantine
    #delimit ;
    eventdd `v' i.comuna `base' `controls' `cond',  baseline(`b')
    timevar(timeToClose) cluster(comuna) lags(20) leads(60) accum
    endpoints_op(ms(Dh) mc(midblue)) coef_op(ms(Dh) mc(midblue))
    graph_op(legend(off) ylabel(#10, format(%9.1f) labsize(medium))
             ytitle("Criminal Complaints per 100,000", size(medlarge)) 
             xlabel(-60 "{&le} 15 Jan 19" -50 "20 Mar 19" -40 "4 Jun 19"
                    -30 "13 Aug 19" -20 "22 Oct 19" -10 "31 Dec 19"
                    0 "10 Mar 20" 10 "19 May 20"
                    20 "{&ge} 28 Jul 20", angle(45) labsize(medium))
             xtitle(""));
    graph export "$GRA/eventdd_COVIDControls_`v'.eps", replace;
    #delimit cr		
    		
    *(5) COVID controls, quarantine control, mobility control
    local controls caseRate pcr positivity quarantine movilidad2 p_comunal_m p_comunal_h
    #delimit ;
    eventdd `v' i.comuna `base' `controls' `cond',  baseline(`b')
    timevar(timeToClose) cluster(comuna) lags(20) leads(60) accum
    endpoints_op(ms(Dh) mc(midblue)) coef_op(ms(Dh) mc(midblue))
    graph_op(legend(off) ylabel(#10, format(%9.1f) labsize(medium))
             ytitle("Criminal Complaints per 100,000", size(medlarge)) 
             xlabel(-60 "{&le} 15 Jan 19" -50 "20 Mar 19" -40 "4 Jun 19"
                    -30 "13 Aug 19" -20 "22 Oct 19" -10 "31 Dec 19"
                    0 "10 Mar 20" 10 "19 May 20"
                    20 "{&ge} 28 Jul 20", angle(45) labsize(medium))
             xtitle(""));
    graph export "$GRA/eventdd_mobility_`v'.eps", replace;
    #delimit cr

    //Comparison event study
    gen BETAv = .
    gen LBv   = .
    gen UBv   = .
    local j=1
    foreach l of numlist `leads' {
        qui replace BETAv = _b[lead`l'] in `j'
        qui replace LBv   = _b[lead`l']-1.96*_se[lead`l'] in `j'
        qui replace UBv   = _b[lead`l']+1.96*_se[lead`l'] in `j'
        local ++j
    }
    qui replace BETAv = 0 in `j'
    qui replace LBv   = 0 in `j'
    qui replace UBv   = 0 in `j'
    local ++j
    foreach l of numlist 0(1)20 {
        qui replace BETAv = _b[lag`l'] in `j'
        qui replace LBv   = _b[lag`l']-1.96*_se[lag`l'] in `j'
        qui replace UBv   = _b[lag`l']+1.96*_se[lag`l'] in `j'
        local ++j
    }
    #delimit ;
    twoway rarea LB UB TIME, color(gs10%50) 
    || scatter BETA TIME, mc(red%70)
    || rarea LBq UBq TIME, color(gs10%50) yaxis(2) ylabel(-1(0)1, axis(2))
    || scatter BETAq TIME, mc(purple%70) yaxis(2) 
    || rcap LBv UBv TIME, color(black%50)
    || scatter BETAv TIME, ms(Dh) mc(midblue) xline(0, lcolor(black))
    yline(0, lcolor(black))
    ylabel(-4(1)4, labsize(medium)) ytitle("Rate per 100,000", size(medlarge))
    ytitle("Lockdown", axis(2) size(medlarge))
    legend(order(5 "Criminal Complaints" 6 "95% CI" 1 "COVID cases" 3 "Lockdown"
                 2 "95% CI") pos(6) rows(1));
    #delimit cr
    graph export "$GRA/eventJoint_close_X_`v'.pdf", replace
    #delimit ;
    twoway rarea LB UB TIME, color(gs10%50) 
    || scatter BETA TIME, mc(red%70)
    || rcap LBv UBv TIME, color(black%50)
    || scatter BETAv TIME, ms(Dh) mc(midblue) 
    ylabel(`yl') ytitle("Rate per 100,000")
    xline(0, lcolor(black)) yline(0, lcolor(black))
    legend(order(4 "Criminal Complaints" 3 "95% CI" 2 "COVID cases" 1 "95% CI")
           pos(6) rows(1));
    #delimit cr
    graph export "$GRA/eventJoint_close_X_`v'.pdf", replace
    drop BETAv UBv LBv 
}
*/
drop LB UB TIME BETA LBq UBq BETAq lag* lead*
/*
gen monthsToClose = month-tm(2020m3)
foreach v of varlist rate rateSA rateV {
    if `"`v'"'=="rate" {
        local b=-1
        local leads 60(-1)2
        local yl -4(1)4
    }
    else {
        local b=-3
        local leads 60(-1)4 2 1
    }
    if `"`v'"'=="rateSA" local yl -3(1)3
    if `"`v'"'=="rateV"  local yl -1(1)2    

    #delimit ;
    eventdd `v' i.comuna `base' `cond', baseline(`b')
    timevar(timeToClose) cluster(comuna) lags(10) leads(14) accum
    endpoints_op(ms(Dh) mc(midblue)) coef_op(ms(Dh) mc(midblue))
    graph_op(legend(off) xtitle("Calendar Month", size(medlarge))
             ytitle("Criminal Complaints per 100,000", size(medlarge))
             ylabel(#6,format(%5.1f) labsize(medlarge))
             xlabel(-14 "{&le} Jan 2019" -10 "May 2019" -5 "Oct 2019"
                    0 "Mar 2020" 5 "Aug 2020" 10 "{&ge} Jan 2021",
                    labsize(medium) angle(15)  ));
    graph export "$GRA/eventdd_month_`v'.eps", replace;
    #delimit cr
}
*/


*-----------------------------------------------------------------------------
*-- (4b) Event Studies -- reopening
*-------------------------------------------------------------------------------
*time to open schools
bys comuna (week): egen minOpen = min(week) if SchoolOpen_i==1
bys comuna: egen openStart = min(minOpen)
gen timeToOpen = week-openStart

*keep only post close period
*drop if timeToClose<0

gen y2019    = year==2019
gen y2020    = year==2020


gen lead20 = timeToOpen<=-20
foreach num of numlist 19(-1)2 {
    gen lead`num' = timeToOpen == -`num'
}
foreach num of numlist 0(1)39 {
    gen lag`num' = timeToOpen == `num'

}
gen lag40 = timeToOpen>=40
areg caseRate lead* lag*, absorb(comuna) 
gen TIME = _n-21 in 1/60
gen BETA = .
gen LB   = .
gen UB   = .
local j=1
foreach l of numlist 20(-1)2 {
    qui replace BETA = _b[lead`l'] in `j'
    qui replace LB   = _b[lead`l']-1.96*_se[lead`l'] in `j'
    qui replace UB   = _b[lead`l']+1.96*_se[lead`l'] in `j'
    local ++j
}
qui replace BETA = 0 in `j'
qui replace LB   = 0 in `j'
qui replace UB   = 0 in `j'
local ++j
foreach l of numlist 0(1)40 {
    qui replace BETA = _b[lag`l'] in `j'
    qui replace LB   = _b[lag`l']-1.96*_se[lag`l'] in `j'
    qui replace UB   = _b[lag`l']+1.96*_se[lag`l'] in `j'
    local ++j
}
twoway rarea LB UB TIME, color(gs10%50) || scatter BETA TIME, mc(red%70)
graph export "$GRA/eventCOVID_open.pdf", replace

local varr rate rateSA rateV
local base w1-w51 
local leads 20
local cond if year>2016 [aw=populationyoung]

foreach v of local varr {
    if `"`v'"'=="rate"   local yl -4(1)4
    if `"`v'"'=="rateSA" local yl -3(1)3
    if `"`v'"'=="rateV"  local yl -1(1)2    
    *(2) week and comuna fe
    #delimit ;
    eventdd `v' i.comuna `base' `cond',
    timevar(timeToOpen) cluster(comuna) lags(40) leads(`leads') accum
    endpoints_op(ms(Dh) mc(midblue)) coef_op(ms(Dh) mc(midblue))
    graph_op(legend(off)
             xtitle("Weeks Relative to Schools Reopening", size(medlarge))
             ylabel(#10, format(%9.1f) labsize(medium))
             ytitle("Criminal Complaints per 100,000", size(medlarge)) 
             xlabel(-20 "{&le} -20" -15 "-15" -10 "-10" -5 "-5" 0 "0" 5 "5"
                    10 "10" 20 "20" 30 "30" 40 "{&ge} 40", labsize(medium)));
    graph export "$GRA/eventdd2_noControls_`v'.eps", replace;
    #delimit cr

    //Comparison event study
    gen BETAv = .
    gen LBv   = .
    gen UBv   = .
    local j=1
    foreach l of numlist 20(-1)2 {
        qui replace BETAv = _b[lead`l'] in `j'
        qui replace LBv   = _b[lead`l']-1.96*_se[lead`l'] in `j'
        qui replace UBv   = _b[lead`l']+1.96*_se[lead`l'] in `j'
        local ++j
    }
    qui replace BETAv = 0 in `j'
    qui replace LBv   = 0 in `j'
    qui replace UBv   = 0 in `j'
    local ++j
    foreach l of numlist 0(1)40 {
        qui replace BETAv = _b[lag`l'] in `j'
        qui replace LBv   = _b[lag`l']-1.96*_se[lag`l'] in `j'
        qui replace UBv   = _b[lag`l']+1.96*_se[lag`l'] in `j'
        local ++j
    }
    #delimit ;
    twoway rarea LB UB TIME, color(gs10%50) 
    || scatter BETA TIME, mc(red%70)
    || rcap LBv UBv TIME, color(black%50)
    || scatter BETAv TIME, ms(Dh) mc(midblue) 
    ylabel(`yl', labsize(medium)) ytitle("Rate per 100,000", size(medlarge))
    xline(-1, lcolor(black)) yline(0, lcolor(black%70) lpattern(solid))
    xlabel(-20 "{&le} -20" -15 "-15" -10 "-10" -5 "-5" 0 "0"
           5 "5" 10 "10" 20 "20" 30 "30" 40 "{&ge} 40", labsize(medium))
    xtitle("Weeks Relative to Schools Reopening", size(medlarge))
    legend(order(4 "Criminal Complaints" 3 "95% CI" 2 "COVID cases" 1 "95% CI")
           pos(1) ring(0) rows(1));
    #delimit cr
    graph export "$GRA/eventJoint_open_`v'.pdf", replace
    drop BETAv UBv LBv 
    //End
    
    *(3) week and comuna fe, quarantine control
    #delimit ;
    eventdd `v' i.comuna `base' quarantine `cond',
    timevar(timeToOpen) cluster(comuna) lags(40) leads(`leads') accum
    endpoints_op(ms(Dh) mc(midblue)) coef_op(ms(Dh) mc(midblue))
    graph_op(legend(off) ylabel(#10, format(%9.1f) labsize(medlarge))
             xtitle("Weeks Relative to Schools Reopening", size(medlarge))
             ytitle("Criminal Complaints per 100,000", size(medlarge)) 
             xlabel(-20 "{&le} -20" -15 "-15" -10 "-10" -5 "-5" 0 "0" 5 "5"
                    10 "10" 20 "20" 30 "30" 40 "{&ge} 40", labsize(medium)));
    graph export "$GRA/eventdd2_QuarantineControls_`v'.eps", replace;
    #delimit cr

    *(4) COVID controls, quarantine and epi controles
    local controls caseRate pcr positivity quarantine
    #delimit ;
    eventdd `v' i.comuna `base' `controls' `cond',
    timevar(timeToOpen) cluster(comuna) lags(40) leads(`leads') accum
    endpoints_op(ms(Dh) mc(midblue)) coef_op(ms(Dh) mc(midblue))
    graph_op(legend(off) ylabel(#10, format(%9.1f) labsize(medium))
             xtitle("Weeks Relative to Schools Reopening", size(medlarge))
             ytitle("Criminal Complaints per 100,000", size(medlarge)) 
             xlabel(-20 "{&le} -20" -15 "-15" -10 "-10" -5 "-5" 0 "0" 5 "5"
                    10 "10" 20 "20" 30 "30" 40 "{&ge} 40", labsize(medium)));
    graph export "$GRA/eventdd2_COVIDControls_`v'.eps", replace;
    #delimit cr		
    		
    *(5) COVID controls, quarantine control, mobility control
    local controls caseRate pcr positivity quarantine movilidad2 p_comunal_m p_comunal_h
    #delimit ;
    eventdd `v' i.comuna `base' `controls' `cond',
    timevar(timeToOpen) cluster(comuna) lags(40) leads(`leads') accum
    endpoints_op(ms(Dh) mc(midblue)) coef_op(ms(Dh) mc(midblue))
    graph_op(legend(off) ylabel(#10, format(%9.1f) labsize(medum))
             xtitle("Weeks Relative to Schools Reopening", size(medlarge))
             ytitle("Criminal Complaints per 100,000", size(medlarge)) 
             xlabel(-20 "{&le} -20" -15 "-15" -10 "-10" -5 "-5" 0 "0" 5 "5"
                    10 "10" 20 "20" 30 "30" 40 "{&ge} 40", labsize(medium)));
    graph export "$GRA/eventdd2_mobility_`v'.eps", replace;
    #delimit cr		
}


//By month
bys comuna (month): egen minMonthOpen = min(month) if SchoolOpen_i==1
bys comuna: egen openMonthStart = min(minMonthOpen)
gen monthsToOpen = month-openMonthStart
foreach v of varlist rate rateSA rateV {
    #delimit ;
    eventdd `v' i.comuna `base' `cond', 
    timevar(monthsToOpen) cluster(comuna) lags(14) leads(5) accum
    endpoints_op(ms(Dh) mc(midblue)) coef_op(ms(Dh) mc(midblue))
    graph_op(legend(off)
             xtitle("Months Relative to Schools Reopening", size(medlarge))
             ytitle("Criminal Complaints per 100,000", size(medlarge))
             ylabel(#6,format(%9.1f)labsize(medlarge))
             xlabel(-5 "{&le} -5" -4 "-4" -2 "-2"
                    0 "0" 2 "2" 4 "4" 6 "6" 8 "8" 10 "10" 12 "12" 14 "{&ge} 14",
                    labsize(medlarge)));
    graph export "$GRA/eventdd_month_open_`v'.eps", replace;
    #delimit cr
}
exit

*-------------------------------------------------------------------------------
* Figure 2: Heterogeneity
*-------------------------------------------------------------------------------
use $DAT/`data', clear
merge m:1 comuna using "$DAT/attendance", gen(_mergeAttend)

*local options
local cond1 "if year>=2019 [aw=populationyoung]"
local cond "if year>=2019"
local opt2 "cluster(comuna) abs(comuna)"
local indvar1 "SchoolClose2 SchoolOpen_i"

*matrix for store results
foreach en in V SA R {
    matrix `en'3=J(36,5,.) //week comuna fe and COVID controls
}

local varr rate rateSA rateV
foreach v of local varr {
    if "`v'"=="rate" local en V
    if "`v'"=="rateSA" local en SA
    if "`v'"=="rateV" local en R

    *for outcome mean
    qui sum `v' if week<=61 & year>=2019 [aw=populationyoung]
    local `en'_mean=`r(mean)'
    matrix `en'3[1,5] = `r(mean)'

    *(2) week comuna fe and COVID controls
    local controls quarantine caseRate pcr positivity
    qui areg `v' `indvar1' `controls' i.w `cond1', `opt2'
	
    matrix `en'3[1,1] = _b[SchoolClose2]
    matrix `en'3[2,1] = _b[SchoolOpen_i]
    matrix `en'3[1,2] = _b[SchoolClose2] - invttail(e(df_r),0.025)*_se[SchoolClose]
    matrix `en'3[2,2] = _b[SchoolOpen_i] - invttail(e(df_r),0.025)*_se[SchoolOpen_i]
    matrix `en'3[1,3] = _b[SchoolClose2] + invttail(e(df_r),0.025)*_se[SchoolClose]
    matrix `en'3[2,3] = _b[SchoolOpen_i] + invttail(e(df_r),0.025)*_se[SchoolOpen_i]
    matrix `en'3[1,4] = e(N)
}

*-------*
*By Age
*-------*
local varr rate rateSA rateV
foreach v of local varr {
    if "`v'"=="rate" local en V
    if "`v'"=="rateSA" local en SA
    if "`v'"=="rateV" local en R
    local j=4
    local k=5
    forvalues i=1/5 {
		qui sum `v'`i' if week<=61 & year>=2019 [aw=population`i']
		matrix `en'3[`j',5] = `r(mean)'

        *(2)week comuna fe and COVID controls
        local controls quarantine caseRate pcr positivity
        qui areg `v'`i' `indvar1' `controls'  i.w `cond' [aw=population`i'], `opt2'
        matrix `en'3[`j',1] = _b[SchoolClose2]
        matrix `en'3[`k',1] = _b[SchoolOpen_i]
        matrix `en'3[`j',2] = _b[SchoolClose2] - invttail(e(df_r),0.025)*_se[SchoolClose]
        matrix `en'3[`k',2] = _b[SchoolOpen_i] - invttail(e(df_r),0.025)*_se[SchoolOpen_i]
        matrix `en'3[`j',3] = _b[SchoolClose2] + invttail(e(df_r),0.025)*_se[SchoolClose]
        matrix `en'3[`k',3] = _b[SchoolOpen_i] + invttail(e(df_r),0.025)*_se[SchoolOpen_i]
        matrix `en'3[`j',4] = e(N)
		
        local j=`j'+2
        local k=`k'+2
    }
}

*-------*
*By Sex
*-------*
foreach en in V SA R {
    if "`en'"=="V"  local depvar rate
    if "`en'"=="SA" local depvar rateSA
    if "`en'"=="R"  local depvar rateV
    local j=15
    local k=16
    foreach i in Girls Boys {
        *for outcome mean
        qui sum `depvar'`i' if week<=61 & year>=2019 [aw=population`i']
        matrix `en'3[`j',5] = `r(mean)'
		
        *(2) week comuna fe and COVID controls
        local controls quarantine caseRate pcr positivity
        qui areg `depvar'`i' `indvar1' `controls' i.w `cond' [aw=population`i'], `opt2'
        matrix `en'3[`j',1] = _b[SchoolClose2]
        matrix `en'3[`k',1] = _b[SchoolOpen_i]
        matrix `en'3[`j',2] = _b[SchoolClose2] - invttail(e(df_r),0.025)*_se[SchoolClose]
        matrix `en'3[`k',2] = _b[SchoolOpen_i] - invttail(e(df_r),0.025)*_se[SchoolOpen_i]
        matrix `en'3[`j',3] = _b[SchoolClose2] + invttail(e(df_r),0.025)*_se[SchoolClose]
        matrix `en'3[`k',3] = _b[SchoolOpen_i] + invttail(e(df_r),0.025)*_se[SchoolOpen_i]
        matrix `en'3[`j',4] = e(N)
		
        local j=`j'+2
        local k=`k'+2
    }
}

*-----------*
*Development
*-----------*
foreach en in V SA R {
    if "`en'"=="V"  local depvar rate
    if "`en'"=="SA" local depvar rateSA
    if "`en'"=="R"  local depvar rateV
    local j=20
    local k=21
    local rnk A MA M MB B
    foreach i of local rnk {
        *local options
        local cond "if year>=2019 & r_IDC=="`i'" [aw=populationyoung]"
        *for outcome mean
        qui sum `depvar' if week<=61 & r_IDC=="`i'" & year>=2019 [aw=populationyoung]
        matrix `en'3[`j',5] = `r(mean)'

        *(2) week comuna fe and COVID controls
        local controls quarantine caseRate pcr positivity
        qui areg `depvar' `indvar1' `controls' i.w `cond', `opt2'
        matrix `en'3[`j',1] = _b[SchoolClose2]
        matrix `en'3[`k',1] = _b[SchoolOpen_i]
        matrix `en'3[`j',2] = _b[SchoolClose2] - invttail(e(df_r),0.025)*_se[SchoolClose]
        matrix `en'3[`k',2] = _b[SchoolOpen_i] - invttail(e(df_r),0.025)*_se[SchoolOpen_i]
        matrix `en'3[`j',3] = _b[SchoolClose2] + invttail(e(df_r),0.025)*_se[SchoolClose]
        matrix `en'3[`k',3] = _b[SchoolOpen_i] + invttail(e(df_r),0.025)*_se[SchoolOpen_i]
        matrix `en'3[`j',4] = e(N)
		
        local j=`j'+2
        local k=`k'+2
    }
}

*----------*
*Quarantine
*----------*
*indicator for municipality under quarantine early of later
gen q=0
replace q=1 if week>=65 & week<=87
replace q=2 if week>87
egen ind=mean(quarantine) if q==0, by(comuna) 
replace ind=0 if ind==.
egen ind2=mean(quarantine) if q==1, by(comuna)
replace ind2=0 if ind2==.
egen ind3=mean(quarantine) if q==2, by(comuna) 
replace ind3=0 if ind3==.
egen c2=mean(ind2), by(comuna) 
egen c3=mean(ind3), by(comuna) 
replace c2=1 if c2!=0
replace c3=2 if c3!=0 & c2!=1
replace c3=0 if c3!=2
gen d=c2+c3
drop q ind*
la def d 0 "Never" 1 "Early" 2 "Later"
la val d d
xtset comuna week

foreach en in V SA R {
    if "`en'"=="V"  local depvar rate
    if "`en'"=="SA" local depvar rateSA
    if "`en'"=="R"  local depvar rateV
    local j=31
    local k=32
    forvalues i=0(1)2 {
        if "`i'"=="0" local m ind0
        if "`i'"=="1" local m ind1
        if "`i'"=="2" local m ind2
        *local options
        local cond "if year>=2019 & d==`i' [aw=populationyoung]"
        *for outcome mean
        qui sum `depvar' if week<=61 & d==`i' & year>=2019 [aw=populationyoung]
        matrix `en'3[`j',5] = `r(mean)'
		
        *(2) week comuna fe and COVID controls
        local controls quarantine caseRate pcr positivity
        qui areg `depvar' `indvar1' `controls' i.w `cond', `opt2'
        matrix `en'3[`j',1] = _b[SchoolClose2]
        matrix `en'3[`k',1] = _b[SchoolOpen_i]
        matrix `en'3[`j',2] = _b[SchoolClose2] - invttail(e(df_r),0.025)*_se[SchoolClose]
        matrix `en'3[`k',2] = _b[SchoolOpen_i] - invttail(e(df_r),0.025)*_se[SchoolOpen_i]
        matrix `en'3[`j',3] = _b[SchoolClose2] + invttail(e(df_r),0.025)*_se[SchoolClose]
        matrix `en'3[`k',3] = _b[SchoolOpen_i] + invttail(e(df_r),0.025)*_se[SchoolOpen_i]
        matrix `en'3[`j',4] = e(N)
		
        local j=`j'+2
        local k=`k'+2
    }
}

*-----------------------------
*SCHOOL CLOSURE AND REOPENING
*-----------------------------
*VIF
clear
svmat V3
gen gr=1 if V34!=.
replace gr=2 if V34==. & V31!=.
gen orden=-_n	
gen l=-6.2
gen u=1
gen x1=-6.2
gen x2=-5
*color in age range
gen orden1=-3
replace orden1=-14 if orden==-36
*color in development range
gen orden2=-19
replace orden2=-30 if orden==-36
format V35 %9.3f
gen aux=V34/54214*100  
format aux %9.0f
gen aux3=" ("
gen aux4=")"
egen lab=concat(V34 aux3 aux aux4), format(%9.0fc)
set obs 37
replace orden1=0 in 37

#delimit ;
twoway  rarea l u orden1, hor color(gs14) fcol(gs14) fi(gs14) 
        || rarea l u orden2, hor color(gs14) fcol(gs14) fi(gs14)
        || pci 0 0 -36 0, lp(dash) lc(red)
        || rcap V32 V33 orden if gr==1, hor lc(black)
        || rcap V32 V33 orden if gr==2, hor lc(blue)
        || scatter orden V31 if gr==1, mc(black) msym(Dh)
        || scatter orden V31 if gr==2, mc(blue) msym(O)
        || scatter orden x1 if V34!=., mlabel(lab) ms(none)
        || scatter orden x2 if V34!=., mlabel(V35) ms(none)
        text(0.7   -5.8 "{bf:Observations (%)}", size(.25cm))
        text(0.7   -4.6 "{bf:Baseline rate}", size(.25cm))
        text(-2.9  -6.7 "{bf:Age Group}", size(.22cm))
        text(-13.9  -6.6 "{bf:Sex}", size(.22cm))
        text(-18.9 -6.8 "{bf:Development}", size(.22cm))
        text(-29.9 -6.7 "{bf:Quarantine}", size(.22cm))	
        text(-41   -1.5 "Change in reporting per 100,000 children", size(.36cm))	
        ylab(-1  "{bf:Overall}" -4  "   [1-6]"    -6  "   [7-10]"
             -8  "   [11-13]"   -10  "   [14-15]" -12  "   [16-17]"
             -15  "Female"      -17 "Male"        -20 "High"
             -22 "Medium-High"  -24 "Medium"      -26 "Medium-Low"
             -28 "Low"          -31 "Never"       -33 "Early Quarantine"
             -35 "Later Quarantine", labsize(vsmall) nogrid) 
        xlabel(-4(1)1, nogrid format(%9.1f)) xtitle("  ") ytitle("")
        yticks() yline(0, ext lp(solid) lc(gs10))
        legend(order(6 "School Closure" 7 "School Reopening") pos(1) col(2)); 
graph export "$GRA/SchoolsClose_3_both.eps", replace;
#delimit cr


*SEXUAL ABUSE
clear
svmat SA3
gen gr=1 if SA34!=.
replace gr=2 if SA34==. & SA31!=.
gen orden=-_n	
gen l=-5.2
gen u=2
gen x1=-5.2
gen x2=-3.8
*color in age range
gen orden1=-3
replace orden1=-14 if orden==-36
*color in development range
gen orden2=-19
replace orden2=-30 if orden==-36
format SA35 %9.3f
gen aux=SA34/53179*100  
format aux %9.0f
gen aux3=" ("
gen aux4=")"
egen lab=concat(SA34 aux3 aux aux4), format(%9.0fc)
set obs 37
replace orden1=0 in 37
	
#delimit ;
twoway  rarea l u orden1, hor color(gs14) fcol(gs14) fi(gs14) 
        || rarea l u orden2, hor color(gs14) fcol(gs14) fi(gs14)
        || pci 0 0 -36 0, lp(dash) lc(red)
        || rcap SA32 SA33 orden if gr==1, hor lc(black)
        || rcap SA32 SA33 orden if gr==2, hor lc(blue)
        || scatter orden SA31 if gr==1, mc(black) msym(Dh)
        || scatter orden SA31 if gr==2, mc(blue) msym(O)
        || scatter orden x1 if SA34!=., mlabel(lab) ms(none)
        || scatter orden x2 if SA34!=., mlabel(SA35) ms(none)
        text(0.7   -4.8 "{bf:Observations (%)}", size(.25cm))
        text(0.7   -3.5 "{bf:Baseline rate}", size(.25cm))
        text(-2.9  -5.7 "{bf:Age Group}", size(.22cm))
        text(-13.9  -5.6 "{bf:Sex}", size(.22cm))
        text(-18.9 -5.8 "{bf:Development}", size(.22cm))
        text(-29.9 -5.7 "{bf:Quarantine}", size(.22cm))	
        text(-41   -0.5 "Change in reporting per 100,000 children", size(.36cm))	
        ylab(-1  "{bf:Overall}" -4  "   [1-6]"    -6  "   [7-10]"
             -8  "   [11-13]"   -10  "   [14-15]" -12  "   [16-17]"
             -15  "Female"      -17 "Male"        -20 "High"
             -22 "Medium-High"  -24 "Medium"      -26 "Medium-Low"
             -28 "Low"          -31 "Never"       -33 "Early Quarantine"
             -35 "Later Quarantine", labsize(vsmall) nogrid) 
        xlabel(-3(1)2, nogrid format(%9.1f))
        yticks() yline(0, ext lp(solid) lc(gs10))
        xtitle("  ") ytitle("") legend(order(6 "School Closure" 7 "School Reopening") pos(1) col(2)); 
graph export "$GRA/SchoolsClose_3_SA_both.eps", replace;
#delimit cr


*RAPE
clear
svmat R3
gen gr=1 if R34!=.
replace gr=2 if R34==. & R31!=.
gen orden=-_n	
gen l=-1.7
gen u=0.5
gen x1=-1.7
gen x2=-1.3
*color in age range
gen orden1=-3
replace orden1=-14 if orden==-36
*color in development range
gen orden2=-19
replace orden2=-30 if orden==-36
format R35 %9.3f
gen aux=R34/53179*100  
format aux %9.0f
gen aux3=" ("
gen aux4=")"
egen lab=concat(R34 aux3 aux aux4), format(%9.0fc)
set obs 37
replace orden1=0 in 37
	
#delimit ;
twoway  rarea l u orden1, hor color(gs14) fcol(gs14) fi(gs14) 
        || rarea l u orden2, hor color(gs14) fcol(gs14) fi(gs14)
        || pci 0 0 -36 0, lp(dash) lc(red)
        || rcap R32 R33  orden if gr==1, hor lc(black)
        || rcap R32 R33 orden if gr==2, hor lc(blue)
        || scatter orden R31 if gr==1, mc(black) msym(Dh)
        || scatter orden R31 if gr==2, mc(blue) msym(O)
        || scatter orden x1 if R34!=., mlabel(lab) ms(none)
        || scatter orden x2 if R34!=., mlabel(R35) ms(none)
        text(0.7   -1.6 "{bf:Observations (%)}", size(.25cm))
        text(0.7   -1.2 "{bf:Baseline rate}", size(.25cm))
        text(-2.9  -1.86 "{bf:Age Group}", size(.22cm))
        text(-13.9  -1.85 "{bf:Sex}", size(.22cm))
        text(-18.9 -1.89 "{bf:Development}", size(.22cm))
        text(-29.9 -1.86 "{bf:Quarantine}", size(.22cm))	
        text(-41   -0.25 "Change in reporting per 100,000 children", size(.36cm))	
        ylab(-1  "{bf:Overall}" -4  "   [1-6]"    -6  "   [7-10]"
             -8  "   [11-13]"   -10  "   [14-15]" -12  "   [16-17]"
             -15  "Female"      -17 "Male"        -20 "High"
             -22 "Medium-High"  -24 "Medium"      -26 "Medium-Low"
             -28 "Low"          -31 "Never"       -33 "Early Quarantine"
             -35 "Later Quarantine", labsize(vsmall) nogrid)  
        xlabel(-1(0.5)0.5, nogrid format(%9.1f))
        yticks() yline(0, ext lp(solid) lc(gs10))
        xtitle("  ") ytitle("") legend(order(6 "School Closure" 7 "School Reopening") pos(1) col(2)); 
graph export "$GRA/SchoolsClose_3_R_both.eps", replace;
#delimit cr
*/

*-----------------------------------------------------------------------------
*-- (6a) Event Studies by month -- closure
*-------------------------------------------------------------------------------
use $DAT/`data', clear

gen monthsToClose = month-tm(2020m3)
exit


local controls caseRate pcr positivity quarantine movilidad2 p_comunal_m p_comunal_h
local grp by(month comuna) 
collapse (first) populationyoung (mean) `controls' (sum) caso casoV casoSA, `grp'


xtset comuna month
gen timeToClose = month-tm(2020m3)
gen mes = mod(month,12)+1
gen rate   = caso  /populationyoung*100000
gen rateV  = casoV /populationyoung*100000
gen rateSA = casoSA/populationyoung*100000

#delimit ;
eventdd caso i.mes i.comuna  [aw=populationyoung],
timevar(timeToClose) cluster(comuna) lags(10) leads(14) accum
endpoints_op(ms(Dh) mc(midblue)) coef_op(ms(Dh) mc(midblue))
ci(rarea, color(gs10%50))
graph_op(legend(off) xtitle("Calendar Month", size(medlarge))
         ytitle("Criminal Complaints per 100,000", size(medlarge))
         ylabel(#6,format(%9.0f) labsize(medlarge))
         xlabel(-14 "{&le} Jan 2019" -10 "May 2019" -5 "Oct 2019"
                0 "Mar 2020" 5 "Aug 2020" 10 "{&ge} Jan 2021",
                labsize(medium) angle(15)  ));
graph export "$GRA/eventdd_month_rate.pdf", replace;
#delimit cr
exit

local cond "if year>=2019 [aw=populationyoung]"
qui tab w, gen(w)
local cond [aw=populationyoung]
local cond if year>2016 [aw=populationyoung]
local varr rate rateSA rateV
local base w1-w51 

//Compare with caseRate
gen lead60 = timeToClose<=-60
foreach num of numlist 59(-1)2 {
    gen lead`num' = timeToClose == -`num'
}
foreach num of numlist 0(1)19 {
    gen lag`num' = timeToClose == `num'

}
gen lag20 = timeToClose>=20

areg caseRate lead* lag*, absorb(comuna) 
gen TIME = _n-61 in 1/80
gen BETA = .
gen LB   = .
gen UB   = .
local j=1
foreach l of numlist 60(-1)2 {
    qui replace BETA = _b[lead`l'] in `j'
    qui replace LB   = _b[lead`l']-1.96*_se[lead`l'] in `j'
    qui replace UB   = _b[lead`l']+1.96*_se[lead`l'] in `j'
    local ++j
}
qui replace BETA = 0 in `j'
qui replace LB   = 0 in `j'
qui replace UB   = 0 in `j'
local ++j
foreach l of numlist 0(1)20 {
    qui replace BETA = _b[lag`l'] in `j'
    qui replace LB   = _b[lag`l']-1.96*_se[lag`l'] in `j'
    qui replace UB   = _b[lag`l']+1.96*_se[lag`l'] in `j'
    local ++j
}
twoway rarea LB UB TIME, color(gs10%50) || scatter BETA TIME, mc(red%70)
graph export "$GRA/eventCOVID_close.pdf", replace


//Compare with quaratine
areg quarantine lead* lag*, absorb(comuna) 
gen BETAq = .
gen LBq   = .
gen UBq   = .
local j=1
foreach l of numlist 60(-1)2 {
    qui replace BETAq = _b[lead`l'] in `j'
    qui replace LBq   = _b[lead`l']-1.96*_se[lead`l'] in `j'
    qui replace UBq   = _b[lead`l']+1.96*_se[lead`l'] in `j'
    local ++j
}
qui replace BETAq = 0 in `j'
qui replace LBq   = 0 in `j'
qui replace UBq   = 0 in `j'
local ++j
foreach l of numlist 0(1)20 {
    qui replace BETAq = _b[lag`l'] in `j'
    qui replace LBq   = _b[lag`l']-1.96*_se[lag`l'] in `j'
    qui replace UBq   = _b[lag`l']+1.96*_se[lag`l'] in `j'
    local ++j
}
twoway rarea LBq UBq TIME, color(gs10%50) || scatter BETAq TIME, mc(red%70)
graph export "$GRA/eventQuarantine_close.pdf", replace

//xtitle("Weeks Relative to Schools Close")
//xlabel(-60 "{&le} 15/01/2019" -50 "-50" -40 "-40" -30 "-30"
//-20 "-20" -15 "-15" -10 "-10" -5 "-5" 0 "0" 5 "5" 10 "10"
//15 "15" 20 "{&ge} 20")
local yl = 4
foreach v of local varr {
    if `"`v'"'=="rate" {
        local b=-1
        local leads 60(-1)2
        local yl -4(1)4
    }
    else {
        local b=-3
        local leads 60(-1)4 2 1
    }
    if `"`v'"'=="rateSA" local yl -3(1)3
    if `"`v'"'=="rateV"  local yl -1(1)2    
    
    *(2) week and comuna fe
    #delimit ;
    eventdd `v' i.comuna `base' `cond', baseline(`b')
    timevar(timeToClose) cluster(comuna) lags(20) leads(60) accum
    endpoints_op(ms(Dh) mc(midblue)) coef_op(ms(Dh) mc(midblue))
    graph_op(legend(off)  ylabel(#10, format(%9.1f) labsize(medium))
             ytitle("Criminal Complaints per 100,000", size(medlarge)) 
             xlabel(-60 "{&le} 15 Jan 19" -50 "20 Mar 19" -40 "4 Jun 19"
                    -30 "13 Aug 19" -20 "22 Oct 19" -10 "31 Dec 19"
                    0 "10 Mar 20" 10 "19 May 20"
                    20 "{&ge} 28 Jul 20", angle(45) labsize(medium))
             xtitle(""));
    graph export "$GRA/eventdd_noControls_`v'.eps", replace;
    #delimit cr
    //Comparison event study
    gen BETAv = .
    gen LBv   = .
    gen UBv   = .
    local j=1
    foreach l of numlist `leads' {
        qui replace BETAv = _b[lead`l'] in `j'
        qui replace LBv   = _b[lead`l']-1.96*_se[lead`l'] in `j'
        qui replace UBv   = _b[lead`l']+1.96*_se[lead`l'] in `j'
        local ++j
    }
    qui replace BETAv = 0 in `j'
    qui replace LBv   = 0 in `j'
    qui replace UBv   = 0 in `j'
    local ++j
    foreach l of numlist 0(1)20 {
        qui replace BETAv = _b[lag`l'] in `j'
        qui replace LBv   = _b[lag`l']-1.96*_se[lag`l'] in `j'
        qui replace UBv   = _b[lag`l']+1.96*_se[lag`l'] in `j'
        local ++j
    }
    #delimit ;
    twoway rarea LB UB TIME, color(gs10%50) 
    || scatter BETA TIME, mc(red%70)
    || rcap LBv UBv TIME, color(black%50)
    || scatter BETAv TIME, ms(Dh) mc(midblue) 
    ylabel(`yl', labsize(medium)) ytitle("Rate per 100,000", size(medlarge))
    xline(-1, lcolor(black)) yline(0, lcolor(black%70) lpattern(solid))
    xlabel(-60 "{&le} 15 Jan 2019" -50 "20 Mar 2019" -40 "4 Jun 2019"
           -30 "13 Aug 2019" -20 "22 Oct 2019" -10 "31 Dec 2019"
           0 "10 Mar 2020" 10 "19 May 2020" 20 "{&ge} 28 Jul 2020", angle(45))
    xtitle("Calendar Time", size(medlarge))
    legend(order(4 "Criminal Complaints" 3 "95% CI" 2 "COVID cases" 1 "95% CI")
           pos(1) ring(0) rows(1));
    #delimit cr
    graph export "$GRA/eventJoint_close_`v'.pdf", replace
    drop BETAv UBv LBv 
    
    *(3) week and comuna fe, quarantine control
    #delimit ;
    eventdd `v' i.comuna `base' quarantine `cond',  baseline(`b')
    timevar(timeToClose) cluster(comuna) lags(20) leads(60) accum
    endpoints_op(ms(Dh) mc(midblue)) coef_op(ms(Dh) mc(midblue))
    graph_op(legend(off)  ylabel(#10, format(%9.1f) labsize(medium))
             ytitle("Criminal Complaints per 100,000", size(medlarge)) 
             xlabel(-60 "{&le} 15 Jan 19" -50 "20 Mar 19" -40 "4 Jun 19"
                    -30 "13 Aug 19" -20 "22 Oct 19" -10 "31 Dec 19"
                    0 "10 Mar 20" 10 "19 May 20"
                    20 "{&ge} 28 Jul 20", angle(45) labsize(medium))
             xtitle(""));
    graph export "$GRA/eventdd_QuarantineControls_`v'.eps", replace;
    #delimit cr

    *(4) COVID controls, quarantine control
    local controls caseRate pcr positivity quarantine
    #delimit ;
    eventdd `v' i.comuna `base' `controls' `cond',  baseline(`b')
    timevar(timeToClose) cluster(comuna) lags(20) leads(60) accum
    endpoints_op(ms(Dh) mc(midblue)) coef_op(ms(Dh) mc(midblue))
    graph_op(legend(off) ylabel(#10, format(%9.1f) labsize(medium))
             ytitle("Criminal Complaints per 100,000", size(medlarge)) 
             xlabel(-60 "{&le} 15 Jan 19" -50 "20 Mar 19" -40 "4 Jun 19"
                    -30 "13 Aug 19" -20 "22 Oct 19" -10 "31 Dec 19"
                    0 "10 Mar 20" 10 "19 May 20"
                    20 "{&ge} 28 Jul 20", angle(45) labsize(medium))
             xtitle(""));
    graph export "$GRA/eventdd_COVIDControls_`v'.eps", replace;
    #delimit cr		
    		
    *(5) COVID controls, quarantine control, mobility control
    local controls caseRate pcr positivity quarantine movilidad2 p_comunal_m p_comunal_h
    #delimit ;
    eventdd `v' i.comuna `base' `controls' `cond',  baseline(`b')
    timevar(timeToClose) cluster(comuna) lags(20) leads(60) accum
    endpoints_op(ms(Dh) mc(midblue)) coef_op(ms(Dh) mc(midblue))
    graph_op(legend(off) ylabel(#10, format(%9.1f) labsize(medium))
             ytitle("Criminal Complaints per 100,000", size(medlarge)) 
             xlabel(-60 "{&le} 15 Jan 19" -50 "20 Mar 19" -40 "4 Jun 19"
                    -30 "13 Aug 19" -20 "22 Oct 19" -10 "31 Dec 19"
                    0 "10 Mar 20" 10 "19 May 20"
                    20 "{&ge} 28 Jul 20", angle(45) labsize(medium))
             xtitle(""));
    graph export "$GRA/eventdd_mobility_`v'.eps", replace;
    #delimit cr

    //Comparison event study
    gen BETAv = .
    gen LBv   = .
    gen UBv   = .
    local j=1
    foreach l of numlist `leads' {
        qui replace BETAv = _b[lead`l'] in `j'
        qui replace LBv   = _b[lead`l']-1.96*_se[lead`l'] in `j'
        qui replace UBv   = _b[lead`l']+1.96*_se[lead`l'] in `j'
        local ++j
    }
    qui replace BETAv = 0 in `j'
    qui replace LBv   = 0 in `j'
    qui replace UBv   = 0 in `j'
    local ++j
    foreach l of numlist 0(1)20 {
        qui replace BETAv = _b[lag`l'] in `j'
        qui replace LBv   = _b[lag`l']-1.96*_se[lag`l'] in `j'
        qui replace UBv   = _b[lag`l']+1.96*_se[lag`l'] in `j'
        local ++j
    }
    #delimit ;
    twoway rarea LB UB TIME, color(gs10%50) 
    || scatter BETA TIME, mc(red%70)
    || rarea LBq UBq TIME, color(gs10%50) yaxis(2) ylabel(-1(0)1, axis(2))
    || scatter BETAq TIME, mc(purple%70) yaxis(2) 
    || rcap LBv UBv TIME, color(black%50)
    || scatter BETAv TIME, ms(Dh) mc(midblue) xline(0, lcolor(black))
    yline(0, lcolor(black))
    ylabel(-4(1)4, labsize(medium)) ytitle("Rate per 100,000", size(medlarge))
    ytitle("Lockdown", axis(2) size(medlarge))
    legend(order(5 "Criminal Complaints" 6 "95% CI" 1 "COVID cases" 3 "Lockdown"
                 2 "95% CI") pos(6) rows(1));
    #delimit cr
    graph export "$GRA/eventJoint_close_X_`v'.pdf", replace
    #delimit ;
    twoway rarea LB UB TIME, color(gs10%50) 
    || scatter BETA TIME, mc(red%70)
    || rcap LBv UBv TIME, color(black%50)
    || scatter BETAv TIME, ms(Dh) mc(midblue) 
    ylabel(`yl') ytitle("Rate per 100,000")
    xline(0, lcolor(black)) yline(0, lcolor(black))
    legend(order(4 "Criminal Complaints" 3 "95% CI" 2 "COVID cases" 1 "95% CI")
           pos(6) rows(1));
    #delimit cr
    graph export "$GRA/eventJoint_close_X_`v'.pdf", replace
    drop BETAv UBv LBv 
}
drop LB UB TIME BETA LBq UBq BETAq lag* lead*




*-----------------------------------------------------------------------------
*-- (4b) Event Studies -- reopening
*-------------------------------------------------------------------------------
*time to open schools
bys comuna (week): egen minOpen = min(week) if SchoolOpen_i==1
bys comuna: egen openStart = min(minOpen)
gen timeToOpen = week-openStart

*keep only post close period
*drop if timeToClose<0

gen y2019    = year==2019
gen y2020    = year==2020


gen lead20 = timeToOpen<=-20
foreach num of numlist 19(-1)2 {
    gen lead`num' = timeToOpen == -`num'
}
foreach num of numlist 0(1)39 {
    gen lag`num' = timeToOpen == `num'

}
gen lag40 = timeToOpen>=40
areg caseRate lead* lag*, absorb(comuna) 
gen TIME = _n-21 in 1/60
gen BETA = .
gen LB   = .
gen UB   = .
local j=1
foreach l of numlist 20(-1)2 {
    qui replace BETA = _b[lead`l'] in `j'
    qui replace LB   = _b[lead`l']-1.96*_se[lead`l'] in `j'
    qui replace UB   = _b[lead`l']+1.96*_se[lead`l'] in `j'
    local ++j
}
qui replace BETA = 0 in `j'
qui replace LB   = 0 in `j'
qui replace UB   = 0 in `j'
local ++j
foreach l of numlist 0(1)40 {
    qui replace BETA = _b[lag`l'] in `j'
    qui replace LB   = _b[lag`l']-1.96*_se[lag`l'] in `j'
    qui replace UB   = _b[lag`l']+1.96*_se[lag`l'] in `j'
    local ++j
}
twoway rarea LB UB TIME, color(gs10%50) || scatter BETA TIME, mc(red%70)
graph export "$GRA/eventCOVID_open.pdf", replace

local varr rate rateSA rateV
local base w1-w51 
local leads 20
local cond if year>2016 [aw=populationyoung]

foreach v of local varr {
    if `"`v'"'=="rate"   local yl -4(1)4
    if `"`v'"'=="rateSA" local yl -3(1)3
    if `"`v'"'=="rateV"  local yl -1(1)2    
    *(2) week and comuna fe
    #delimit ;
    eventdd `v' i.comuna `base' `cond',
    timevar(timeToOpen) cluster(comuna) lags(40) leads(`leads') accum
    endpoints_op(ms(Dh) mc(midblue)) coef_op(ms(Dh) mc(midblue))
    graph_op(legend(off)
             xtitle("Weeks Relative to Schools Reopening", size(medlarge))
             ylabel(#10, format(%9.1f) labsize(medium))
             ytitle("Criminal Complaints per 100,000", size(medlarge)) 
             xlabel(-20 "{&le} -20" -15 "-15" -10 "-10" -5 "-5" 0 "0" 5 "5"
                    10 "10" 20 "20" 30 "30" 40 "{&ge} 40", labsize(medium)));
    graph export "$GRA/eventdd2_noControls_`v'.eps", replace;
    #delimit cr

    //Comparison event study
    gen BETAv = .
    gen LBv   = .
    gen UBv   = .
    local j=1
    foreach l of numlist 20(-1)2 {
        qui replace BETAv = _b[lead`l'] in `j'
        qui replace LBv   = _b[lead`l']-1.96*_se[lead`l'] in `j'
        qui replace UBv   = _b[lead`l']+1.96*_se[lead`l'] in `j'
        local ++j
    }
    qui replace BETAv = 0 in `j'
    qui replace LBv   = 0 in `j'
    qui replace UBv   = 0 in `j'
    local ++j
    foreach l of numlist 0(1)40 {
        qui replace BETAv = _b[lag`l'] in `j'
        qui replace LBv   = _b[lag`l']-1.96*_se[lag`l'] in `j'
        qui replace UBv   = _b[lag`l']+1.96*_se[lag`l'] in `j'
        local ++j
    }
    #delimit ;
    twoway rarea LB UB TIME, color(gs10%50) 
    || scatter BETA TIME, mc(red%70)
    || rcap LBv UBv TIME, color(black%50)
    || scatter BETAv TIME, ms(Dh) mc(midblue) 
    ylabel(`yl', labsize(medium)) ytitle("Rate per 100,000", size(medlarge))
    xline(-1, lcolor(black)) yline(0, lcolor(black%70) lpattern(solid))
    xlabel(-20 "{&le} -20" -15 "-15" -10 "-10" -5 "-5" 0 "0"
           5 "5" 10 "10" 20 "20" 30 "30" 40 "{&ge} 40", labsize(medium))
    xtitle("Weeks Relative to Schools Reopening", size(medlarge))
    legend(order(4 "Criminal Complaints" 3 "95% CI" 2 "COVID cases" 1 "95% CI")
           pos(1) ring(0) rows(1));
    #delimit cr
    graph export "$GRA/eventJoint_open_`v'.pdf", replace
    drop BETAv UBv LBv 
    //End
    
    *(3) week and comuna fe, quarantine control
    #delimit ;
    eventdd `v' i.comuna `base' quarantine `cond',
    timevar(timeToOpen) cluster(comuna) lags(40) leads(`leads') accum
    endpoints_op(ms(Dh) mc(midblue)) coef_op(ms(Dh) mc(midblue))
    graph_op(legend(off) ylabel(#10, format(%9.1f) labsize(medlarge))
             xtitle("Weeks Relative to Schools Reopening", size(medlarge))
             ytitle("Criminal Complaints per 100,000", size(medlarge)) 
             xlabel(-20 "{&le} -20" -15 "-15" -10 "-10" -5 "-5" 0 "0" 5 "5"
                    10 "10" 20 "20" 30 "30" 40 "{&ge} 40", labsize(medium)));
    graph export "$GRA/eventdd2_QuarantineControls_`v'.eps", replace;
    #delimit cr

    *(4) COVID controls, quarantine and epi controles
    local controls caseRate pcr positivity quarantine
    #delimit ;
    eventdd `v' i.comuna `base' `controls' `cond',
    timevar(timeToOpen) cluster(comuna) lags(40) leads(`leads') accum
    endpoints_op(ms(Dh) mc(midblue)) coef_op(ms(Dh) mc(midblue))
    graph_op(legend(off) ylabel(#10, format(%9.1f) labsize(medium))
             xtitle("Weeks Relative to Schools Reopening", size(medlarge))
             ytitle("Criminal Complaints per 100,000", size(medlarge)) 
             xlabel(-20 "{&le} -20" -15 "-15" -10 "-10" -5 "-5" 0 "0" 5 "5"
                    10 "10" 20 "20" 30 "30" 40 "{&ge} 40", labsize(medium)));
    graph export "$GRA/eventdd2_COVIDControls_`v'.eps", replace;
    #delimit cr		
    		
    *(5) COVID controls, quarantine control, mobility control
    local controls caseRate pcr positivity quarantine movilidad2 p_comunal_m p_comunal_h
    #delimit ;
    eventdd `v' i.comuna `base' `controls' `cond',
    timevar(timeToOpen) cluster(comuna) lags(40) leads(`leads') accum
    endpoints_op(ms(Dh) mc(midblue)) coef_op(ms(Dh) mc(midblue))
    graph_op(legend(off) ylabel(#10, format(%9.1f) labsize(medum))
             xtitle("Weeks Relative to Schools Reopening", size(medlarge))
             ytitle("Criminal Complaints per 100,000", size(medlarge)) 
             xlabel(-20 "{&le} -20" -15 "-15" -10 "-10" -5 "-5" 0 "0" 5 "5"
                    10 "10" 20 "20" 30 "30" 40 "{&ge} 40", labsize(medium)));
    graph export "$GRA/eventdd2_mobility_`v'.eps", replace;
    #delimit cr		
}
