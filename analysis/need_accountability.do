cd "/Users/amb/Desktop/analysis"

********************************************************************************
*** HEADER                                                                   ***
********************************************************************************
version 14.2

set more off, permanently
set scheme sj


********************************************************************************
*** MAIN, SOCIODEMOGRAPHICS                                                  ***
********************************************************************************
use "need_accountability_main_wide.dta", clear

gen income_quota = .
   replace income_quota = 0 if equivalent_household_net_income <  1100
   replace income_quota = 1 if equivalent_household_net_income >= 1100 & equivalent_household_net_income < 1500
   replace income_quota = 2 if equivalent_household_net_income >= 1500 & equivalent_household_net_income < 2000
   replace income_quota = 3 if equivalent_household_net_income >= 2000 & equivalent_household_net_income < 2600
   replace income_quota = 4 if equivalent_household_net_income >= 2600

la var income_quota "Equivalent Household Net Income"
label define income_quota_lb 0 "0 – 1100" 1 "1100 – 1500" 2 "1500 – 2000" 3 "2000 – 2600" 4 "> 2600"
   label values income_quota income_quota_lb

gen age_quota = .
   replace age_quota = 0 if age >= 18 & age <= 29
   replace age_quota = 1 if age >= 30 & age <= 39
   replace age_quota = 2 if age >= 40 & age <= 49
   replace age_quota = 3 if age >= 50 & age <= 59
   replace age_quota = 4 if age >= 60 & age <= 69

la var age_quota "Age"
label define age_quota_lb 0 "18 – 29" 1 "30 – 39" 2 "40 – 49" 3 "50 – 59" 4 "60 – 69"
   label values age_quota age_quota_lb

tab gender
tab income_quota
tab age_quota

bysort accountability: tab gender
bysort accountability: tab income_quota
bysort accountability: tab age_quota


/* correlations between socio-demografics and the share allocated to Person A */
foreach y in age equivalent_household_net_income gender {
   di "Correlations for variable `y'"
   forval x = 1/10 {
      pwcorr share_a_`x' `y', sig
   }
}

use "need_accountability_main_long.dta", clear


/* Kruskal-Wallis equality-of-populations rank tests */
forval scenario = 0/1 {
   if `scenario' == 0 {
      di "Need Scenario"
   }
   else {
      di "Productivity Scenario"
   }
   forval case = 1/5 {
      di "Case `case'"
      kwallis share_a if case == `case' & scenario == `scenario', by(accountability)
   }
}


/* regressions for interactions with Case 2 in the Need Scenario */
gen is_case_2 = 0
   replace is_case_2 = 1 if case == 2 & scenario == 0

la var is_case_2 "Case 2"

xtset id

xtreg share_a equivalent_household_net_income accountability gender age is_case_2, vce(robust)
xtreg share_a equivalent_household_net_income accountability gender c.age##is_case_2, vce(robust)


********************************************************************************
*** PILOT, ACCOUNTABILITY FIGURE                                             ***
********************************************************************************
use "need_accountability_pilot.dta", clear

mean judgment, over(scenario accountability)

bysort scenario: ttest judgment, by(accountability) level(90)

preserve
   collapse (mean) mean_judgment = judgment (sd) sd_judgment = judgment (count) n = judgment, by(scenario accountability)

   generate judgement_hi = mean_judgment + invttail(n / 2 - 1, 0.05) * (sd_judgment / sqrt(n / 2))
   generate judgement_lo = mean_judgment - invttail(n / 2 - 1, 0.05) * (sd_judgment / sqrt(n / 2))

   generate tnr = .
      replace tnr = 2.25 if scenario == 0 & accountability == 0
      replace tnr = 3.25 if scenario == 0 & accountability == 1
      replace tnr = 0    if scenario == 1 & accountability == 0
      replace tnr = 1    if scenario == 1 & accountability == 1

   twoway (bar mean_judgment tnr if accountability == 0, fcolor(gs10) lcolor(black) lwidth(medium) barwidth(0.9)) ///
          (bar mean_judgment tnr if accountability == 1, fcolor(white) lcolor(black) lwidth(medium) barwidth(0.9)) ///
          (rcap judgement_hi judgement_lo tnr, lcolor(black)) ///
          (pci  9.9 0    10.4 0, lcolor(black)) ///
          (pci 10.4 0    10.4 0.2, lcolor(black)) ///
          (pci  9.9 1    10.4 1, lcolor(black)) ///
          (pci 10.4 1    10.4 0.8, lcolor(black)) ///
  	      (pci  9.9 2.25 10.4 2.25, lcolor(black)) ///
          (pci 10.4 2.25 10.4 2.45, lcolor(black)) ///
          (pci  9.9 3.25 10.4 3.25, lcolor(black)) ///
          (pci 10.4 3.25 10.4 3.05, lcolor(black)), ///
          title("") ///
          xtitle("") ///
          xlabel(0 `" "Need" "Low Accountability" "' ///
                 1 `" "Need" "High Accountability" "' ///
                 2.25 `" "Productivity" "Low Accountability" "' ///
                 3.25 `" "Productivity" "High Accountability" "', ///
                 labsize(small)) ///
          ytitle("Mean Accountability Judgment") ///
          yscale(range(1 10.5)) ///
          ylabel(1 (1) 10) ///
          legend(off) ///
          text(10.4 0.5 "p {&le} 0.01", place(c)) ///
          text(10.4 2.75 "p {&le} 0.01", place(c)) ///
          graphregion(color(white))
   graph export "pilot_accountability.pdf", replace
restore


********************************************************************************
*** MAIN, ACCOUNTABILITY FIGURE                                              ***
********************************************************************************
use "need_accountability_main_wide", clear

preserve
   keep id accountability_need accountability_productivity accountability

   rename accountability_need judgment0
   rename accountability_productivity judgment1

   reshape long judgment, i(id) j(frame)

   bysort frame: ttest judgment, by(accountability) level(90)

   collapse (mean) meanj = judgment (sd) sdj = judgment (count) n = judgment, by(frame accountability)

   generate hi_j = meanj + invttail(n / 2 - 1, 0.05) * (sdj / sqrt(n / 2))
   generate low_j = meanj - invttail(n / 2 - 1, 0.05) * (sdj / sqrt(n / 2))

   generate tnr = .
      replace tnr = 0 if frame == 0 & accountability == 0
      replace tnr = 1 if frame == 0 & accountability == 1
      replace tnr = 2.25 if frame == 1 & accountability == 0
      replace tnr = 3.25 if frame == 1 & accountability == 1

   local pposition = 6.5
   local pposition1 = `pposition' - 0.5
   local ylim = `pposition' + 0.25

   twoway (bar meanj tnr if accountability == 0, fcolor(gs10) lcolor(black) lwidth(medium) barwidth(0.9)) ///
          (bar meanj tnr if accountability == 1, fcolor(gs4) lcolor(black) lwidth(medium) barwidth(0.9)) ///
          (rcap hi_j low_j tnr, lcolor(black)) ///
          (pci `pposition1' 0 `pposition' 0, lcolor(black)) ///
          (pci `pposition'  0 `pposition' 0.2, lcolor(black)) ///
          (pci `pposition1' 1 `pposition' 1, lcolor(black)) ///
          (pci `pposition'  1 `pposition' 0.8, lcolor(black)) ///
          (pci `pposition1' 2.25 `pposition' 2.25, lcolor(black)) ///
          (pci `pposition'  2.25 `pposition' 2.45, lcolor(black)) ///
          (pci `pposition1' 3.25 `pposition' 3.25, lcolor(black)) ///
          (pci `pposition'  3.25 `pposition' 3.05, lcolor(black)), ///
          title("") ///
          xtitle("") ///
          xlabel(0 `" "Need" "Low Accountability" "' ///
                 1 `" "Need" "High Accountability" "' ///
                 2.25 `" "Productivity" "Low Accountability" "' ///
                 3.25 `" "Productivity" "High Accountability" "', ///
                 labsize(small)) ///
          ytitle("Mean Accountability Judgment") ///
          yscale(range(1 `ylim')) ///
          ylabel(1 (1) `pposition') ///
          legend(off) ///
          text(`pposition' 0.5 "p {&le} 0.01", place(c)) ///
          text(`pposition' 2.75 "p {&le} 0.01", place(c)) ///
          graphregion(color(white))
   graph export "main_accountability.pdf", replace
restore


********************************************************************************
*** MAIN, MEANS                                                              ***
********************************************************************************
use "need_accountability_main_long", clear

gen deviation_a = .
   replace deviation_a = (share_a - 0.5) / (share_need_a - 0.5) if scenario == 0
   replace deviation_a = (0.5 - share_a) / (0.5 - share_productivity_a) if scenario == 1

table case accountability scenario, contents(mean share_a semean share_a) row format(%9.3f)
by accountability scenario, sort: ttest share_a == share_productivity_a, level(90)
by accountability scenario, sort: ttest share_a == share_need_a, level(90)
by scenario accountability case, sort: ttest share_a == share_productivity_a, level(90)
by scenario accountability case, sort: ttest share_a == share_need_a, level(90)
by scenario, sort: ttest share_a, by(accountability) unequal welch level(90)
by scenario case, sort: ttest share_a, by(accountability) unequal welch level(90)

preserve
   by accountability scenario, sort: tabstat deviation_a if deviation_a < 0 | deviation_a > 1, statistics(count)

   drop if deviation_a < 0 | deviation_a > 1

   table case accountability scenario, contents(mean deviation_a semean deviation_a) row format(%9.3f)
   by scenario, sort: ttest deviation_a, by(accountability) unequal welch level(90)
   by scenario case, sort: ttest deviation_a, by(accountability) unequal welch level(90)
   table case accountability scenario, contents(freq) row
restore


********************************************************************************
*** MAIN, SHARE FIGURE                                                       ***
********************************************************************************
use "need_accountability_main_long.dta", clear

mean share_need_a share_productivity_a, over(scenario accountability)

preserve
   collapse (mean) meanj = share_a (sd) sdj = share_a (count) n = share_a, by(scenario accountability)

   generate hi_j = meanj + invttail(n - 1, 0.05) * (sdj / sqrt(n))
   generate low_j = meanj - invttail(n - 1, 0.05) * (sdj / sqrt(n))

   generate tnr = .
      replace tnr = 0 if scenario == 0 & accountability == 0
      replace tnr = 1 if scenario == 0 & accountability == 1
      replace tnr = 2.25 if scenario == 1 & accountability == 0
      replace tnr = 3.25 if scenario == 1 & accountability == 1

   twoway (bar meanj tnr if accountability == 0, fcolor(gs10) lcolor(black) lwidth(medium) barwidth(0.9)) ///
          (bar meanj tnr if accountability == 1, fcolor(gs4) lcolor(black) lwidth(medium) barwidth(0.9)) ///
          (rcap hi_j low_j tnr, lcolor(black)) ///
          (pci 0.717 -0.45 0.717 0.45, lcolor(black)) ///
          (pci 0.717  0.55 0.717 1.45, lcolor(black)) ///
          (pci 0.5    1.80 0.5   2.70, lcolor(black)) ///
          (pci 0.5    2.80 0.5   3.70, lcolor(black)) ///
          (pci 0.5   -0.45 0.5   0.45, lpattern(dash) lcolor(black)) ///
          (pci 0.5    0.55 0.5   1.45, lpattern(dash) lcolor(black)) ///
          (pci 0.283  1.80 0.283 2.70, lpattern(dash) lcolor(black)) ///
          (pci 0.283  2.80 0.283 3.70, lpattern(dash) lcolor(black)), ///
          title("") ///
          xtitle("") ///
          xlabel(0 `" "Need" "Low Accountability" "' ///
                 1 `" "Need" "High Accountability" "' ///
                 2.25 `" "Productivity" "Low Accountability" "' ///
                 3.25 `" "Productivity" "High Accountability" "', ///
                 labsize(small)) ///
          ytitle("logshare{subscript:{it:A}}") ///
          yscale(range(0 (0.1) 0.7)) ///
          ylabel(0 (0.1) 0.7) ///
          legend(off) ///
          graphregion(color(white))
   graph export "main_mean_share.pdf", replace
restore


********************************************************************************
*** MAIN, DEVIATION FIGURE                                                   ***
********************************************************************************
use "need_accountability_main_long.dta", clear

gen deviation_a = .
   replace deviation_a = (share_a - 0.5) / (share_need_a - 0.5) if scenario == 0
   replace deviation_a = (0.5 - share_a) / (0.5 - share_productivity_a) if scenario == 1

gen devdummy = 0
   replace devdummy = 1 if deviation < 0 | deviation > 1

by scenario case, sort: tabulate devdummy accountability, chi2

preserve
   drop if deviation < 0 | deviation > 1

   collapse (mean) meanj = deviation_a (sd) sdj = deviation_a (count) n = deviation_a, by(scenario accountability)

   generate hi_j = meanj + invttail(n - 1, 0.05) * (sdj / sqrt(n))
   generate low_j = meanj - invttail(n - 1, 0.05) * (sdj / sqrt(n))

   generate tnr = .
      replace tnr = 0 if scenario == 0 & accountability == 0
      replace tnr = 1 if scenario == 0 & accountability == 1
      replace tnr = 2.25 if scenario == 1 & accountability == 0
      replace tnr = 3.25 if scenario == 1 & accountability == 1

   twoway (rcap hi_j low_j tnr, lcolor(black)) ///
          (scatter meanj tnr if accountability == 0, msymbol(square) mfcolor(gs10) mlcolor(black) msize(large)) ///
          (scatter meanj tnr if accountability == 1, msymbol(diamond) mfcolor(gs4) mlcolor(black) msize(large)), ///
          title("") ///
          xtitle("") ///
          xscale(range(-0.5 3.75)) ///
          xlabel(0 `" "Need" "Low Accountability" "' ///
                 1 `" "Need" "High Accountability" "' ///
                 2.25 `" "Productivity" "Low Accountability" "' ///
                 3.25 `" "Productivity" "High Accountability" "', ///
                 labsize(small)) ///
          ytitle("deviation{subscript:{it:A}}") ///
          yscale(range(0 (0.1) 1.0)) ///
          ylabel(0 (0.1) 1.0) ///
          legend(off) ///
          graphregion(color(white))
   graph export "main_mean_deviation.pdf", replace
restore


********************************************************************************
*** MAIN, MEAN SHARE NEED FIGURE                                             ***
********************************************************************************
use "need_accountability_main_long.dta", clear

local ymin = 0.3
local ymax = 0.9
local pposition = 0.68
local p1 = "p = 0.230"
local xtitle = "Case (Need Scenario)"

preserve
   keep if scenario == 0

   replace case = mod(case - 1, 5) + 1

   collapse (mean) meanshare = share_a (sd) sdj = share_a (count) n = share_a, by(case accountability)

   generate hi_j = meanshare + invttail(n - 1, 0.05) * (sdj / sqrt(n))
   generate low_j = meanshare - invttail(n - 1, 0.05) * (sdj / sqrt(n))

   generate rescase = (case - 1) * 2 + case if accountability == 0
      replace rescase = (case - 1) * 2 + case + 1 if accountability == 1

   twoway (bar meanshare rescase if accountability == 0, fcolor(gs10) lcolor(black) lwidth(medium)) ///
          (bar meanshare rescase if accountability == 1, fcolor(gs4) lcolor(black) lwidth(medium)) ///
          (rcap hi_j low_j rescase, lcolor(black)) ///
          (pci 0.5   0.5 0.5   2.5, lpattern(dash) lcolor(black)) ///
          (pci 0.5   3.5 0.5   5.5, lpattern(dash) lcolor(black)) ///
          (pci 0.5   6.5 0.5   8.5, lpattern(dash) lcolor(black)) ///
          (pci 0.5   9.5 0.5  11.5, lpattern(dash) lcolor(black)) ///
          (pci 0.5  12.5 0.5  14.5, lpattern(dash) lcolor(black)) ///
          (pci 0.6   0.5 0.6   2.5, lpattern(solid) lcolor(black)) ///
          (pci 0.64  3.5 0.64  5.5, lpattern(solid) lcolor(black)) ///
          (pci 0.71  6.5 0.71  8.5, lpattern(solid) lcolor(black)) ///
          (pci 0.78  9.5 0.78 11.5, lpattern(solid) lcolor(black)) ///
          (pci 0.86 12.5 0.86 14.5, lpattern(solid) lcolor(black)), ///
          xtitle("`xtitle'") ///
          xlabel(1.5 "1" 4.5 "2" 7.5 "3" 10.5 "4" 13.5 "5") ///
          ytitle("logshare{subscript:{it:A}}") ///
          yscale(range(`ymin' (0.1) `ymax')) ///
          ylabel(`ymin' (0.1) `ymax') ///
          legend(off) ///
          text(0.9  1.5 "DD", place(c)) ///
          text(0.9  4.5 "DS", place(c)) ///
          text(0.9  7.5 "XS", place(c)) ///
          text(0.9 10.5 "SS", place(c)) ///
          text(0.9 13.5 "SS", place(c)) ///
          graphregion(color(white))
   graph export "main_share_need.pdf", replace
restore

by scenario case, sort: ttest share_a, by(accountability) unequal welch level(90)


********************************************************************************
*** MAIN, MEAN SHARE PRODUCTIVITY FIGURE                                     ***
********************************************************************************
use "need_accountability_main_long.dta", clear

local ymin = 0.0
local ymax = 0.6
local pposition = 0.53
local p1 = "p {&le} 0.01"
local xtitle = "Case (Productivity Scenario)"

preserve
   keep if scenario == 1

   replace case = mod(case - 1, 5) + 1

   collapse (mean) meanshare = share_a (sd) sdj = share_a (count) n = share_a, by(case accountability)

   generate hi_j = meanshare + invttail(n - 1, 0.05) * (sdj / sqrt(n))
   generate low_j = meanshare - invttail(n - 1, 0.05) * (sdj / sqrt(n))

   generate rescase = (case - 1) * 2 + case if accountability == 0
      replace rescase = (case - 1) * 2 + case + 1 if accountability == 1

   twoway (bar meanshare rescase if accountability == 0, fcolor(gs10) lcolor(black) lwidth(medium)) ///
          (bar meanshare rescase if accountability == 1, fcolor(gs4) lcolor(black) lwidth(medium)) ///
          (rcap hi_j low_j rescase, lcolor(black)) ///
          (pci 0.5   0.5 0.5   2.5, lpattern(solid) lcolor(black)) ///
          (pci 0.5   3.5 0.5   5.5, lpattern(solid) lcolor(black)) ///
          (pci 0.5   6.5 0.5   8.5, lpattern(solid) lcolor(black)) ///
          (pci 0.5   9.5 0.5  11.5, lpattern(solid) lcolor(black)) ///
          (pci 0.5  12.5 0.5  14.5, lpattern(solid) lcolor(black)) ///
          (pci 0.4   0.5 0.4   2.5, lpattern(dash) lcolor(black)) ///
          (pci 0.36  3.5 0.36  5.5, lpattern(dash) lcolor(black)) ///
          (pci 0.29  6.5 0.29  8.5, lpattern(dash) lcolor(black)) ///
          (pci 0.22  9.5 0.22 11.5, lpattern(dash) lcolor(black)) ///
          (pci 0.14 12.5 0.14 14.5, lpattern(dash) lcolor(black)), ///
          xtitle("`xtitle'") ///
          xlabel(1.5 "1" 4.5 "2" 7.5 "3" 10.5 "4" 13.5 "5") ///
          ytitle("logshare{subscript:{it:A}}") ///
          yscale(range(`ymin' (0.1) `ymax')) ///
          ylabel(`ymin' (0.1) `ymax') ///
          legend(off) ///
          text(0.6 1.5  "SS", place(c)) ///
          text(0.6 4.5  "SD", place(c)) ///
          text(0.6 7.5  "DX", place(c)) ///
          text(0.6 10.5 "DD", place(c)) ///
          text(0.6 13.5 "DD", place(c)) ///
          graphregion(color(white))
   graph export "main_share_productivity.pdf", replace
restore

by scenario case, sort: ttest share_a, by(accountability) unequal welch level(90)


********************************************************************************
*** MAIN, MEAN DEVIATION NEED FIGURE                                         ***
********************************************************************************
use "need_accountability_main_long.dta", clear

local ymin = 0
local ymax = 1
local xtitle = "Case (Need Scenario)"

gen deviation_a = .
   replace deviation_a = (share_a - 0.5) / (share_need_a - 0.5) if scenario == 0
   replace deviation_a = (0.5 - share_a) / (0.5 - share_productivity_a) if scenario == 1

preserve
   drop if deviation < 0 | deviation > 1

   keep if scenario == 0

   replace case = mod(case - 1,5) + 1

   collapse (mean) meanj = deviation_a (sd) sdj = deviation_a (count) n = deviation_a, by(case accountability)

   generate hi_j = meanj + invttail(n - 1, 0.05) * (sdj / sqrt(n))
   generate low_j = meanj - invttail(n - 1, 0.05) * (sdj / sqrt(n))

   generate rescase = (case - 1) * 2 + case if accountability == 0
      replace rescase = (case - 1) * 2 + case + 1 if accountability == 1

   twoway (rcap hi_j low_j rescase, lcolor(black)) ///
          (scatter meanj rescase if accountability == 0, msymbol(square) mfcolor(gs10) mlcolor(black) msize(large)) ///
          (scatter meanj rescase if accountability == 1, msymbol(diamond) mfcolor(gs4) mlcolor(black) msize(large)), ///
          xtitle("`xtitle'") ///
          xscale(range(0.5 14.5)) ///
          xlabel(1.5 "1" 4.5 "2" 7.5 "3" 10.5 "4" 13.5 "5") ///
          ytitle("deviation{subscript:{it:A}}") ///
          yscale(range(`ymin' (0.1) `ymax')) ///
          ylabel(`ymin' (0.1) `ymax') ///
          legend(off) ///
          text(1 1.5  "DD", place(c)) ///
          text(1 4.5  "DS", place(c)) ///
          text(1 7.5  "XS", place(c)) ///
          text(1 10.5 "SS", place(c)) ///
          text(1 13.5 "SS", place(c)) ///
          graphregion(color(white))
   graph export "main_deviation_need.pdf", replace
restore


********************************************************************************
*** MAIN, MEAN DEVIATION PRODUCTIVITY FIGURE                                 ***
********************************************************************************
use "need_accountability_main_long.dta", clear

local ymin = 0
local ymax = 1
local xtitle = "Case (Productivity Scenario)"

gen deviation_a = .
   replace deviation_a = (share_a - 0.5) / (share_need_a - 0.5) if scenario == 0
   replace deviation_a = (0.5 - share_a) / (0.5 - share_productivity_a) if scenario == 1

preserve
   drop if deviation < 0 | deviation > 1

   keep if scenario == 1

   replace case = mod(case - 1, 5) + 1

   collapse (mean) meanj = deviation_a (sd) sdj = deviation_a (count) n = deviation_a, by(case accountability)

   generate hi_j = meanj + invttail(n - 1, 0.05) * (sdj / sqrt(n))
   generate low_j = meanj - invttail(n - 1, 0.05) * (sdj / sqrt(n))

   generate rescase = (case - 1) * 2 + case if accountability == 0
      replace rescase = (case - 1) * 2 + case + 1 if accountability == 1

   twoway (rcap hi_j low_j rescase, lcolor(black)) ///
          (scatter meanj rescase if accountability == 0, msymbol(square) mfcolor(gs10) mlcolor(black) msize(large)) ///
          (scatter meanj rescase if accountability == 1, msymbol(diamond) mfcolor(gs4) mlcolor(black) msize(large)), ///
          xtitle("`xtitle'") ///
          xscale(range(0.5 14.5)) ///
          xlabel(1.5 "1" 4.5 "2" 7.5 "3" 10.5 "4" 13.5 "5") ///
          ytitle("deviation{subscript:{it:A}}") ///
          yscale(range(`ymin' (0.1) `ymax')) ///
          ylabel(`ymin' (0.1) `ymax') ///
          legend(off) ///
          text(1 1.5  "SS", place(c)) ///
          text(1 4.5  "SD", place(c)) ///
          text(1 7.5  "DX", place(c)) ///
          text(1 10.5 "DD", place(c)) ///
          text(1 13.5 "DD", place(c)) ///
          graphregion(color(white))
   graph export "main_deviation_productivity.pdf", replace
restore

by scenario case, sort: ttest deviation_a, by(accountability) unequal welch level(90)


********************************************************************************
*** MAIN, REGRESSIONS AND MARGINAL EFFECTS                                   ***
********************************************************************************
use "need_accountability_main_long.dta", clear

gen deviation_a = .
   replace deviation_a = (share_a - share_productivity) / (share_need_a - share_productivity_a) if scenario == 0
   replace deviation_a = (share_need - share_a) / (share_need_a - share_productivity_a) if scenario == 1

gen accountjudg = .
   replace accountjudg = accountability_need if scenario == 0
   replace accountjudg = accountability_productivity if scenario == 1

xtset id


/* randomization of scenario and case */
tabulate pos block_order, chi2


/* pooled over cases, share */
xtreg share_a i.scenario i.block_order#i.pos, re vce(robust) level(90)
   eststo share_sce

xtreg share_a i.accountability i.block_order#i.pos, re vce(robust) level(90)
   eststo share_acc

xtreg share_a i.scenario##i.accountability i.block_order#i.pos, re vce(robust) level(90)
   eststo share_acc_sce

xtreg share_a i.scenario##i.accountability ///
   age ///
   i.gender ///
   equivalent_household_net_income ///
   i.smoker ///
   i.cardiovascular_disease ///
   i.metabolic_disease ///
   locus_of_control ///
   political_attitude ///
   criteria_likert_need ///
   criteria_likert_productivity ///
   criteria_likert_equality ///
   accountjudg ///
   i.block_order#i.pos, ///
   re vce(robust) level(90)
   eststo share_acc_sce_c

margins i.scenario, dydx(i.accountability) level(90) saving(main_account_share, replace)
margins i.scenario, dydx(i.accountability) level(90) pwcompare(effects)
margins i.scenario#i.accountability, level(90)

preserve
   clear
   use main_account_share

   twoway (rcap _ci_ub _ci_lb _m1, lcolor(black)) ///
          (scatter _margin _m1 if _m1 == 0, msymbol(circle) mfcolor(gs10) mlcolor(black) msize(large)) ///
          (scatter _margin _m1 if _m1 == 1, msymbol(triangle) mfcolor(gs4) mlcolor(black) msize(large)), ///
          xtitle("") ///
          xscale(range(-0.25 1.25)) ///
          xlabel(0 `" "Need" "Scenario" "' ///
                 1 `" "Productivity" "Scenario" "', ///
                 labsize(small)) ///
          ytitle("{&Delta}logshare{subscript:{it:A}}") ///
          yline(0, lcolor(black) lpattern(dash)) ///
          legend(off) ///
          text(-0.01 0.5 "Marginal Effect of High Accountability", place(c)) ///
          graphregion(color(white))
   graph export "main_marg_share.pdf", replace
restore


/* by cases, share */
xtreg share_a i.case i.block_order#i.pos, re vce(robust) level(90)
   eststo share_dif

xtreg share_a i.scenario##i.accountability##i.case i.block_order#i.pos, re vce(robust) level(90)
   eststo share_acc_sce_dif

xtreg share_a i.scenario##i.accountability##i.case ///
   age ///
   i.gender ///
   equivalent_household_net_income ///
   i.smoker ///
   i.cardiovascular_disease ///
   i.metabolic_disease ///
   locus_of_control ///
   political_attitude ///
   criteria_likert_need ///
   criteria_likert_productivity ///
   criteria_likert_equality ///
   accountjudg ///
   i.block_order#i.pos, ///
   re vce(robust) level(90)
   eststo share_acc_sce_dif_c

margins i.scenario, dydx(i.accountability) at(case = 1) at(case = 2) at(case = 3) at(case = 4) at(case = 5) level(90) saving(main_account_share_case, replace)
margins i.scenario, dydx(i.accountability) at(case = 1) level(90) pwcompare(effects)
margins i.scenario, dydx(i.accountability) at(case = 2) level(90) pwcompare(effects)
margins i.scenario, dydx(i.accountability) at(case = 3) level(90) pwcompare(effects)
margins i.scenario, dydx(i.accountability) at(case = 4) level(90) pwcompare(effects)
margins i.scenario, dydx(i.accountability) at(case = 5) level(90) pwcompare(effects)
margins i.scenario#i.accountability, at(case = 1) at(case = 2) at(case = 3) at(case = 4) at(case = 5) level(90)

margins i.case#i.accountability, dydx(scenario) level(90) saving(main_scenario_share, replace)
margins i.accountability, dydx(i.scenario) level(90)
margins i.accountability, dydx(i.scenario) level(90) pwcompare(effects)
margins i.accountability, dydx(i.scenario) at(case = 1) level(90) pwcompare(effects)
margins i.accountability, dydx(i.scenario) at(case = 2) level(90) pwcompare(effects)
margins i.accountability, dydx(i.scenario) at(case = 3) level(90) pwcompare(effects)
margins i.accountability, dydx(i.scenario) at(case = 4) level(90) pwcompare(effects)
margins i.accountability, dydx(i.scenario) at(case = 5) level(90) pwcompare(effects)

margins if accountability == 0, dydx(scenario) at(case = 1) at(case = 2) at(case = 3) at(case = 4) at(case = 5) pwcompare(effects) mcompare(bonferroni) level(90)
margins if accountability == 1, dydx(scenario) at(case = 1) at(case = 2) at(case = 3) at(case = 4) at(case = 5) pwcompare(effects) mcompare(bonferroni) level(90)
margins if scenario == 0, dydx(accountability) at(case = 1) at(case = 2) at(case = 3) at(case = 4) at(case = 5) pwcompare(effects) mcompare(bonferroni) level(90)
margins if scenario == 1, dydx(accountability) at(case = 1) at(case = 2) at(case = 3) at(case = 4) at(case = 5) pwcompare(effects) mcompare(bonferroni) level(90)

margins i.scenario#i.accountability#i.case, post
scalar sn1 = 2 * (0.5 - _b[0.scenario#0.accountability#1.case])
scalar sn2 = 2 * (0.5 - _b[0.scenario#0.accountability#2.case])
scalar sn3 = 2 * (0.5 - _b[0.scenario#0.accountability#3.case])
scalar sn4 = 2 * (0.5 - _b[0.scenario#0.accountability#4.case])
scalar sn5 = 2 * (0.5 - _b[0.scenario#0.accountability#5.case])
scalar sp1 = 2 * (0.5 - _b[0.scenario#1.accountability#1.case])
scalar sp2 = 2 * (0.5 - _b[0.scenario#1.accountability#2.case])
scalar sp3 = 2 * (0.5 - _b[0.scenario#1.accountability#3.case])
scalar sp4 = 2 * (0.5 - _b[0.scenario#1.accountability#4.case])
scalar sp5 = 2 * (0.5 - _b[0.scenario#1.accountability#5.case])
display sn1 sn2 sn3 sn4 sn5 sp1 sp2 sp3 sp4 sp5
display _b[0.scenario#0.accountability#1.case]
display _se[0.scenario#0.accountability#1.case]
display _b[0.scenario#0.accountability#2.case]
display _se[0.scenario#0.accountability#2.case]
display _b[0.scenario#0.accountability#3.case]
display _se[0.scenario#0.accountability#3.case]
display _b[0.scenario#0.accountability#4.case]
display _se[0.scenario#0.accountability#4.case]
display _b[0.scenario#0.accountability#5.case]
display _se[0.scenario#0.accountability#5.case]
display _b[1.scenario#0.accountability#1.case]
display _se[1.scenario#0.accountability#1.case]
display _b[1.scenario#0.accountability#2.case]
display _se[1.scenario#0.accountability#2.case]
display _b[1.scenario#0.accountability#3.case]
display _se[1.scenario#0.accountability#3.case]
display _b[1.scenario#0.accountability#4.case]
display _se[1.scenario#0.accountability#4.case]
display _b[1.scenario#0.accountability#5.case]
display _se[1.scenario#0.accountability#5.case]
test (0.scenario#0.accountability#1.case) = 1 - (1.scenario#0.accountability#1.case)
test (0.scenario#0.accountability#2.case) = 1 - (1.scenario#0.accountability#2.case)
test (0.scenario#0.accountability#3.case) = 1 - (1.scenario#0.accountability#3.case)
test (0.scenario#0.accountability#4.case) = 1 - (1.scenario#0.accountability#4.case)
test (0.scenario#0.accountability#5.case) = 1 - (1.scenario#0.accountability#5.case)
display _b[0.scenario#1.accountability#1.case]
display _se[0.scenario#1.accountability#1.case]
display _b[0.scenario#1.accountability#2.case]
display _se[0.scenario#1.accountability#2.case]
display _b[0.scenario#1.accountability#3.case]
display _se[0.scenario#1.accountability#3.case]
display _b[0.scenario#1.accountability#4.case]
display _se[0.scenario#1.accountability#4.case]
display _b[0.scenario#1.accountability#5.case]
display _se[0.scenario#1.accountability#5.case]
display _b[1.scenario#1.accountability#1.case]
display _se[1.scenario#1.accountability#1.case]
display _b[1.scenario#1.accountability#2.case]
display _se[1.scenario#1.accountability#2.case]
display _b[1.scenario#1.accountability#3.case]
display _se[1.scenario#1.accountability#3.case]
display _b[1.scenario#1.accountability#4.case]
display _se[1.scenario#1.accountability#4.case]
display _b[1.scenario#1.accountability#5.case]
display _se[1.scenario#1.accountability#5.case]
test (0.scenario#1.accountability#1.case) = 1 - (1.scenario#1.accountability#1.case)
test (0.scenario#1.accountability#2.case) = 1 - (1.scenario#1.accountability#2.case)
test (0.scenario#1.accountability#3.case) = 1 - (1.scenario#1.accountability#3.case)
test (0.scenario#1.accountability#4.case) = 1 - (1.scenario#1.accountability#4.case)
test (0.scenario#1.accountability#5.case) = 1 - (1.scenario#1.accountability#5.case)

estimates restore share_acc_sce_dif_c
margins i.scenario#i.accountability, post
display _b[0.scenario#0.accountability]
display _se[0.scenario#0.accountability]
display _b[1.scenario#0.accountability]
display _se[1.scenario#0.accountability]
test (0.scenario#0.accountability) = 1 - (1.scenario#0.accountability)
display _b[0.scenario#1.accountability]
display _se[0.scenario#1.accountability]
display _b[1.scenario#1.accountability]
display _se[1.scenario#1.accountability]
test (0.scenario#1.accountability) = 1 - (1.scenario#1.accountability)

preserve
   clear

   use main_account_share_case

   replace _at = _at + 0.2 if _m1 == 1

   twoway (rcap _ci_ub _ci_lb _at, lcolor(black)) ///
          (scatter _margin _at if _m1 == 0, msymbol(circle) mfcolor(gs10) mlcolor(black) msize(large)) ///
          (scatter _margin _at if _m1 == 1, msymbol(triangle) mfcolor(gs4) mlcolor(black) msize(large)), ///
          xtitle("Case") ///
          xscale(range(0.75 5.5)) ///
          xlabel(1.1 "1" 2.1 "2" 3.1 "3" 4.1 "4" 5.1 "5") ///
          ytitle("{&Delta}logshare{subscript:{it:A}}") ///
          yline(0, lcolor(black) lpattern(dash)) ///
          legend(order(2 "Need Scenario" 3 "Productivity Scenario") cols(1) ring(0) bplacement(swest)) ///
          text(0.015 4.3 "Marginal Effect of High Accountability", place(c)) ///
          graphregion(color(white))
   graph export "main_marg_share_case.pdf", replace
restore

preserve
   clear

   use main_scenario_share

   replace _m1 = _m1 + 0.2 if _m2 == 1

   twoway (rcap _ci_ub _ci_lb _m1, lcolor(black)) ///
          (scatter _margin _m1 if _m2 == 0, msymbol(circle) mfcolor(gs10) mlcolor(black) msize(large)) ///
          (scatter _margin _m1 if _m2 == 1, msymbol(triangle) mfcolor(gs4) mlcolor(black) msize(large)) ///
          (pci `=sn1' 0.9 `=sn1' 1.1, lcolor(black) lwidth(thick)) ///
          (pci `=sn2' 1.9 `=sn2' 2.1, lcolor(black) lwidth(thick)) ///
          (pci `=sn3' 2.9 `=sn3' 3.1, lcolor(black) lwidth(thick)) ///
          (pci `=sn4' 3.9 `=sn4' 4.1, lcolor(black) lwidth(thick)) ///
          (pci `=sn5' 4.9 `=sn5' 5.1, lcolor(black) lwidth(thick)) ///
          (pci `=sp1' 1.1 `=sp1' 1.3, lcolor(black) lwidth(thick)) ///
          (pci `=sp2' 2.1 `=sp2' 2.3, lcolor(black) lwidth(thick)) ///
          (pci `=sp3' 3.1 `=sp3' 3.3, lcolor(black) lwidth(thick)) ///
          (pci `=sp4' 4.1 `=sp4' 4.3, lcolor(black) lwidth(thick)) ///
          (pci `=sp5' 5.1 `=sp5' 5.3, lcolor(black) lwidth(thick)), ///
          xtitle("Case") ///
          xscale(range(0.75 5.5)) ///
          xlabel(1.1 "1" 2.1 "2" 3.1 "3" 4.1 "4" 5.1 "5") ///
          ytitle("{&Delta}logshare{subscript:{it:A}}") ///
          yscale(range(-0.2 (0.05) 0)) ///
          yline(0, lcolor(black) lpattern(dash)) ///
          legend(order(2 "Low Accountability" 3 "High Accountability") cols(1) ring(0) bplacement(swest)) ///
          text(-0.015 4.3 "Marginal Effect of Productivity Scenario", place(c)) ///
          graphregion(color(white))
   graph export "main_marg_share_scenario.pdf", replace
restore

esttab share_sce share_acc share_acc_sce share_acc_sce_c share_dif share_acc_sce_dif share_acc_sce_dif_c using main_gls_share.tex, label se scalars(chi2 r2_w r2_b r2_o) star(* 0.10 ** 0.05 *** 0.01) replace


/* pooled over cases, deviation */
preserve
   replace accountability = 2 if accountability == 1 & scenario == 1
   replace accountability = 1 if accountability == 0 & scenario == 1
   replace accountability = 0 if accountability == 2 & scenario == 1

   drop if deviation < 0 | deviation > 1

   xttobit deviation_a i.scenario i.block_order#i.pos, re ll(0) ul(1) vce(oim) level(90)
      eststo dev_sce

   xttobit deviation_a i.accountability i.block_order#i.pos, re ll(0) ul(1) vce(oim) level(90)
      eststo dev_acc

   xttobit deviation_a i.accountability##i.scenario i.block_order#i.pos, re ll(0) ul(1) vce(oim) level(90)
      eststo dev_acc_sce

   xttobit deviation_a i.accountability##i.scenario i.block_order#i.pos ///
      age ///
      i.gender ///
      equivalent_household_net_income ///
      i.smoker ///
      i.cardiovascular_disease ///
      i.metabolic_disease ///
      locus_of_control ///
      political_attitude ///
      criteria_likert_need ///
      criteria_likert_productivity ///
      criteria_likert_equality ///
      accountjudg, ///
      re ll(0) ul(1) vce(oim) level(90)
      eststo dev_acc_sce_c

   margins i.scenario, dydx(i.accountability) level(90) saving(main_account_dev, replace)
   margins i.scenario, dydx(i.accountability) level(90) pwcompare(effects)

   clear
   use main_account_dev

   twoway (rcap _ci_ub _ci_lb _m1, lcolor(black)) ///
          (scatter _margin _m1 if _m1 == 0, msymbol(circle) mfcolor(gs10) mlcolor(black) msize(large)) ///
          (scatter _margin _m1 if _m1 == 1, msymbol(triangle) mfcolor(gs4) mlcolor(black) msize(large)), ///
          xtitle("") ///
          xscale(range(-0.25 1.25)) ///
          ytitle("- 1{stSymbol:*}|{&Delta}deviation{subscript:{it:A}}|") ///
          yscale(range(0 -0.8)) ///
          ylabel(-0.8 (0.2) 0) ///
          yline(0, lcolor(black) lpattern(dash)) ///
          xlabel(0 `" "Need" "Scenario" "' 1 `" "Productivity" "Scenario" "', labsize(small)) ///
          legend(off) ///
          text(-0.1 0.5 "Marginal Effect of High Accountability", place(c)) ///
          graphregion(color(white))
   graph export "main_marg_deviation.pdf", replace
restore


/* by cases, deviation */
preserve
   replace accountability = 2 if accountability == 1 & scenario == 1
   replace accountability = 1 if accountability == 0 & scenario == 1
   replace accountability = 0 if accountability == 2 & scenario == 1

   drop if deviation < 0 | deviation > 1

   xttobit deviation_a i.case, re ll(0) ul(1) vce(oim) level(90)
      eststo dev_dif

   xttobit deviation_a i.accountability##i.scenario##i.case i.block_order#i.pos, re ll(0) ul(1) vce(oim) level(90)
      eststo dev_acc_sce_dif

   xttobit deviation_a i.accountability##i.scenario##i.case i.block_order#i.pos ///
      age ///
      i.gender ///
      equivalent_household_net_income ///
      i.smoker ///
      i.cardiovascular_disease ///
      i.metabolic_disease ///
      locus_of_control ///
      political_attitude ///
      criteria_likert_need ///
      criteria_likert_productivity ///
      criteria_likert_equality ///
      accountjudg, ///
      re ll(0) ul(1) vce(oim) level(90)

   xttobit deviation_a i.accountability##i.scenario##i.case ///
      age ///
      i.gender ///
      equivalent_household_net_income ///
      i.smoker ///
      i.cardiovascular_disease ///
      i.metabolic_disease ///
      locus_of_control ///
      political_attitude ///
      criteria_likert_need ///
      criteria_likert_productivity ///
      criteria_likert_equality ///
      accountjudg, ///
      re ll(0) ul(1) vce(oim) level(90)
   eststo dev_acc_sce_dif_c

   margins i.scenario, dydx(i.accountability) at(case = 1) at(case = 2) at(case = 3) at(case = 4) at(case = 5) level(90) saving(main_account_weight_case, replace)

   margins i.case#i.scenario, dydx(accountability) level(90)
   margins i.scenario, dydx(i.accountability) level(90)
   margins i.scenario, dydx(i.accountability) level(90) pwcompare(effects)
   margins i.scenario, dydx(i.accountability) at(case = 1) level(90) pwcompare(effects)
   margins i.scenario, dydx(i.accountability) at(case = 2) level(90) pwcompare(effects)
   margins i.scenario, dydx(i.accountability) at(case = 3) level(90) pwcompare(effects)
   margins i.scenario, dydx(i.accountability) at(case = 4) level(90) pwcompare(effects)
   margins i.scenario, dydx(i.accountability) at(case = 5) level(90) pwcompare(effects)

   margins i.case#i.accountability, dydx(scenario) level(90) saving(main_scenario_dev, replace)
   margins i.accountability, dydx(i.scenario) level(90)
   margins i.accountability, dydx(i.scenario) level(90) pwcompare(effects)
   margins i.accountability, dydx(i.scenario) at(case = 1) level(90) pwcompare(effects)
   margins i.accountability, dydx(i.scenario) at(case = 2) level(90) pwcompare(effects)
   margins i.accountability, dydx(i.scenario) at(case = 3) level(90) pwcompare(effects)
   margins i.accountability, dydx(i.scenario) at(case = 4) level(90) pwcompare(effects)
   margins i.accountability, dydx(i.scenario) at(case=5) level(90) pwcompare(effects)

   margins if accountability == 0, dydx(scenario) at(case = 1) at(case = 2) at(case = 3) at(case = 4) at(case = 5) pwcompare(effects) mcompare(bonferroni) level(90)
   margins if accountability == 1, dydx(scenario) at(case = 1) at(case = 2) at(case = 3) at(case = 4) at(case = 5) pwcompare(effects) mcompare(bonferroni) level(90)
   margins if scenario == 0, dydx(accountability) at(case = 1) at(case = 2) at(case = 3) at(case = 4) at(case = 5) pwcompare(effects) mcompare(bonferroni) level(90)
   margins if scenario == 1, dydx(accountability) at(case = 1) at(case = 2) at(case = 3) at(case = 4) at(case = 5) pwcompare(effects) mcompare(bonferroni) level(90)

   clear
   use main_account_weight_case

   replace _at = _at + 0.2 if _m1 == 1

   twoway (rcap _ci_ub _ci_lb _at, lcolor(black)) ///
          (scatter _margin _at if _m1 == 0, msymbol(circle) mfcolor(gs10) mlcolor(black) msize(large)) ///
          (scatter _margin _at if _m1 == 1, msymbol(triangle) mfcolor(gs4) mlcolor(black) msize(large)), ///
          xtitle("Case") ///
          xscale(range(0.75 5.5)) ///
          ytitle("- 1{stSymbol:*}|{&Delta}deviation{subscript:{it:A}}|") ///
          yscale(range(-1 (0.2) 0)) ///
          yline(0, lcolor(black) lpattern(dash)) ///
          legend(order(2 "Need Scenario" 3 "Productivity Scenario") cols(1) ring(0) bplacement(swest)) ///
          text(-0.1 4.3 "Marginal Effect of High Accountability", place(c)) ///
          graphregion(color(white))
   graph export "main_marg_deviation_case.pdf", replace

   clear
   use main_scenario_dev

   replace _m1 = _m1 + 0.2 if _m2 == 1

   twoway (rcap _ci_ub _ci_lb _m1, lcolor(black)) ///
          (scatter _margin _m1 if _m2 == 0, msymbol(circle) mfcolor(gs10) mlcolor(black) msize(large)) ///
          (scatter _margin _m1 if _m2 == 1, msymbol(triangle) mfcolor(gs4) mlcolor(black) msize(large)), ///
          xtitle("Case") ///
          xscale(range(0.75 5.5)) ///
          xlabel(1.1 "1" 2.1 "2" 3.1 "3" 4.1 "4" 5.1 "5") ///
          ytitle("{&Delta}deviation{subscript:{it:A}}") ///
          yscale(range(-0.2 (0.05) 0)) ///
          yline(0, lcolor(black) lpattern(dash)) ///
          legend(order(2 "Low Accountability" 3 "High Accountability") cols(1) ring(0) bplacement(swest)) ///
          text(-0.2 4.3 "Marginal Effect of Productivity Scenario", place(c)) ///
          graphregion(color(white))
   graph export "main_marg_deviation_scenario.pdf", replace
restore

esttab dev_sce dev_acc dev_acc_sce dev_acc_sce_c dev_dif dev_acc_sce_dif dev_acc_sce_dif_c using main_gls_dev.tex, label se scalars(chi2) star(* 0.10 ** 0.05 *** 0.01) replace


********************************************************************************
*** MAIN, ACCOUNTABILITY JUDGMENT                                            ***
********************************************************************************
use "need_accountability_main_long.dta", clear

gen oversupply = 0
   replace oversupply = 1 if scenario == 0 & case >= 3
   replace oversupply = 1 if scenario == 1 & case <  3

gen deviation_a = .
   replace deviation_a = (share_a - share_productivity) / (share_need_a - share_productivity_a) if scenario == 0
   replace deviation_a = (share_need - share_a) / (share_need_a - share_productivity_a) if scenario == 1

gen accountjudg = .
   replace accountjudg = accountability_need if scenario == 0
   replace accountjudg = accountability_productivity if scenario == 1

xtset id

xtreg share_a i.accountability##i.scenario##c.accountjudg i.block_order#i.pos ///
   age ///
   i.gender ///
   equivalent_household_net_income ///
   i.smoker ///
   i.cardiovascular_disease ///
   i.metabolic_disease ///
   locus_of_control ///
   political_attitude ///
   criteria_likert_need ///
   criteria_likert_productivity ///
   criteria_likert_equality, ///
   re vce(robust) level(90)

margins i.scenario, dydx(i.accountability) at(accountjudg = (1(1)7)) level(90) saving(main_account_judg_sha, replace)
display _coef[1.accountability#accountjudg]
test _coef[1.accountability#accountjudg] = 0
display _coef[1.accountability#accountjudg] + _coef[1.scenario#1.accountability#accountjudg]
test _coef[1.accountability#accountjudg] + _coef[1.scenario#1.accountability#accountjudg] = 0
display _coef[1.scenario#1.accountability#accountjudg]
test _coef[1.scenario#1.accountability#accountjudg] = 0

replace accountability = 2 if accountability == 1 & scenario == 1
replace accountability = 1 if accountability == 0 & scenario == 1
replace accountability = 0 if accountability == 2 & scenario == 1

drop if deviation < 0 | deviation > 1

xttobit deviation_a i.accountability##i.scenario##c.accountjudg i.block_order#i.pos ///
   age ///
   i.gender ///
   equivalent_household_net_income ///
   i.smoker ///
   i.cardiovascular_disease ///
   i.metabolic_disease ///
   locus_of_control ///
   political_attitude ///
   criteria_likert_need ///
   criteria_likert_productivity ///
   criteria_likert_equality, ///
   re ll(0) ul(1) vce(oim) level(90)

margins i.scenario, dydx(i.accountability) at(accountjudg = (1(1)7)) level(90) saving(main_account_judg_dev, replace)
display _coef[1.accountability#accountjudg]
test _coef[1.accountability#accountjudg] = 0
display _coef[1.accountability#accountjudg] + _coef[1.scenario#1.accountability#accountjudg]
test _coef[1.accountability#accountjudg] + _coef[1.scenario#1.accountability#accountjudg] = 0
display _coef[1.scenario#1.accountability#accountjudg]
test _coef[1.scenario#1.accountability#accountjudg] = 0

xttobit deviation_a i.accountability##i.scenario##i.oversupply##c.accountjudg ///
   age ///
   i.gender ///
   equivalent_household_net_income ///
   i.smoker ///
   i.cardiovascular_disease ///
   i.metabolic_disease ///
   locus_of_control ///
   political_attitude ///
   criteria_likert_need ///
   criteria_likert_productivity ///
   criteria_likert_equality, ///
   re ll(0) ul(1) vce(oim) level(90)

margins i.scenario#i.oversupply, dydx(i.accountability) at(accountjudg = (1(1)7)) level(90) saving(main_account_judgde_over, replace)

clear
use main_account_judg_sha

twoway (rarea _ci_ub _ci_lb _at if _m1 == 0, lcolor(black) fcolor(gs12)) ///
       (line _margin _at if _m1 == 0, lcolor(black) lpattern(solid) lwidth(thick)), ///
       title("Need Scenario", size(small)) ///
       xtitle("Accountability Judgment") ///
       xlabel(1.1 "1" 2.1 "2" 3.1 "3" 4.1 "4" 5.1 "5" 6.1 "6" 7.1 "7") ///
       ytitle("{&Delta}logshare{subscript:{it:A}}") ///
       yscale(range(-0.15 0.05)) ///
       ylabel(-0.15(0.05)0.05) ///
       yline(0, lcolor(black) lpattern(dash)) ///
       legend(order(1 "90% CI ({&beta} = – 0.001, p = 0.833)") cols(1) ring(0) bplacement(swest)) ///
       graphregion(color(white)) ///
       saving(main_accountability_a, replace)

clear
use main_account_judg_sha

twoway (rarea _ci_ub _ci_lb _at if _m1 == 1, lcolor(black) fcolor(white)) ///
       (line _margin _at if _m1 == 1, lcolor(black) lpattern(solid) lwidth(thick)), ///
       title("Productivity Scenario", size(small)) ///
       xtitle("Accountability Judgment") ///
       xlabel(1.1 "1" 2.1 "2" 3.1 "3" 4.1 "4" 5.1 "5" 6.1 "6" 7.1 "7") ///
       ytitle("{&Delta}logshare{subscript:{it:A}}") ///
       yline(0, lcolor(black) lpattern(dash)) ///
       legend(order(1 "90% CI ({&beta} = – 0.007, p = 0.381)") cols(1) ring(0) bplacement(swest)) ///
       graphregion(color(white)) ///
       saving(main_accountability_b, replace)

clear
use main_account_judg_dev

twoway (rarea _ci_ub _ci_lb _at if _m1 == 0, lcolor(black) fcolor(gs12)) ///
       (line _margin _at if _m1 == 0, lcolor(black) lpattern(solid) lwidth(thick)), ///
       title("Need Scenario", size(small)) ///
       xtitle("Accountability Judgment") ///
       xlabel(1.1 "1" 2.1 "2" 3.1 "3" 4.1 "4" 5.1 "5" 6.1 "6" 7.1 "7") ///
       ytitle("– 1{stSymbol:*}|{&Delta}deviation{subscript:{it:A}}|") ///
       yscale(range(-1 0.2)) ///
       ylabel(-1(0.2)0.2) ///
       yline(0, lcolor(black) lpattern(dash)) ///
       legend(order(1 "90% CI ({&beta} = – 0.039, p = 0.378)") cols(1) ring(0) bplacement(swest)) ///
       graphregion(color(white)) ///
       saving(main_accountability_c, replace)

clear
use main_account_judg_dev

twoway (rarea _ci_ub _ci_lb _at if _m1 == 1, lcolor(black) fcolor(white)) ///
       (line _margin _at if _m1 == 1, lcolor(black) lpattern(solid) lwidth(thick)), ///
       title("Productivity Scenario", size(small)) ///
       xtitle("Accountability Judgment") ///
       xlabel(1.1 "1" 2.1 "2" 3.1 "3" 4.1 "4" 5.1 "5" 6.1 "6" 7.1 "7") ///
       ytitle("– 1{stSymbol:*}|{&Delta}deviation{subscript:{it:A}}|") ///
       yscale(range(-1 0.1)) ///
       ylabel(-1(0.2)0.2) ///
       yline(0, lcolor(black) lpattern(dash)) ///
       legend(order(1 "90% CI ({&beta} = – 0.060, p = 0.180)") cols(1) ring(0) bplacement(swest)) ///
       graphregion(color(white)) ///
       saving(main_accountability_d, replace)

graph combine main_accountability_a.gph main_accountability_b.gph main_accountability_c.gph main_accountability_d.gph, col(2) graphregion(color(white)) saving(main_accountability, replace)
graph export "main_accountability_judg.pdf", replace


********************************************************************************
*** MAIN, INDIVIDUAL DATA (I)                                                ***
********************************************************************************
use "need_accountability_main_long.dta", clear


/* strict */
preserve
   gen need_c = .
      replace need_c = 0 if scenario == 0
      replace need_c = 1 if scenario == 0 & share_a == share_need_a

   gen prod_c = .
      replace prod_c = 0 if scenario == 1
      replace prod_c = 1 if scenario == 1 & share_a == share_productivity_a

   gen equ_nc = .
      replace equ_nc = 0 if scenario == 0
      replace equ_nc = 1 if scenario == 0 & share_a == 0.5

   gen equ_pc = .
      replace equ_pc = 0 if scenario == 1
      replace equ_pc = 1 if scenario == 1 & share_a == 0.5

   collapse (sum) need_c equ_nc prod_c equ_pc, by(id accountability)

   gen need_d = 0
      replace need_d = 1 if need_c == 5

   gen equ_nd = 0
      replace equ_nd = 1 if equ_nc == 5

   gen prod_d = 0
      replace prod_d = 1 if prod_c == 5

   gen equ_pd = 0
      replace equ_pd = 1 if equ_pc == 5

   ttest need_d, by(accountability) unequal welch level(90)
   ttest equ_nd, by(accountability) unequal welch level(90)
   ttest prod_d, by(accountability) unequal welch level(90)
   ttest equ_pd, by(accountability) unequal welch level(90)
restore


/* +/- 5% */
preserve
   gen need_c = .
      replace need_c = 0 if scenario == 0
      replace need_c = 1 if scenario == 0 & abs((share_a - share_need_a) / share_need_a) <= 0.05

   gen prod_c = .
      replace prod_c = 0 if scenario == 1
      replace prod_c = 1 if scenario == 1 & abs((share_a - share_productivity_a) / share_productivity_a) <= 0.05

   gen equ_nc = .
      replace equ_nc = 0 if scenario == 0
      replace equ_nc = 1 if scenario == 0 & abs((share_a - 0.5) / 0.5) <= 0.05

   gen equ_pc = .
      replace equ_pc = 0 if scenario == 1
      replace equ_pc = 1 if scenario == 1 & abs((share_a - 0.5) / 0.5) <= 0.05

   gen mix_nc = .
      replace mix_nc = 0 if scenario == 0
      replace mix_nc = 1 if scenario == 0 & abs((share_a - ((share_need_a - share_productivity_a) / 2 + share_productivity_a)) / ((share_need_a - share_productivity_a) / 2 + share_productivity_a)) <= 0.10

   gen mix_pc = .
      replace mix_pc = 0 if scenario == 1
      replace mix_pc = 1 if scenario == 1 & abs((share_a - ((share_need_a - share_productivity_a) / 2 + share_productivity_a)) / ((share_need_a - share_productivity_a) / 2 + share_productivity_a)) <= 0.10

   collapse (sum) need_c equ_nc prod_c equ_pc mix_nc mix_pc, by(id accountability)

   gen need_d = 0
      replace need_d = 1 if need_c == 5

   gen equ_nd = 0
      replace equ_nd = 1 if equ_nc == 5

   gen prod_d = 0
      replace prod_d = 1 if prod_c == 5

   gen equ_pd = 0
      replace equ_pd = 1 if equ_pc == 5

   gen mix_nd = 0
      replace mix_nd = 1 if mix_nc == 5

   gen mix_pd = 0
      replace mix_pd = 1 if mix_pc == 5

   ttest need_d, by(accountability) unequal welch level(90)
   ttest equ_nd, by(accountability) unequal welch level(90)
   ttest prod_d, by(accountability) unequal welch level(90)
   ttest equ_pd, by(accountability) unequal welch level(90)
   ttest mix_nd, by(accountability) unequal welch level(90)
   ttest mix_pd, by(accountability) unequal welch level(90)
restore


/* type */
preserve
   gen need_c = .
      replace need_c = 0 if scenario == 0
      replace need_c = 1 if scenario == 0 & abs((share_a - share_need_a) / share_need_a) <= 0.05

   gen prod_c = .
      replace prod_c = 0 if scenario == 1
      replace prod_c = 1 if scenario == 1 & abs((share_a - share_productivity_a) / share_productivity_a) <= 0.05

   gen equ_nc = .
      replace equ_nc = 0 if scenario == 0
      replace equ_nc = 1 if scenario == 0 & abs((share_a - 0.5) / 0.5) <= 0.05

   gen equ_pc = .
      replace equ_pc = 0 if scenario == 1
      replace equ_pc = 1 if scenario == 1 & abs((share_a - 0.5) / 0.5) <= 0.05

   replace case = case + 5 if scenario == 1

   keep id case need_c prod_c equ_nc equ_pc

   reshape wide need_c prod_c equ_nc equ_pc, i(id) j(case)

   gen need_prod_n = .
      replace need_prod_n = 0 if need_c1 != .
      replace need_prod_n = 1 if need_c1 == 1 & need_c2 == 1 & equ_nc3 == 1 & equ_nc4 == 1 & equ_nc5 == 1

   gen need_prod_p = .
      replace need_prod_p = 0 if need_c6 != .
      replace need_prod_p = 1 if need_c8 == 1 & need_c9 == 1 & need_c10 == 1 & equ_nc6 == 1 & equ_nc7 == 1
restore


********************************************************************************
*** MAIN, INDIVIDUAL DATA (II)                                               ***
********************************************************************************
use "need_accountability_main_long.dta", clear

gen dec_type = 0
   replace dec_type = 1 if share_a < share_productivity_a - 0.01
   replace dec_type = 2 if share_a >= share_productivity_a - 0.01 & share_a <= share_productivity_a + 0.01
   replace dec_type = 3 if share_a > share_productivity_a + 0.01 & share_a < share_need_a - 0.01
   replace dec_type = 4 if share_a >= share_need_a - 0.01 & share_a <= share_need_a + 0.01
   replace dec_type = 5 if share_a > share_need_a + 0.01

gen dec_type_2 = 0
   replace dec_type_2 = 1 if scenario == 0 & case == 1 & share_a >= 0.64 & share_a <= 0.66
   replace dec_type_2 = 1 if scenario == 0 & case == 2 & share_a >= 0.64 & share_a <= 0.66
   replace dec_type_2 = 1 if scenario == 0 & case == 3 & share_a >= 0.64 & share_a <= 0.66
   replace dec_type_2 = 1 if scenario == 0 & case == 4 & share_a >= 0.615 & share_a <= 0.635
   replace dec_type_2 = 1 if scenario == 0 & case == 5 & share_a >= 0.615 & share_a <= 0.635
   replace dec_type_2 = 1 if scenario == 1 & share_a >= (1 - share_productivity_a) - 0.01 & share_a <= (1 - share_productivity) + 0.01

la var dec_type "Decision Type"
label define dec_type_lb 0 "Unclass." 1 "Less" 2 "Prod" 3 "NeedProd" 4 "Need" 5 "More"
   label values dec_type dec_type_lb

la var dec_type_2 "Net Split or Swap"
label define dec_type_2_lb 0 "No" 1 "Yes"
   label values dec_type_2 dec_type_2_lb

by scenario accountability, sort: tabulate case dec_type, row
by scenario accountability, sort: tabulate case dec_type_2, row

preserve
   collapse (count) n = id (sum) m = dec_type_2, by(dec_type scenario accountability case)

   label define case_lb 1 "Case 1" 2 "Case 2" 3 "Case 3" 4 "Case 4" 5 "Case 5"
      label values case case_lb

   replace n = n / 91 if accountability == 0
   replace n = n / 109 if accountability == 1
   replace m = m / 91 if accountability == 0
   replace m = m / 109 if accountability == 1
   replace dec_type = dec_type + 0.4 if accountability == 1

   twoway (bar n dec_type if accountability == 0 & scenario == 0, fcolor(gs10) lcolor(black) barwidth(0.4)) ///
          (bar n dec_type if accountability == 1 & scenario == 0, fcolor(gs4) lcolor(black) barwidth(0.4)) ///
          (bar m dec_type if scenario == 0, fcolor(black) lcolor(none) barwidth(0.4)), ///
          subtitle(, ring(0) pos(1) nobexpand fcolor(white) lcolor(white)) ///
		  by(case, cols(1) graphregion(color(white)) note("") legend(pos(5))) ///
          xtitle("Need Scenario") ///
          xlabel(1.2 "Less" ///
                 2.2 `" "Equal" "Split" "' ///
                 3.2 `" "Partial" "Compensation" "' ///
                 4.2 `" "Need" "Share" "' ///
                 5.2 "More") ///
          ytitle("Relative Frequency") ///
          legend(size( * 0.8) order(3 "Net Split" 1 "Low Accountability") region(lwidth(none)) col(2)) ///
          saving(main_types_cn, replace)

   twoway (bar n dec_type if accountability == 0 & scenario == 1, fcolor(gs10) lcolor(black) barwidth(0.4)) ///
          (bar n dec_type if accountability == 1 & scenario == 1, fcolor(gs4) lcolor(black) barwidth(0.4)) ///
          (bar m dec_type if scenario == 1, fcolor(black) lcolor(none) barwidth(0.4)), ///
          subtitle(, ring(0) pos(1) nobexpand fcolor(white) lcolor(white)) ///
          by(case, cols(1) graphregion(color(white)) note("") legend(pos(7))) ///
          xtitle("Productivity Scenario") ///
          xlabel(1.2 "Less" ///
                 2.2 `" "Productivity" "Share" "' ///
                 3.2 `" "Partial" "Compensation" "' ///
                 4.2 `" "Equal" "Split" "' ///
                 5.2 "More") ///
          ytitle("Relative Frequency") ///
          legend(size( * 0.8) order(2 "High Accountability" 3 "Swap") region(lwidth(none)) col(2)) ///
          saving(main_types_cp, replace)

   graph combine main_types_cn.gph main_types_cp.gph, col(2) graphregion(color(white)) xsize(4) altshrink saving(main_types_1, replace)
   graph export "main_types_1.pdf", replace
restore

gen equal_d = 0
   replace equal_d = 1 if scenario == 0 & dec_type == 2
   replace equal_d = 1 if scenario == 1 & dec_type == 4

gen prop_d = 0
   replace prop_d = 1 if scenario == 0 & dec_type == 4
   replace prop_d = 1 if scenario == 1 & dec_type == 2

gen part_d = 0
   replace part_d = 1 if scenario == 0 & dec_type == 3
   replace part_d = 1 if scenario == 1 & dec_type == 3

gen net_d = 0
   replace net_d = 1 if scenario == 0 & dec_type == 1
   replace net_d = 1 if scenario == 1 & dec_type == 1

by scenario, sort: tabulate equal_d accountability, chi2 column

preserve
   replace accountability = 2 if accountability == 1 & scenario == 1
   replace accountability = 1 if accountability == 0 & scenario == 1
   replace accountability = 0 if accountability == 2 & scenario == 1

   xtset id

   xtprobit equal_d i.scenario##i.accountability, vce(robust)

   margins i.scenario##i.accountability
   margins i.scenario, dydx(i.accountability) level(90)
   margins i.scenario, dydx(i.accountability) pwcompare(effects) level(90)

   gen diff = share_need_a - share_productivity_a

   xtprobit equal_d i.scenario##i.accountability##c.diff, vce(robust)

   margins i.scenario, dydx(i.accountability) at(diff = 0.1) at(diff = 0.14) at(diff = 0.21) at(diff = 0.28) at(diff = 0.36) level(90)
   margins i.scenario, dydx(i.accountability) at(diff = 0.1) at(diff = 0.14) level(90) pwcompare(effects)
   margins i.scenario, dydx(i.accountability) at(diff = 0.14) at(diff = 0.21) level(90) pwcompare(effects)
   margins i.scenario, dydx(i.accountability) at(diff = 0.21) at(diff = 0.28) level(90) pwcompare(effects)
   margins i.scenario, dydx(i.accountability) at(diff = 0.28) at(diff = 0.36) level(90) pwcompare(effects)
restore

preserve
   gen oversupply = 0
      replace oversupply = 1 if scenario == 0 & case >= 3
      replace oversupply = 1 if scenario == 1 & case < 3

   replace accountability = 2 if accountability == 1 & scenario == 1
   replace accountability = 1 if accountability == 0 & scenario == 1
   replace accountability = 0 if accountability == 2 & scenario == 1

   xtset id

   xtprobit equal_d i.oversupply##i.accountability, re vce(robust) level(90)

   margins i.oversupply, dydx(i.accountability) level(90)
   margins i.oversupply, dydx(i.accountability) level(90) pwcompare(effects)
restore

collapse (sum) equal_d prop_d part_d net_d (first) accountability, by(id scenario)

gen equal_type_d = 0
   replace equal_type_d = 1 if equal_d == 5

gen prop_type_d = 0
   replace prop_type_d = 1 if prop_d == 5

gen part_type_d = 0
   replace part_type_d = 1 if part_d == 5

gen net_type_d = 0
   replace net_type_d = 1 if net_d == 5

by scenario, sort: tabulate equal_type_d accountability, chi2 column
by scenario, sort: tabulate prop_type_d accountability, chi2 column
by scenario, sort: tabulate part_type_d accountability, chi2 column
by scenario, sort: tabulate net_type_d accountability, chi2 column

preserve
   replace accountability = 2 if accountability == 1 & scenario == 1
   replace accountability = 1 if accountability == 0 & scenario == 1
   replace accountability = 0 if accountability == 2 & scenario == 1

   xtset id

   xtprobit equal_type_d i.scenario##i.accountability, vce(robust)

   margins i.scenario, dydx(i.accountability) level(90)
   margins i.scenario, dydx(i.accountability) pwcompare(effects) level(90)
restore

gen stypes = .
   replace stypes = 1 if equal_type_d == 1
   replace stypes = 2 if part_type_d == 1
   replace stypes = 3 if prop_type_d == 1
   replace stypes = 4 if net_type_d == 1
   replace stypes = stypes + 0.4 if accountability == 1

collapse (count) n = id, by(stypes scenario accountability)

replace n = n / 91 if accountability == 0
replace n = n / 109 if accountability == 1

drop if stypes == .

twoway (bar n stype if accountability == 0 & scenario == 0, fcolor(gs10) lcolor(black) barwidth(0.4)) ///
       (bar n stype if accountability == 1 & scenario == 0, fcolor(gs4) lcolor(black) barwidth(0.4)), ///
       xtitle("Need Scenario") ///
       xscale(range(0.7 4.5)) ///
       xlabel(1.2 `" "Equal" "Split" "' ///
              2.2 "Partial" ///
              3.2 "Need" ///
              4.2 `" "Net" "Split" "') ///
       ytitle("Relative Frequency") ///
       yscale(range(0 0.4)) ///
       legend(size( * 0.8) order(1 "Low Accountability") region(lwidth(none)) col(1) pos(5)) ///
       text(0.38 1.2 "***") ///
       text(0.38 3.2 "*") ///
       graphregion(color(white)) ///
       saving(main_types_sta, replace)

twoway (bar n stype if accountability == 0 & scenario == 1, fcolor(gs10) lcolor(black) barwidth(0.4)) ///
       (bar n stype if accountability == 1 & scenario == 1, fcolor(gs4) lcolor(black) barwidth(0.4)), ///
       xtitle("Productivity Scenario") ///
       xscale(range(0.7 4.5)) ///
       xlabel(1.2 `" "Equal" "Split" "' ///
              2.2 "Partial" ///
              3.2 "Productivity" ///
              4.2 "Swap") ///
       ytitle("Relative Frequency") ///
       yscale(range(0 0.4)) ///
       legend(size( * 0.8) order(2 "High Accountability") region(lwidth(none)) col(1) pos(7)) ///
       text(0.38 3.2 "***") ///
       graphregion(color(white)) ///
       saving(main_types_stb, replace)

graph combine main_types_sta.gph main_types_stb.gph, col(2) graphregion(color(white)) ycommon saving(main_types_2, replace)
graph export "main_types_2.pdf", replace


********************************************************************************
*** MAIN, REGRESSION SUPPLY SITUATION                                        ***
********************************************************************************
use "need_accountability_main_long.dta", clear

gen accountjudg = .
   replace accountjudg = accountability_need if scenario == 0
   replace accountjudg = accountability_productivity if scenario == 1

gen supply_a = 0
   replace supply_a = 1 if (scenario == 0 & case == 3)
   replace supply_a = 2 if (scenario == 0 & case > 3) | (scenario == 1 & case == 1)

gen supply_b = 0
   replace supply_b = 1 if (scenario == 1 & case == 3)
   replace supply_b = 2 if (scenario == 0 & case > 1) | (scenario == 1 & case < 3)

label variable supply_a "Supply Sit. A"
label variable supply_b "Supply Sit. B"

label define supply_lb 0 "Undersupply" 1 "Need Satisfaction" 2 "Oversupply"
   label values supply_a supply_b supply_lb

gen sup_sit = 0
   replace sup_sit = 1 if scenario == 0 & case == 2
   replace sup_sit = 2 if scenario == 0 & case == 3
   replace sup_sit = 3 if scenario == 0 & case == 4
   replace sup_sit = 3 if scenario == 0 & case == 5
   replace sup_sit = 3 if scenario == 1 & case == 1
   replace sup_sit = 1 if scenario == 1 & case == 2
   replace sup_sit = 4 if scenario == 1 & case == 3

label variable sup_sit "Supply Situation"

label define sup_sit_lb 0 "UU" 1 "UO" 2 "XO" 3 "OO" 4 "UX"
   label values sup_sit sup_sit_lb

gen p_a = 0
   replace p_a = 1000 if scenario == 0
   replace p_a = 1200 if scenario == 1 & case == 1
   replace p_a = 800  if scenario == 1 & case == 2
   replace p_a = 400  if scenario == 1 & case == 3
   replace p_a = 200  if scenario == 1 & case == 4
   replace p_a = 100  if scenario == 1 & case == 5

gen p_b = 0
   replace p_b = 1000 if scenario == 0
   replace p_b = 1800 if scenario == 1 & case == 1
   replace p_b = 1400 if scenario == 1 & case == 2
   replace p_b = 1000 if scenario == 1 & case == 3
   replace p_b = 700  if scenario == 1 & case == 4
   replace p_b = 600  if scenario == 1 & case == 5

gen n_a = 0
   replace n_a = 1800 if scenario == 0 & case == 1
   replace n_a = 1400 if scenario == 0 & case == 2
   replace n_a = 1000 if scenario == 0 & case == 3
   replace n_a = 700  if scenario == 0 & case == 4
   replace n_a = 600  if scenario == 0 & case == 5
   replace n_a = 1000 if scenario == 1

gen n_b = 0
   replace n_b = 1200 if scenario == 0 & case == 1
   replace n_b = 800  if scenario == 0 & case == 2
   replace n_b = 400  if scenario == 0 & case == 3
   replace n_b = 200  if scenario == 0 & case == 4
   replace n_b = 100  if scenario == 0 & case == 5
   replace n_b = 1000 if scenario == 1

gen p_total = p_a + p_b

xtset id

xtreg share_a i.accountability##i.scenario##i.sup_sit, re vce(robust) level(90)


/* randomization of scenario and case */
tabulate pos block_order, chi2


exit
