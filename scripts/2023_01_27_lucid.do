cd "~/Dropbox/xuncertainty/data"
insheet using "2023_01_27.csv", clear names

drop if inlist(_n, 1, 2)
drop if distributionchannel == "preview"

destring num* original_sure original_uncertain uncertain_clarified direct_comp followup black_dem dem_black evang_rep rep_evang unemp_reps unemploy_dems inflation_reps inflation_dems edit_reps edit_dems political_party, replace

* Calculate numeracy scores
recode num* (1=1)(else = 0)
egen num_score = rowmean(num*)
tab num_score

* Gen attention check var
gen attn = 0
replace attn = 1 if attn_check == "1,2"

* Drop inattentive
drop if attn == 0

******************************
*** LIST ET AL REPLICATION *** 
******************************

* Recode vars so that 0 = higher value and 1 = lower value (irrational choice)
recode original_sure (1=1)(2=0)
recode original_uncertain (1=1)(2=0)
recode uncertain_clarified (1=1)(2=0)
recode direct_comp (1=1)(2=0)

* Let's first examine the direct comparison.
tab direct_comp /* 69.8% make irrational choice */

* Let's now compare original_sure to original_uncertain. If List et al are correct, we should observe higher rates of taking the higher value in the original_sure condition, even if the higher value item is worth more in original_uncertain.
ttest original_sure == original_uncertain, unpaired unequal
	* Sure enough, 61.5% make the irrational choice in the uncertain condition, whereas just 36.5% make the irrational choice in the certain condition. That is, people are more willing to take a $50 gift card over $25 cash than they are to take a lottery worth $75 over $25 in cash.
	
* Let's now compare original_uncertain to uncertain_clarified. If we are right, the % taking the lottery here should be higher in uncertain_clarified.
ttest original_uncertain == uncertain_clarified, unpaired unequal
	* Just 44.7% make the irrational choice when the value of the lottery is clarified, compared to 61.5% when it is not clarified. Suggests that at least some portion of the "uncertainty effect" is about misunderstanding how the lottery works.
	
* More thorough comparison -- original uncertain vs. uncertain clarified
gen uncertain_choice = .
replace uncertain_choice = original_uncertain
replace uncertain_choice = uncertain_clarified if uncertain_choice == .
gen clarification = 0 if uncertain_choice != .
replace clarification = 1 if uncertain_clarified != .

reg uncertain_choice clarification
reg uncertain_choice clarification num_score
reg uncertain_choice i.clarification##c.num_score

* What about comparing all three conditions
gen choice = .
replace choice = original_uncertain
replace choice = uncertain_clarified if choice == .
replace choice = original_sure if choice == .
gen uncertain_unclear = 0
replace uncertain_unclear = 1 if cond == "original_uncertain"
gen uncertain_clear = 0
replace uncertain_clear = 1 if cond == "clarifying"

reg choice uncertain_unclear uncertain_clear


*********************************
*** PIOH & REPRESENTATIVENESS ***
*********************************

gen groupfirst = 0
replace groupfirst = 1 if rh_cond == "groupfirst"

reg dem_black groupfirst


************************
*** ECON PERCEPTIONS ***
************************

* Clean partisanship variables
recode political_party (9 = 6)(10 = 7)(6 = 3)(7 = 4)(8 = 5)
recode political_party (1/3 = 1)(4 = 2)(5/7 = 3), gen(pid3)
	label define party_lbl 1 "Democratic" 2 "Independent" 3 "Republican", replace
	label values pid3 party_lbl
	
* Manipulate to get in/out party conditions
gen econ_op = .
replace econ_op = 0 if pid3 == 1 & econ_cond == "dems"
replace econ_op = 0 if pid3 == 3 & econ_cond == "reps"
replace econ_op = 1 if pid3 == 1 & econ_cond == "reps"
replace econ_op = 1 if pid3 == 3 & econ_cond == "dems"

gen unemploy = unemp_reps
replace unemploy = unemploy_dems if unemploy == .
gen inflate = .
replace inflate = inflation_reps
replace inflate = inflation_dems if inflate == .

reg unemploy econ_op
reg inflate econ_op

* Another way to investigate: what % say "got worse"?
recode unemploy (1/2 = 0)(3 = 1), gen(unemploy_worse)
recode inflate (1/2 = 0)(3 = 1), gen(inflate_worse)

reg unemploy_worse econ_op /* 9.5% say it got worse in in-party condition, compared to 14.7% in  out-party condition */
reg inflate_worse econ_op /* 8.5% say it got worse in in-party condition, compared to 16.8% in out-party condition */ 


**********************
*** ERROR CATCHING ***
**********************

* Manipulate to get in/out party conditions
gen error_op = 0 if edit_cond == "r" & pid3 == 3
replace error_op = 0 if edit_cond == "d" & pid3 == 1
replace error_op = 1 if edit_cond == "r" & pid3 == 1
replace error_op = 1 if edit_cond == "d" & pid3 == 3

gen errors_caught = edit_dems
replace errors_caught = edit_reps if errors_caught == .

* Dealing with outliers
tab errors_caught
	* Looks like > 10 are outliers
	replace errors_caught = . if errors_caught > 10
		*NOTE: Could also winsorize

reg errors_caught error_op
	* Nothing there
