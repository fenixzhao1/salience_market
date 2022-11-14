***** how treatments affect trade price *****
* open dataset
use "~\Dropbox\Working Papers\Salience in Asset Markets\data\salience_market\data_reg.dta", clear

* generate treatment dummies
gen three_states = 0 
replace three_states = 1 if num_states == 3

gen only_a = 0
replace only_a = 1 if asset_type == "only A"
gen only_b = 0
replace only_b = 1 if asset_type == "only B"

* compare price with baseline 100
gen price_base = price - 100

* regressions
reg price_base three_states state_independent only_a ///
    if asset_id == "A" & practice == 0, cluster(session)
outreg2 using "~\Dropbox\Working Papers\Salience in Asset Markets\data\salience_market\regression\stata_table", tex nonote se replace nolabel bdec(2)
reg price_base three_states state_independent only_b ///
    if asset_id == "B" & practice == 0, cluster(session)
outreg2 using "~\Dropbox\Working Papers\Salience in Asset Markets\data\salience_market\regression\stata_table", tex nonote se append nolabel bdec(2)

reg price_base three_states state_independent only_a round_number ///
    if asset_id == "A" & practice == 0, cluster(session)
outreg2 using "~\Dropbox\Working Papers\Salience in Asset Markets\data\salience_market\regression\stata_table", tex nonote se append nolabel bdec(2)
reg price_base three_states state_independent only_b round_number ///
    if asset_id == "B" & practice == 0, cluster(session)
outreg2 using "~\Dropbox\Working Papers\Salience in Asset Markets\data\salience_market\regression\stata_table", tex nonote se append nolabel bdec(2)





