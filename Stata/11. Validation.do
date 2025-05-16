


*Validation do-file 
tempfile R_dataset

//Load database produced 
import delimited "$tempsim/income_tax_collapse_R", clear 
save  "$tempsim/income_tax_collapse_R", replace 

use "$tempsim/income_tax_collapse.dta", clear 
*replace income_tax_1=income_tax_1+1
cf income_tax_1 income_tax_2 income_tax_3 using "$tempsim/income_tax_collapse_R.dta", all verbose


 