/*--------------------------------------------------------------------------------
*--------------------------------------------------------------------------------
* Program: Program for the Impact of Fiscal Reforms - CEQ Senegal
* Author: 	JuanP. Baquero
* Date: 		11 Nov 2020
* Title: 	Generate Output for Simulation
*--------------------------------------------------------------------------------
*--------------------------------------------------------------------------------
*Note: Each output goes in long format  to a hidden sheet call all_`sheetnm'

Version 2. 
	- Change the refrence income for marginal contributions for all categories
	- Minor: commenting do-file and Making comments and Pendent
	- Added a new category for subsidies (before were together with transfers, not correct because their marginal contributions are measured differently

	
Pendent : 
_---------------------------------------------------------------------------------*/

if $save_scenario == 1 {	
	global sheetname "${scenario_name_save}"
}
if $save_scenario == 0 & $load_scenario == 1 {	
	global sheetname "${scenario_name_load}"
}
if $load_scenario == 0 & $save_scenario == 0 {	
	global sheetname "User_def_sce"
}

*---- Macros for household values 

		
	local Directaxes 		"${Directaxes}"
	local Contributions 	"${Contributions}" 
	local DirectTransfers   "${DirectTransfers}"
	local Subsidies         "${Subsidies}"
	local Indtaxes 			"${Indtaxes}"
	local InKindTransfers	"${InKindTransfers}" 

	local tax dirtax_total `Directaxes' ss_contribs_total `Contributions'
	local indtax indtax_total `Indtaxes' Tax_TVA
	local inkind inktransf_total `InKindTransfers' inktransf_educ inktransf_health
	local transfer dirtransf_total `DirectTransfers' am_prog_sa ss_ben_sa
	local subsidies subsidy_total `Subsidies' subsidy_elec subsidy_fuel
	local income ymp yn yd yc yf 
	local concs `tax' `indtax' `transfer' `inkind' `income' `subsidies'

	
*Macros at per-capita values 
	foreach x in tax indtax inkind transfer income concs subsidies {
		local `x'_pc
		foreach y of local `x' {
			local `x'_pc ``x'_pc' `y'_pc 	
		}
	}
*Other macros 
	*local rank ymp_pc
	local pline zref line_1 line_2 line_3
	
	
*===============================================================================
		* Save Scenario
*===============================================================================

if $save_scenario == 1 {
	
	global c:all globals
	macro list c

	clear
	gen globalname = ""
	gen globalcontent = ""
	local n = 1
	foreach glob of global c{
		dis `"`glob' = ${`glob'}"'
		set obs `n'
		replace globalname = "`glob'" in `n'
		replace globalcontent = `"${`glob'}"' in `n'
		local ++n
	}

	foreach gloname in c thedo_pre theado thedo xls_sn data_out tempsim presim data_dev data_sn path S_4 S_3 S_level S_ADO S_StataSE S_FLAVOR S_OS S_OSDTL S_MACH save_scenario load_scenario devmode asserts_ref2018 {
		cap drop if globalname == "`gloname'"
	}
		
	export excel "$xls_out", sheet("p_${scenario_name_save}") sheetreplace first(variable)
	noi dis "{opt All the parameters of scenario ${scenario_name_save} have been saved to Excel.}"
		
	*Add saved scenario to list of saved scenarios
	import excel "$xls_out", sheet(Scenario) first clear cellrange(A1)
	drop if Scenario_list == ""
	expand 2 in -1
	replace Scenario_list = "${scenario_name_save}" in -1
	duplicates drop
	gen ord = 2
	replace ord = 1 if Scenario_list == "Ref_2018"
	replace ord = 3 if Scenario_list == "User_def_sce"
	sort ord, stable
	drop ord
	
	export excel "$xls_out", sheet("Scenario", modify) cell(A2)
}


*===============================================================================
		*Netcash Position
*===============================================================================

{
* net cash ymp

	use "$data_out/output", clear
	
	keep hhid `concs_pc' pondih *_centile_pc deciles_pc 
	
	foreach x in `tax' `indtax'  {
		gen share_`x'_pc= -`x'_pc/ymp_pc
	}		
	
	foreach x in `transfer' `inkind' `subsidies' {
		gen share_`x'_pc= `x'_pc/ymp_pc
	}
		
	*replace share_snit_hh_ae = - share_snit_hh_ae
	keep deciles_pc share* pondih	
		
	groupfunction [aw=pondih], mean (share*) by(deciles_pc) norestore
	
	reshape long share_, i(deciles_pc) j(variable) string
		gen measure = "netcash" 
		rename share_ value
	
	tempfile netcash_ymp
	save `netcash_ymp'


}		

*===============================================================================
		*Absolute, relative and co	verage 
*===============================================================================

	
	* All 
* benefits, coverage beneficiaries by all	
use "$data_out/output",  clear	
	
	
*Relative incidence
gen ri_dirtax=dirtax_total_pc/ymp_pc
assert ymp_pc>0
 
collapse (mean) ri_dirtax (sum) dirtax_total_pc  [iw=pondih], by (deciles_pc)

*Absolute incidence
egen total_dir_taxes=total(dirtax_total_pc)
gen ai_dirtax= dirtax_total_pc/ total_dir_taxes
 
label var ai_dirtax "absolute incidence"
label var ri_dirtax "relative incidence"

export excel "$xls_out", sheet("all${sheetname}_simplified") sheetreplace first(variable)


