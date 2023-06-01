
* prepare df
	use "D:\Project\working_bb\ma-master-build\4_ma_vam\output\va_core48", clear
	gen lvl = grade > 5															
	replace lvl = 0 if grade == 6 & (grade_span == "KG-06":grade_span | grade_span == "KG-08":grade_span)	// count g6 in k-6 or k-8 as elem

	tab grade lvl

	egen tch_yr = group(mepid syear)
	replace section_id = tch_yr if lvl == 0		// for elem, use tch-yr as class
	egen sch_yr = group(sch_code syear)

	gen test = nce_ela
	replace test = nce_mat if subject == "Math":subject
	
	

drop course section course_grade grade_code lag_2x_ela_nce lag_2x_mat_nce retained lag_retained lag2_retained absence ln_absence ln_lag_absence ln_lag2_absence days_susp ln_days_susp ln_lag_days_susp ln_lag2_days_susp gpa lag_gpa lead_gpa eng saf env clc rel par emo psf bul ins men dis mtel_subj mtel_clst exp_lic exp_hire crx_lag_retained scx_lag_retained crx_ln_lag_absence scx_ln_lag_absence crx_ln_lag_days_susp scx_ln_lag_days_susp crx_lag_gpa

drop org_code teach_weight parcc online nce_ela nce_mat lag2_std_noncog_factor voc grade_span tr_grade scx_lep scx_male scx_frl scx_sped_fi scx_sped_pi scx_sped_ss scx_asian scx_black scx_hpi scx_hisp scx_amind scx_mult scx_lag_mat_nce scx_lag_ela_nce scx_lag_gpa scx_lag_std_noncog_factor track_course_id track_sch_id

keep if subject <= 52
 
* export delimited "D:\Project\working_bb\vamR_test_data\vam_data.csv", replace
save "D:\Project\working_bb\vamR_test_data\vam_data", replace

* test vam

cap log close _all
log using vam_stata, name("vam_stat") text replace

	local controls ///
		i.grade##(c.lag_mat_nce##c.lag_mat_nce##c.lag_mat_nce ///
		c.lag_ela_nce##c.lag_ela_nce##c.lag_ela_nce ///
		c.lag_std_noncog_factor##c.lag_std_noncog_factor##c.lag_std_noncog_factor) ///
		i.grade##i.(lep male frl sped_fi sped_pi sped_ss asian black hpi hisp amind mult ///
		i.syear) ///
		i.grade##c.(crx_lep crx_male crx_frl crx_sped* ///
		crx_asian crx_black crx_hpi crx_hisp crx_amind crx_mult ///
		crx_lag_mat_nce crx_lag_ela_nce crx_lag_std_noncog_factor)
		
	use "D:\Project\working_bb\vamR_test_data\vam_data", clear
	destring sch_code mepid, replace
	
	timer clear 1
	timer on 1

	vam test, teacher(mepid) year(syear) class(section_id) ///
			by(lvl subject) ///
			controls(`controls') ///
			/*absorb(sch_code)*/ quasi tfx_resid(mepid) ///
			data(merge tv score_r) driftlimit(7)

	keep mepid syear lvl subject tv*
	duplicates drop
	drop tv_ss
	
	timer off 1
	qui timer list 1
	di round(`r(t1)'/60, .1) " minutes"
	
	save "D:\Project\working_bb\vamR_test_data\tv", replace


cap log close



