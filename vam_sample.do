
* prepare df
	use "D:\Project\working_bb\ma-master-build\4_ma_vam\output\va_core48", clear
	keep if subject == 52
	drop if lag_mat_nce == . | lag_ela_nce == .
	gen lvl = grade > 5															
	replace lvl = 0 if grade == 6 & (grade_span == "KG-06":grade_span | grade_span == "KG-08":grade_span)	// count g6 in k-6 or k-8 as elem

	tab grade lvl

	egen tch_yr = group(mepid syear)
	replace section_id = tch_yr if lvl == 0		// for elem, use tch-yr as class
	egen sch_yr = group(sch_code syear)

	gen test = nce_ela
	replace test = nce_mat if subject == "Math":subject
	drop if test == .

	save "vam_data", replace

* test vam

	use vam_data, clear
	destring mepid sch_code, replace

	vam test, teacher(mepid) year(syear) class(section_id) ///
			by(lvl) ///
			controls(lag_mat_nce lag_ela_nce lag_std_noncog_factor) ///
			absorb(sch_code) /*tfx_resid(mepid)*/ ///
			data(merge tv score_r) driftlimit(7)

* more stuff

	keep if lvl == 0
	areg test lag_mat_nce lag_ela_nce lag_std_noncog_factor if lvl == 0, absorb(sch_code)
	
	predict score_r, r

	scalar N = e(N)
	scalar n_par = e(df_m) + e(df_a) + 1

	bys mepid syear lvl section_id: egen n_tested = count(score_r)
	by mepid syear lvl section_id: egen class_mean = mean(score_r)
	by mepid syear lvl section_id: gen index = _n

	sum score_r
	scalar V = r(Var)
	scalar var_total = r(Var)*((N - 1)/(N - n_par))

	gen individual_dev_from_class = score_r - class_mean
			
	count if index==1 & n_tested!=0
	scalar num_class = r(N)

	sum individual_dev_from_class
	scalar var_ind = r(Var)*((N - 1)/(N - num_class - n_par + 1))
	scalar var_class = var_total - var_ind
	scalar list
	
	* 1 obs per tch-class-yr
	by mepid syear lvl section_id: keep if _n==1
	g weight=1/(var_class + var_ind/n_tested)

	gen excess_weight=(missing(weight))
	replace weight=1 if missing(weight)
	
	count

	collapse (mean) class_mean (rawsum) weight n_tested excess_weight [aw=weight], by(mepid syear lvl) fast
		replace weight=weight-excess_weight
		
	count
		
		tsset mepid syear
		
	corr class_mean f2.class_mean [aw = n_tested + f2.n_tested], cov
	corr class_mean f2.class_mean [aw = n_tested + f2.n_tested] if n_tested != . & n_tested > 0 & f2.n_tested != . & f2.n_tested > 0, cov








