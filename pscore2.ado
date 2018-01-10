/*current version 03.05.2013*/
program define pscore2, eclass
	* version 11.1
	syntax varlist [if] [in] [fweight iweight pweight], blockid(string) pscore(string)/*
	*/ [KSMirnov VARiance TENForce WIlk MEDian SUPPlied logit COMsup level(real 0.2) REscale by(real 0.5) REVert DETail SUMmary QUIetly] 
	tokenize `varlist'
	
	tempvar touse
	gen `touse' = 0
	qui replace `touse' = 1 `if' `in'
	
	******************************************************************************************************************************
	******************************************************************************************************************************
	/* PROPENSITY SCORE MODEL */
	******************************************************************************************************************************
	******************************************************************************************************************************
	
	loc T `1'
	di in ye _newline(3) "**************************************************** "
	di in ye	         "Propensity score model"
	di in ye	         "**************************************************** "
	/* ERROR messages in case of ambiguous options specified*/
	if `"`supplied'"' != `""' & ( `"`pscore(string)'"' != `""' |  `"`logit'"' != `""') {
		di in red "Option supplied can not be specified along with logit"
		error 198
	}
	/* CHECK
	if `"`by'"' != `""'  & `"`rescale'"' == `""' {
		di in red "by( ) cannot be specified without rescale"
		error 198
	}
	*/
	if `"`quietly'"' != `""'  & `"`summary'"' == `""' {
		di in red "quietly does not make sense without summary being specified"
		error 198
	}
	if `"`detail'"' != `""'  { /*DETAIL OPTION*/
		loc noi noi
	}
	qui {
		/* OPTION TO HAVE PROPENSITY SCORE EXTERNALLY SUPPLIED*/
		/* must be second argument in varlist */
		if `"`supplied'"' != `""' {
			noi di _newline(1) in ye "Note: " in bl "The option to supply the propensity score externally has been selected"
			loc p `2'
			if `"`detail'"' != `""' {
				noi su `p' if `touse' == 1, det
			}
			macro shift
		}
		macro shift
		loc X `*'
		else { /* ESTIMATE PROPENSITY SCORE */
		    if "`weight'" != "" { /* if weights are specified */
				tempvar wv
				qui gen double `wv' `exp'
				local w [`weight'=`wv']
				replace `touse' = 0 if `wv'==0
			}
			/* ESTIMATE PROPENSITY SCORE MODEL (default is probit) */
			if `"`logit'"' != `""' {
				`noi' logit `varlist' [`weight'`exp'] if `touse'==1 
			}
			else {		
				`noi' probit `varlist' [`weight'`exp'] if `touse'==1
			}
			/* PREDICT PROPENSITY SCORE */
			tempvar p
			predict double `p' if `touse' == 1
		}
		/* OPTION: restrict to common support */
		if `"`comsup'"' != `""' {
			su `p' if `T' == 1
			tempname mintreat maxtreat
			sca `mintreat' = r(min)
			sca `maxtreat' = r(max)
			replace `p' = . if `p' < `mintreat' | `p' > `maxtreat'
			replace `touse' = 0 if `p' == .
			
			noi di _newline(1) in ye "Note: " in bl "The common support option has been selected"
			noi di  in bl  "=> The region of common support is [" round(`mintreat',.0001) ", " round(`maxtreat',.0001) "]"
			
		}
		/* REVERT OPTION*/
		if `"`revert'"' != `""' {
			noi di _newline(1) in ye "Note: " in bl "Searching in direction of maximum propensity score"
			replace `p' = 1 - `p'
		}
		if `"`revert'"' == `""' {
			noi di _newline(1) in ye "Note: " in bl "Searching in direction of minimum propensity score"
		}
		
	}
	
	******************************************************************************************************************************
	******************************************************************************************************************************
	/* GRID SEARCH ALGORITHM */
	******************************************************************************************************************************
	******************************************************************************************************************************
	
	di in ye _newline(3) "**************************************************** "
	di in ye	         "Initializing and computing grid search"
	di in ye	         "**************************************************** " 
	/* ERROR messages in case of ambiguous options specified*/
	if (`"`wilk'"' != `""' & ( `"`median'"' != `""' | `"`ksmirnov'"' !=  `""' |  `"`variance'"' ! = `""'))  ///
	|  (`"`median'"' != `""'  & (`"`wilk'"' != `""' |`"`ksmirnov'"' !=  `""' |  `"`variance'"' ! = `""'))   ///
	|  (`"`ksmirnov'"' !=  `""' & ( `"`median'"' != `""' | `"`wilk'"' !=  `""' |  `"`variance'"' ! = `""')) ///
	|  (`"`variance'"' ! = `""' & ( `"`median'"' != `""' | `"`ksmirnov'"' !=  `""' |  `"`wilk'"' ! = `""')){
		di in red "Ambiguous options specified"
		error 198
	}
	if `"`tenforce'"' != `""'  & (`"`ksmirnov'"' !=  `""' |  `"`variance'"' ! = `""') {
		di in red "Ambiguous options specified"
		error 198
	}
	/* OPTION t-test for discrete regressors */
	if `"`tenforce'"' != `""' {
		di _newline(2) in ye "Note: " in bl "Option of " in ye "mean-comparison test" in bl " also for discrete regressors has been selected"
		di ""
	}
	/* OPTION Wikoxon rank sum test for cont. regressors */
	if `"`wilk'"' != `""' {
		di _newline(2) in ye "Note: " in bl "Option of " in ye "Wilkoxon rank sum test" in bl " for continuous regressors has been selected"
		di ""
	}
	/* OPTION median test for cont. regressors */
	if `"`median'"' != `""' {
		di _newline(2) in ye "Note: " in bl "Option of " in ye "median test for continuous regressors" in bl " has been selected"
		di ""
	}
	/* OPTION variance and means*/
	if `"`variance'"' != `""' {
		di _newline(2) in ye "Note: " in bl "Option to test for " in ye "equal means and variances" in  bl " has been selected"
		di ""
	} 
	/* OPTION ksmirnov*/
	if `"`ksmirnov'"' != `""' {
		di _newline(2) in ye "Note: " in bl "Option to use " in ye "Kolmogorov-Smirnov equality-of-distributions test" in bl " has been selected"
		di ""
	}
	if `"`rescale'"' != `""' {
		if (`by' <= 0 | `by' >= 1){
			di in red "Scaling factor must be in (0,1)"
			error 198
		}
		di _newline(1) in ye "Note: " in bl "Option to rescale testing interval by factor " in ye "0" in ye "`by'" in bl " has been selected"
		di ""
	}
	/* SEARCH */
	qui {
		// rename vars
		loc k = 0
		foreach x in `X' {
			loc k = `k' + 1
			tempvar x`k'
			gen double `x`k'' = `x'
		}
		loc K = `k'
		// preallocate temporary objects 
		tempvar intervals converged 
		gen double `intervals' = .
		gen double `converged' = .
		// initialize grid search parameters
		su `p' 
		tempname max scale
		sca `max'   = r(max)
		sca `scale' = r(min)
		sort `p'
		replace `intervals' = r(min) in 1
		loc cc = 1
		loc not = 0
		// do the work
		while `scale' != `max' { /*iterate over propensity score interval until `scale' is equal to `max'*/

			loc converge = 0
			loc diverge  = 0
			if `"`rescale'"' == `""' { 
				loc int0 = `max' - `scale'
				loc int = `int0'
				loc intcount = 1
			}
			if `"`rescale'"' != `""' {
				loc int = `max' - `scale'
			}
			while `converge' != 1 & `diverge' != 1 { /*for each interval iterate until either convergence or divergence is achieved*/
				// check number of observations within remaining testing interval: [`scale';`scale' + `int'] 
				foreach j in 0 1 {
					if `scale' + `int' != `max' { /*upper interval bound is exclusive*/
						su `T' if `T' == `j' & `p' >= `scale' & `p' < `scale' + `int'
					}
					else {  /*upper interval bound is inclusive*/
						su `T' if `T' == `j' & `p' >= `scale' & `p' <= `scale' + `int'
					}
					loc n`j' = r(N)	
				}
				//if there are sufficient obs, start testing
				if `n1' >= 2 & `n0' >= 2 {
					loc k = 1
					while `k' <= `K' {
						loc pval ""
						// check whether regressor is the same for everyone
						if `scale' + `int' != `max' { /*upper interval bound is exclusive*/
							su  `x`k'' if `p' >= `scale' & `p' < `scale' + `int'
						}
						else { /*upper interval bound is inclusive*/
							su  `x`k'' if `p' >= `scale' & `p' <= `scale' + `int'
						}
						loc sd1  = r(sd)
						if `sd1' == 0 { /*NON-VARYING DATA */
							loc pval = `level'+ 1 // dummy argument for `pval' that regressor is fine
							loc not = `not' + 1
						}
						else {
							// check whether data is binary or continuous
							su `x`k'' if `x`k''!=0  
							loc nn1  = r(N)
							loc sum1 = r(sum)
							/* OPTION Kolmogorov-Smirnov test for all regressors*/
							if `"`ksmirnov'"' != `""' {
								if `scale' + `int' != `max' { /*upper interval bound is exclusive*/
									ksmirnov `x`k'' if `p' >= `scale' & `p' < `scale' + `int', by(`T')
								}
								else { /*upper interval bound is inclusive*/
									ksmirnov `x`k'' if `p' >= `scale' & `p' <= `scale' + `int', by(`T')
								}
								if `nn1'==`sum1' { /*DESCRETE DATA */
									loc pval = r(p_cor)
								}
								else {
									loc pval = r(p) 
								}
								loc not = `not' + 1
							}
							/* OPTION test for equal means and variances*/
							if `"`variance'"' != `""' {/*equal means and variances*/
								if `scale' + `int' != `max' { /*upper interval bound is exclusive*/
								    ttest `x`k'' if `p' >= `scale' & `p' < `scale' + `int', by(`T')
									loc pt = r(p)
									sdtest `x`k'' if `p' >= `scale' & `p' < `scale' + `int', by(`T')
									loc ps = r(p)
								}
								else { /*upper interval bound is inclusive*/
									ttest `x`k'' if `p' >= `scale' & `p' <= `scale' + `int', by(`T')
									loc pt = r(p)
									loc not = `not' + 1
									sdtest `x`k'' if `p' >= `scale' & `p' <= `scale' + `int', by(`T')
									loc ps = r(p)
									loc not = `not' + 1
								}
								///here
								if ("`pt'" == "." | "`ps'" == ".") {
									loc pval = .
								}
								else {
									loc pval = min(`pt',`ps')
								}
								loc not = `not' + 2
							}
							
							/* OPTION Wilkoxon rank sum test for all regressors */
							if `"`wilk'"' != `""' {
								if `scale' + `int' != `max' { /*upper interval bound is exclusive*/
									ranksum `x`k'' if `p' >= `scale' & `p' < `scale' + `int', by(`T')
								}
								else { /*upper interval bound is inclusive*/
									ranksum `x`k'' if `p' >= `scale' & `p' <= `scale' + `int', by(`T')
								}
								loc pval = (1-normal(abs(r(z))))*2
								loc not = `not' + 1
							}
							
							/* OPTION median test for all regressors */
							if `"`median'"' != `""' {
								if `scale' + `int' != `max' { /*upper interval bound is exclusive*/
									median `x`k'' if `p' >= `scale' & `p' < `scale' + `int', by(`T')
								}
								else { /*upper interval bound is inclusive*/
									median `x`k'' if `p' >= `scale' & `p' <= `scale' + `int', by(`T')
								}
								loc pval = r(p_cc) 
								loc not = `not' + 1
							}
							
							/* OPTION t-test for all regressors */
							if `"`tenforce'"' != `""' {
								if `scale' + `int' != `max' { /*upper interval bound is exclusive*/
									ttest `x`k'' if `p' >= `scale' & `p' < `scale' + `int', by(`T')
								}
								else { /*upper interval bound is inclusive*/
									ttest `x`k'' if `p' >= `scale' & `p' <= `scale' + `int', by(`T')
								}
								loc pval = r(p)
								loc not = `not' + 1
							} 
							
							/* DEFAULT */ 
							if (`"`wilk'"' == `""' & `"`median'"' == `""' & `"`tenforce'"' == `""' & `"`ksmirnov'"' == `""' &  `"`variance'"' == `""') {
								if `nn1'==`sum1' { /*DESCRETE DATA: DEFAULT Wilkoxon rank sum test */
								    if `scale' + `int' != `max' { /*upper interval bound is exclusive*/
										ranksum `x`k'' if `p' >= `scale' & `p' < `scale' + `int', by(`T')
									}
									else { /*upper interval bound is inclusive*/
										ranksum `x`k'' if `p' >= `scale' & `p' <= `scale' + `int', by(`T')
									}
									loc pval = (1-normal(abs(r(z))))*2 // asymptotic approximation
									loc not = `not' + 1
								}
								else { /* DEFAULT for continuous regressors is t-test */
									if `scale' + `int' != `max' { /*upper interval bound is exclusive*/
										ttest `x`k'' if `p' >= `scale' & `p' < `scale' + `int', by(`T')
									}
									else { /*upper interval bound is inclusive*/
										ttest `x`k'' if `p' >= `scale' & `p' <= `scale' + `int', by(`T')
									}
									loc pval = r(p)
									loc not = `not' + 1
								}
							} 
						}
						
						// balancing for k^th regressor is given and it is the last of `K' regressors
						if (`pval' > `level' & "`pval'" != "." & `k' == `K') {
							loc k = `k' + 1
							loc converge = 1 
						}
						// balancing for k^th regressor is given
						if `pval'  > `level' & "`pval'" != "." &  `k' < `K'{
							loc k = `k' + 1 
						}
						// k^th regressor fails balancing 
						if (`pval' <= `level' | "`pval'" == ".") {				
							loc k = `K' + 1		  // exit while loop over regressors							
							*loc int = `int'/2     // rescale testing interval 
							if `"`rescale'"' == `""' { 
								loc intcount = `intcount' + 1
								loc int = `int0'/`intcount'
							}
							if `"`rescale'"' != `""' { 
								loc int = `int'*`by'
							}
							/*check number of observations*/
							foreach j in 0 1 { /* by definition always exclusive */
								count if `T' == `j' & `p' >= `scale' & `p' < `scale' + `int' /*`insert'*/
								loc n`j' = r(N)			
							}
							/* if there are insufficient obs, report divergence */
							if (`n1' < 2 | `n0' < 2) {
								loc diverge = 1
							}
						}
					}
				}
				/*if there are not sufficient observations on [`scale';`max'], stop and truncate interval */ 
				else {
					loc diverge = 1			
				}
			}
			noi di _newline(1) as bl "Interval `cc' complete "
			/* update stuff and collect results*/
			sca `scale' = `scale' + `int'		
			loc cc = `cc' + 1
			replace `intervals' = `scale' in `cc'
			if `converge' ==  1 {
				replace `converged' = 1  in `cc' 
				noi di as bl "(convergence achieved)"
			}
			else if `diverge' == 1 {
				replace `converged' = 0 in `cc'
				noi di as bl "(convergence not achieved - truncating interval)"
			}
		} 
	}
	
	******************************************************************************************************************************
	******************************************************************************************************************************
	/* CREATE STRATA IDENTIFIER AND CONCLUDE */
	******************************************************************************************************************************
	******************************************************************************************************************************
	
	ereturn clear
	
	di in ye _newline(3) "**************************************************** "
	di in ye	         "Estimation results"
	di in ye	         "**************************************************** "
	qui {
		/* create group identifier*/
		su `intervals' 
		loc N = r(N)-1
		tempvar id 
		gen double `id' = .
		forv i = 1/`N' {
			if `i' < `N' {
				replace `id' = `i' if `p' >= `intervals'[`i'] & `p' < `intervals'[`i'+1]  & `converged'[`i'+1] == 1
			}
			else {
			    replace `id' = `i' if `p' >= `intervals'[`i'] & `p' <= `intervals'[`i'+1] & `converged'[`i'+1] == 1 
			}
		}
		egen `blockid' = group(`id') if `id' != .
		la var `blockid' "strata identifier"
		
		/* create propensity score variable according to the estimated strata*/
		replace `p' = . if `blockid' == .
		gen double `pscore' = `p'*(`"`revert'"' == `""') + (1 -`p')*(`"`revert'"' != `""')
		la var `pscore' "estimated propensity score"
		
		su `blockid'
		
		ereturn sca converged = (r(N)!=0)  // 0 if no balanced strata could be found
		ereturn sca shareN = r(N) / _N     // share of total observations classified
		ereturn sca classifiedN = r(N)     // number of total observations classified
		
		/* error message in case there are no balanced strata */
		if r(N) == 0 {
			noi di _newline(1) in red "Nothing to report"
			noi di ""
			error 198
		}
		
		loc N = r(max)
		
		ereturn sca strata = `N'      // estimated number of strata
		
		tempname prstrata strataN tot  // 
		
		mat `prstrata' = J(`N',3,.)
		mat `strataN'  = J(`N',4,.)
		
		if `"`summary'"' != `""' {		
			
			tempname pvals pv fitness
			matrix `pvals' = J(rowsof(`prstrata'),`K'+1,.)
			
			if `"`variance'"' != `""' {
				tempname pvalsvar pv1 fitness1
				matrix `pvalsvar' = J(rowsof(`prstrata'),`K'+1,.)
			}
		}
		
		forv i = 1/`N' {
			noi di _newline(1) in ye "`blockid' = " `i'
			su `p' if `blockid' == `i' 
			
			loc lower = r(min)*(`"`revert'"' == `""') + (1-r(min))*(`"`revert'"' != `""')
			loc upper = r(max)*(`"`revert'"' == `""') + (1-r(max))*(`"`revert'"' != `""')
			
			noi di /*_newline(1)*/ in ye "---------------------------------------------------"
			if `i' != `N' {
				noi di in ye "Estimated propensity score in ["  round(`lower',.0001) "," round(`upper',.0001) ")"
			}
			else {
				noi di in ye "Estimated propensity score in ["  round(`lower',.0001) "," round(`upper',.0001) "]"
			}
			
			mat `prstrata'[`i',1] = `lower'
			mat `prstrata'[`i',2] = `upper'
			mat `prstrata'[`i',3] = `i'
			
			su `blockid' if `blockid' == `i' & `T' == 1
			
			mat `strataN'[`i',1] = r(N)
			
			noi di  in  ye "Number of treated obs. = " r(N) 
			su `blockid' if `blockid' == `i' & `T' == 0
			
			mat `strataN'[`i',2] = r(N)
			mat `strataN'[`i',3] = `strataN'[`i',1] + `strataN'[`i',2]
			mat `strataN'[`i',4] = `i'
			
			noi di in ye "Number of control obs. = " r(N) 
			noi di in ye "---------------------------------------------------"
			noi di ""
			
			/* Extract estimated p-values if summary option is specified */
			/* Report estimated p-values in estimation output if quietly is not specified*/
			
			if `"`quietly'"' == `""' {
				loc noi noi
			}
			
			if `"`summary'"' != `""' { 
				
				loc cc = 0
				foreach x in `X' {
				    
					loc cc = `cc' + 1
					
					// check whether regressor is the same for everyone 
					su  `x' if `blockid' == `i'
					loc sd1  = r(sd)
					if `sd1' == 0 {
						`noi' di in bl "level of " in ye "`x'" in bl " is the same for everyone"
						 sca `pv' = 1
					}
					else {
						su `x' if `x'!=0
						loc nn1 = r(N)
						loc sum1 = r(sum)
						if `"`ksmirnov'"' != `""' { 
							ksmirnov `x' if `blockid' == `i', by(`T')
							if `nn1'==`sum1' { /*DESCRETE DATA */
								`noi' di in bl  "p-value (continuity corrected) equality of distributions test " in ye "`x'" in bl " = "  round(r(p_cor),.0001)
								sca `pv' = r(p_cor)
							}
							else {
								`noi' di in bl  "p-value equality of distributions test " in ye "`x'" in bl " = "  round(r(p),.0001)
								sca `pv' = r(p)
							}
						}
						if `"`variance'"' != `""' {/*equal means and variances*/
							ttest `x' if `blockid' == `i', by(`T')
							`noi' di in bl  "p-value mean comparison test " in ye "`x'" in bl " = "  round(r(p),.0001)
							sca `pv' = r(p)
							sdtest `x' if `blockid' == `i', by(`T')
							`noi' di in bl  "p-value variance comparison test " in ye "`x'" in bl " = "  round(r(p),.0001)
							sca `pv1' = r(p)
						}
						if  (`"`ksmirnov'"' == `""' & `"`variance'"' == `""') {
							if `nn1'==`sum1' {
								if `"`tenforce'"' != `""' { 
									ttest `x' if `blockid' == `i', by(`T')
									`noi' di in bl  "p-value mean comparison test " in ye "`x'" in bl " = " round(r(p),.0001)
									sca `pv' = r(p)
								}
								else {
									ranksum `x' if `blockid' == `i', by(`T')
									`noi' di in bl  "p-value Wilcoxon rank-sum test " in ye "`x'" in bl " = "  round((1-normal(abs(r(z))))*2,.0001)
									sca `pv' = round((1-normal(abs(r(z))))*2,.0001)
								}
							}
							else {
								if `"`wilk'"' != `""' {
									ranksum `x' if `blockid' == `i', by(`T')
									`noi' di in bl  "p-value Wilcoxon rank-sum test " in ye "`x'" in bl " = "  round((1-normal(abs(r(z))))*2,.0001)
									sca `pv' = round((1-normal(abs(r(z))))*2,.0001)
								}
								if `"`median'"' != `""' {
									median `x' if `blockid' == `i', by(`T')
									`noi' di in bl  "p-value (continuity corrected) median comparison test " in ye "`x'" " = "  round(r(p_cc),.0001)
									sca `pv' = r(p_cc)
								}
								else {
									ttest `x' if `blockid' == `i', by(`T') 
									`noi' di in bl  "p-value mean comparison test " in ye "`x'" in bl " = "  round(r(p),.0001)
									sca `pv' = r(p)
								}
							}
						}
					}
					matrix `pvals'[`i',`cc'] = `pv'
					if `"`variance'"' != `""' {
						matrix `pvalsvar'[`i',`cc'] = `pv1'
					}
				}
				matrix `pvals'[`i',`K'+1] = `i'
				if `"`variance'"' != `""' {
					matrix `pvalsvar'[`i',`K'+1] = `i'
				}
			}
		}
		
		if (`"`summary'"'  !=  `""')  {
		    mat coln `pvals' = `X' stratumID
			ereturn matrix pvals = `pvals'
			mata: st_matrix("`fitness'",rowmin(st_matrix("e(pvals)")))
			mat coln `fitness' = minpval
			ereturn matrix fitness = `fitness'
			if (`"`variance'"' != `""') {
				mat coln `pvalsvar' = `X' stratumID
				ereturn matrix pvals_v = `pvalsvar'
				mata: st_matrix("`fitness1'",rowmin(st_matrix("e(pvals_v)")))
				mat coln `fitness1' = minpval
				ereturn matrix fitness_v = `fitness1'
			}
		}
		
		mat coln `strataN'  = Ntreated Ncontrols sum stratumID
		if (`"`revert'"' != `""') {
			mat coln `prstrata' = upper lower stratumID
		}
		if (`"`revert'"' == `""') {
			mat coln `prstrata' = lower upper stratumID
		}
		ereturn matrix scstrata = `prstrata'
		ereturn matrix strataN  = `strataN'
		
		tabulate `T' if `id' !=., matcell(`tot')
		mat rown `tot' =  Ncontrols Ntreated 
		ereturn matrix tot = `tot'
		
	}
	noi di ""
	noi di in ye "---------------------------------------------------"
	noi di in ye "---------------------------------------------------"
	noi di ""
	no di in bl "Total number of tests conducted = " in ye "`not'"
	noi di in ye "---------------------------------------------------"
	noi di in ye "---------------------------------------------------"
	
end
