program define Bpscore2, eclass
  * version 11.1
  syntax varlist[if] [in] [fweight iweight pweight], y(string) reps(real) gen(string)/*
  */ [KSMirnov TTest WIlk logit COMsup level(real 0.2) REscale by(real 0.5) REVert/*
  */ SUBSampling siz(real 0.666666) CONTrols RANDboot subset(real 0.333333) NOIsily] 
 
  qui{
	
	tokenize `varlist'
 
	loc T `1'
	mac shift 
	loc X `*'
	
	loc hand_com = ""
	loc hand_logit = ""
	loc hand_rev = ""
	
	if `"`comsup'"' != `""' {	
	
		loc hand_com = "com"
	
	}
	
	if `"`logit '"' != `""' {	
	
		loc hand_logit = "logit"
	
	}
	
	if `"`revert '"' != `""' {	
	
		loc hand_rev = "rev"
	
	}
	
	if `"`ksmirnov'"' != `""' {	
	
		loc test_hand = "ksm"
	
	}  
	
	if `"`ttest'"'!= `""' {	
	
		loc test_hand = "tt"
	
	}
	
	
	if `"`wilk'"'!= `""' {	
	
		loc test_hand = "wi"
	
	}
	
	if  `"`wilk'"'!= `""'  & `"`ksmirnov'"' != `""' /*
	*/|| `"`wilk'"'!= `""'  & `"`ttest'"' != `""' /*
	*/|| `"`ttest'"' != `""' & `"`ksmirnov'"' != `""' /*
	*/|| `"`ttest'"' != `""' & `"`ksmirnov'"' != `""' & `"`wilk'"'!= `""'{
	
		di in red "invalid test type"
		error 198
	}
	
	
	gen double `gen' = .

	tempname timr1
	
	sca `timr1' = c(current_time)
	
	*noi di `timr1'
	
	//// STANDARD BOOTSTRAP (default) ////
	
	if `"`subsampling'"' == `""' & `"`controls'"' == `""' & `"`randboot'"' == `""' {	
	
		if `"`noisily'"' != `""' {	
			
			noi di ""
			noi di "Applying standard bootstrap"
			noi di ""
			
		}
		
		forv b = 1/`reps'{
		
		    if `"`noisily'"' != `""' {	
			
				noi di "Replication `b' of `reps'"
			
			}
			oneBoot `varlist', y(`y')/*
				*/`test_hand' `hand_logit' `hand_com'  level(`level') `hand_rev'
			
			replace `gen' = e(att) in `b'
		}
	}
	
	//// BOOTSTRAP FROM THE CONTROLS ////
	
	else if `"`controls'"' != `""' {	
	
		if `"`noisily'"' != `""' {	
		
			noi di ""
			noi di "Applying bootstrap from the controls"
			noi di ""
			
		} 
	
		// Save temp data on treated for repeated appending during resampling  
		// from the controls 
		
		preserve
	
			keep if `T' == 1
			save tmp1BOOTpsc2.dta, replace
	
		restore
	
		forv b = 1/`reps'{
		
			if `"`noisily'"' != `""' {	
			
				noi di "Replication `b' of `reps'"
			
			}
	
			oneBootCONTR `varlist', y(`y')/*
				*/`test_hand' `hand_logit' `hand_com'  level(`level') `hand_rev' 
		
			replace `gen' = e(att) in `b'
			
		}
	
	}
	
	//// SUBSAMPLING (draw a subset of all obs. without replacement) ////
	
	else if `"`subsampling'"' != `""' {	
	
		if `"`noisily'"' != `""' {	
			
			noi di ""
			noi di "Applying subsampling"
			noi di ""
			
		}
	
	
		forv b = 1/`reps'{
		
			if `"`noisily'"' != `""' {	
			
				noi di "Replication `b' of `reps'"
			
			}
	
			oneSubs `varlist', y(`y')/*
					*/`test_hand' `hand_logit' `hand_com'  level(`level') `hand_rev' siz(`siz')
		
			replace `gen' = e(att) in `b'
		}
	
	}
	
	//// RF-style ////
	
	else if `"`randboot'"' != `""' {	
	
		if `"`noisily'"' != `""' {	
			
			noi di ""
			noi di "Applying bootstrap with randomization"
			noi di ""
			
		}
	
		preserve
	
			keep `X'
			mata: X = st_data(.,.)
	
		restore
	
		mata: K = cols(X)
		
		forv b = 1/`reps'{
		
			if `"`noisily'"' != `""' {	
			
				noi di "Replication `b' of `reps'"
			
			}
	
			oneRandBoot `varlist', y(`y') subset(`subset')/*
				*/`test_hand' `hand_logit' `hand_com'  level(`level') `hand_rev' 
	
	
			replace `gen' = e(att) in `b'
			
		}
	
	}
	
	ereturn clear
	qui su `gen' 
	ereturn scalar att = r(mean)
  
  }
  
end
	
