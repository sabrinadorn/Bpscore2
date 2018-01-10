program define oneBoot, eclass
	* version 11.1
	syntax varlist[if] [in] [fweight iweight pweight], y(string)/*
	*/ [KSMirnov Ttest WIlk logit COMsup level(real 0.2) REscale by(real 0.5) REVert] 
	
	tokenize `varlist'
 
	loc T `1'
	
	loc hand_com = ""
	loc hand_logit = ""
	loc hand_rev = ""
	
	tempname blockid 
	tempname pscore
	
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
	
		loc test_hand = "tenf"
	
	}
	
	
	if `"`wilk'"'!= `""' {	
	
		loc test_hand = "wi"
	
	}
	
	
	if `"`rescale'"' == `""' {	
	
		loc cmd "qui pscore2 `varlist', blockid(`blockid') pscore(`pscore') `hand_com' `test_hand' level(`level') `rev'"
	
	}
	else{
	
		loc cmd  "qui pscore2 `varlist', blockid(`blockid') pscore(`pscore') `hand_com' `test_hand' level(`level') `rev' rescale by(`by')"
	
	}
	
	preserve
			
		bsample _N
		qui `cmd'
		qui atts `y' `T', pscore(`pscore') blockid(`blockid') 
		ereturn clear
		ereturn scalar att = r(atts)
		
	restore
		
	
end
