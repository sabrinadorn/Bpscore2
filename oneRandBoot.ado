program define oneRandBoot, eclass
	* version 11.1
	* data last modified: 11.09.17
	syntax varlist [if] [in] [fweight iweight pweight], y(string) subset(real )/*
	*/ [KSMirnov Ttest WIlk logit COMsup level(real 0.2) REscale by(real 0.5) REVert] 
	* subset(real 0.333)
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
	
	mata: Z = X
	mata: kk = cols(X)
	noi noi mata: kk
	mata: take = round((1-strtoreal(st_local("subset")))*kk)
	mata: k = jumble(1::K)[1::take]
	mata: Z = X[,k]
	
	noi mata: k
	noi mata: Z
	
	
	if `"`rescale'"' == `""' {	
	
		glo cmd "pscore2 `T' ZZZ*, blockid(`blockid') pscore(`pscore') `hand_com' `test_hand' level(`level') `rev'"
	
	}
	else{
		
		glo cmd "pscore2 `T' ZZZ*, blockid(`blockid') pscore(`pscore') `hand_com' `test_hand' level(`level') `rev' rescale by(`by')"
	
	}
	
	
	preserve
		
		keep `T' `y'
		
		mata: st_matrix("ZZZ",Z)
		svmat ZZZ
		
		bsample _N
		
		$cmd
		qui atts `y' `T', pscore(`pscore') blockid(`blockid') 
		ereturn clear
		ereturn scalar att = r(atts)
		
	restore
	
end
