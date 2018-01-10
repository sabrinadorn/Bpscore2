clear all
prog drop _all

glo path "/Users/sdorn/Dropbox/CV/links/Balancing/Bpscore2"

cd $path

use DWNSW.dta

glo X "age educ black hisp marr nodegr re75 re74 u74 u75"

/// pscore2 ///  

cap drop id 
cap drop pscore
pscore2 treat $X, blockid(id) pscore(pscore) tenf level(0.15) 
atts re78 treat, pscore(pscore) blockid(id) 

cap drop id 
cap drop pscore
pscore2 treat $X, blockid(id) pscore(pscore) ksm level(0.25) 
atts re78 treat, pscore(pscore) blockid(id) 

cap drop id 
cap drop pscore
pscore2 treat $X, blockid(id) pscore(pscore) tenf level(0.15) rescale by(0.99)
atts re78 treat, pscore(pscore) blockid(id) 

cap drop id 
cap drop pscore
pscore2 treat $X, blockid(id) pscore(pscore) ksm level(0.25) rescale by(0.9)
atts re78 treat, pscore(pscore) blockid(id) 

/// Bpscore2 ///  

set seed 1

glo bootreps = 100 // bootstrap replications

cap drop att_est
Bpscore2 treat $X, y(re78) reps($bootreps) gen(att_est) ksm level(0.25) 
di e(att)

cap drop att_est
Bpscore2 treat $X, y(re78) reps($bootreps) gen(att_est) ttest level(0.15) 
di e(att)

cap drop att_est
Bpscore2 treat $X, y(re78) reps($bootreps) gen(att_est) rand subset(0.2) noi ksm level(0.25) 
di e(att)

help Bpscore2
