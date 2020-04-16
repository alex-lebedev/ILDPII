tcsh
cd /Users/alebedev/GitHub/ILDPII/HUD/GSR

set xx = `ls *acq`
foreach i($xx)
	acq2mat $i $i.mat
end

