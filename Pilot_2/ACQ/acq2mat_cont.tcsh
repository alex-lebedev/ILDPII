tcsh
cd /Users/alebedev/GitHub/ILDPII/Pilot_2/ACQ

set xx = `ls *acq`
foreach i($xx)
	acq2mat $i $i.mat
end

