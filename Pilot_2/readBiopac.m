cd /Users/alebedev/GitHub/ILDPII/Pilot_2


subs = dir( 'ACQ/*.acq' );
subs={subs.name};


addpath('~/Desktop/MATLAB_DIR/my_functions/load_acq_20110222/')

for i = 1:length(subs)
    acq_raw = load_acq(['ACQ/' subs{i}])
    rdata = acq_raw.data;
    save(['ACQ/' subs{i} 'mat'], 'rdata')
end
