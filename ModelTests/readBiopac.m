cd /Users/alebedev/GitHub/ILDP2_pilot
addpath('~/Desktop/MATLAB_DIR/my_functions/load_acq_20110222/')

acq_raw = load_acq('ILDP_Alex_pilot.acq'); % takes time
rdata = acq_raw.data;
save('ildp2_pilot_gsr.mat', 'rdata')
