load allsubjs_scored_dec.mat
load exp_rev_design.mat
load stim_orders_for_learning.mat
load EXPT.mat
isgood = EXPT.using;
isgood(2) = 0;
isgood(end) = 0;
%%
for i = 1:length(EXPT.subjects)
    orig_conds{i} = [];
    if EXPT.instructions{i} == 'X'
        tmp = conds{i};
        tmp2 = tmp;
        wh1 = find(tmp==0);
        wh2 = find(tmp==1);
        for j = 21:40
            if tmp(j)==1
                tmp2(j) = 0;
            elseif tmp(j)==0
                tmp2(j) = 1;
            elseif tmp(j)==2
                tmp2(j) = 3;
            end
        end
        for j = 61:length(tmp)
            if tmp(j)==1
                tmp2(j) = 0;
            elseif tmp(j)==0
                tmp2(j) = 1;
            elseif tmp(j)==2
                tmp2(j) = 3;
            end
        end
        orig_conds{i} = tmp2; % orig cs- = 0, orig cs+ = 1, origcs+ + shock = 2 , origcs- + shock = 3;
    elseif EXPT.instructions{i} == 'Y'
        tmp = conds_feedback{i};
        tmp2 = tmp;
        wh1 = find(tmp==0);
        wh2 = find(tmp==1);
        for j = revs{i}(1):revs{i}(2)-1
            if tmp(j)==1
                tmp2(j) = 0;
            elseif tmp(j)==0
                tmp2(j) = 1;
            elseif tmp(j)==2
                tmp2(j) = 3;
            end
        end
        for j = revs{i}(3):length(tmp)
            if tmp(j)==1
                tmp2(j) = 0;
            elseif tmp(j)==0
                tmp2(j) = 1;
            elseif tmp(j)==2
                tmp2(j) = 3;
            end
        end
        orig_conds{i} = tmp2; % orig cs- = 0, orig cs+ = 1, shock = 2;
    end
end
%%
wh_exp_A = [];
wh_exp_B = [];
wh_imp_A = [];
wh_imp_B = [];
for i = 1:length(EXPT.subjects) %issue w 1, 15, 49, 58
    if isgood(i) == 1
        if EXPT.instructions{i} == 'X'
            if EXPT.version{i} == 'A'
                wh_exp_A(end+1,1) = i;
            elseif EXPT.version{i} == 'B'
                wh_exp_B(end+1,1) = i;
            end
        elseif EXPT.instructions{i} == 'Y'
            if EXPT.version{i} == 'A'
                wh_imp_A(end+1,1) = i;
            elseif EXPT.version{i} == 'B'
                wh_imp_B(end+1,1) = i;
            end
        end
    end
end
all_exp_subjs = sort([wh_exp_A; wh_exp_B]);
all_imp_subjs = sort([wh_imp_A; wh_imp_B]);
%%
clear subjs_using*
Aconds_all = orig_conds{wh_imp_A(3)};
whUSA = find(Aconds_all>1);
whrevA = whUSA(5:4:16);

Bconds_all = orig_conds{wh_imp_B(3)};
whUSB = find(Bconds_all>1);
whrevB = whUSB(5:4:16);

% CHANGE THIS FOR WHOLE TASK
%lasttrialA = whrevA(2)-1;
%lasttrialB = whrevB(2)-1;
lasttrialA = 80;
lasttrialB = 80;


for i = 1:length(all_imp_subjs)
    if EXPT.version{all_imp_subjs(i)}=='A'
        if length(scr_sqrt_normUS{all_imp_subjs(i)})==80
            subjs_using_stims{i} = order_A(1:lasttrialA,:);
        else
            subjs_using_stims{i} = order_A(81-length(scr_sqrt_normUS{all_imp_subjs(i)}):lasttrialA,:);
        end
    elseif EXPT.version{all_imp_subjs(i)}=='B'
        if length(scr_sqrt_normUS{all_imp_subjs(i)})==80
            subjs_using_stims{i} = order_B(1:lasttrialB,:);;
        else
            subjs_using_stims{i} = order_A(81-length(scr_sqrt_normUS{all_imp_subjs(i)}):lasttrialB,:);
        end
    end
    subjs_using_dat{i} = scr_sqrt_normUS{all_imp_subjs(i)}(1:length(subjs_using_stims{i}))';
    wh_shock = find(subjs_using_stims{i}(:,3)==1);
    subjs_using_dat_noshock{i} = subjs_using_dat{i}; subjs_using_dat_noshock{i}(wh_shock) = [];
end
revvec = [ones(20,1); ones(20,1) * -1; ones(20,1); ones(20,1)* -1];