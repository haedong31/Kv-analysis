%% ----- Code arguments & Model information ----- %%
clc
close all
clearvars
warning('off','all')

group = "wt";
exp_num = "exp1";
save_dir = fullfile(pwd,strcat("fusion_",exp_num),group);
mkdir(save_dir)

% currents to be included: iktof, iktos, ikslow1, ikslow2, ikss
current_names = {'iktof','ikslow1','ikslow2','ikss'};

tune_idx = cell(5,1);
tune_idx{1} = [1,2,3,4,5,7,11,13];
tune_idx{2} = [2,3];
tune_idx{3} = [1,2,4,5,9,10,11];
tune_idx{4} = [1,2,3];
tune_idx{5} = [1,2,3,4];

pdefault = cell(5,1);
pdefault{1} = [33,15.5,20,7,0.03577,0.06237,0.18064,0.3956,...
    0.000152,0.067083,0.00095,0.051335,0.3846];
pdefault{2} = [-1050,270,0.0629];
pdefault{3} = [22.5,45.2,40.0,7.7,5.7,0.0629,6.1,18,2.058,803.0,0.16];
pdefault{4} = [4912,5334,0.16];
pdefault{5} = [0.0862,1235.5,13.17,0.0611];

[mdl_struct, psize] = gen_mdl_struct(current_names, tune_idx);
[p0, lb, ub] = gen_param_bounds(mdl_struct, psize, pdefault);

holdv = -70;
ek = -91.1;
protocol{1} = holdv;
protocol{2} = ek;

%% ----- Optimization loop (populational) ----- %%
% meta data of experimental datasets
matching_table = readtable(fullfile(pwd,"mgat1ko_data",strcat("matching-table-",group,".xlsx")));
pop_data_dir = fullfile("mgat1ko_data",strcat(group,"-preprocessed"));
pop_file_names = matching_table.trace_file_name_4half;
pop_file_names(cellfun(@isempty,pop_file_names)) = []; % exclude null rows
pop_file_names = string(pop_file_names);
num_pop_files = length(pop_file_names);

protocol{3} = -30:10:50;
pop_ideal_holdt = 120; 
pop_ideal_endt = 4.6*1000;

% import entire set of experimental data
trace_data = cell(1,num_pop_files);
t = cell(1,num_pop_files);
holdt = cell(1,num_pop_files);
pulset = cell(1,num_pop_files);
for i=1:num_pop_files
    fpath = fullfile(pwd,pop_data_dir,pop_file_names(i));
    trace_data{i} = table2array(readtable(fpath));
    runningt = trace_data{i}(:,1);
    
    % estimate the critical time points 
    [~,ideal_hold_idx] = min(abs(runningt-pop_ideal_holdt));
    [~,ideal_end_idx] = min(abs(runningt-pop_ideal_endt));
    
    t{i} = runningt;
    holdt{i} = runningt(1:ideal_hold_idx);
    pulset{i} = runningt(ideal_hold_idx+1:end)-runningt(ideal_hold_idx+1);
end

% create data structure carrying actual data as well as all time protocol info
data_struct = cell2struct([trace_data;t;holdt;pulset],...
    ["trace_data","t","holdt","pulset"],1);

% optimization options for populational model
options = optimoptions(@fmincon, ...
    'Algorithm','interior-point','Display','off', ...
    'MaxFunctionEvaluations',1e+6,'SpecifyObjectiveGradient',false, ...
    'UseParallel',true);
A = [];
b = [];
Aeq = [];
beq = [];
nonlcon = [];

% obj_rmse_pop(p0,@kcurrent_basic,mdl_struct,pdefault,protocol,data_struct)
tic
opt_fun_pop = @(pp) obj_rmse_pop(pp,@kcurrent_basic,mdl_struct,pdefault,protocol,data_struct);
[pop_sol,fval] = fmincon(opt_fun_pop,p0,A,b,Aeq,beq,lb,ub,nonlcon,options);
save_sol(pop_sol,fullfile(save_dir,"pop_sol.xlsx"),mdl_struct,pdefault);
toc

%% ----- Optimization loop (individual) ----- %%
holdv = -70;
minv = 50;
vstep = 10;
num_steps = 1;
ek = -91.1;

protocol{1} = holdv;
protocol{2} = ek;
protocol{3} = minv:vstep:(minv+vstep*(num_steps-1));
idv_ideal_holdt = 470;
idv_ideal_endt = 25*1000;

idv_data_dir = fullfile(pwd,"mgat1ko_data",strcat(group,"-preprocessed-25s"));
idv_file_names = matching_table.trace_file_name_25;
idv_file_names(cellfun(@isempty,idv_file_names)) = []; % exclude null rows
idv_file_names = string(idv_file_names);
num_idv_files = length(idv_file_names);

options = optimoptions(@fmincon, ...
    'Algorithm','interior-point','Display','off', ...
    'MaxFunctionEvaluations',1e+6,'SpecifyObjectiveGradient',false, ...
    'UseParallel',true);
num_iters = 10;
lambda = 0.25;
outf = fopen(strcat(exp_num,"_",group,".txt"), 'w');
for i = 1:num_idv_files
    tic
    % read data
    running_file = idv_file_names(i);
    file_path = fullfile(idv_data_dir,running_file);
    trace_data = table2array(readtable(file_path));
    t = trace_data(:,1);

    % estimate the critical time points
    [~,ideal_hold_idx] = min(abs(t-idv_ideal_holdt));
    [~,ideal_end_idx] = min(abs(t-idv_ideal_endt));
    t = trace_data(1:ideal_end_idx,1);
    yksum = trace_data(1:ideal_end_idx,2:end);

    % save time information in the protocol
    protocol{4} = t;
    protocol{5} = t(1:ideal_hold_idx);
    protocol{6} = t(ideal_hold_idx+1:end)-t(ideal_hold_idx+1);
    
    % objective function
%     obj_rmse_idv(p0,pop_sol,0.1,@kcurrent_basic,mdl_struct,pdefault,protocol,yksum)
    opt_fun = @(p) obj_rmse_idv(p,pop_sol,lambda,@kcurrent_basic,mdl_struct,pdefault,protocol,yksum);

    % initial points
    init_pts = lhsdesign(num_iters,psize);
    init_pts = scale_param(init_pts,lb,ub);
    init_pts(1,:) = p0';

    sol_mx = NaN(num_iters,psize);
    rmse_list = NaN(num_iters,1);
    for j=1:num_iters
        try
            [sol,fval] = fmincon(opt_fun,init_pts(j,:)',A,b,Aeq,beq,lb,ub,nonlcon,options);
            sol_mx(j,:) = sol;
            rmse_list(j) = fval;
        catch msg
            disp(msg.message)
            rmse_list(j) = 1e+5;
        end
    end

    % print optimization results for the current file
    for j = 1:num_iters
        outs = sprintf('[File %i/%i] %s [Reps %i/%i] Min RMSE: %f',...
            i,num_idv_files,running_file,j,num_iters,rmse_list(j));
        fprintf(outf,'%s\n',outs);
        disp(outs)
    end
    
    % save the entire solution matrix
    save_path1 = fullfile(save_dir,strcat("raw_sol_",idv_file_names(i)));
    writematrix(sol_mx,save_path1);
    
    % best solution in terms of minimum RMSE
    [~,best_fit_idx] = min(rmse_list);
    best_sol = sol_mx(best_fit_idx,:);
    save_path2 = fullfile(save_dir,running_file);
    save_sol(best_sol,save_path2,mdl_struct,pdefault);
    toc
end
fclose(outf);
poolobj = gcp('nocreate');
delete(poolobj);

%% ----- Custom functions ----- %%
function [mdl_struct,psize] = gen_mdl_struct(current_names,tune_idx)
    num_currents = length(current_names);
    
    % index 1
    idx_info1 = cell(1,num_currents);
    for i = 1:num_currents
        switch current_names{i}
            case 'iktof'
                idx_info1{i} = tune_idx{1};
            case 'iktos'
                idx_info1{i} = tune_idx{2};
            case 'ikslow1'
                idx_info1{i} = tune_idx{3};
            case 'ikslow2'
                idx_info1{i} = tune_idx{4};
            case 'ikss'
                idx_info1{i} = tune_idx{5};
        end
    end

    % index 2
    idx_info2 = cell(1,num_currents);
    psize = 0;
    for i = 1: num_currents
        psize_old = psize;
        psize = psize + length(idx_info1{i});
        idx_info2{i} = (1+psize_old):(psize);
    end

    model_info = [current_names; idx_info1; idx_info2];
    field_names = ["name","idx1","idx2"];
    mdl_struct = cell2struct(model_info, field_names, 1);    
end

function [p0,lb,ub] = gen_param_bounds(mdl_struct,psize,pdefault)
    p0 = NaN(psize,1);
    lb = NaN(psize,1);
    ub = NaN(psize,1);    

    % lower bound
    lb_ktof = NaN(1,length(pdefault{1}));
    lb_ktof([1:4, 13]) = [-70, eps, eps, 1, eps];
    lb_ktof([5,6,10,12]) = pdefault{1}([5,6,10,12])*0.15;
    lb_ktof([7,8]) = pdefault{1}([7,8])*0.1;
    lb_ktof([9,11]) = pdefault{1}([9,11])*0.7;
    lb_ktos = pdefault{2};
    lb_ktos(1) = lb_ktos(1)*1.95;
    lb_ktos(2:end) = lb_ktos(2:end)*0.05;
    lb_kslow1 = [-70, -70, -70, 1, 1, eps, eps, eps, eps, 50+eps, eps];
    lb_kslow2 = [eps, 5000, eps];
    lb_kss = [eps, eps, eps, eps];

    % upper bound
    ub_ktof = NaN(1,length(pdefault{1}));
    ub_ktof([1:4, 13]) = [70, 40, 40, 14, 1];
    ub_ktof(5:12) = pdefault{1}(5:12)*1.95;
    ub_ktos = pdefault{2};
    ub_ktos(1) = ub_ktos(1)*0.05;
    ub_ktos(2:end) = ub_ktos(2:end)*1.95;
    ub_kslow1 = [50, 50, 50, 20, 20, 1, 30, 50, 20, 2000, 1];
    ub_kslow2 = [5000, 10000, 1];
    ub_kss = [1, 2000, 100, 1];

    % assign the bounds according to the model structure
    current_names = {mdl_struct.name};
    idx_info1 = {mdl_struct.idx1};
    idx_info2 = {mdl_struct.idx2};
    for i = 1:length(current_names)
        switch current_names{i}
            case 'iktof'
                p0(idx_info2{i}) = pdefault{1}(idx_info1{i});
                lb(idx_info2{i}) = lb_ktof(idx_info1{i});
                ub(idx_info2{i}) = ub_ktof(idx_info1{i});
            case 'iktos'
                p0(idx_info2{i}) = pdefault{2}(idx_info1{i});
                lb(idx_info2{i}) = lb_ktos(idx_info1{i});
                ub(idx_info2{i}) = ub_ktos(idx_info1{i});
            case 'ikslow1'
                p0(idx_info2{i}) = pdefault{3}(idx_info1{i});
                lb(idx_info2{i}) = lb_kslow1(idx_info1{i});
                ub(idx_info2{i}) = ub_kslow1(idx_info1{i});
            case 'ikslow2'
                p0(idx_info2{i}) = pdefault{4}(idx_info1{i});
                lb(idx_info2{i}) = lb_kslow2(idx_info1{i});
                ub(idx_info2{i}) = ub_kslow2(idx_info1{i});
            case 'ikss'
                p0(idx_info2{i}) = pdefault{5}(idx_info1{i});
                lb(idx_info2{i}) = lb_kss(idx_info1{i});
                ub(idx_info2{i}) = ub_kss(idx_info1{i});
        end
    end
end

function scaledp = scale_param(unitp,lb,ub)
    [num_pt, num_var] = size(unitp);
    scaledp = NaN(num_pt, num_var);
    for i = 1:num_pt
        scaledp(i, :) = unitp(i,:).*(ub-lb)' + lb';
    end
end

function save_sol(sol,save_path,mdl_struct,pdefault)
    current_names = string({mdl_struct.name});
    writematrix(current_names,save_path, 'Sheet','Parameters', 'Range','A1');
    
    plens = cellfun(@length,pdefault);
    max_plen = max(plens);
    sol_mx = NaN(max_plen,length(mdl_struct));
    for j = 1:length(mdl_struct)
        switch current_names{j}
            case "iktof"
                sol_kto = NaN(max_plen,1);
                sol_kto(1:plens(1)) = pdefault{1};
                sol_kto(mdl_struct(j).idx1) = sol(mdl_struct(j).idx2);
                sol_mx(:,j) = sol_kto;
            case "iktos"
                sol_ktos = NaN(max_plen,1);
                sol_ktos(1:plens(2)) = pdefault{2};
                sol_ktos(mdl_struct(j).idx1) = sol(mdl_struct(j).idx2);
                sol_mx(:,j) = sol_ktos;
            case "ikslow1"
                sol_kslow1 = NaN(max_plen,1);
                sol_kslow1(1:plens(3)) = pdefault{3};
                sol_kslow1(mdl_struct(j).idx1) = sol(mdl_struct(j).idx2);
                sol_mx(:,j) = sol_kslow1;
            case "ikslow2"
                sol_kslow2 = NaN(max_plen,1);
                sol_kslow2(1:plens(4)) = pdefault{4};
                sol_kslow2(mdl_struct(j).idx1) = sol(mdl_struct(j).idx2);
                sol_mx(:,j) = sol_kslow2;
            case "ikss"
                sol_kss = NaN(max_plen,1);
                sol_kss(1:plens(5)) = pdefault{5};
                sol_kss(mdl_struct(j).idx1) = sol(mdl_struct(j).idx2);
                sol_mx(:,j) = sol_kss;
        end
    end
    writematrix(sol_mx,save_path, 'Sheet','Parameters', 'Range','A2');
end

function z = obj_rmse_pop(p,kcurrent_model,mdl_struct,pdefault,protocol,data_struct)
    volts = protocol{3};
    num_files = length(data_struct); % i; # of subjects
    num_volts = length(volts); % k; # of measurements
    
    rmse_list = NaN(num_files,num_volts);
    for i=1:num_files
        yksum = data_struct(i).trace_data(:,2:end);
        protocol{4} = data_struct(i).t;
        protocol{5} = data_struct(i).holdt;
        protocol{6} = data_struct(i).pulset;

        hold_idx = length(protocol{5});
        for j=1:num_volts
            ymx = kcurrent_model(p,mdl_struct,pdefault,protocol,volts(j));
            yhat = sum(ymx,2);
            yj = yksum(:,j);
            rmse_list(i,j) = sqrt(mean((yj(hold_idx+1:end) - yhat(hold_idx+1:end)).^2));
        end
    end
    z = sum(rmse_list,'all');
end

function z = obj_rmse_idv(p,pp,l,kcurrent_model,mdl_struct,pdefault,protocol,yksum)
    volts = protocol{3};
    num_volts = length(volts);
    hold_idx = length(protocol{5});

    rmse_list = NaN(num_volts,1);
    for i = 1:num_volts
        yi = yksum(:,i);
        ymx = kcurrent_model(p, mdl_struct, pdefault, protocol, volts(i));
        yhat = sum(ymx,2);
        rmse_list(i) = sqrt(mean((yi(hold_idx+1:end) - yhat(hold_idx+1:end)).^2));
    end
    z = (1-l)*sum(rmse_list) + l*sum(abs(p-pp));
end
