% fam_max_like.m
%
% Created: 04/21/2024, Evan Layher (fam_max_like.m)
% Revised: 04/21/2024, Evan Layher
%
% Maximum likelihood estimates for ROC curves (aggregated data)

fclose('all'); clear all; clc % clear window and variables

mainDir = 'C:\Users\ealay\Box Sync\google_drive\ucsb\lab\face_audio_mem\';
dataDir = [mainDir, 'data\'];

rocCodeDir = 'C:\Users\ealay\Box Sync\google_drive\code\toolboxes\roc_toolbox-1.1.4\';

dataFile = [dataDir, 'face_aud_mem_resps_group.csv']; % input file
outDir = dataDir;
outFile = [outDir, 'fam_max_est.csv'];

if ~exist(dataFile, 'file')
    fprintf('MISSING FILE: %s\n', dataFile);
    return
end

addpath(genpath(rocCodeDir)); % ROC toolbox

% convert extreme threshold values
minT = -5; % minimum threshold
maxT = 5; % maximum threshold

head = 'cond,t1,t2,t3,a,b,da,az,d,vo';

data = tdfread(dataFile, ','); % read in data file

condLabs = {'aud-none','aud-familiar','aud-unfamiliar','img-none','img-familiar','img-unfamiliar'}; % output condition labels
totConds = length(condLabs); % total conditions per subject

model = 'uvsd';
parNames = {'Dprime' 'Vo'}; % mu, sigma (target Gaussian distribution)
fitStat = '-LL'; % -LL likelihood

fidOut = fopen(outFile, 'w'); % create output file
fprintf(fidOut, '%s\n', head); % print header to file
tmSt = tic; % start time

for i = 1:totConds % loop thru conditions
    oRs = [data.old1(i),data.old2(i),data.old3(i),data.old4(i)]; % old items
    nRs = [data.new1(i),data.new2(i),data.new3(i),data.new4(i)]; % new items

    [x0,LB,UB] = gen_pars(model,size(oRs,2),size(oRs,1),parNames); % reset parameters
    roc = roc_solver(oRs,nRs,model,fitStat,x0,LB,UB,'figure',false,'verbose',false); % max likelihood

    % organize values (thresholds are inputted backwards, multiply by -1)
    d = -1 * roc.uvsd_model.parameters.Dprime; % d'

    vo = roc.uvsd_model.parameters.Vo; % target distribution variance

    b = 1 ./ vo; % b'

    a = d .* b; % a'

    da = a ./ sqrt(0.5 * (1 + b .^ 2)); % da

    az = normcdf(da ./ sqrt(2)); % Az

    th = roc.uvsd_model.parameters.criterion;

    fprintf(fidOut,'%s,%.5f,%.5f,%.5f,%.5f,%.5f,%.5f,%.5f,%.5f,%.5f\n', ...
        condLabs{i}, th, a, b, da, az, d, vo);
end % for i = 1:totConds

fclose('all'); % close out files
fprintf('CREATED: %s\n',outFile); % alert user when file is created