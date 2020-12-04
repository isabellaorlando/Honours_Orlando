clear
S.paths.project     = '/Users/claireocallaghan/Desktop/PD_Atomoxetine_HRV/PreProcessing';
S.paths.data        = fullfile(S.paths.project,'data');
S.paths.analysis    = fullfile(S.paths.project,'analysis');
filelist = rdir(fullfile(S.paths.data,'*.mat')); filelist = {filelist.name}';

mkdir(fullfile(S.paths.analysis,'Figures_QC'));
cd(S.paths.analysis)

% --------------------
% Initialise settings
% ---------------------

HRVparams = InitializeHRVparams('hrvLC'); % include the project name
HRVparams.Fs            = 1000; % Sampling Frequency in Hz
HRVparams.writedata     = fullfile(S.paths.analysis,'HRV_Output');

%Global settings (window size)
HRVparams.windowlength = 600;	      % Default: 300, seconds
HRVparams.increment = 1;             % Default: 30, seconds increment
HRVparams.numsegs = 5;                % Default: 5, number of segments to collect with lowest HR
HRVparams.RejectionThreshold = .20;   % Default: 0.2, amount (%) of data that can be rejected before a
                                      % window is considered too low quality for analysis
HRVparams.MissingDataThreshold = .30; % Default: 0.15, maximum percentage of data allowable to be missing
                                      % from a window .15 = 15%
                              
%Debug settings
HRVparams.rawsig        = 1;           % Load raw signal if it is available for debugging
HRVparams.debug         = 1;

%SQI Analysis Settings
HRVparams.sqi.LowQualityThreshold = 0.3; % Default: 0.9, Threshold for which SQI represents good data
HRVparams.sqi.windowlength = 20;         % Default: 10, seconds, length of the comparison window
HRVparams.sqi.increment = 1;             % Default: 1, seconds
HRVparams.sqi.TimeThreshold = 0.1;       % Default: 0.1, seconds
HRVparams.sqi.margin = 2;                % Default: 2, seconds, Margin time not include in comparison

%Preprocess Settings
HRVparams.preprocess.figures    = 1;                   % Figures on = 1, Figures off = 0

%AF Detection Settings and PVC detection
HRVparams.af.on = 1;              % Default: 1, AF Detection On or Off
HRVparams.af.windowlength = 30;   % Default: 30, Set to include ~30 beats in each window
HRVparams.af.increment = 30;      % Default: 30, No overlap necessary in AF feat calc

HRVparams.PVC.qrsth = 0.1;        % Default: 0.1, threshold for qrs detection

%Time Domain Analysis Settings
HRVparams.timedomain.on = 1;             % Default: 1, Time Domain Analysis 1=On or 0=Off
HRVparams.timedomain.dataoutput = 1;     % 1 = Print results to .txt file
                                         % Anything else = utputs to return variables only
                                         % returned variables
HRVparams.timedomain.alpha = 50   ;      % Default: 50 ,In msec
HRVparams.timedomain.win_tol = .20;      % Default: .15, Maximum percentage of data allowable 
                                         % to be missing from a window
                                         
%Frequency Domain Analysis Settings
HRVparams.freq.on = 1;                   % Default: 1, Frequency Domain Analysis 1=On or 0=Off
HRVparams.freq.plot_on = 0;

%SDANN and SDNNI Analysis Settings
HRVparams.sd.on = 1;                        % Default: 1, SD analysis 1=On or 0=Off
HRVparams.sd.segmentlength = 300;           % Default: 300, windows length in seconds

%PRSA Analysis Settings
HRVparams.prsa.on = 1;             % Default: 1, PRSA Analysis 1=On or 0=Off

%Peak Detection Settings
% The following settings are for jqrs.m
%Using all defaults

%Entropy Settings
% Multiscale Entropy
HRVparams.MSE.on = 1;                      % Default: 1, MSE Analysis 1=On or 0=Off

% SampEn an ApEn 
HRVparams.Entropy.on = 1;                  % Default: 1, MSE Analysis 1=On or 0=Off

%DFA Settings
HRVparams.DFA.on = 1;                      % Default: 1, DFA Analysis 1=On or 0=Off

%Poincaré plot
HRVparams.poincare.on = 1;     % Default: 1, Poincare Analysis 1=On or 0=Off

%Heart Rate Turbulence (HRT) - Settings
HRVparams.HRT.on = 0;                        % Default: 1, HRT Analysis 1=On or 0=Off
HRVparams.HRT.GraphOn = 0;

%Output Settings
HRVparams.gen_figs = 1;
HRVparams.save_figs = 1;            % Save generated figures
HRVparams.output.separate = 0;          % Default : 1 = separate files for each subject
                                        %           0 = all results in one file
if HRVparams.save_figs == 1
    HRVparams.gen_figs = 1;
end                                        


% --------------------------
% Loop through all subjects
% -------------------------
for iSub = 1:numel(filelist)
    % --------------------------
    % 1. Load subject's ecg data 
    % -------------------------
    [~,subID] =fileparts(filelist{iSub});
    load(filelist{iSub});
    ecgSignal = data;
    
    % PVC classification
    PVCs = PVC_detect(ecgSignal,iSub,HRVparams);
    
    % ------------------------------------------------------------------
    % 2. Analyze data using HRV PhysioNet Cardiovascular Signal Toolbox
    % ------------------------------------------------------------------
    [results, resFilename] = Main_HRV_Analysis(ecgSignal,[],'ECGWaveform',...
                                                HRVparams,subID);                                       
                                     
    % Save NN plot
    title(['NNplot ' regexprep(subID,'_','-')]);
    set(gcf,'Position',[100 100 4000 600]);
    f_out = fullfile(S.paths.analysis,'Figures_QC',[subID '_NNplot.pdf']);
    export_fig(f_out,'-q201','-transparent','-append'); 
    
    
    
     % Plot detected PVCs
    if HRVparams.gen_figs
        figure(1)
        plot(ecgSignal,'LineWidth',1)
        xlabel('PVCs detected','FontSize',16)
        ylabel('mV','FontSize',16)
        hold on 
        plot(PVCs, ecgSignal(PVCs), 'o','MarkerSize',8);
    end
    
     % Save PVC plot
    title(['PVCplot ' regexprep(subID,'_','-')]);
    set(gcf,'Position',[0 0 1980 1000]);
    f_out = fullfile(S.paths.analysis,'Figures_QC',[subID '_PVCplot.pdf']);
    export_fig(f_out,'-q201','-transparent');
                                                                        
    
    % -------------------------------------------------
    % 3. Load QRS annotation saved by Main_HRV_Analysis to plot ECG and
    % Peak detection
    % -------------------------------------------------
    annotName = strcat(HRVparams.writedata, filesep, 'Annotation',filesep,...
                                                        {subID});
    jqrs_ann = read_ann( annotName{1} , 'jqrs');
    wqrs_ann = read_ann( annotName{1} , 'wqrs');

    % For visualisation purposes recompute bsqi
    [sqijw, StartIdxSQIwindows] = bsqi(jqrs_ann,wqrs_ann,HRVparams);

    % time vector for visualization (in seconds)
    time = cumsum(repmat(1/HRVparams.Fs,size(ecgSignal,1),1));

    % Plot detected beats
    if HRVparams.gen_figs
        Plot_SignalDetection_SQI(time, ecgSignal, jqrs_ann, sqijw,'ECG')
     end
     
     % Save plot to file
    title([regexprep(subID,'_','-')]);
    set(gcf,'Position',[0 0 1980 1000]);
    f_out = fullfile(S.paths.analysis,'Figures_QC',[subID '_ECGplot.pdf']);
    export_fig(f_out,'-q201','-transparent');
    close all;
end

% Collate all plots in a single pdf
f_pdfs = rdir(fullfile(S.paths.analysis,'Figures_QC/*.pdf')); f_pdfs = {f_pdfs.name}';
f_out  = fullfile(S.paths.analysis,'Figures_QC/QC_plots.pdf');
append_pdfs(f_out,f_pdfs{:});

