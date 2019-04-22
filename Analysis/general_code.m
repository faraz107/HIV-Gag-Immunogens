%%
% CODE FOR FORMING THE SECTORS IN HIV GAG BY EMPLOYING THE Corr-ITSPCA
% MEHTOD TO ESTIMATE THE 9 STRONGEST PCs OF THE GAG SAMPLE CORRELATION
% MATRIX
% 
% Author: Ahmed Abdul Quadeer
% Editor: Syed Faraz Ahmed

%% Setting up paths (of functions and data files required) and necessary parameters

clear all;close all;clc

% Adding paths of required functions and datafiles
%run startup.m
addpath functions
addpath datafiles

% Setting font type and size

set(0,'DefaultAxesFontName','Arial')
set(0,'DefaultTextFontName','Arial')
set(0,'DefaultAxesFontSize',20)
set(0,'DefaultTextFontSize',20)

%% Pre-processing of the raw MSA of protein sequences 'msa'

[B,Bcap,lambda,site_freq_no_mutation,true_indices,freq_bin,prev_aa,N,M,ls] = preprocessing_gag2017(msa);

%%  Computing the maximum eigenvalue obtained in the null case

% Percentile to define the significant threshold

thresh = 1e-2;

% Number of random shuffles

N_shuffles = 500;

[lambda_max_rnd,pos_thresh,neg_thresh,max_pos_corr_max,max_neg_corr_max] = ...
    computing_lambda_max_rnd(Bcap,N_shuffles,thresh)

%Number of significant PCs

alpha = sum(lambda > lambda_max_rnd);

%% Forming sectors by employing Corr-ITSPCA

% Computing the sparsity threshold, gamma_k for Corr-ITSPCA

gamma_k = computing_gamma_k(alpha,lambda,N,M);

% Computing sparse principal components using Corr-ITSPCA

PC_roca = CorrITSPCA(Bcap, gamma_k, alpha);

% Forming sectors

[sec_eig_roca, sec_eig_roca_true, sec_eig_roca_incl_cs, length_sec_roca] = ...
    form_sectors_roca(PC_roca,alpha,site_freq_no_mutation,true_indices,ls);

[sec_eig_new, sec_eig_true_new, sec_eig_incl_cs_new, length_sec_new, alpha_new] = ...
    form_new_sectors(sec_eig_roca, sec_eig_roca_true, sec_eig_roca_incl_cs, length_sec_roca, alpha);

sec_eig_roca = sec_eig_new;
sec_eig_roca_true = sec_eig_true_new;
sec_eig_roca_incl_cs = sec_eig_incl_cs_new;
length_sec_roca = length_sec_new;
alpha = alpha_new;

n_secs_roca = length(sec_eig_roca);

%% Computing correlation matrices

C = corrcoef(Bcap); %sample correlation matrix
C_hat = compute_clean_C(Bcap,alpha,lambda); %cleaned standardized correlation matrix

%% Percentage overlap 

calculate_overlap(sec_eig_roca,sec_eig_pca,n_secs_roca,n_secs_pca);

%% Statistics of RoCA sectors

[mc,mean_abs_corr,per_neg_corr,per_pos_corr] = ...
    stats_sectors(C_hat,sec_eig_roca,n_secs_roca,freq_bin,pos_thresh,neg_thresh);
