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

addpath functions
addpath datafiles

% Setting font type and size

set(0,'DefaultAxesFontName','Arial')
set(0,'DefaultTextFontName','Arial')
set(0,'DefaultAxesFontSize',20)
set(0,'DefaultTextFontSize',20)

%%

% Loading biochemical domains and immunological information
biodomain = biochemical_domains('Gag');

% Load data
load data_processed_gag
load data_shuffling_gag

%Number of significant PCs
alpha = 9;

% Loading P24 intrahexamer interface sites defined at 9 Angstrom mapped to
% HXB2 site ids
load('datafiles\sites_involved_in_interface_hexamer_p24_9.mat')
biodomain(3).sites = sites_involved_in_interface_hexamer+132;

% Loading Gag sites that are involved in HLA associated polymorphism
load('datafiles\hla_poly_sites.mat')
biodomain(8).name = 'HLA associated polymorphic sites';
biodomain(8).sites = hla_sites;

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

fprintf('\n-----------------------------------------------------------------------------------\n')
fprintf('Number of sectors formed: %d\n',n_secs_roca)
fprintf('-----------------------------------------------------------------------------------\n')

fprintf('\n-----------------------------------------------------------------------------------\n')
fprintf('Number of sites within Sector %d:  %d sites\n',[1:n_secs_roca; length_sec_roca])
fprintf('-----------------------------------------------------------------------------------\n')


fprintf('\n-----------------------------------------------------------------------------------\n')
fprintf('List of sites including conserved neighbors are in "sec_eig_roca_incl_cs" \n')
fprintf('-----------------------------------------------------------------------------------\n')

%% Computing correlation matrices

C = corrcoef(Bcap); %sample correlation matrix
C_hat = compute_clean_C(Bcap,alpha,lambda); %cleaned standardized correlation matrix

%% Statistical signficance of biochemical association of sectors

sec_asso_roca = zeros(1,length(biodomain)); %sector associated to a particular biochemical domain
pvalue_asso_roca = zeros(1,length(biodomain)); %statistical significance of the sector-biochemical domain association

for kk = 1:length(biodomain)
    [sec_asso_roca(kk),pvalue_asso_roca(kk)] = ...
        compute_association(biodomain(kk).sites,sec_eig_roca_incl_cs,sec_eig_roca_true,ls,M);
end

fprintf('\n-----------------------------------------------------------------------------------\n')
fprintf('Significance of inferred %s sectors using RoCA\n',protein)
fprintf('-----------------------------------------------------------------------------------\n')

for kk = 1:length(biodomain)
    fprintf('Sector %d is associated with %s (P = %.2e).\n',...
        sec_asso_roca(kk),biodomain(kk).name,pvalue_asso_roca(kk));
end


%% Pmatrix

for kk = 1:length(biodomain)
    for mm = 1:n_secs_roca
        [~,pvalue_asso(kk,mm)] = ...
            compute_association(biodomain(kk).sites,sec_eig_roca_incl_cs{mm},sec_eig_roca_true{mm},ls,M);
    end
end

Pmatrix = cell(length(biodomain)+1,n_secs_roca+1);
Pmatrix{1,1} = 'Association';
for kk = 2:size(Pmatrix,1)
    Pmatrix{kk,1} = biodomain(kk-1).name;
end
for kk = 2:size(Pmatrix,2)
    Pmatrix{1,kk} = sprintf('Sector%d',kk-1);
end

for kk = 2:size(Pmatrix,1)
    for mm = 2:size(Pmatrix,2)
        Pmatrix{kk,mm} = pvalue_asso(kk-1,mm-1);
    end
end

Pmatrix

