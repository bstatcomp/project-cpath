# WD, libraries, and sourcing --------------------------------------------------
my_path <- dirname(rstudioapi::getSourceEditorContext()$path)
my_path <- strsplit(my_path, split = "/")[[1]]
my_path <- paste0(my_path[-c(length(my_path) - 1, length(my_path))], collapse = "/")
setwd(my_path)
library(loo)
source("./data/generalized_logistic_model/data.R")
source("./data/generalized_logistic_model/init.R")
source("./R/sample_from_exe.R")

# data input -------------------------------------------------------------------
in_data <- nlist(N,
                 P,
                 M,
                 IDp,
                 IDs,
                 AGE,
                 SEX,
                 COMED,
                 APOE4,
                 time,
                 S)
in_init <- nlist(theta_S0,
                 theta_r,
                 theta_SEX,
                 theta_AGE,
                 theta_APOE4_b,
                 theta_APOE4_r,
                 theta_COMED,
                 omega_pb,
                 omega_pr,
                 omega_sb,
                 omega_sr,
                 eta_pb,
                 eta_pr,
                 eta_sb,
                 eta_sr,
                 tau,
                 beta,
                 kel,
                 keq,
                 beta_bateman)

# run sampling -----------------------------------------------------------------
b <- sample_from_exe(data_list   = in_data, 
                     init_list   = in_init, 
                     num_samples = 10,
                     num_warmup  = 10)

# bad data ---------------------------------------------------------------------
source("./data/generalized_logistic_model/data_bad.R")
in_data <- nlist(N,
                 P,
                 M,
                 IDp,
                 IDs,
                 AGE,
                 SEX,
                 COMED,
                 APOE4,
                 time,
                 S)
b <- sample_from_exe(data_list   = in_data, 
                     init_list   = in_init, 
                     num_samples = 10,
                     num_warmup  = 10)
