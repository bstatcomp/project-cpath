# WD, libraries, and sourcing --------------------------------------------------
my_path <- dirname(rstudioapi::getSourceEditorContext()$path)
my_path <- strsplit(my_path, split = "/")[[1]]
my_path <- paste0(my_path[-c(length(my_path) - 1, length(my_path))], collapse = "/")
setwd(my_path)
library(loo)
library(tidyr)
source("./data/generalized_logistic_model/init.R")
source("./R/sample_from_exe.R")
source("./R/df_to_list.R")

# data input -------------------------------------------------------------------
in_data <- readRDS("./data/generalized_logistic_model/data_df.rds")
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
b <- sample_from_exe(df             = in_data, 
                     SubjectIdVar   = IDp,
                     StudyIdVar     = IDs,
                     TimeVar        = time,
                     ScoreVar       = S,
                     SecondScoreVar = NULL,
                     CovariatesX    = ~ AGE + SEX,
                     CovariatesY    = ~ COMED,
                     CovariatesZ    = NULL,
                     covariates     = ~ SEX + AGE + APOE4 + COMED,
                     init_list      = in_init, 
                     num_samples    = 10,
                     num_warmup     = 10)

# # Not all initialization values
# tmp_init      <- in_init
# tmp_init[[1]] <- NULL
# b <- sample_from_exe(df          = in_data, 
#                      covariates  = ~ SEX + AGE + APOE4 + COMED,
#                      init_list   = tmp_init, 
#                      num_samples = 10,
#                      num_warmup  = 10)
# 
# tmp_init      <- in_init
# tmp_init[[1]] <- NULL
# b <- sample_from_exe(df          = in_data, 
#                      covariates  = ~ SEX + AGE + APOE4 + COMED,
#                      num_samples = 10,
#                      num_warmup  = 10)