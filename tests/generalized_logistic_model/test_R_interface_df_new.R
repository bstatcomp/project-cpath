# WD, libraries, and sourcing --------------------------------------------------
my_path <- dirname(rstudioapi::getSourceEditorContext()$path)
my_path <- strsplit(my_path, split = "/")[[1]]
my_path <- paste0(my_path[-c(length(my_path) - 1, length(my_path))], collapse = "/")
setwd(my_path)
library(loo)
source("./data/generalized_logistic_model/init.R")
source("./R/sample_from_exe_new.R")
source("./R/df_to_list_new.R")

# data input -------------------------------------------------------------------
in_data <- readRDS("./data/generalized_logistic_model/data_df.rds")
set.seed(1)
is_pbo  <- sample(c(0,1), nrow(in_data2), replace = TRUE)
is_pbo2 <- sample(c(0,1), nrow(in_data2), replace = TRUE)
in_data <- cbind(in_data, "placebo" = is_pbo, "placebo2" = is_pbo2)
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
b <- sample_from_exe_new(df             = in_data,
                         SubjectIdVar   = IDp,
                         StudyIdVar     = IDs,
                         TimeVar        = time,
                         ScoreVar       = S,
                         is_pbo         = placebo,
                         CovariatesR    = ~ AGE + SEX,
                         CovariatesB    = ~ COMED,
                         init_list      = in_init,
                         num_samples    = 10,
                         num_warmup     = 10)
b$N
b$P
b$M
b$Q_r
b$Q_m
b$multiplicative_s


# no covariates ----------------------------------------------------------------
b <- sample_from_exe_new(df             = in_data,
                         SubjectIdVar   = IDp,
                         StudyIdVar     = IDs,
                         TimeVar        = time,
                         ScoreVar       = S,
                         is_pbo         = placebo,
                         m_b            = 1,
                         CovariatesR    = NULL,
                         CovariatesB    = ~ COMED,
                         init_list      = in_init,
                         num_samples    = 10,
                         num_warmup     = 10)
b$N
b$P
b$M
b$Q_r
b$Q_m
b$multiplicative_s

# two scores -------------------------------------------------------------------
in_data2 <- cbind(in_data, "S2" = in_data$S)

b <- sample_from_exe_new(df             = in_data2, 
                         SubjectIdVar   = IDp,
                         StudyIdVar     = IDs,
                         TimeVar        = time,
                         ScoreVar       = S,
                         is_pbo         = placebo,
                         CovariatesR    = NULL,
                         CovariatesB    = ~ COMED,
                         ScoreVar2      = S2,
                         is_pbo2        = placebo2,
                         CovariatesR2   = ~ AGE + COMED,
                         CovariatesB2   = ~ COMED,
                         init_list      = in_init, 
                         num_samples    = 10,
                         num_warmup     = 10)
b$N
b$P
b$M
b$Q_r
b$Q_m
b$multiplicative_s
b$Q_m2
b$Q_m
b$Q_r2


# NAs in scores ----------------------------------------------------------------
ind1     <- sample(1:nrow(in_data2), size = nrow(in_data2) * 0.2)
ind2     <- sample(1:nrow(in_data2), size = nrow(in_data2) * 0.3)
in_data2$S[ind1]  <- NA
in_data2$S2[ind2] <- NA

b <- sample_from_exe_new(df             = in_data2, 
                         SubjectIdVar   = IDp,
                         StudyIdVar     = IDs,
                         TimeVar        = time,
                         ScoreVar       = S,
                         is_pbo         = placebo,
                         CovariatesR    = NULL,
                         CovariatesB    = ~ COMED,
                         ScoreVar2      = S2,
                         is_pbo2        = placebo2,
                         CovariatesR2   = ~ AGE + COMED,
                         CovariatesB2   = ~ COMED,
                         init_list      = in_init, 
                         num_samples    = 10,
                         num_warmup     = 10)
b$N
b$P
b$M
b$Q_r
b$Q_m
b$multiplicative_s
b$Q_m2
b$Q_m
b$Q_r2
b$N2
dim(b$X_r)
dim(b$X_r2)


# NAs in scores + missing data in covariates -----------------------------------
in_data3 <- in_data2
ind1     <- sample(1:nrow(in_data3), size = nrow(in_data3) * 0.4)
in_data3$COMED[ind1] <- NA


b <- sample_from_exe_new(df             = in_data3, 
                         SubjectIdVar   = IDp,
                         StudyIdVar     = IDs,
                         TimeVar        = time,
                         ScoreVar       = S,
                         is_pbo         = placebo,
                         CovariatesR    = NULL,
                         CovariatesB    = ~ COMED,
                         ScoreVar2      = S2,
                         is_pbo2        = placebo2,
                         CovariatesR2   = ~ AGE + COMED,
                         CovariatesB2   = ~ COMED,
                         init_list      = in_init, 
                         num_samples    = 10,
                         num_warmup     = 10)


# NAs in scores + missing data in covariates -----------------------------------
in_data4 <- in_data2
ind1     <- sample(1:nrow(in_data4), size = nrow(in_data4) * 0.4)
in_data4$AGE[ind1] <- NA


b <- sample_from_exe_new(df             = in_data4, 
                         SubjectIdVar   = IDp,
                         StudyIdVar     = IDs,
                         TimeVar        = time,
                         ScoreVar       = S,
                         is_pbo         = placebo,
                         CovariatesR    = NULL,
                         CovariatesB    = ~ COMED,
                         ScoreVar2      = S2,
                         is_pbo2        = placebo2,
                         CovariatesR2   = ~ AGE + COMED,
                         CovariatesB2   = ~ COMED,
                         init_list      = in_init, 
                         num_samples    = 10,
                         num_warmup     = 10)


# NAs in scores + missing data in covariates -----------------------------------
in_data4 <- in_data2
ind1     <- sample(1:nrow(in_data4), size = nrow(in_data4) * 0.4)
in_data4$IDp[ind1] <- NA


b <- sample_from_exe_new(df             = in_data4, 
                         SubjectIdVar   = IDp,
                         StudyIdVar     = IDs,
                         TimeVar        = time,
                         ScoreVar       = S,
                         is_pbo         = placebo,
                         CovariatesR    = NULL,
                         CovariatesB    = ~ COMED,
                         ScoreVar2      = S2,
                         is_pbo2        = placebo2,
                         CovariatesR2   = ~ AGE + COMED,
                         CovariatesB2   = ~ COMED,
                         init_list      = in_init, 
                         num_samples    = 10,
                         num_warmup     = 10)


