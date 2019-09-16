## WD, libraries, and sourcing -------------------------------------------------
my_path <- dirname(rstudioapi::getSourceEditorContext()$path)
my_path <- strsplit(my_path, split = "/")[[1]]
my_path <- paste0(my_path[-c(length(my_path), length(my_path))], 
                  collapse = "/")
setwd(my_path)
library(loo)
library(rstan)
library(mcmcse)
# source("./R/sampling_gpu.R")
# source("./R/df_to_list.R")
library(GLMCPath)

## data input ------------------------------------------------------------------
niter   <- 20
in_data <- read.csv("./data/generalized_logistic_model/data_for_stan_model_cov_smallset2.csv")

# add placebo column
is_pbo  <- rep(0, nrow(in_data))
is_pbo[in_data$IDs %in% c(13,14,15,16)]  <- 1
in_data <- cbind(in_data[ ,-1], "placebo" = is_pbo)

head(in_data)


## 1 ---------------------------------------------------------------------------
## normal
exe_samps        <- sampling_gpu(df             = in_data,
                                 SubjectIdVar   = IDp,
                                 StudyIdVar     = IDs,
                                 TimeVar        = TIME,
                                 ScoreVar       = ADAS,
                                 is_pbo         = placebo,
                                 CovariatesR    = ~ AGE + SEX,
                                 CovariatesB    = ~ COMED,
                                 num_samples    = niter / 2,
                                 num_warmup     = niter / 2,
                                 seed           = 1,
                                 gpu_enabled    = 1)
exe_samps$maps
stan_mod <- exe_samps$stan_model
stan_mod
traceplot(stan_mod)


## 2 ---------------------------------------------------------------------------
## no covariates
exe_samps        <- sampling_gpu(df             = in_data,
                                 SubjectIdVar   = IDp,
                                 StudyIdVar     = IDs,
                                 TimeVar        = TIME,
                                 ScoreVar       = ADAS,
                                 is_pbo         = placebo,
                                 num_samples    = niter / 2,
                                 num_warmup     = niter / 2,
                                 seed           = 1,
                                 gpu_enabled    = 1)
exe_samps$maps
stan_mod <- exe_samps$stan_model
stan_mod
traceplot(stan_mod)


## 3 ---------------------------------------------------------------------------
## multiplicative
exe_samps        <- sampling_gpu(df             = in_data,
                                 SubjectIdVar   = IDp,
                                 StudyIdVar     = IDs,
                                 TimeVar        = TIME,
                                 ScoreVar       = ADAS,
                                 is_pbo         = placebo,
                                 CovariatesR    = ~ AGE + SEX,
                                 CovariatesB    = ~ COMED,
                                 num_samples    = niter / 2,
                                 num_warmup     = niter / 2,
                                 seed           = 1,
                                 gpu_enabled    = 1,
                                 m_r            = 1,
                                 m_b            = 1,
                                 mod_name       = "generalized_logistic_model")
exe_samps$maps
stan_mod <- exe_samps$stan_model
stan_mod
traceplot(stan_mod)
