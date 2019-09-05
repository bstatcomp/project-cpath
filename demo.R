## WD, libraries, and sourcing -------------------------------------------------
my_path <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(my_path)
library(loo)
library(rstan)
library(mcmcse)
source("./R/sample_from_exe_new.R")
source("./R/df_to_list_new.R")


## data input ------------------------------------------------------------------
set.seed(1)
niter   <- 20
in_data <- read.csv("./data/generalized_logistic_model/data_for_stan_model_cov_smallset2.csv")
is_pbo  <- rep(0, nrow(in_data))
is_pbo[in_data$IDs %in% c(13,14,15,16)]  <- 1
in_data <- cbind(in_data, "placebo" = is_pbo)
head(in_data)

## 1 ---------------------------------------------------------------------------
## normal
exe_samps <- sample_from_exe_new(df             = in_data,
                                 SubjectIdVar   = IDp,
                                 StudyIdVar     = IDs,
                                 TimeVar        = TIME,
                                 ScoreVar       = ADAS,
                                 is_pbo         = placebo,
                                 CovariatesR    = ~ AGE + SEX,
                                 CovariatesB    = ~ COMED,
                                 num_samples    = niter / 2,
                                 num_warmup     = niter / 2,
                                 seed           = 1)
exe_samps$maps
stan_mod <- exe_samps$stan_model
plot(stan_mod)
traceplot(stan_mod)
