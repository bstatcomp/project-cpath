## WD --------------------------------------------------------------------------

## !!!
## If not using RStudio, change the working directory by hand to the root
## of the project folder, that contains the "tests" folder.
## Use setwd("path_to_folder").
## If using RStudio, the below code will automatically set the path, relative
## to the path of this script.
my_path <- dirname(rstudioapi::getSourceEditorContext()$path)
my_path <- strsplit(my_path, split = "/")[[1]]
my_path <- paste0(my_path[-c((length(my_path) - 2):(length(my_path)))],
                  collapse = "/")
setwd(my_path)

# libraries and sourcing -------------------------------------------------------
library(loo)
library(rstan)



## functions -------------------------------------------------------------------
reindex <- function  (x) {
  new_inds <- x
  for (i in 1:length(x)) {
    new_inds[i] <- which(x[i] == unique(x))
  }
  return (new_inds)
}


## data input ------------------------------------------------------------------
set.seed(1)
niter   <- 100
in_data <- read.csv("./data/generalized_logistic_model/data_for_stan_model_cov_smallset2.csv")
is_pbo  <- rep(0, nrow(in_data))
# is_pbo[in_data$IDs %in% c(13,14,15,16)]  <- 1
is_pbo[in_data$IDs %in% c(15)]  <- 1
in_data <- cbind(in_data, "placebo" = is_pbo)


## 1 ---------------------------------------------------------------------------
## normal
exe_samps        <- sampling_gpu2(df             = in_data,
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
exe_samps$data_used$N
exe_samps$data_used$Q_r
head(exe_samps$data_used$X_r)
head(in_data[ ,c("AGE", "SEX")])
exe_samps$data_used$is_pbo


## 2 ---------------------------------------------------------------------------
## Placebos at different studies
tmp_data <- in_data
is_pbo   <- rep(0, nrow(tmp_data))
is_pbo[tmp_data$IDs %in% c(2,5,15,16)]  <- 1
tmp_data$placebo <- is_pbo

exe_samps        <- sampling_gpu(df             = tmp_data,
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
exe_samps$data_used$is_pbo


## 3 ---------------------------------------------------------------------------
## No covariates
exe_samps        <- sampling_gpu(df             = in_data,
                                 SubjectIdVar   = IDp,
                                 StudyIdVar     = IDs,
                                 TimeVar        = TIME,
                                 ScoreVar       = ADAS,
                                 is_pbo         = placebo,
                                 num_samples    = niter / 2,
                                 num_warmup     = niter / 2,
                                 seed           = 1)
exe_samps$data_used$Q_r
head(exe_samps$data_used$Q_r)


## 4 ---------------------------------------------------------------------------
## Non-empty intersection between core attributes and covariates
exe_samps        <- sampling_gpu(df             = in_data,
                                 SubjectIdVar   = IDp,
                                 StudyIdVar     = IDs,
                                 TimeVar        = TIME,
                                 ScoreVar       = ADAS,
                                 CovariatesR    = ~ AGE + ADAS,
                                 is_pbo         = placebo,
                                 num_samples    = niter / 2,
                                 num_warmup     = niter / 2,
                                 seed           = 1)

## 5 ---------------------------------------------------------------------------
## Non-existing column.
exe_samps        <- sampling_gpu(df             = in_data,
                                 SubjectIdVar   = IDp,
                                 StudyIdVar     = sID,
                                 TimeVar        = TIME,
                                 ScoreVar       = ADAS,
                                 CovariatesR    = ~ AGE + ADAS,
                                 is_pbo         = placebo,
                                 num_samples    = niter / 2,
                                 num_warmup     = niter / 2,
                                 seed           = 1)

## 5 ---------------------------------------------------------------------------
## Non-existing covariate.
exe_samps        <- sampling_gpu(df             = in_data,
                                 SubjectIdVar   = IDp,
                                 StudyIdVar     = IDs,
                                 TimeVar        = TIME,
                                 ScoreVar       = ADAS,
                                 CovariatesR    = ~ AGE + MMM,
                                 is_pbo         = placebo,
                                 num_samples    = niter / 2,
                                 num_warmup     = niter / 2,
                                 seed           = 1)

## 6 ---------------------------------------------------------------------------
## Two full scores
tmp_data    <- in_data
tmp_data$S2 <- tmp_data$ADAS
exe_samps        <- sampling_gpu(df             = tmp_data,
                                 SubjectIdVar   = IDp,
                                 StudyIdVar     = IDs,
                                 TimeVar        = TIME,
                                 ScoreVar       = ADAS,
                                 is_pbo         = placebo,
                                 CovariatesR    = ~ AGE + SEX,
                                 CovariatesB    = ~ COMED,
                                 ScoreVar2      = S2,
                                 is_pbo2        = placebo,
                                 CovariatesR2   = ~ AGE,
                                 CovariatesB2   = ~ COMED + SEX,
                                 num_samples    = niter / 2,
                                 num_warmup     = niter / 2,
                                 seed           = 1)
exe_samps$data_used$N
exe_samps$data_used$N2
exe_samps$data_used$Q_r
exe_samps$data_used$Q_r2
head(exe_samps$data_used$X_r)
head(exe_samps$data_used$X_r2)


## 7 ---------------------------------------------------------------------------
## Two scores with NAs in each
tmp_data    <- in_data
tmp_data$S2 <- tmp_data$ADAS
tmp_data[tmp_data$IDs %in% c(2,3,4,5), ]$ADAS <- NA
tmp_data[tmp_data$IDs == 15, ]$S2  <- NA
exe_samps        <- sampling_gpu(df             = tmp_data,
                                 SubjectIdVar   = IDp,
                                 StudyIdVar     = IDs,
                                 TimeVar        = TIME,
                                 ScoreVar       = ADAS,
                                 is_pbo         = placebo,
                                 CovariatesR    = ~ AGE + SEX,
                                 CovariatesB    = ~ COMED,
                                 ScoreVar2      = S2,
                                 is_pbo2        = placebo,
                                 CovariatesR2   = ~ AGE,
                                 CovariatesB2   = ~ COMED + SEX,
                                 num_samples    = niter / 2,
                                 num_warmup     = niter / 2,
                                 seed           = 1)
exe_samps$data_used$P
exe_samps$data_used$P2
exe_samps$data_used$M
exe_samps$data_used$M2
head(in_data[!(in_data$IDs %in% c(2,3,4,5)),c("AGE", "SEX")])
head(exe_samps$data_used$X_r)
any(!(in_data[!(in_data$IDs %in% c(2,3,4,5)),c("AGE", "SEX")] == exe_samps$data_used$X_r))
any(!(in_data[!(in_data$IDs %in% c(15)),c("COMED", "SEX")] == exe_samps$data_used$X_s2))
