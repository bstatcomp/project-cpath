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
library(mcmcse)
source("./R/sampling_gpu.R")


## functions -------------------------------------------------------------------
reindex <- function  (x) {
  new_inds <- x
  for (i in 1:length(x)) {
    new_inds[i] <- which(x[i] == unique(x))
  }
  return (new_inds)
}


## data input ------------------------------------------------------------------
mod_name <- "one_score"


## Generate toy dataset
patients_per_study       <- 6
measurements_per_patient <- 4
number_of_studies        <- 4
time_points              <- 0:(measurements_per_patient-1) * 0.25

toy_data <- NULL
set.seed(0)
for (i in 1:number_of_studies) {
  for (j in 1:patients_per_study) {
    # covariates
    comed  <- sample(0:1, 1) # receiving co-medication reduces progression rate by half
    age    <- rnorm(1)       # age raises the baseline for 0.05 per standard deviation
    # study effect
    study1 <- 0
    if (i == 1) study1 <- -0.27          # Some external effect on Study 1 reduced the baseline of all patients in that study
    # patient effect
    p_rate     <- runif(1, 0.90, 1.10)
    p_baseline <- runif(1, -0.05, 0.05)

    add <- data.frame(IDp        = (i - 1) * patients_per_study + j,
                      IDs        = i,
                      COMED      = comed,
                      AGE        = age,
                      p_baseline = p_baseline,
                      p_rate     = p_rate,
                      S2 = time_points * p_rate * 0.1 / (comed + 1) + 0.37 + age * 0.05 + study1 + p_baseline+ rnorm(measurements_per_patient, 0, 0.02),
                      S1 = time_points * p_rate * 0.05 + 0.20 + age * 0.001 + p_baseline + rnorm(measurements_per_patient, 0, 0.01),
                      time       = time_points)
    toy_data <- rbind(toy_data, add)
  }
}

# just in case (score needs to be between 0 and 1)
toy_data$S1 <- ifelse(toy_data$S1 <= 0, 0.01, toy_data$S1)
toy_data$S2 <- ifelse(toy_data$S2 <= 0, 0.01, toy_data$S2)


toy_data$placebo1 = 0
toy_data$placebo2 = 0


## 1 ---------------------------------------------------------------------------
## normal
niter <- 1000
exe_samps        <- sampling_gpu(df             = toy_data,
                                 SubjectIdVar   = IDp,
                                 StudyIdVar     = IDs,
                                 TimeVar        = time,
                                 ScoreVar       = S1,
                                 is_pbo         = placebo1,
                                 # CovariatesR    = ~ AGE + COMED,
                                 # CovariatesB    = ~ AGE + COMED,
                                 CovariatesR    = ~ COMED + AGE,
                                 CovariatesB    = ~ COMED + AGE,
                                 # ScoreVar2      = S2,
                                 # is_pbo2        = placebo2,
                                 # CovariatesR2   = ~ AGE + COMED,
                                 # CovariatesB2   = ~ AGE + COMED,
                                 # CovariatesR2   = ~ COMED + AGE,
                                 # CovariatesB2   = ~ COMED + AGE,
                                 num_samples    = niter / 2,
                                 num_warmup     = niter / 2,
                                 seed           = 1)


# Predict directly -------------------------------------------------------------
samp_df <- exe_samps$pred_samples
preds   <- samp_df[ ,-c(1:11)]
preds   <- apply(preds, 1, mean)
plot_df <- cbind(samp_df[ ,1:11], "pred" = preds)
ggplot(plot_df, aes(x = score, y = pred, color = as.factor(IDs))) +
  geom_line() +
  facet_wrap(~ scoreN) +
  geom_abline(slope = 1)



# Check if samp_df and toy_data have the correct values at correct rows
tmp_df    <- samp_df[ ,1:11]
joined_df <- left_join(toy_data, tmp_df, by = c("IDp", "IDs", "time"))
summary(joined_df[joined_df$scoreN == 2, "S2"] == joined_df[joined_df$scoreN == 2, "score"])
summary(joined_df[joined_df$scoreN == 1, "S1"] == joined_df[joined_df$scoreN == 1, "score"])

