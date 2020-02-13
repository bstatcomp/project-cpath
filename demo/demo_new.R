## WD, libraries, and sourcing -------------------------------------------------
my_path <- dirname(rstudioapi::getSourceEditorContext()$path)
my_path <- strsplit(my_path, split = "/")[[1]]
my_path <- paste0(my_path[-c(length(my_path), length(my_path))], 
                  collapse = "/")
setwd(my_path)
library(rstan)
library(dplyr)
library(tidyr)
library(ggplot2)
library(GLMCPath)

# source("./support_scripts/sampling_gpu_new.R")

dataset <- readRDS("./data/generalized_logistic_model/demo/real_small_placebo_1.rds")
df      <- dataset$table_data
niter   <- 40

## normal model ----------------------------------------------------------------
glm_samples <- sampling_gpu_new(df           = df,
                                SubjectIdVar = IDp,
                                StudyIdVar   = IDs,
                                TimeVar      = TIME,
                                ScoreVar     = ADAS,
                                is_pbo       = is_pbo,
                                CovariatesR  = ~ AGE + SEX,
                                CovariatesB  = ~ COMED,
                                num_samples  = niter / 2,
                                num_warmup   = niter / 2,
                                seed         = 1,
                                gpu_enabled  = 1)
## check fit (need to join df and prediction samples)
psmp    <- glm_samples$pred_samples # get prediction samples data frame
X       <- psmp[ ,-(1:11)] # get only prediction samples
preds   <- apply(X, 1, mean) # get mean of prediction samples
tmp_df  <- cbind(psmp[ ,c("IDp", "IDs", "TIME", "scoreN")], "pred" = preds)
tmp_df$scoreN[tmp_df$scoreN == 1] <- "ADAS"

dat_eval <- df %>%
  select(IDp, IDs, ADAS, TIME) %>%
  gather(key = scoreN, value = orig_val, ADAS)

plot_df <- left_join(dat_eval, tmp_df, by = c("IDp", "IDs", "TIME", "scoreN"))
ggplot(plot_df, aes(x = orig_val, y = pred)) +
  geom_point() +
  geom_abline(aes(intercept = 0, slope = 1), color = "red")


## multiplicative --------------------------------------------------------------
glm_samples <- sampling_gpu_new(df           = df,
                                SubjectIdVar = IDp,
                                StudyIdVar   = IDs,
                                TimeVar      = TIME,
                                ScoreVar     = ADAS,
                                is_pbo       = is_pbo,
                                CovariatesR  = ~ AGE + SEX,
                                CovariatesB  = ~ COMED,
                                num_samples  = niter / 2,
                                num_warmup   = niter / 2,
                                m_r          = 1,
                                m_b          = 0,
                                seed         = 1,
                                gpu_enabled  = 1)
## check fit
psmp    <- glm_samples$pred_samples
X       <- psmp[ ,-(1:11)]
preds   <- apply(X, 1, mean)
tmp_df  <- cbind(psmp[ ,c("IDp", "IDs", "TIME", "scoreN")], "pred" = preds)
tmp_df$scoreN[tmp_df$scoreN == 1] <- "ADAS"

dat_eval <- df %>%
  select(IDp, IDs, ADAS, TIME) %>%
  gather(key = scoreN, value = orig_val, ADAS)

plot_df <- left_join(dat_eval, tmp_df, by = c("IDp", "IDs", "TIME", "scoreN"))
ggplot(plot_df, aes(x = orig_val, y = pred)) +
  geom_point() +
  geom_abline(aes(intercept = 0, slope = 1), color = "red")


## no covariates ---------------------------------------------------------------
glm_samples <- sampling_gpu_new(df           = df,
                                SubjectIdVar = IDp,
                                StudyIdVar   = IDs,
                                TimeVar      = TIME,
                                ScoreVar     = ADAS,
                                is_pbo       = is_pbo,
                                num_samples  = niter / 2,
                                num_warmup   = niter / 2,
                                seed         = 1,
                                gpu_enabled  = 1)
## check fit 
psmp    <- glm_samples$pred_samples
X       <- psmp[ ,-(1:11)]
preds   <- apply(X, 1, mean)
tmp_df  <- cbind(psmp[ ,c("IDp", "IDs", "TIME", "scoreN")], "pred" = preds)
tmp_df$scoreN[tmp_df$scoreN == 1] <- "ADAS"

dat_eval <- df %>%
  select(IDp, IDs, ADAS, TIME) %>%
  gather(key = scoreN, value = orig_val, ADAS)

plot_df <- left_join(dat_eval, tmp_df, by = c("IDp", "IDs", "TIME", "scoreN"))
ggplot(plot_df, aes(x = orig_val, y = pred)) +
  geom_point() +
  geom_abline(aes(intercept = 0, slope = 1), color = "red")


## two-score -------------------------------------------------------------------
dataset <- readRDS("./data/generalized_logistic_model/demo/toy_small_two_scores_separate_placebo_2_3.rds")
df      <- dataset$table_data
glm_samples <- sampling_gpu_new(df           = df,
                                SubjectIdVar = IDp,
                                StudyIdVar   = IDs,
                                TimeVar      = time,
                                ScoreVar     = S1,
                                is_pbo       = is_pbo,
                                ScoreVar2    = S2,
                                is_pbo2      = is_pbo,
                                CovariatesR2 = ~ COMED + AGE,
                                CovariatesB2 = ~ COMED + AGE,
                                num_samples  = niter / 2,
                                num_warmup   = niter / 2,
                                seed         = 1,
                                gpu_enabled  = 1)
## check fit
psmp    <- glm_samples$pred_samples
X       <- psmp[ ,-(1:10)]
preds   <- apply(X, 1, mean)
tmp_df  <- cbind(psmp[ ,c("IDp", "IDs", "time", "scoreN")], "pred" = preds)
tmp_df$scoreN[tmp_df$scoreN == 1] <- "S1"
tmp_df$scoreN[tmp_df$scoreN == 2] <- "S2"

dat_eval <- df %>%
  select(IDp, IDs, S1, S2, time) %>%
  gather(key = scoreN, value = orig_val, S1, S2)

plot_df <- left_join(dat_eval, tmp_df, by = c("IDp", "IDs", "time", "scoreN"))
plot_df <- plot_df[!is.na(plot_df$orig_val), ]
ggplot(plot_df, aes(x = orig_val, y = pred)) +
  geom_point() +
  facet_wrap(~ scoreN) +
  geom_abline(aes(intercept = 0, slope = 1), color = "red")
