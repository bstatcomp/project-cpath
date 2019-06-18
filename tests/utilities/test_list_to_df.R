# WD, libraries, and sourcing --------------------------------------------------
my_path <- dirname(rstudioapi::getSourceEditorContext()$path)
my_path <- strsplit(my_path, split = "/")[[1]]
my_path <- paste0(my_path[-c(length(my_path) - 1, length(my_path))], collapse = "/")
setwd(my_path)
library(loo)
source("./data/generalized_logistic_model/data.R")
source("./R/list_to_df.R")
source("./R/df_to_list.R")

# to df ------------------------------------------------------------------------
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

df <- list_to_df(in_data)
head(df)


# test parameter names ---------------------------------------------------------

# ok
my_list <- df_to_list(df,
                      SubjectIdVar   = IDp,
                      StudyIdVar     = IDs,
                      TimeVar        = time,
                      ScoreVar       = S,
                      covariates = ~ AGE + SEX + COMED + APOE4)
names(my_list)
my_list

# one name not in df
my_list <- df_to_list(df,
                      SubjectIdVar   = IDa,
                      StudyIdVar     = IDs,
                      TimeVar        = time,
                      ScoreVar       = S,
                      covariates = ~ AGE + SEX + COMED + APOE4)

# missing one name
my_list <- df_to_list(df,
                      SubjectIdVar   = IDp,
                      TimeVar        = time,
                      ScoreVar       = S,
                      covariates = ~ AGE + SEX + COMED + APOE4)

# second score same name
my_list <- df_to_list(df,
                      SubjectIdVar   = IDp,
                      StudyIdVar     = IDs,
                      TimeVar        = time,
                      ScoreVar       = S,
                      SecondScoreVar = S,
                      covariates = ~ AGE + SEX + COMED + APOE4)

# second score not in df
my_list <- df_to_list(df,
                      SubjectIdVar   = IDp,
                      StudyIdVar     = IDs,
                      TimeVar        = time,
                      ScoreVar       = S,
                      SecondScoreVar = S2,
                      covariates = ~ AGE + SEX + COMED + APOE4)


# covariates -------------------------------------------------------------------
# same covariates for X and Y
my_list <- df_to_list(df,
                      SubjectIdVar   = IDp,
                      StudyIdVar     = IDs,
                      TimeVar        = time,
                      ScoreVar       = S,
                      CovariatesX    = ~ AGE + SEX,
                      CovariatesY    = ~ SEX + COMED,
                      covariates = ~ AGE + SEX + COMED + APOE4)

# covariate in core attributes
my_list <- df_to_list(df,
                      SubjectIdVar   = IDp,
                      StudyIdVar     = IDs,
                      TimeVar        = time,
                      ScoreVar       = S,
                      CovariatesX    = ~ AGE + SEX + IDp,
                      covariates = ~ AGE + SEX + COMED + APOE4)