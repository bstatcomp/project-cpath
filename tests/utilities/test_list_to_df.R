# WD, libraries, and sourcing --------------------------------------------------
my_path <- dirname(rstudioapi::getSourceEditorContext()$path)
my_path <- strsplit(my_path, split = "/")[[1]]
my_path <- paste0(my_path[-c(length(my_path) - 1, length(my_path))], collapse = "/")
setwd(my_path)
library(loo)
source("./data/generalized_logistic_model/data.R")
source("./R/list_to_df.R")


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


my_list <- df_to_list(df, c("AGE", "SEX", "COMED", "APOE4"))
names(my_list)
my_list
