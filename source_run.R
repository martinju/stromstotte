
# Schedule this to run at 13.15 every day

library(data.table)

read_new_data <- TRUE
playground = TRUE
use_past_k_days <- 31
k_bootstrap <- 5*10^2
compensation_prop <- 0.80
compensation_threshold <- 0.70*1.25
areas <- c("NO2")#,"NO1")#c("NO1","NO2")
plot_CI_ints <- c(0.99,0.95,0.9,0.8,0.5)
plot_avg_measures <- c("mean","median")
database_filename <- "database_nordpool.csv"
today <- as.IDate("2022-09-20") #as.IDate(Sys.time())
seed = 12345
method = "daily_auto.arima"#"all_indep"
model_training_range <- c(as.IDate("2022-01-01"),as.IDate("2022-08-31"))

source("run.R")

estimated_meanprice
