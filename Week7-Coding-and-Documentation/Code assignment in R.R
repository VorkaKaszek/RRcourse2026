
# Sets the path to the parent directory of RR classes
setwd("Z:\\File folders\\Teaching\\Reproducible Research\\2023\\Repository\\RRcourse2023\\6. Coding and documentation")

#   Import data from the O*NET database, at ISCO-08 occupation level.
# The original data uses a version of SOC classification, but the data we load here
# are already cross-walked to ISCO-08 using: https://ibs.org.pl/en/resources/occupation-classifications-crosswalks-from-onet-soc-to-isco/

# The O*NET database contains information for occupations in the USA, including
# the tasks and activities typically associated with a specific occupation.

task_data = read.csv("Data\\onet_tasks.csv")
# isco08 variable is for occupation codes
# the t_* variables are specific tasks conducted on the job

# read employment data from Eurostat
# These datasets include quarterly information on the number of workers in specific
# 1-digit ISCO occupation categories. (Check here for details: https://www.ilo.org/public/english/bureau/stat/isco/isco08/)

# in one loop
library(readxl)

for (i in 1:9) {
  assign(
    paste0("isco", i),
    read_excel("Data/Eurostat_employment_isco.xlsx", sheet = paste0("ISCO", i))
  )
}

# Function to calculate totals for any country
calculate_total <- function(country) {
  total <- 0
  
  for (i in 1:9) {
    df <- get(paste0("isco", i))
    total <- total + df[[country]]
  }
  
  return(total)
}

# Countries of interest
countries <- c("Belgium", "Spain", "Poland")

# Create total_* objects dynamically
for (country in countries) {
  assign(paste0("total_", country), calculate_total(country))
}
# Add ISCO column using loop
for (i in 1:9) {
  df <- get(paste0("isco", i))
  df$ISCO <- i
  assign(paste0("isco", i), df)
}

# Combine all datasets
all_data <- do.call(rbind, lapply(1:9, function(i) get(paste0("isco", i))))

# Function to repeat totals correctly
repeat_total <- function(total_vector, times) {
  rep(total_vector, times)
}

all_data$total_Belgium <- repeat_total(total_Belgium, 9)
all_data$total_Spain   <- repeat_total(total_Spain, 9)
all_data$total_Poland  <- repeat_total(total_Poland, 9)

# And this will give us shares of each occupation among all workers in a period-country

countries <- c("Belgium", "Spain", "Poland")

for (country in countries) {
  all_data[[paste0("share_", country)]] <- all_data[[country]] / all_data[[paste0("total_", country)]]
}

# Now let's look at the task data. We want the first digit of the ISCO variable only
library(stringr)
library(dplyr)

task_data$isco08_1dig <- as.numeric(str_sub(task_data$isco08, 1, 1))

# And we'll calculate the mean task values at a 1-digit level 
# (more on what these tasks are below)

aggdata <- aggregate(task_data, by = list(task_data$isco08_1dig),
                     FUN = mean, na.rm = TRUE)
aggdata$isco08 <- NULL

# We'll be interested in tracking the intensity of Non-routine cognitive analytical tasks
# Using a framework reminiscent of the work by David Autor.


#Let's combine the data.
combined <- left_join(all_data, aggdata, by = c("ISCO" = "isco08_1dig"))

# Traditionally, the first step is to standardise the task values using weights 
# defined by share of occupations in the labour force. This should be done separately
# for each country. Standardisation -> getting the mean to 0 and std. dev. to 1.
# Let's do this for each of the variables that interests us:

library(Hmisc)

# function for weighted standardisation
weighted_std <- function(x, w) {
  m <- wtd.mean(x, w)
  s <- sqrt(wtd.var(x, w))
  (x - m) / s
}

tasks <- c("t_4A2a4", "t_4A2b2", "t_4A4a1")

for (task in tasks) {
  for (country in countries) {
    combined[[paste0("std_", country, "_", task)]] <-
      weighted_std(combined[[task]], combined[[paste0("share_", country)]])
  }
}

# The next step is to calculate the `classic` task content intensity, i.e.
# how important is a particular general task content category in the workforce
# Here, we're looking at non-routine cognitive analytical tasks, as defined
# by David Autor and Darron Acemoglu:

for (country in countries) {
  combined[[paste0(country, "_NRCA")]] <-
    combined[[paste0("std_", country, "_t_4A2a4")]] +
    combined[[paste0("std_", country, "_t_4A2b2")]] +
    combined[[paste0("std_", country, "_t_4A4a1")]]
}

# And we standardise NRCA in a similar way.
for (country in countries) {
  combined[[paste0("std_", country, "_NRCA")]] <-
    weighted_std(combined[[paste0(country, "_NRCA")]],
                 combined[[paste0("share_", country)]])
}

# Finally, to track the changes over time, we have to calculate a country-level mean
# Step 1: multiply the value by the share of such workers.

for (country in countries) {
  combined[[paste0("multip_", country, "_NRCA")]] <-
    combined[[paste0("std_", country, "_NRCA")]] *
    combined[[paste0("share_", country)]]
}

# Step 2: sum it up (it basically becomes another weighted mean)

agg_list <- list()

for (country in countries) {
  agg_list[[country]] <- aggregate(
    combined[[paste0("multip_", country, "_NRCA")]],
    by = list(combined$TIME),
    FUN = sum, na.rm = TRUE
  )
}

agg_Spain   <- agg_list[["Spain"]]
agg_Belgium <- agg_list[["Belgium"]]
agg_Poland  <- agg_list[["Poland"]]

# We can plot it now!

for (country in countries) {
  agg <- agg_list[[country]]
  plot(agg$x, xaxt = "n", main = country)
  axis(1, at = seq(1, length(agg$x), 3),
       labels = agg$Group.1[seq(1, length(agg$x), 3)])
}

