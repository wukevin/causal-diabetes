rm(list = ls())  # Clear everything
gc()
library(rstudioapi)
library(stats)
library(DOS2)
library(optmatch)
library(lmtest)
library(RItools)
library(ggplot2)
library(ggpubr)
library(plyr)
library(gtools)  # For quantcut
library(AER)
library(randomForest)
library(MLmetrics)
options("optmatch_max_problem_size" = Inf)
current_path = rstudioapi::getActiveDocumentContext()$path
setwd(dirname(current_path))
source("utility.R")

### Read in the data and do some basic preprocessing
diabetes <- read.csv("diabetic_data.csv")
# Limit analysis to african americans and caucasians for simplicity
diabetes <- diabetes["|"(diabetes$race == "AfricanAmerican", diabetes$race == "Caucasian"), ]
# Limit analysis to <30 and NO readmission (exclude >30 day readmission)
print(table(diabetes$readmitted))
diabetes <- diabetes['|'(diabetes$readmitted == "NO", diabetes$readmitted == "<30"), ]
# Tweak naming to be more conducive
diabetes$y = diabetes$readmitted == "<30"  # y = 1 indicates readmission
rownames(diabetes) <- as.character(1:nrow(diabetes))  # smahal complains otherwise

### Preprocessing
# Convert some fields to boolean values
to_bool <- function(fieldname, data) {
  # Convert the field into a boolean field and attach it to data
  uniq = sort(unique(data[[fieldname]]))  # https://stackoverflow.com/questions/23717397/how-to-use-a-string-variable-to-select-a-data-frame-column-using-notation
  val = uniq[1]
  if ("Yes" %in% uniq) {
    val = "Yes"
  }
  stopifnot(length(uniq) == 2)
  newname = paste(fieldname, "is", tolower(val), sep=".")
  data[[newname]] = data[[fieldname]] == val
  return(data)
}
for (x in c("race", "gender", "change", "diabetesMed", "readmitted")) {
  diabetes = to_bool(x, diabetes)
}
# Convert some fields to factor
factor_cols <- c("age", "weight")
diabetes[factor_cols] = lapply(diabetes[factor_cols], as.factor)
# Get the factor levels
diabetes[paste(factor_cols, "idx", sep=".")] = lapply(diabetes[factor_cols], as.integer)
table(diabetes$race)
table(diabetes$readmitted)
table(diabetes$weight)

naive.df = data.frame(diabetes$y, diabetes$race)
naive.tab = table(naive.df$diabetes.y, naive.df$diabetes.race)
print(naive.tab)
print(chisq.test(naive.tab))

### Consider race the treatment variable
z <- as.numeric(diabetes$race.is.africanamerican)
zb <- as.logical(z)
diabetes$z <- z
diabetes$zb <- zb

### Define covariates
# weight is very commonly missing
# num lab procedures - # lab tests performed during the encounter
# num procedures - # non-lab procedures
# num medications - # generic names administered
# num outpatient - # outpatient visits in the last year
# num emergency - # emergency visits in the last year
# num inpatient - # inpatient visits in the last year
# number diagnoses - number of diagnoses enter into the system (unsure if this should be included)
# change - was there a change in diabetes medication
# diabetesMed - is on diabetes medication
covariates <- c(
  "age.idx", "time_in_hospital", "num_lab_procedures", "num_procedures", "num_medications",
  "number_outpatient", "number_emergency", "number_inpatient", "change.is.ch", "diabetesMed.is.yes"
)

## Plot the covariates
# https://www.statology.org/ggplot2-legend-size/
p = ggplot(diabetes, aes(x=num_lab_procedures, color=race, fill=race)) +
  geom_histogram(alpha=0.5, position='identity') +
  scale_color_manual(values=c("#E69F00", "#999999")) +
  scale_fill_manual(values = c("#E69F00", "#999999")) +
  theme(legend.position = c(0.8, 0.8), legend.title = element_text(size=6), legend.text = element_text(size=5), legend.key.size = unit(0.5, 'cm'))
ggsave("cov/n_lab_procedures.pdf", width=7, height=5, units='cm')
wilcox.test(diabetes[diabetes$z == 1,]$num_lab_procedures, diabetes[diabetes$z == 0,]$num_lab_procedures, alternative='greater')


p = ggplot(diabetes, aes(x=num_procedures, color=race, fill=race)) +
  geom_histogram(alpha=0.5, position='identity', binwidth=1) +
  scale_color_manual(values=c("#E69F00", "#999999")) +
  scale_fill_manual(values = c("#E69F00", "#999999")) +
  theme(legend.position = 'none')
ggsave("cov/n_procedures.pdf", width=7, height=5, units='cm')
wilcox.test(diabetes[diabetes$z == 1,]$num_procedures, diabetes[diabetes$z == 0,]$num_procedures, alternative='less')

p = ggplot(diabetes, aes(x=num_medications, color=race, fill=race)) +
  geom_histogram(alpha=0.5, position='identity', binwidth=5) +
  scale_color_manual(values=c("#E69F00", "#999999")) +
  scale_fill_manual(values = c("#E69F00", "#999999")) +
  theme(legend.position = 'none')
ggsave("cov/n_meds.pdf", width=7, height=5, units='cm')
wilcox.test(diabetes[diabetes$z == 1,]$num_medications, diabetes[diabetes$z == 0,]$num_medications, alternative='less')

p = ggplot(diabetes, aes(x=number_outpatient, color=race, fill=race)) +
  geom_histogram(alpha=0.5, position='identity', binwidth=2) +
  scale_color_manual(values=c("#E69F00", "#999999")) +
  scale_fill_manual(values = c("#E69F00", "#999999")) +
  theme(legend.position = 'none')
ggsave("cov/n_outpatient.pdf", width=7, height=5, units='cm')
wilcox.test(diabetes[diabetes$z == 1,]$number_outpatient, diabetes[diabetes$z == 0,]$number_outpatient, alternative='less')

p = ggplot(diabetes, aes(x=number_inpatient, color=race, fill=race)) +
  geom_histogram(alpha=0.5, position='identity', binwidth=2) +
  scale_color_manual(values=c("#E69F00", "#999999")) +
  scale_fill_manual(values = c("#E69F00", "#999999")) +
  theme(legend.position = 'none')
ggsave("cov/n_inpatient.pdf", width=7, height=5, units='cm')
wilcox.test(diabetes[diabetes$z == 1,]$number_inpatient, diabetes[diabetes$z == 0,]$number_inpatient, alternative='greater')

p = ggplot(diabetes, aes(x=number_emergency, color=race, fill=race)) +
  geom_histogram(alpha=0.5, position='identity', binwidth=4) +
  scale_color_manual(values=c("#E69F00", "#999999")) +
  scale_fill_manual(values = c("#E69F00", "#999999")) +
  theme(legend.position = 'none')
ggsave("cov/n_emergency.pdf", width=7, height=5, units='cm')
wilcox.test(diabetes[diabetes$z == 1,]$number_emergency, diabetes[diabetes$z == 0,]$number_emergency, alternative='greater')

p = ggplot(diabetes, aes(x=age.idx, color=race, fill=race)) +
  geom_histogram(alpha=0.5, position='identity', binwidth=1) +
  scale_color_manual(values=c("#E69F00", "#999999")) +
  scale_fill_manual(values = c("#E69F00", "#999999")) +
  theme(legend.position = 'none')
ggsave("cov/age.pdf", width=7, height=5, units='cm')
wilcox.test(diabetes[diabetes$z == 1,]$age.idx, diabetes[diabetes$z == 0,]$age.idx)

# Do some matching based on robust mahalanobis distance
sdist = smahal(z, diabetes[, covariates])  # Appears to use indices instead of row.names
matches = pairmatch(sdist, controls=1, data=diabetes)
matches.summary <- summarize.match(diabetes, matches)
pdf("matching/robust_no_caliper_matching.pdf", width=8, height=6)
plot(
  xBalance(
    z ~ age.idx + time_in_hospital + num_lab_procedures + num_procedures + num_medications +
        number_outpatient + number_emergency + number_inpatient + change.is.ch + diabetesMed.is.yes +
        strata(matches) - 1,
    data=diabetes,
  )
)
title(main="Covariate balance")
dev.off()

# FRT on paired examples
y.diffs = as.integer(matches.summary$y.1) - as.integer(matches.summary$y.0)
y.diffs.mean = mean(y.diffs)
resampled.means = c()
set.seed(6489)
for (i in 1:10000) {
  signs <- sample(c(1, -1), size=length(y.diffs), replace=TRUE)
  resampled <- signs * y.diffs
  resampled.means = c(resampled.means, mean(resampled))
}
print(mean(resampled.means < y.diffs.mean))
p <- ggplot(data=data.frame(resampled.means), mapping=aes(resampled.means)) +
  geom_histogram(color='darkblue', fill='lightblue') +
  geom_vline(aes(xintercept=y.diffs.mean), color='blue', linetype='dashed')
ggsave("matching/matched_diffs.pdf", width=9, height=7, units='cm')


# Propensity score
prop_score_lr = glm(
  z ~ age.idx + time_in_hospital + num_lab_procedures + num_procedures + num_medications +
    number_outpatient + number_emergency + number_inpatient + change.is.ch + diabetesMed.is.yes,
  data=diabetes,
  family=binomial,
)
prop_scores = prop_score_lr$fitted.values
mean((z - prop_scores) ** 2)
diabetes$prop <- prop_scores

# Visualize propensity scores
ps_df = data.frame(prop_scores, zb)
mu = ddply(ps_df, "zb", summarise, grp.mean=mean(prop_scores))
p <- ggplot(ps_df, aes(x=prop_scores, color=zb, fill=zb)) +
  geom_histogram(position='identity', bins=30, alpha=0.5) +
  geom_vline(data=mu, aes(xintercept=grp.mean, color=zb), linetype='dashed') +
  scale_color_manual(values=c("#E69F00", "#999999")) +
  scale_fill_manual(values = c("#E69F00", "#999999")) +
  ggtitle("Propensity scores") + labs(fill = "Is African American") +
  guides(color = "none") + theme(legend.position = c(0.8, 0.8))
ggsave("ps/prop_scores.pdf", width=12, height=9, units='cm')

# Stratified difference in means
strat.diff <- function(data = diabetes, q=10) {
  prop_strata = quantcut(diabetes$prop, q=q)
  diabetes$prop.strata <- prop_strata
  n.strata = length(levels(data$prop.strata))
  sre.pi = rep(0, n.strata)
  sre.tau = rep(0, n.strata)
  sre.s.1 = rep(0, n.strata)
  sre.s.0 = rep(0, n.strata)
  sre.n.1 = rep(0, n.strata)  # Shat^2(1)
  sre.n.0 = rep(0, n.strata)  # Shat^2(0)

  for (l in seq_along(levels(data$prop.strata))) {
    strata.data = data[data$prop.strata == levels(data$prop.strata)[l], ]
    sre.pi[l] = nrow(strata.data) / nrow(data)
    z1.data = strata.data[strata.data$z == 1, ]
    n.1 = nrow(z1.data)
    sre.n.1[l] <- n.1
    z0.data = strata.data[strata.data$z == 0, ]
    n.0 = nrow(z0.data)
    sre.n.0[l] <- n.0
    print(c(nrow(z1.data), nrow(z0.data)))
    sre.tau[l] = mean(z1.data$y) - mean(z0.data$y)

    z1.square_errors = (z1.data$y - mean(z1.data$y)) ** 2
    sre.s.1[l] <- sum(z1.square_errors) / (n.1 - 1)  # S hat squared

    z0.square_errors = (z0.data$y - mean(z0.data$y)) ** 2
    sre.s.0[l] <- sum(z0.square_errors) / (n.0 - 1)
  }
  sre.V.terms = (sre.pi ** 2) * ( (sre.s.1 / sre.n.1) + (sre.s.0 / sre.n.0) )
  print(round(sre.tau, digits=4))
  print(round(sre.s.1, digits=4))
  print(round(sre.s.0, digits=4))
  sre.V = sum(sre.V.terms)
  print(sqrt(sre.V))
  print(c(
    sum(sre.tau * sre.pi) + 1.96 * sqrt(sre.V),
    sum(sre.tau * sre.pi) - 1.96 * sqrt(sre.V)
  ))
}
strat.diff()

# Random forest propensity score
prop_score_rf = randomForest(
  z ~ age.idx + time_in_hospital + num_lab_procedures + num_procedures + num_medications +
    number_outpatient + number_emergency + number_inpatient + change.is.ch + diabetesMed.is.yes,
  data=diabetes,
)
rf_prop_scores = prop_score_rf$predicted
mean((z - rf_prop_scores) ** 2)

rf_ps_df = data.frame(rf_prop_scores, zb)
mu = ddply(rf_ps_df, "zb", summarise, grp.mean=mean(rf_prop_scores))
p <- ggplot(rf_ps_df, aes(x=rf_prop_scores, color=zb, fill=zb)) +
  geom_histogram(position='identity', bins=30, alpha=0.5) +
  geom_vline(data=mu, aes(xintercept=grp.mean, color=zb), linetype='dashed') +
  scale_color_manual(values=c("#E69F00", "#999999")) +
  scale_fill_manual(values = c("#E69F00", "#999999")) +
  ggtitle("Propensity scores (RF)") + labs(fill = "Is African American") +
  guides(color = "none") + theme(legend.position = c(0.8, 0.8))
ggsave("ps/rf_prop_scores.pdf", width=12, height=9, units='cm')

diabetes$prop <- rf_prop_scores
strat.diff()
