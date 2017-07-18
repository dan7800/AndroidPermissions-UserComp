#MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM
# Initialize Boilerplate
#WWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWW
source("boilerplate.R")

#MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM
# Mann-Whitney-Wilcoxon and Cliff's Delta
#WWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWW

analysis.outcome <- data.frame()

######################################
# Query Data
######################################
dataset <- get.correctness() %>%
  mutate(precision = (tp / (tp + fp)), recall = (tp / (tp + fn))) %>%
  mutate(f = (2 * precision * recall / (precision + recall)))

######################################
# Analyze
######################################

#XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
# Precision

analysis.dataset <- dataset %>%
  filter(type %in% c("21", "21E")) %>%
  select(type, precision) %>%
  na.omit(.)

population.one <- analysis.dataset %>%
  filter(type == "21")
population.two <- analysis.dataset %>%
  filter(type == "21E")

wresult <- run.wilcox(population.one, population.two, "precision")
cresult <- cliff.delta(precision ~ type, data = analysis.dataset)

analysis.outcome <- rbind(
  analysis.outcome,
  data.frame(
    "metric" = "Precision",
    "p" = sprintf("%.4e", wresult$wilcox.test.out$p.value),
    "is.significant" = wresult$is.significant,
    "lower.median" = wresult$one.median,
    "compare.median" = wresult$median.ieq,
    "upper.median" = wresult$two.median,
    "lower.mean" = wresult$one.mean,
    "compare.mean" = wresult$mean.ieq,
    "upper.mean" = wresult$two.mean,
    "effect" = cresult$magnitude,
    "delta" = cresult$estimate
  )
)

#XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
# Recall

analysis.dataset <- dataset %>%
  filter(type %in% c("21", "21E")) %>%
  select(type, recall) %>%
  na.omit(.)

population.one <- analysis.dataset %>%
  filter(type == "21")
population.two <- analysis.dataset %>%
  filter(type == "21E")

wresult <- run.wilcox(population.one, population.two, "recall")
cresult <- cliff.delta(recall ~ type, data = analysis.dataset)

analysis.outcome <- rbind(
  analysis.outcome,
  data.frame(
    "metric" = "Recall",
    "p" = sprintf("%.4e", wresult$wilcox.test.out$p.value),
    "is.significant" = wresult$is.significant,
    "lower.median" = wresult$one.median,
    "compare.median" = wresult$median.ieq,
    "upper.median" = wresult$two.median,
    "lower.mean" = wresult$one.mean,
    "compare.mean" = wresult$mean.ieq,
    "upper.mean" = wresult$two.mean,
    "effect" = cresult$magnitude,
    "delta" = cresult$estimate
  )
)

#XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
# F-measure

analysis.dataset <- dataset %>%
  filter(type %in% c("21", "21E")) %>%
  select(type, f) %>%
  na.omit(.)

population.one <- analysis.dataset %>%
  filter(type == "21")
population.two <- analysis.dataset %>%
  filter(type == "21E")

wresult <- run.wilcox(population.one, population.two, "f")
cresult <- cliff.delta(f ~ type, data = analysis.dataset)

analysis.outcome <- rbind(
  analysis.outcome,
  data.frame(
    "metric" = "F-measure",
    "p" = sprintf("%.4e", wresult$wilcox.test.out$p.value),
    "is.significant" = wresult$is.significant,
    "lower.median" = wresult$one.median,
    "compare.median" = wresult$median.ieq,
    "upper.median" = wresult$two.median,
    "lower.mean" = wresult$one.mean,
    "compare.mean" = wresult$mean.ieq,
    "upper.mean" = wresult$two.mean,
    "effect" = cresult$magnitude,
    "delta" = cresult$estimate
  )
)