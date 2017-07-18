#MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM
# Initialize Boilerplate
#WWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWW
source("boilerplate.R")

#MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM
# Distributions
#WWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWW

#MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM
# Number of Question
title <- "Distribution of Correctness"

######################################
# Query Data
######################################
dataset <- get.correctness() %>%
  mutate(precision = (tp / (tp + fp)), recall = (tp / (tp + fn))) %>%
  mutate(f = (2 * precision * recall / (precision + recall)))

######################################
# Plot
######################################
plot.dataset <- dataset %>%
  select(type, recall, precision, f) %>%
  melt(.)
plot.dataset$variable <- factor(
  plot.dataset$variable, levels = c('precision', 'recall', 'f')
)

#XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
# Box Plot

# File Name           bxcorrectness
# Export Resolution   1800 x 700
ggplot(plot.dataset, aes(x = type, y = value, fill = variable)) +
  geom_violin(alpha = 0.25) +
  geom_boxplot(width = 0.1) +
  scale_x_discrete(labels = APPTYPE.LABLES) +
  scale_y_continuous(labels = scales::percent) +
  facet_grid( ~ variable, labeller = as_labeller(METRIC.LABELS)) +
  labs(title = title, x = "Application Type", y = "Metric Value") +
  get.theme() +
  theme(legend.position = "none")