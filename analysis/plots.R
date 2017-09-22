# Initialize Boilerplate ----
source("boilerplate.R")

## Overall Rating of the Application ====
title <- "Distribution of Overall Rating"

## Query Data
dataset <- GetRatings() %>%
  rename(value = rating)

## Plot
plot.dataset <- dataset

png("diagrams/bxratings.png", width = 500, height = 600)
ggplot(plot.dataset, aes(x = version, y = value, fill = version)) +
  geom_violin(alpha = 0.25) +
  geom_boxplot(width = 0.1) +
  scale_x_discrete(labels = VERSION.LABLES) +
  labs(title = title, x = "Version", y = "Rating") +
  get.theme() +
  theme(legend.position = "none")
dev.off()

## Users' Perception of Security ====
title <- "Distribution of Users' Perception of Security"

## Query Data
dataset <- GetPerception() %>%
  rename(value = perception)

## Plot
plot.dataset <- dataset

png("diagrams/bxperception.png", width = 500, height = 600)
ggplot(plot.dataset, aes(x = version, y = value, fill = version)) +
  geom_violin(alpha = 0.25) +
  geom_boxplot(width = 0.1) +
  scale_x_discrete(labels = VERSION.LABLES) +
  labs(title = title, x = "Version", y = "Perception") +
  get.theme() +
  theme(legend.position = "none")
dev.off()

## Correctness ====
title <- "Distribution of Correctness"

## Query Data
dataset <- GetCorrectness() %>%
  mutate(precision = tp / (tp + fp)) %>%
  mutate(recall = tp / (tp + fn)) %>%
  mutate(f = 2 * precision * recall / (precision + recall)) %>%
  select(version, precision, recall, f)

## Plot
plot.dataset <- dataset %>%
  na.omit(.) %>%
  melt(., id.vars = c("version"))

png("diagrams/bxcorrectness.png", width = 1500, height = 600)
ggplot(plot.dataset, aes(x = version, y = value, fill = version)) +
  geom_violin(alpha = 0.25) +
  geom_boxplot(width = 0.1) +
  scale_x_discrete(labels = VERSION.LABLES) +
  scale_y_continuous(labels = scales::percent) +
  facet_wrap(~ variable, labeller = as_labeller(METRIC.LABELS)) +
  labs(title = title, x = "Version", y = "Metric Value") +
  get.theme() +
  theme(legend.position = "none")
dev.off()

## Correctness: Random Sample ====
title <- "Distribution of Correctness (Random)"

## Query Data
dataset <- GetRandomCorrectness() %>%
  mutate(precision = tp / (tp + fp)) %>%
  mutate(recall = tp / (tp + fn)) %>%
  mutate(f = 2 * precision * recall / (precision + recall)) %>%
  select(version, precision, recall, f)

## Plot
plot.dataset <- dataset %>%
  na.omit(.) %>%
  melt(., id.vars = c("version"))

png("diagrams/bxrandomcorrectness.png", width = 1500, height = 600)
ggplot(plot.dataset, aes(x = version, y = value, fill = version)) +
  geom_violin(alpha = 0.25) +
  geom_boxplot(width = 0.1) +
  scale_x_discrete(labels = VERSION.LABLES) +
  scale_y_continuous(labels = scales::percent) +
  facet_wrap(~ variable, labeller = as_labeller(METRIC.LABELS)) +
  labs(title = title, x = "Version", y = "Metric Value") +
  get.theme() +
  theme(legend.position = "none")
dev.off()