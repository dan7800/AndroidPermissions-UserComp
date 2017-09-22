# Initialize Boilerplate ----
source("boilerplate.R")

## Correctness ====

## Query Data
dataset <- GetCorrectness() %>%
  mutate(precision = tp / (tp + fp)) %>%
  mutate(recall = tp / (tp + fn)) %>%
  mutate(f = 2 * precision * recall / (precision + recall))

## Test: Precision
analysis.dataset <- dataset %>%
  select(version, metric = precision) %>%
  na.omit(.)

TestAssociation(analysis.dataset, c("21", "23", "21E"), VERSION.LABLES)

## Test: Recall
analysis.dataset <- dataset %>%
  select(version, metric = recall) %>%
  na.omit(.)

TestAssociation(analysis.dataset, c("21", "23", "21E"), VERSION.LABLES)

## Test: F-measure
analysis.dataset <- dataset %>%
  select(version, metric = f) %>%
  na.omit(.)

TestAssociation(analysis.dataset, c("21", "23", "21E"), VERSION.LABLES)

## Correctness: Random Sample ====

## Query Data
dataset <- GetRandomCorrectness() %>%
  mutate(precision = tp / (tp + fp)) %>%
  mutate(recall = tp / (tp + fn)) %>%
  mutate(f = 2 * precision * recall / (precision + recall))

## Test: Precision
analysis.dataset <- dataset %>%
  select(version, metric = precision) %>%
  na.omit(.)

TestAssociation(analysis.dataset, c("23Rnd", "21ERnd"), VERSION.LABLES)

## Test: Recall
analysis.dataset <- dataset %>%
  select(version, metric = recall) %>%
  na.omit(.)

TestAssociation(analysis.dataset, c("23Rnd", "21ERnd"), VERSION.LABLES)

## Test: F-measure
analysis.dataset <- dataset %>%
  select(version, metric = f) %>%
  na.omit(.)

TestAssociation(analysis.dataset, c("23Rnd", "21ERnd"), VERSION.LABLES)