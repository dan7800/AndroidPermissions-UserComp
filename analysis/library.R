# Load Externals ----
source("theme.R")
source("constants.R")
source("data.R")

# Function Definitions ----

# Generic ====
InitLibraries <- function() {
  libraries <- c(
    "plyr", "dplyr", "ggplot2", "tidyr", "reshape2", "effsize"
  )
  for(lib in libraries){
    suppressPackageStartupMessages(library(lib, character.only = T))
  }
}

# Database ====
GetDbConnection <- function(db.settings) {
  connection <- Connect(
    provider = db.settings$provider,
    host = db.settings$host, port = db.settings$port,
    user = db.settings$user, password = db.settings$password,
    dbname = db.settings$dbname
  )
  return(connection)
}

Connect <- function(provider, host, port, user, password, dbname) {
  connection <- NULL

  if(provider == "PostgreSQL")
    library("RPostgreSQL")
  else if(provider == "SQLite")
    library("RSQLite")
  else
    stop(sprintf("Database provider %s not supported.", provider))

  connection <- dbConnect(
    dbDriver(provider),
    host = host, port = port, user = user, password = password, dbname = dbname
  )

  return(connection)
}

Disconnect <- function(connection){
    return(dbDisconnect(connection))
}

GetData <- function(connection, query){
    return(dbGetQuery(connection, query))
}

## Correlation ====
GetSpearmansRho <- function(dataset, column.one, column.two, p.value = 0.05){
  correlation <- cor.test(
    dataset[[column.one]], dataset[[column.two]],
    method = "spearman", exact = F
  )
  p <- 1.0
  rho <- 0.0
  if (!is.na(correlation$p.value)) {
    p <- correlation$p.value
    rho <- correlation$estimate
  }

  if(p > p.value){
    warning(paste("Spearman's correlation insignificant with p-value =", p))
  }
  return(list("p" = p, "significant" = p <= p.value, "rho" = rho))
}

GetCorrelation <- function(dataset, ignore = NA){
  if (!missing(ignore)) {
    dataset <- dataset %>%
      select(-(one_of(ignore)))
  }

  dataset <- na.omit(dataset)
  correlation <- dataset %>%
    cor(., method = "spearman")

  correlation[lower.tri(correlation)] <- NA
  correlation <- melt(correlation)
  correlation <- na.omit(correlation)

  correlation <- FixCorrelation(dataset, correlation)

  return(correlation)
}

FixCorrelation <- function(dataset, correlations, p.value = 0.05) {
  for(index in seq(nrow(correlations))){
    var1 <- as.character(correlations[index, ]$Var1)
    var2 <- as.character(correlations[index, ]$Var2)
    correlation <- cor.test(
      dataset[[var1]], dataset[[var2]], method = "spearman", exact = F
    )
    if(correlation$p.value > p.value){
      cat("-", var1, "and", var2, "\n")
      correlations[
        correlations$Var1 == var1 & correlations$Var2 == var2,
        ]$value <- 0
    }
  }
  return(correlations)
}

## Association ====
GetAssociation <- function(x, y, x.label, y.label, p.value = 0.05) {
  htest <- wilcox.test(x, y)

  p <- htest$p.value
  if(p > p.value){
    warning(paste("Association outcome insignificant with p-value =", p))
  }

  test.outcome <- list()
  test.outcome[["p"]] <- p
  test.outcome[["significant"]] <- p <= p.value
  test.outcome[[paste("mean.", x.label, sep = "")]] <- mean(x)
  test.outcome[[paste("mean.", y.label, sep = "")]] <- mean(y)
  test.outcome[[paste("median.", x.label, sep = "")]] <- median(x)
  test.outcome[[paste("median.", y.label, sep = "")]] <- median(y)
  return(test.outcome)
}

GetOneSampleAssociation <- function(x, p.value = 0.05) {
  htest <- wilcox.test(x, mu = 0)
  p <- htest$p.value
  if(p > p.value){
    warning(paste("Association outcome insignificant with p-value =", p))
  }

  test.outcome <- list()
  test.outcome[["p"]] <- p
  test.outcome[["significant"]] <- p <= p.value
  test.outcome[["mean"]] <- mean(x)
  return(test.outcome)
}

GetEffectSize <- function(x, y, p.value = 0.05) {
  htest <- wilcox.test(x, y)

  p <- 1.0
  delta <- 0.0
  if (!is.na(htest$p.value)) {
    p <- htest$p.value
    delta <- cliff.delta(x, y)$estimate
  }
  if(p > p.value){
    warning(paste("Association outcome insignificant with p-value =", p))
  }

  test.outcome <- list()
  test.outcome[["p"]] <- p
  test.outcome[["significant"]] <- p <= p.value
  test.outcome[["delta"]] <- delta

  return(test.outcome)
}

TestAssociation <- function(dataset, versions, labels, p.value = 0.05){
  combinations <- combn(versions, m = 2)
  for(column in seq(ncol(combinations))){
    version.a <- combinations[1,column]
    version.b <- combinations[2,column]
    population.a <- (dataset %>% filter(version == version.a))$metric
    population.b <- (dataset %>% filter(version == version.b))$metric
    label.a <- labels[version.a]
    label.b <- labels[version.b]
    
    htest <- wilcox.test(population.a, population.b, exact = F)

    p <- htest$p.value
    significance <- ""
    if (p <= p.value){
      significance <- "*"
    }

    dataset.subset <- dataset %>% filter(version %in% c(version.a, version.b))
    dataset.subset$version <- factor(
      dataset.subset$version, levels = unique(dataset.subset$version)
    )
    cliffs.delta <- cliff.delta(metric ~ version, data = dataset.subset)
    d <- cliffs.delta$estimate
    interpretation <- paste(" (", cliffs.delta$magnitude, ")", sep="")

    means <- data.frame(
      "type.a" = mean(population.a), "type.b" = mean(population.b)
    )
    medians <- data.frame(
      "type.a" = median(population.a), "type.b" = median(population.b)
    )
    summary.stats <- rbind(medians, means)
    colnames(summary.stats) <- c(label.a, label.b)
    rownames(summary.stats) <- c("Median", "Mean")

    cat(paste(rep("-", times = 79), collapse = ""), "\n")
    cat("", label.a, "vs.", label.b, "\n")
    cat(paste(rep("-", times = 79), collapse = ""), "\n")
    cat("p-value ", p, significance, "\n", sep = "")
    cat("Effect  ", d, interpretation, "\n", sep = "")
    print(
      format(
        summary.stats, width = max(sapply(as.character(labels), nchar))
      )
    )
  }
}

## Independence ====
GetIndependence <- function(x, y, p.value = 0.05) {
  test.outcome <- chisq.test(x, y)

  p <- test.outcome$p.value
  if(p > p.value){
    warning(paste("Association outcome insignificant with p-value =", p))
  }

  test.outcome <- list()
  test.outcome[["p"]] <- p
  test.outcome[["significant"]] <- p <= p.value
  return(test.outcome)
}

## Normality ===
GetNormality <- function(data, p.value = 0.05) {
  test.outcome <- ad.test(data)
  return(list("p" = test.outcome$p.value,
              "is.normal" = test.outcome$p.value > p.value))
}

TestNormality <- function(data, label) {
  test.outcome <- ad.test(data)
  if (test.outcome$p.value < 0.05) {
    cat(label, "IS NOT normally distributed (p=", test.outcome$p.value, ")\n")
  } else {
    cat(label, "IS normally distributed (p=", test.outcome$p.value, ")\n")
  }
}
