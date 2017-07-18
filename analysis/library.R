#MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM
# Load Externals
#WWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWW
source("theme.R")
source("constants.R")
source("data.R")

#MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM
# Function Definitions
#WWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWW

######################################
# Generic
######################################
init.libraries <- function(){
  options(java.parameters = "-Xmx6g")
  libraries <- c(
    "plyr", "dplyr", "ggplot2", "tidyr", "reshape2", "effsize"
  )
  for(lib in libraries){
    suppressPackageStartupMessages(library(lib, character.only = T))
  }
}

get.db.connection <- function(environment = "DEVELOPMENT"){
  if(environment == "PRODUCTION"){
    return(
      db.connect(
        provider = "PostgreSQL",
        user = "", password = "",
        host = "", port = "",
        dbname = ""
      )
    )
  } else if(environment == "DEVELOPMENT") {
    return(
      db.connect(
        provider = "SQLite", dbname = "data/db.sqlite3"
      )
    )
  } else {
    stop(sprint("Unknown environment %s.", environment))
  }
}

db.connect <- function(host = NA, port = NA, user = NA, password = NA, dbname,
                       provider = "SQLite"){
  connection <- NULL
  
  if(provider == "PostgreSQL"){
    library("RPostgreSQL")
    driver <- dbDriver(provider)
    connection <- dbConnect(driver,
                            host=host,
                            port=port,
                            user=user,
                            password=password,
                            dbname=dbname
    )
  } else if(provider == "MySQL"){
    library("RMySQL")
    driver <- dbDriver(provider)
    connection <- dbConnect(driver,
                            host=host,
                            port=port,
                            user=user,
                            password=password,
                            dbname=dbname
    )
  } else if(provider == "SQLite"){
    library("RSQLite")
    driver <- dbDriver(provider)
    connection <- dbConnect(driver,
                            dbname=dbname
    )
  } else {
    # TODO: Add other providers
    stop(sprint("Database provider %s not supported.", provider))
  }
  
  return(connection)
}

db.disconnect <- function(connection){
  return(dbDisconnect(connection))
}

db.get.data <- function(connection, query){
  return(dbGetQuery(connection, query))
}

######################################
# Analysis
######################################

run.wilcox <- function(population.one, population.two, metric, alpha = 0.05){
  population.one.metric <- population.one[[metric]]
  population.two.metric <- population.two[[metric]]
  
  result <- vector(mode = "list", length = 8)
  names(result) <- c(
    "wilcox.test.out", "is.significant",
    "one.median", "median.ieq", "two.median",
    "one.mean", "mean.ieq", "two.mean"
  )
  result$wilcox.test.out <- wilcox.test(
    population.one.metric, population.two.metric
  )
  result$is.significant <- "No"
  if(result$wilcox.test.out$p.value <= alpha){
    result$is.significant <- "Yes"
  }
  
  result$one.median = round(median(population.one.metric, na.rm=TRUE), 4)
  result$two.median = round(median(population.two.metric, na.rm=TRUE), 4)
  result$median.ieq = get.inequality(result$one.median, result$two.median)
  
  result$one.mean = round(mean(population.one.metric, na.rm=TRUE), 4)
  result$two.mean = round(mean(population.two.metric, na.rm=TRUE), 4)
  result$mean.ieq = get.inequality(result$one.mean, result$two.mean)

  return(result)
}

get.inequality <- function(one, two){
  if(one == two)
    return("=")
  else if(one > two)
    return(">")
  else
    return("<")
}
