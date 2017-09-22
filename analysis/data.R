# Data Source: Database ----

GetRatings <- function() {
  query <- "
    SELECT app AS version, CAST(overall_rating_of_the_app AS INT) AS rating
    FROM view_join_user_rvevcombined
    WHERE app IN ('21', '23', '21E');
  "
  connection <- GetDbConnection(db.settings)
  dataset <- GetData(connection, query)
  dataset$version <- factor(dataset$version, levels = c("21", "23", "21E"))
  Disconnect(connection)
  return(dataset)
}

GetPerception <- function() {
  query <- "
    SELECT app AS version,
      CAST(To_What_Degree_Do_You_Agree_With_The_Statement_I_Feel_More_Secure_Using_The_Application AS INT)
        AS perception
    FROM view_join_user_rvevcombined
    WHERE app IN ('21', '23', '21E');
  "
  connection <- GetDbConnection(db.settings)
  dataset <- GetData(connection, query)
  dataset$version <- factor(dataset$version, levels = c("21", "23", "21E"))
  Disconnect(connection)
  return(dataset)
}

GetCorrectness <- function() {
  query <- "
    SELECT AppID AS version,
      CAST(TruePositivePerms_Count AS INT) AS tp,
      CAST(TrueNegativePerms_Count AS INT) AS tn,
      CAST(FalsePositivePerms_Count AS INT) AS fp,
      CAST(FalseNegativePerms_Count AS INT) AS fn
    FROM view_lib13
    WHERE AppID IN ('21', '23', '21E');
  "
  connection <- GetDbConnection(db.settings)
  dataset <- GetData(connection, query)
  dataset$version <- factor(dataset$version, levels = c("21", "23", "21E"))
  Disconnect(connection)
  return(dataset)
}

GetRandomCorrectness <- function() {
  query <- "
    SELECT AppID AS version,
      CAST(TruePositivePerms_Count AS INT) AS tp,
      CAST(TrueNegativePerms_Count AS INT) AS tn,
      CAST(FalsePositivePerms_Count AS INT) AS fp,
      CAST(FalseNegativePerms_Count AS INT) AS fn
    FROM view_lib13
    WHERE AppID IN ('23Rnd', '21ERnd');
  "
  connection <- GetDbConnection(db.settings)
  dataset <- GetData(connection, query)
  dataset$version <- factor(dataset$version, levels = c("23Rnd", "21ERnd"))
  Disconnect(connection)
  return(dataset)
}