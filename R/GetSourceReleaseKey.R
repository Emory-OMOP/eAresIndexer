#' @title getSourceReleaseKey
#' @description Returns the source release key for this CDM database for use within ARES
#'
#' @export
getSourceReleaseKey <- function(connectionDetails, cdmDatabaseSchema) {
  sql <- "SELECT * from @cdmDatabaseSchema.cdm_source"
  renderedSql <- SqlRender::render(sql,cdmDatabaseSchema = cdmDatabaseSchema)
  translatedRenderedSql <- SqlRender::translate(renderedSql,connectionDetails$dbms)

  connection <- DatabaseConnector::connect(connectionDetails)
  results <- DatabaseConnector::querySql(connection = connection, sql = translatedRenderedSql)
  
  # Normalize column names to uppercase to be resilient to Redshift/Postgres lowercase returns
  if (!is.null(results)) {
    names(results) <- toupper(names(results))
  } else {
    warning("Query to cdm_source returned NULL. Unable to generate source release key.")
    return(NULL)
  }
  
  releaseId <- format(lubridate::ymd(results[1,"CDM_RELEASE_DATE"]),"%Y%m%d")
  sourceKey <- gsub(" ", "_", results[1,"CDM_SOURCE_ABBREVIATION"])

  return(file.path(sourceKey,releaseId))
}
