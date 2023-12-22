nor_population_by_age_internal <- function(
  data,
  vals,
  name
  ){

  . <- age <- age_cat <- pop_jan1_n <- calyear <- location_code <- sex <- imputed <- granularity_geo <- NULL

  d <- copy(data[age %in% vals])
  d[, age_cat := name]

  d <- d[!is.na(age_cat),.(
    pop_jan1_n = sum(pop_jan1_n)
  ),keyby=.(
    calyear, location_code, age=age_cat, sex, imputed, granularity_geo
  )]
  setcolorder(d, c("granularity_geo", "location_code", "age", "sex", "calyear", "pop_jan1_n", "imputed"))

  return(d)
}

#' Population in Norway by categories
#'
#' A function that easily categorizes the Norwegian population into different age categories.
#'
#' @param cats A list containing vectors that you want to categorize.
#' @param include_total Boolean. Should 'total' be included as an age cat?
#' @param include_9999 Boolean. Should the current year is duplicated and added as "calyear==9999".
#' This is in accordance with the cstidy principles regarding granularity_time=="event_*".
#' @param border The year in which Norwegian geographical boundaries were designated (2020, 2024).
#' @examples
#' \dontrun{
#' nor_population_by_age_cats(cats = list(c(1:10), c(11:20)))
#' }
#' @return A data.table containing the following columns:
#' - granularity_geo
#' - location_code
#' - age (as specified in the argument "cats")
#' - sex ("total")
#' - calyear
#' - pop_jan1_n
#' - imputed
#' @export
nor_population_by_age_cats <- function(
  cats = NULL,
  include_total = TRUE,
  include_9999 = FALSE,
  border = csdata::config$border_nor
){

  calyear <- NULL
  if(is.null(cats)) cats <- list()
  stopifnot(is.list(cats))
  stopifnot(border %in% c(2020, 2024))

  if(border==2020){
    x <- get0("nor_population_by_age_b2020", envir = asNamespace("csdata"))
  } else if(border==2024){
    x <- get0("nor_population_by_age_b2024", envir = asNamespace("csdata"))
  }
  data <- copy(x)

  if(include_total){
    cats[[length(cats)+1]] <- -99:1000
    names(cats)[length(cats)] <- "total"
  }

  retval <- vector("list", length = length(cats))
  for(i in seq_along(cats)){
    vals <- cats[[i]]
    name <- names(cats)[i]
    if(is.null(name) | is.na(name)){
      name <- paste0(formatC(vals[1],width=3,flag="0"),"_",formatC(vals[length(vals)],width=3,flag="0"))
    } else if(name==""){
      name <- paste0(formatC(vals[1],width=3,flag="0"),"_",formatC(vals[length(vals)],width=3,flag="0"))
    }

    retval[[i]] <- nor_population_by_age_internal(
      data,
      vals = vals,
      name = name
    )
  }

  retval <- rbindlist(retval)

  # 9999 as current year
  if(include_9999){
    x <- retval[calyear==format.Date(Sys.time(),"%Y")]
    x[, calyear := 9999]
    retval <- rbindlist(list(retval, x), use.names = T)
  }

  return(retval)
}
