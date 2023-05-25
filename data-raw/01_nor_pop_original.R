library(data.table)
#' Population in Norway (2020 borders).
#'
#' We conveniently package population data taken from Statistics Norway.
#' This data is licensed under the Norwegian License for
#' Open Government Data (NLOD) 2.0.
#'
#' This dataset contains national/county/municipality/ward (city district) level population data
#' for every age (0 to 105 years old). The national level data is from year 1846, while all the
#' other levels have data from 2005.
#'
#' The counties and municipalities are updated for the 2020 borders.
#'
#' @format
#' \describe{
#' \item{year}{Year.}
#' \item{location_code}{The location code.}
#' \item{granularity_geo}{National/County/Municipality/BAregion.}
#' \item{age}{1 year ages from 0 to 105.}
#' \item{pop_jan1}{Number of people as of 1st of January.}
#' \item{imputed}{FALSE if real data. TRUE if it is the last real data point carried forward.}
#' }
#' @source \url{https://www.ssb.no/en/statbank/table/07459/tableViewLayout1/}
"norway_population_by_age_b2020"


nor_population_by_age_original <- function() {

  # x_year_end <- 2020
  # variables used in data.table functions in this function
  . <- NULL
  value <- NULL
  age <- NULL
  Var2 <- NULL
  agecont <- NULL
  pop <- NULL
  municip_code <- NULL
  municip_code_current <- NULL
  year_end <- NULL
  level <- NULL
  region <- NULL
  variable <- NULL
  agenum <- NULL
  imputed <- NULL
  county_code <- NULL
  municip_code_end <- NULL
  sex <- NULL
  contents <- NULL
  x <- NULL
  # end



  # municip and ward ----
  ### gen_norway_population_by_age() has one other dataset, 2021
  cat("creating population for municip and ward ... \n")

  popFiles <- c(
    "Personer2005-2009.csv",
    "Personer2010-2014.csv",
    "Personer2015-2018.csv",
    "Personer2019.csv",
    "Personer2020.csv",
    "Personer2021.csv",
    "Personer2022.csv",
    "Personer2023.csv",
    "Personerward2001-2020.csv",
    "Personerward2021.csv",
    "Personerward2022.csv",
    "Personerward2023.csv"

  )
  pop <- vector("list", length = length(popFiles))
  for (i in seq_along(pop)) {
    pop[[i]] <- fread(fs::path("data-raw", "files", "population", popFiles[i]), encoding = "UTF-8")
    pop[[i]] <- melt.data.table(pop[[i]], id.vars = c("region", "age"))
  }
  pop <- rbindlist(pop)

  # region, municip
  pop[, region := stringr::str_remove(region, "^K-")]
  pop[, municip_code := sprintf("municip_nor%s", stringr::str_extract(region, "^[0-9][0-9][0-9][0-9]"))]

  # correctly identify ward/bydels
  pop[, ward_code := sprintf("%s", stringr::str_extract(region, "^[0-9][0-9][0-9][0-9][0-9][0-9]"))]
  pop[, ward_prefix := ""]
  pop[ward_code!="NA" & municip_code %in% c("municip_nor0301"), ward_prefix := "wardoslo_nor"]
  pop[ward_code!="NA" & municip_code %in% c("municip_nor1201", "municip_nor4601"), ward_prefix := "wardbergen_nor"]
  pop[ward_code!="NA" & municip_code %in% c("municip_nor1103"), ward_prefix := "wardstavanger_nor"]
  pop[ward_code!="NA" & municip_code %in% c("municip_nor1601", "municip_nor5001"), ward_prefix := "wardtrondheim_nor"]

  # ward oslo
  pop[,ward_code := paste0(ward_prefix,ward_code)]
  pop[ward_code=="wardoslo_nor030116", ward_code := "extrawardoslo_nor030116"]
  pop[ward_code=="wardoslo_nor030117", ward_code := "extrawardoslo_nor030117"]
  pop[ward_code!="NA", municip_code := ward_code]


  # add calyear, age numeric
  pop[, calyear := as.numeric(stringr::str_extract(variable, "[0-9][0-9][0-9][0-9]$"))]
  pop[, agenum := as.numeric(stringr::str_extract(age, "^[0-9]*"))]
  pop[, age := NULL]
  setnames(pop, "agenum", "age")
  pop

  # sum population by municip
  pop <- pop[municip_code != "municip_norNA"]
  pop_municip <- pop[, .(
    population = sum(value)
  ), keyby = .(
    municip_code, age, calyear
  )]


  # Fixing broken parts in the population data ----
  # part 1: municip_nor0710

  pop_municip0706 <- pop_municip[municip_code == "municip_nor0710" & calyear <= 2017]
  pop_municip0706[, population := max(population), by = age]
  pop_municip0706 <- pop_municip0706[calyear != max(calyear)]
  pop_municip0706[, municip_code := "municip_nor0706"]
  pop_municip0706[, population := round(population/3)]
  pop_municip <- rbind(pop_municip, pop_municip0706)

  pop_municip0719 <- pop_municip[municip_code == "municip_nor0710" & calyear <= 2017]
  pop_municip0719[, population := max(population), by = age]
  pop_municip0719 <- pop_municip0719[calyear != max(calyear)]
  pop_municip0719[, municip_code := "municip_nor0719"]
  pop_municip0719[, population := round(population/3)]
  pop_municip <- rbind(pop_municip, pop_municip0719)

  pop_municip0720 <- pop_municip[municip_code == "municip_nor0710" & calyear <= 2017]
  pop_municip0720[, population := max(population), by = age]
  pop_municip0720 <- pop_municip0720[calyear != max(calyear)]
  pop_municip0720[, municip_code := "municip_nor0720"]
  pop_municip0720[, population := round(population/3)]
  pop_municip <- rbind(pop_municip, pop_municip0720)

  # # part 2: municip_nor1756
  pop_municip1723 <- pop_municip[municip_code == "municip_nor1756" & calyear <= 2012]
  pop_municip1723[, population := max(population), by = age]
  pop_municip1723 <- pop_municip1723[calyear != max(calyear)]
  pop_municip1723[, municip_code := "municip_nor1723"]
  pop_municip1723[, population := round(population/2)]
  pop_municip <- rbind(pop_municip, pop_municip1723)

  pop_municip1729 <- pop_municip[municip_code == "municip_nor1756" & calyear <= 2012]
  pop_municip1729[, population := max(population), by = age]
  pop_municip1729 <- pop_municip1729[calyear != max(calyear)]
  pop_municip1729[, municip_code := "municip_nor1729"]
  pop_municip1729[, population := round(population/2)]
  pop_municip <- rbind(pop_municip, pop_municip1729)

  #
  # # part 3: municip5046
  pop_municip1901 <- pop_municip[municip_code == "municip_nor5046" & calyear <= 2018]
  pop_municip1901[, population := max(population), by = age]
  pop_municip1901 <- pop_municip1901[calyear != max(calyear)]
  pop_municip1901[, municip_code := "municip_nor1901"]
  pop_municip1901[, population := round(population/2)]
  pop_municip <- rbind(pop_municip, pop_municip1901)

  pop_municip1915 <- pop_municip[municip_code == "municip_nor1756" & calyear <= 2018]
  pop_municip1915[, population := max(population), by = age]
  pop_municip1915 <- pop_municip1915[calyear != max(calyear)]
  pop_municip1915[, municip_code := "municip_nor1915"]
  pop_municip1915[, population := round(population/2)]
  pop_municip <- rbind(pop_municip, pop_municip1915)


  # # part 4: municip1505

  pop_municip1503 <- pop_municip[municip_code == "municip_nor1505" & calyear <= 2008]
  pop_municip1503[, population := max(population), by = age]
  pop_municip1503 <- pop_municip1503[calyear != max(calyear)]
  pop_municip1503[, municip_code := "municip_nor1503"]
  pop_municip1503[, population := round(population/2)]
  pop_municip <- rbind(pop_municip, pop_municip1503)

  pop_municip1556 <- pop_municip[municip_code == "municip_nor1505" & calyear <= 2008]
  pop_municip1556[, population := max(population), by = age]
  pop_municip1556 <- pop_municip1556[calyear != max(calyear)]
  pop_municip1556[, municip_code := "municip_nor1556"]
  pop_municip1556[, population := round(population/2)]
  pop_municip <- rbind(pop_municip, pop_municip1556)


  pop_municip[, imputed := FALSE]

  setnames(pop_municip, "municip_code", "location_code")
  setnames(pop_municip, "population", "pop_jan1_n")

  # calyear, municip_code, age, imputed, pop
  # imputing the future (2 calyears+)
  # missing_calyears <- max(pop_municip$calyear):(lubridate::year(lubridate::today()) + 10)
  #
  # if (length(missing_calyears) > 1) {
  #   copied_calyears <- vector("list", length = length(missing_calyears) - 1)
  #   for (i in seq_along(copied_calyears)) {
  #     copied_calyears[[i]] <- pop_municip[calyear == missing_calyears[1]]
  #     copied_calyears[[i]][, calyear := calyear + i]
  #   }
  #   copied_calyears <- rbindlist(copied_calyears)
  #   copied_calyears[, imputed := TRUE]
  #   pop_municip <- rbind(pop_municip, copied_calyears)
  # }

  pop_municip[, granularity_geo := stringr::str_extract(location_code, "^[a-z]+")]

  return(pop_municip)
}

nor_population_by_age_b0000 <- nor_population_by_age_original()
saveRDS(nor_population_by_age_b0000, "data-raw/data-temp/nor_population_by_age_b0000.rds")





