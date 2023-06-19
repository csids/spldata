library(data.table)
devtools::load_all()
source("data-raw/02_nor_loc_redistricting_and_hierarchy.R")

nor_loc_name_ba_wide <- function(x_year_end = 2024){

  ba <- data.table(readxl::read_excel(fs::path("data-raw", "files", "locations", "baregioner_2024.xlsx")))
  setnames(
    ba,
    1:2,
    c(
      "municip",
      "ba"
    )
  )
  ba[, municip_code := paste0(
    "municip_nor",
    formatC(as.numeric(
      stringr::str_extract(municip, "^[0-9]+")),
      width=4,
      flag=0
    ))
  ]
  ba[, baregion_code := paste0(
    "baregion_nor",
    formatC(as.numeric(
      stringr::str_extract(ba, "^[0-9]+")),
      width=3,
      flag=0
    ))
  ]
  ba[, baregion_name := stringr::str_remove_all(ba, "^[0-9]+ ")]


  return(ba)

}




#  POPULATION depends on this ----- #
# gen_norway_locations(2020)
# original: fhidata::norway_locations_b2020
# NEW NAME: location_name_municip_wide

nor_loc_name_municip_wide <- function(x_year_end = 2024) {

  # variables used by data.table
  is_current <- NULL
  # year_end <- NULL

  # x_year_end <- 2020

  # municip ----
  municip <- nor_loc_redistricting_municip(x_year_end = x_year_end, include_extra_vars = T)

  municip <- municip[year == max(year)]
  municip <- unique(municip[, c("municip_code_current", "municip_name",
                                "county_code", "county_name",
                                'mtregion_name','mtregion_code')])
  setnames(municip, "municip_code_current", "municip_code")
  locations <- copy(municip)

  # remove svalbard and missing
  locations <- locations[!municip_code %in% c("notmainlandmunicip_nor2100",
                                              "notmainlandmunicip_nor2200",
                                              "missingmunicip_nor9999")]

  # ba ----
  # if x_year_end = 2024 then include baregions (bo- og arbeidsregioner)
  if(x_year_end == 2024){

    ba <- nor_loc_name_ba_wide()

    locations[
      ba,
      on="municip_code",
      baregion_code := baregion_code
    ]
    locations[
      ba,
      on="municip_code",
      baregion_name := baregion_name
    ]


  }

  d <- copy(locations)
  return(d)
}


nor_loc_lab <- function(ndigit_labid = 6){

  lab_raw <- data.table(readxl::read_excel(fs::path("data-raw", "files", "labnames_labcodes.xlsx")))
  lab <- copy(lab_raw)
  setnames(lab, c("Laboratorium", "Lab_id"), c("lab_name", "lab_code_raw"))
  lab

  # make long id
  # ndigit_labid <- 6

  lab[, ndigit_lab_code := nchar(lab_code_raw)]
  stopifnot(max(lab$ndigit_lab_code)<=ndigit_labid)
  lab[, n_zero := ndigit_labid - ndigit_lab_code]

  zeros <- purrr::map_chr(lab$n_zero, function(x){zero_string(x)})
  lab[, lab_code := paste0("lab_nor",zeros, lab_code_raw)]

  lab[, lab_code_raw := NULL]
  lab[, ndigit_lab_code := NULL]
  lab[, n_zero := NULL]

  setcolorder(lab, c("lab_code", "lab_name"))

  lab
  return(lab)
}




#' Names of areas in Norway that existed in 2020.
#'
#' @format
#' \describe{
#' \item{location_code}{Location code.}
#' \item{location_name}{Location name.}
#' \item{location_name_description_nb}{Location name with additional description.}
#' \item{location_name_file_nb_utf}{Location name that should be used in file names, with Norwegian characters.}
#' \item{location_name_file_nb_ascii}{Location name that should be used in file names, without Norwegian characters.}
#' \item{location_order}{The preferred presentation order}
#' \item{granularity_geo}{nation, county, municip, wardoslo, wardbergen, wardstavanger, wardtrondheim, baregion}
#' }
#' @source \url{https://no.wikipedia.org/wiki/Liste_over_norske_kommunenummer}
"norway_locations_names_b2020"

# Creates the norway_locations data.table
nor_loc_name_all <- function(x_year_end = 2024) {
  # x_year_end <- 2020
  location_wide <- nor_loc_hierarchy_all(x_year_end = x_year_end)

  # municip_code, municip_name
  # county_code, county_name
  # region_code, region_name
  # mtregion_code, mtregion_name
  # baregion_code, baregion_name,
  # ward_code, ward_name
  # missingward_code, missingward_name
  # missingmunicip_code, missingmunicip_name
  # missingcounty_code, missingcounty_name
  # notmainlandmunicip_code, notmainlandmunicip_name
  # notmainlandcounty_code, notmainlandcounty_name

  # all[!is.na(missingward_code)]  # 4 missing wards in oslo, stavanger, bergen, trondheim
  # all[!is.na(missingmunicip_code)|!is.na(missingcounty_code) ]
  # 1 missingmunicip9999, missingcounty99
  # all[!is.na(notmainlandmunicip_code)|!is.na(notmainlandcounty_code)]
  # 2 notmainland2100, 2200 (21, 22)



  # set group order ----
  d <- rbind(
    data.table(location_code = "nation_nor", location_name = "Norge", group_order = 1),

    location_wide[,.(location_code = county_code, location_name = county_name, group_order = 2)],
    location_wide[,.(location_code = notmainlandcounty_code, location_name = notmainlandcounty_name, group_order = 3)],
    location_wide[,.(location_code = missingcounty_code, location_name = missingcounty_name, group_order = 4)],

    location_wide[,.(location_code = municip_code, location_name = municip_name, group_order = 5)],
    location_wide[,.(location_code = notmainlandmunicip_code, location_name = notmainlandmunicip_name, group_order = 6)],
    location_wide[,.(location_code = missingmunicip_code, location_name = missingmunicip_name, group_order = 7)],

    location_wide[,.(location_code = ward_code, location_name = ward_name, group_order = 8)],
    location_wide[,.(location_code = missingward_code, location_name = missingward_name, group_order = 9)],

    location_wide[,.(location_code = georegion_code, location_name = georegion_name, group_order = 11)],
    location_wide[,.(location_code = baregion_code, location_name = baregion_name, group_order = 10)],
    location_wide[,.(location_code = mtregion_code, location_name = mtregion_name, group_order = 12)]
  )

  d[, granularity_geo := get_granularity_geo(location_code)]
  d <- stats::na.omit(unique(d))
  d

  # location order (a-aa) ----
  # d$location_name
  # d[location_name %in% c("Ålesund", "Ås", "Øst")]
  #
  # substr("alesund", 1, 1)
  #
  # nms <- d$location_name
  # substr(nms, 1, 1) %>% unique %>% order
  # sort(c("Oslo", "Ålesund", "Ås", "Øst"))
  # for now it is ordered as aa is the first

  setorder(d, group_order, location_name)
  d[, group_order := NULL]
  d[, location_order := 1:.N]

  # lab ----

  lab <- nor_loc_lab()

  lab_wide <- copy(lab)
  setnames(lab_wide, c("lab_code", "lab_name"), c("location_code", "location_name"))

  lab_wide[, granularity_geo := "lab"]
  lab_wide[, location_order := (1:nrow(lab_wide) + max(d$location_order))]

  d <- rbind(d, lab_wide)
  d



  # location name description nb ----
  d[granularity_geo== "nation", location_name_description_nb := location_name]
  d[granularity_geo== "county", location_name_description_nb := paste0(location_name, " (fylke)")]
  d[granularity_geo== "notmainlandcounty", location_name_description_nb := paste0(location_name, " (fylke)")]
  d[granularity_geo== "missingcounty", location_name_description_nb := paste0(location_name, " (fylke)")]


  d[nor_locations_hierarchy_from_to(from="municip",to="county",include_to_name = T, border = x_year_end),
    on="location_code==from_code",
    location_name_description_nb := paste0(location_name, " (kommune i ", to_name, ")")
  ]
  d[nor_locations_hierarchy_from_to(from="notmainlandmunicip",to="notmainlandcounty",include_to_name = T, border = x_year_end),
    on="location_code==from_code",
    location_name_description_nb := paste0(location_name, " (kommune i ", to_name, ")")
  ]
  d[nor_locations_hierarchy_from_to(from="missingmunicip",to="missingcounty",include_to_name = T, border = x_year_end),
    on="location_code==from_code",
    location_name_description_nb := paste0(location_name, " (kommune i ", to_name, ")")
  ]

  d[nor_locations_hierarchy_from_to(from=c("wardoslo","extrawardoslo"),to="municip",include_to_name = T, border = x_year_end),
    on="location_code==from_code",
    location_name_description_nb := paste0(location_name, " (bydel i ", to_name, ")")
  ]
  d[nor_locations_hierarchy_from_to(from="wardbergen",to="municip",include_to_name = T, border = x_year_end),
    on="location_code==from_code",
    location_name_description_nb := paste0(location_name, " (bydel i ", to_name, ")")
  ]
  d[nor_locations_hierarchy_from_to(from="wardtrondheim",to="municip",include_to_name = T, border = x_year_end),
    on="location_code==from_code",
    location_name_description_nb := paste0(location_name, " (bydel i ", to_name, ")")
  ]
  d[nor_locations_hierarchy_from_to(from="wardstavanger",to="municip",include_to_name = T, border = x_year_end),
    on="location_code==from_code",
    location_name_description_nb := paste0(location_name, " (bydel i ", to_name, ")")
  ]

  d[nor_locations_hierarchy_from_to(from="missingwardoslo",to="municip",include_to_name = T, border = x_year_end),
    on="location_code==from_code",
    location_name_description_nb := paste0(location_name, " (bydel i ", to_name, ")")
  ]
  d[nor_locations_hierarchy_from_to(from="missingwardbergen",to="municip",include_to_name = T, border = x_year_end),
    on="location_code==from_code",
    location_name_description_nb := paste0(location_name, " (bydel i ", to_name, ")")
  ]
  d[nor_locations_hierarchy_from_to(from="missingwardtrondheim",to="municip",include_to_name = T, border = x_year_end),
    on="location_code==from_code",
    location_name_description_nb := paste0(location_name, " (bydel i ", to_name, ")")
  ]
  d[nor_locations_hierarchy_from_to(from="missingwardstavanger",to="municip",include_to_name = T, border = x_year_end),
    on="location_code==from_code",
    location_name_description_nb := paste0(location_name, " (bydel i ", to_name, ")")
  ]

  d[granularity_geo== "baregion", location_name_description_nb := paste0(location_name, " (BA-region)")]
  d[granularity_geo== "mtregion", location_name_description_nb := paste0(location_name, " (Mattilsynet-region)")]
  d[granularity_geo== "georegion", location_name_description_nb := paste0(location_name, " (landsdel)")]

  d[granularity_geo == "lab", location_name_description_nb := paste0(location_name, " (lab)")]

  # location_name_short
  if(x_year_end == 2024){
    d[granularity_geo=="county"]
    d[location_code=="nation_nor", location_name_short := "NOR"]
    d[location_code=="county_nor42", location_name_short := "AGD"] # agder
    d[location_code=="county_nor32", location_name_short := "AKR"] # akershus
    d[location_code=="county_nor33", location_name_short := "BUS"] # buskerud
    d[location_code=="county_nor56", location_name_short := "FNM"] # finnmark
    d[location_code=="county_nor34", location_name_short := "INN"] # innlandet
    d[location_code=="county_nor15", location_name_short := "MRO"] # møre og romsdal
    d[location_code=="county_nor18", location_name_short := "NLD"] # nordland
    d[location_code=="county_nor03", location_name_short := "OSL"] # oslo
    d[location_code=="county_nor11", location_name_short := "ROG"] # rogaland
    d[location_code=="county_nor40", location_name_short := "TEL"] # telemark
    d[location_code=="county_nor55", location_name_short := "TRO"] # troms
    d[location_code=="county_nor50", location_name_short := "TRØ"] # trøndelag
    d[location_code=="county_nor39", location_name_short := "VFO"] # vestfold
    d[location_code=="county_nor46", location_name_short := "VLD"] # vestland
    d[location_code=="county_nor31", location_name_short := "ØFO"] # østfold
    d[granularity_geo=="wardoslo"]
    d[location_code=="wardoslo_nor030112", location_name_short := "Alna"] # alna
    d[location_code=="wardoslo_nor030109", location_name_short := "Bjerke"] # bjerke
    d[location_code=="wardoslo_nor030105", location_name_short := "Frogner"] # frogner
    d[location_code=="wardoslo_nor030101", location_name_short := "Gml Oslo"] # gamle oslo
    d[location_code=="wardoslo_nor030110", location_name_short := "Grorud"] # grorud
    d[location_code=="wardoslo_nor030102", location_name_short := "Grünl"] # grunerløkka
    d[location_code=="wardoslo_nor030108", location_name_short := "N. Aker"] # nordre aker
    d[location_code=="wardoslo_nor030114", location_name_short := "Nordstrand"] # nordstrand
    d[location_code=="wardoslo_nor030103", location_name_short := "Sagn"] # sagene
    d[location_code=="wardoslo_nor030104", location_name_short := "St. Hans"] # st. hanshaugen
    d[location_code=="wardoslo_nor030111", location_name_short := "Stovner"] # stovner
    d[location_code=="wardoslo_nor030115", location_name_short := "S. Nordstrand"] # søndre nordstrand
    d[location_code=="wardoslo_nor030106", location_name_short := "Ullern"] # ullern
    d[location_code=="wardoslo_nor030107", location_name_short := "V. Aker"] # vestre aker
    d[location_code=="wardoslo_nor030113", location_name_short := "Østensjø"] # Østensjø
    d[location_code=="extrawardoslo_nor030117", location_name_short := "Marka"] # marka
    d[location_code=="extrawardoslo_nor030116", location_name_short := "Sentr"] # sentrum
  } else if(x_year_end == 2020){
    d[granularity_geo=="county"]
    d[location_code=="nation_nor", location_name_short := "NOR"]
    d[location_code=="county_nor42", location_name_short := "AGD"] # agder
    d[location_code=="county_nor34", location_name_short := "INN"] # innlandet
    d[location_code=="county_nor15", location_name_short := "MRO"] # møre og romsdal
    d[location_code=="county_nor18", location_name_short := "NLD"] # nordland
    d[location_code=="county_nor03", location_name_short := "OSL"] # oslo
    d[location_code=="county_nor11", location_name_short := "ROG"] # rogaland
    d[location_code=="county_nor54", location_name_short := "TRF"] # troms og finnmark
    d[location_code=="county_nor50", location_name_short := "TRØ"] # trøndelag
    d[location_code=="county_nor38", location_name_short := "VFT"] # vestfold og telemark
    d[location_code=="county_nor46", location_name_short := "VLD"] # vestland
    d[location_code=="county_nor30", location_name_short := "VIK"] # Viken
    d[granularity_geo=="wardoslo"]
    d[location_code=="wardoslo_nor030112", location_name_short := "Alna"] # alna
    d[location_code=="wardoslo_nor030109", location_name_short := "Bjerke"] # bjerke
    d[location_code=="wardoslo_nor030105", location_name_short := "Frogner"] # frogner
    d[location_code=="wardoslo_nor030101", location_name_short := "Gml Oslo"] # gamle oslo
    d[location_code=="wardoslo_nor030110", location_name_short := "Grorud"] # grorud
    d[location_code=="wardoslo_nor030102", location_name_short := "Grünl"] # grunerløkka
    d[location_code=="wardoslo_nor030108", location_name_short := "N. Aker"] # nordre aker
    d[location_code=="wardoslo_nor030114", location_name_short := "Nordstrand"] # nordstrand
    d[location_code=="wardoslo_nor030103", location_name_short := "Sagn"] # sagene
    d[location_code=="wardoslo_nor030104", location_name_short := "St. Hans"] # st. hanshaugen
    d[location_code=="wardoslo_nor030111", location_name_short := "Stovner"] # stovner
    d[location_code=="wardoslo_nor030115", location_name_short := "S. Nordstrand"] # søndre nordstrand
    d[location_code=="wardoslo_nor030106", location_name_short := "Ullern"] # ullern
    d[location_code=="wardoslo_nor030107", location_name_short := "V. Aker"] # vestre aker
    d[location_code=="wardoslo_nor030113", location_name_short := "Østensjø"] # Østensjø
    d[location_code=="extrawardoslo_nor030117", location_name_short := "Marka"] # marka
    d[location_code=="extrawardoslo_nor030116", location_name_short := "Sentr"] # sentrum
  }

  # nb_utf ----
  d[, location_name_file_nb_utf := location_name_description_nb]
  d[, location_name_file_nb_utf := stringr::str_replace_all(location_name_file_nb_utf, "-", "_")]
  d[, location_name_file_nb_utf := stringr::str_replace_all(location_name_file_nb_utf, " ", "_")]
  d[, location_name_file_nb_utf := stringr::str_replace_all(location_name_file_nb_utf, "/", "_")]
  d[, location_name_file_nb_utf := stringr::str_remove_all(location_name_file_nb_utf, "\\.")]
  d[, location_name_file_nb_utf := stringr::str_remove_all(location_name_file_nb_utf, "\\(")]
  d[, location_name_file_nb_utf := stringr::str_remove_all(location_name_file_nb_utf, "\\)")]
  d[, location_name_file_nb_utf := stringr::str_replace_all(location_name_file_nb_utf, "kommune_kommune", "kommune")]
  d[, location_name_file_nb_utf := stringr::str_replace_all(location_name_file_nb_utf, "fylke_fylke", "fylke")]
  d[, location_name_file_nb_utf := stringr::str_replace_all(location_name_file_nb_utf, "bydel_i_Bergen_bydel_i_Bergen", "bydel_i_Bergen")]
  d[, location_name_file_nb_utf := stringr::str_replace_all(location_name_file_nb_utf, "Ukjent_bydel_i_Oslo_bydel_i_Oslo", "Ukjent_bydel_i_Oslo")]
  d[, location_name_file_nb_utf := stringr::str_replace_all(location_name_file_nb_utf, "Ukjent_bydel_i_Stavanger_bydel_i_Stavanger", "Ukjent_bydel_i_Stavanger")]
  d[, location_name_file_nb_utf := stringr::str_replace_all(location_name_file_nb_utf, "Ukjent_bydel_i_Trondheim_bydel_i_Trondheim", "Ukjent_bydel_i_Trondheim")]

  # nb_ascii ----
  d[, location_name_file_nb_ascii := location_name_file_nb_utf]
  d[, location_name_file_nb_ascii := stringr::str_replace_all(location_name_file_nb_ascii, csdata::nb$AA, "A")]
  d[, location_name_file_nb_ascii := stringr::str_replace_all(location_name_file_nb_ascii, csdata::nb$aa, "a")]
  d[, location_name_file_nb_ascii := stringr::str_replace_all(location_name_file_nb_ascii, csdata::nb$AE, "A")]
  d[, location_name_file_nb_ascii := stringr::str_replace_all(location_name_file_nb_ascii, csdata::nb$ae, "a")]
  d[, location_name_file_nb_ascii := stringr::str_replace_all(location_name_file_nb_ascii, csdata::nb$OE, "O")]
  d[, location_name_file_nb_ascii := stringr::str_replace_all(location_name_file_nb_ascii, csdata::nb$oe, "o")]
  # stringi::stri_escape_unicode(stringi::stri_enc_toutf8())
  d[, location_name_file_nb_ascii := stringr::str_replace_all(location_name_file_nb_ascii, "\\u00e1", "a")]
  d[, location_name_file_nb_ascii := stringr::str_replace_all(location_name_file_nb_ascii, "\\u0161", "s")]
  d[, location_name_file_nb_ascii := stringr::str_replace_all(location_name_file_nb_ascii, "\\u00fc", "u")]

  # d[, location_name := stringi::stri_enc_toascii(location_name)]
  # d[, location_name := gsub("\032"stringi::stri_enc_toascii(location_name)]
  #
  # d[, location_name := iconv(location_name, from="ASCII", to="UTF-8")]
  # d[, location_name_description_nb := iconv(location_name_description_nb, from="UTF-8", to="ASCII")]
  # d[, location_name_file_nb_utf := iconv(location_name_file_nb_utf, from="UTF-8", to="ASCII")]



  setcolorder(
    d,
    c(
      "location_code",
      "location_name",
      "location_name_short",
      "location_name_description_nb",
      "location_name_file_nb_utf",
      "location_name_file_nb_ascii",
      "location_order",
      "granularity_geo"
    )
  )

  return(d)
}

#' All names in Norway (programmable borders).
#'
#' @param border The border year
#' @examples
#' norway_locations_names()
#' @export
norway_locations_names <- function(border = csdata::config$border_nor){
  stopifnot(border==2024)
  if(border==2024){
    d <- copy(csdata::norway_locations_names_b2024)
  }
  return(d)
}


# saving internal

env = new.env()
if(file.exists("R/sysdata.rda")) load("R/sysdata.rda", envir = env)

env$nor_locations_names_b2020 <- nor_loc_name_all(2020)
env$nor_locations_names_b2024 <- nor_loc_name_all(2024)

for(i in names(env)){
  .GlobalEnv[[i]] <- env[[i]]
}
txt <- paste0("usethis::use_data(",paste0(names(env),collapse=","),", overwrite = TRUE, internal = TRUE, compress = 'xz', version = 3)")
eval(parse(text = txt))

