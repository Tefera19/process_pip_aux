library(data.table)
library(dplyr)

# Suggestion to harmonized auxiliary data files key variables.

# 1- reference year is going to be renamed as surveyid_year
# 2- reporting level such us pop_data_level, cpi_data_level, pce_data_level, ppp_data_level, and gdp_data_level
# will be renamed as reporting_level

# Here are suggestions for each auxiliary to harmonize their key variables:

# cpi ---------------------------------------------------
# cpi - key variables consisted of country_code, cpi_year, survey_acronym, and cpi_data_level.
# cpi_year and cpi_data_level variables are going to be renamed as surveyid_year and reporting_level
cpi <- pipload::pip_load_aux("cpi")
cpi <- cpi |>
  setnames(c("cpi_year", "cpi_data_level"),
           c("surveyid_year", "reporting_level"))

# pop ---------------------------------------------------
# pop - key variables consisted of country_code, year, and pop_data_level
# year and pop_data_level are going to be renamed as surveyid_year and reporting_level
pop <- pipload::pip_load_aux("pop")
pop <- pop |>
  setnames(c("year", "pop_data_level"),
           c("surveyid_year", "reporting_level"))

# pce ---------------------------------------------------
# pce - key variables consisted of country_code, year, and pce_data_level
# year and pce_data_level are going to be renamed as surveyid_year and reporting_level
pce <- pipload::pip_load_aux("pce")
pce <- pce |>
  setnames(c("year", "pce_data_level"),
           c("surveyid_year", "reporting_level"))

# gdp ---------------------------------------------------
# gdp - key variables consisted of country_code, year, and gdp_data_level
# year and gdp_data_level are going to be renamed as surveyid_year and reporting_level
gdp <- pipload::pip_load_aux("gdp")
gdp <- gdp |>
  setnames(c("year", "gdp_data_level"),
           c("surveyid_year", "reporting_level"))

# gdm ---------------------------------------------------
# gdm key variables consisted of country_code, year, and pop_data_level
# pop_data_level is going to be renamed as reporting_level
gdm <- pipload::pip_load_aux("gdm")
gdm <- gdm |>
  setnames("pop_data_level", "reporting_level")

any(duplicated(gdm,
               by = c("country_code", "surveyid_year",
                      "reporting_level")))

# ppp ---------------------------------------------------
# ppp - key variables consisted of country_code, ppp_year, ppp_data_level, ppp_data_level, release, adaption_version, and cpi_data_level.
# ppp data needs to be filtered by ppp defualt variable and then rename ppp_data_level to reporting_level
# to merge it with any of the auxiliary datasets
ppp <- pipload::pip_load_aux("ppp")
ppp <- ppp[ppp_default == TRUE, ] |>
  setnames("ppp_data_level", "reporting_level")

any(duplicated(ppp,
               by = c("country_code", "reporting_level")))

# maddison ---------------------------------------------------
# maddison - key variables consisted of country_code, and year.
# year is going to be renamed as surveyid_year
maddison <- pipload::pip_load_aux("maddison")
maddison <- maddison |>
  setnames("year", "surveyid_year")

# weo ---------------------------------------------------
# weo - key variables consisted of country_code, and year.
# year is going to be renamed as surveyid_year
weo <- pipload::pip_load_aux("weo")
weo <- weo |>
  setnames("year", "surveyid_year")

# npl ---------------------------------------------------
# npl - key variables consisted of country_code, and reporting_year.
# reporting_year is going to be renamed as surveyid_year * if content of the reporting_year
# is similar to surveyid_year
npl <- pipload::pip_load_aux("npl")
npl <- npl |>
  setnames("reporting_year", "surveyid_year")

# pfw ---------------------------------------------------
# pfw - key variables consisted of country_code, year [surveyid_year][survey_year][reporting_year],
# and welfare_type [survey_acronym]. We need to get reporting_level variable to merge
# pfw data with any of the other auxiliary datasets. We need to generate reporting level variable using cpi dataset as follows:

#### ????? is possible to include reporting_level variable in the pfw? If possible we don't
# need to run the following script that would generate reporting_level.

pfw <- pipload::pip_load_aux("pfw")

pfw_key_options <- pfw[, .(country_code,
                           year,
                           surveyid_year,
                           survey_acronym,
                           survey_coverage,
                           welfare_type,
                           survey_year,
                           cpi_domain,
                           cpi_domain_var)]

pfw_key_options <- pfw_key_options[, cpi_domain_value:=
                                     fifelse(cpi_domain_var == "urban",
                                             0, 1)]

# load cpi ---------------------------------------------------
cpi <- pipload::pip_load_aux("cpi")

cpi_key <- cpi[, .(country_code,
                   # cpi_year,
                   survey_year,
                   survey_acronym,
                   cpi_domain,
                   cpi_domain_value,
                   cpi_data_level)] |>
  setnames("cpi_data_level", "reporting_level")

cpi_key <- cpi_key[, cpi_domain :=
                     fifelse(cpi_domain == "National",
                             1, 2)]

cpi_key$cpi_domain  <-  as.numeric(cpi_key$cpi_domain)

pfw_cpi_key <- cpi_key[pfw_key_options, on = .(country_code, survey_year,
                                               survey_acronym, cpi_domain, cpi_domain_value)]

pfw_cpi_key <- pfw_cpi_key |>
  group_by(country_code, survey_year,
           survey_acronym) |>
  mutate(year_ = mean(year, na.rm = TRUE),
         surveyid_year_ = mean(surveyid_year, na.rm = TRUE)) |>
  ungroup() |>
  mutate(year_ = ifelse(is.na(year_), cpi_year, year_),
         surveyid_year_ = ifelse(is.na(surveyid_year_), cpi_year, surveyid_year_),
         year = ifelse(is.na(year), year_, year),
         surveyid_year = ifelse(is.na(surveyid_year), surveyid_year_, surveyid_year)) |>
  select(country_code, survey_year, survey_acronym, reporting_level, cpi_domain) |>
  setDT()

# add reporting level variable into pfw dataset
pfw <-  pfw_cpi_key[pfw, on= .(country_code, survey_year,
                               survey_acronym, cpi_domain)]

# income_groups ---------------------------------------------------
# income_groups - key variables consisted of country_code and year_data.
# year_data is going to be renamed as surveyid_year
income_groups <- pipload::pip_load_aux("income_groups")
income_groups <- income_groups |>
  setnames("year_data", "surveyid_year")

