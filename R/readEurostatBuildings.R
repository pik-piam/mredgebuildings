#' Read Eurostat data on buildings
#'
#' Read various data sets from Eurostat that are used in the modelling of
#' buildings
#' - nrg_inf_hptc: Heat pumps - technical characteristics by technologies
#' - nrg_d_hhq: Disaggregated final energy consumption in households
#' - lfst_r_lfsd2hh: Number of households by degree of urbanisation (2006-2023)
#' - ilc_hcmh02: Average size of dwelling by household type and degree of urbanisation (2012)
#' - ilc_lvho31: Average size of dwelling by household composition and degree of urbanisation (2020, 2023)
#' - cens_21dwbnr_r3: Dwellings by type of building, number of rooms or useful floor space (2021)
#' - sts_cobp_a: Building permits (1992 - 2024)
#' - ilc_lvhe02: Persons living in private households by heating system used in
#'   the dwelling, household composition and degree of urbanisation (2023)
#' - ilc_lvhe04: Persons heating their dwelling with district heating by used
#'   energy source, household composition and degree of urbanisation (2023)
#' - ilc_lvhe05: Persons heating their dwelling with other than district heating
#'   by used energy source, household composition and degree of urbanisation (2023)
#' - hbs_car_t315: Household characteristics by degree of urbanisation (1988-2020)
#' - ilc_lvho01: Distribution of population by degree of urbanisation, dwelling
#'   type and income group (2003 - 2024)
#' - ilc_lvho03: Average number of rooms per person by tenure status and
#'   dwelling type (2003 - 2024)
#'
#' @note see https://ec.europa.eu/eurostat/web/energy/data/energy-balances
#' for definitions of codes
#'
#' @param subtype Eurostat code of data set
#' @returns MAgPIE object with data
#'
#' @author Robin Hasse
#'
#' @importFrom madrat toolSubtypeSelect
#' @importFrom dplyr %>% select rename
#' @importFrom utils read.csv
#' @importFrom quitte as.quitte
#' @importFrom magclass as.magpie
#' @export

readEurostatBuildings <- function(subtype) {

  # pick file
  files <- list(
    nrg_inf_hptc    = "estat_nrg_inf_hptc_de.csv",
    nrg_d_hhq       = "nrg_d_hhq_linear.csv",
    lfst_r_lfsd2hh  = "estat_lfst_r_lfsd2hh_de.csv",
    ilc_hcmh02      = "estat_ilc_hcmh02_de.csv",
    ilc_lvho31      = "estat_ilc_lvho31_de.csv",
    cens_21dwbnr_r3 = "estat_cens_21dwbnr_r3_de.csv",
    sts_cobp_a      = "estat_sts_cobp_a_de.csv",
    ilc_lvhe02      = "estat_ilc_lvhe02_de.csv",
    ilc_lvhe04      = "estat_ilc_lvhe04_de.csv",
    ilc_lvhe05      = "estat_ilc_lvhe05_de.csv",
    hbs_car_t315    = "estat_hbs_car_t315_de.csv",
    ilc_lvho01      = "estat_ilc_lvho01_de.csv",
    ilc_lvho03      = "estat_ilc_lvho03_de.csv"
  )

  data <- toolSubtypeSelect(subtype, files) %>%
    read.csv() %>%
    select(-any_of(c("DATAFLOW", "LAST.UPDATE", "freq", "OBS_FLAG", "CONF_STATUS"))) %>%
    rename(region = "geo",
           period = "TIME_PERIOD",
           value = "OBS_VALUE") %>%
    as.quitte() %>%
    as.magpie()

  return(data)
}
