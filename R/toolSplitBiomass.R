#' Splits Biomass according to GDP per Capita
#'
#' This is adapted from EDGE-B Function "splitBiomass.R" by Antoine Levesque.
#'
#' Any value belonging to the carrier defined by "varName" will be split into
#' the default carrier names "biomod" and "biotrad" according to weights given
#' by "dfGDPpop".
#'
#' @param df dataframe with carrier as 'variable'
#' @param dfGDPpop dataframe containing the GDP per Capita
#' @param min lower threshold of GDP per Capita in USD 2005
#' @param max upper threshold of GDP per Capita in USD 2005
#' @param varName nomenclature of biomass
#'
#' @returns factor.data.frame
#'
#' @author Hagen Tockhorn
#'
#' @importFrom tidyr one_of
#'
#' @export


toolSplitBiomass <- function(df,
                             dfGDPpop,
                             min = 10E+3,
                             max = 15E+3,
                             varName = "biomass") {

  # FUNCTIONS ------------------------------------------------------------------

  # Returns share of modern Biomass
  shareTrad <- function(x, min, max) {
    share <- pmin(x * 0 + 1, pmax(0 * x, (max - x) / (max - min)))
    return(share)
  }


  # PARAMETERS -----------------------------------------------------------------

  bioComponents <- "^bio(mod|trad).$"

  # PROCESS DATA ---------------------------------------------------------------

  keepCol <- setdiff(colnames(df), c("value", "variable"))

  dfGDPpop <- dfGDPpop %>% rename(gdppop = "value") %>% select(-"variable")

  # Temporary Fix: Rename bio column to not mess up column name overlaps
  if (varName != "biomass") {
    df <- df %>%
      filter(.data[["variable"]] != varName) %>%
      rbind(df %>%
              filter(.data[["variable"]] == varName) %>%
              mutate(variable = "biomass"))
  }


  # Split Biomass
  tmp <- df %>%
    left_join(dfGDPpop, by = c("region", "period")) %>%
    spread(key = "variable", value = "value") %>%
    mutate(shareTrad = shareTrad(.data[["gdppop"]], min, max),
           biomod2 = ifelse(is.na(.data[["biomass"]]),
                            NA,
                            .data[["biomass"]] * (1 - shareTrad)),
           biotrad2 = ifelse(is.na(.data[["biomass"]]),
                             NA,
                             .data[["biomass"]] * shareTrad))


  # Check for already existing Entries and add them up if existing
  bioCols <- grep(bioComponents, colnames(tmp), value = TRUE)

  biomodNames <- grep("^biomod.$", bioCols, value = TRUE)
  biotradNames <- grep("^biotrad.$", bioCols, value = TRUE)
  if (length(biomodNames) > 1) {
    tmp$biomod <- rowSums(tmp[, biomodNames], na.rm = TRUE)
  } else {
    tmp$biomod <- tmp[[biomodNames]]
  }

  if (length(biotradNames) > 1) {
    tmp$biotrad <- rowSums(tmp[, biotradNames], na.rm = TRUE)
  } else {
    tmp$biotrad <- tmp[[biotradNames]]
  }


  # OUTPUT ---------------------------------------------------------------------

  tmp <- tmp %>%
    select(-matches(bioComponents)) %>%
    select(-"gdppop", -"shareTrad", -"biomass") %>%
    gather(key = "variable", value = "value", -one_of(keepCol)) %>%
    quitte::factor.data.frame()


  return(tmp[colnames(df)])

}
