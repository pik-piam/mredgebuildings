#' IEA database of primary, final and useful energy
#'
#' Computes data for the Primary Final Useful (PFU) resolution
#'
#' @return IEA data as MAgPIE object aggregated to country level
#'
#' @author Antoine Levesque, Robin Hasse
#'
#' @seealso \code{\link[madrat]{calcOutput}}
#' @examples
#' \dontrun{ a <- calcOutput("IEAPFU")
#' }
#'
#' @importFrom madrat toolGetMapping readSource
#' @importFrom stats na.omit
#' @importFrom dplyr %>% select filter
#' @importFrom tidyr unite
#' @importFrom magclass dimSums getNames
#' @importFrom utils read.csv2
#' @export

calcIEAPFU <- function() {

  mapping <- toolGetMapping("structuremappingFE_PFU.csv",
                            type = "sectoral", where = "mredgebuildings",
                            returnPathOnly = TRUE) %>%
    read.csv2(stringsAsFactors = FALSE, na.strings = "") %>%
    select("iea_product", "iea_flows", "pfu") %>%
    na.omit()

  # read in data and convert from ktoe to EJ
  data <- readSource("IEA", subtype = "EnergyBalances") * 4.1868E-5

  # mapping to PFU variables
  ieapfu <-  do.call("mbind", lapply(unique(mapping[["pfu"]]), function(item) {
    itemNames <- mapping %>%
      filter(.data[["pfu"]] == item) %>%
      unite("name", "iea_product", "iea_flows", sep = ".") %>%
      getElement("name") %>%
      intersect(getNames(data))
    mappedData <- data[, , itemNames] %>%
      dimSums(dim = 3, na.rm = TRUE)
    getNames(mappedData) <- item
    return(mappedData)
  }))

  return(list(x = ieapfu,
              weight = NULL,
              unit = "EJ",
              description = "IEA Data based on 2017 edition of IEA World Energy Balances with the PFU resolution"))
}
