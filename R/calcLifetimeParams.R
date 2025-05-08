#' Weibull lifetime distribution parameters
#'
#' Parameters for the lifetime of heating systems are taken from a very
#' detailed EIA publication for building sector appliances and equipment. The
#' range of the building shell lifetime is taken from Skarning et al. 2017.
#'
#' @source https://www.eia.gov/analysis/studies/buildings/equipcosts/pdf/full.pdf
#' @source http://dx.doi.org/10.1016/j.enbuild.2017.01.080
#'
#' @param subtype character, type of asset (either `building`, 'heatingSystem'
#'   or 'buildingShell')
#' @returns MagPIE object with Weibull lifetime distribution parameters
#'
#' @author Robin Hasse
#'
#' @importFrom madrat readSource calcOutput toolGetMapping
#' @importFrom magclass add_dimension as.magpie mselect getSets mbind
#' @importFrom quitte inline.data.frame
#' @importFrom dplyr .data %>% mutate filter everything
#'   right_join
#' @importFrom tidyr pivot_longer
#' @export

calcLifetimeParams <- function(subtype) {

  # FUNCTIONS ------------------------------------------------------------------

  # dind Weibull parameters to given mean and standard deviation
  aproxWeibull <- function(m, s, scale = 20, shape = 3, eps = 1E-5, iMax = 100) {
    speed <- c(scale = 1.2, shape = 1)
    calS <- function(scale, shape) {
      scale * sqrt(gamma(1 + 2 / shape) - gamma(1 + 1 / shape)^2)
    }
    calM <- function(scale, shape) {
      scale * gamma(1 + 1 / shape)
    }

    for (i in seq_len(iMax)) {

      sAprox <- calS(scale, shape)
      mAprox <- calM(scale, shape)

      scale <- scale * (m / mAprox)^speed[["scale"]]
      shape <- shape / (s / sAprox)^speed[["shape"]]

      if (all(abs(c(m - mAprox, s - sAprox)) < eps)) break
    }

    return(list(scale = scale, shape = shape))
  }



  # READ & CALCULATE -----------------------------------------------------------


  switch(subtype,

    ## Buildings ====

    building = {

      ### Deetman et al. ####

      res <- readSource("Deetman2020", "residential")
      com <- readSource("Deetman2020", "commercial")

      params <- do.call(mbind, lapply(c("SFH", "MFH", "Com"), function(typ) {
        switch(typ, SFH = res, MFH = res, Com = com) %>%
          add_dimension(add = "typ", nm = typ)
      }))

      params <- mselect(params, variable = c("scale", "shape"))


      ### Sandberg et al. ####

      resEUR <- readSource("Sandberg") %>%
        as_tibble() %>%
        select(-"unit") %>%
        group_by(across(-all_of(c("constructionPeriod", "value")))) %>%
        summarise(value = mean(.data$value, na.rm = TRUE), .groups = "drop") %>%
        group_by(across(-all_of(c("region", "value")))) %>%
        mutate(value = ifelse(is.na(.data$value),
                              mean(.data$value, na.rm = TRUE),
                              .data$value)) %>%
        ungroup() %>%
        pivot_wider(names_from = "variable") %>%
        mutate(params = Map(aproxWeibull,
                            m = .data$averageLifetime,
                            s = .data$averageLifetime / 2,
                            scale = 1.1 * .data$averageLifetime)) %>%
        tidyr::unnest_longer("params", indices_to = "variable", values_to = "value") %>%
        select("region", "variable", "value") %>%
        as.magpie(tidy = TRUE)


      ### combine ####
      eur <- toolGetMapping("regionmappingH12.csv") %>%
        filter(.data$RegionCode == "EUR") %>%
        getElement("CountryCode")

      mselect(params, region = eur, typ = c("SFH", "MFH")) <- resEUR[eur, , ]


      description <- "Weibull lifetime distribution parameters for buildings"

    },


    ## Heating system ====

    heatingSystem = {


      ### EIA equipment data ####

      # (values taken directly from PDF notes)
      # TODO: write a read function for this rich EIA data # nolint: todo_comment_linter.

      # technologies already given with Weibull parameters
      # delay is approximated by adding it to the scale
      params <- inline.data.frame(
        "subsector;   hs;   scale; shape",
        "Res;         ehp1; 16.88;     2" # Residential Air-Source Heat Pumps
      )

      # technologies given with typical ranges
      ranges <- inline.data.frame(
        "subsector;   hs;   from; to",
        "Res;         biom;   12; 25", # Residential Cordwood / Wood Pallet Stoves
        "Res;         h2bo;   20; 30", # Residential Gas-Fired Boilers
        "Res;         gabo;   20; 30", # Residential Gas-Fired Boilers
        "Res;         reel;   15; 30", # Residential Electric Resistance (Unit) Heaters
        "Res;         libo;   18; 28"  # Residential Oil-Fired Boilers
      )

      # technologies given only with central estimate
      central <- inline.data.frame(
        "subsector; hs;    mean",
        "Com;       ehp1;    21", # Commercial Rooftop Heat Pumps
        "Com;       h2bo;    25", # Commercial Gas-Fired Boilers
        "Com;       gabo;    25", # Commercial Gas-Fired Boilers
        "Com;       reel;    18", # Commercial Electric Resistance Heaters
        "Com;       libo;    25", # Commercial Oil-Fired Boilers
        "Res;       dihe;    30", # value assumed, not from EIA
        "Com;       dihe;    30"  # value assumed, not from EIA
      )


      ### derive Weibull parameters ####

      # we assume probabilities of the lower and upper value respectively
      prob <- c(from = 0.25, to = 0.75)
      ranges <- ranges %>%
        mutate(shape = log(log(1 - prob[["to"]]) / log(1 - prob[["from"]]),
                           .data[["to"]] / .data[["from"]]),
               scale = .data[["from"]] / (-log(1 - prob[["from"]]))^(1 / .data[["shape"]]))
      params <- rbind(params, ranges[, colnames(params)])

      # average coefficient of variance (cv)
      cv <- params %>%
        mutate(var = .data[["scale"]]^2 * (gamma(1 + 2 / .data[["shape"]]) -
                                             gamma(1 + 1 / .data[["shape"]])^2),
               mean = .data[["scale"]] * gamma(1 + 1 / .data[["shape"]]),
               cv = sqrt(.data[["var"]]) / .data[["mean"]]) %>%
        getElement("cv") %>%
        mean()

      # for central values, we assume they have the average cv of the other
      # technologies' lifetime distribution
      central <- central %>%
        mutate(sd = cv * .data[["mean"]])
      central <- do.call(rbind,
                         Map(aproxWeibull, m = central$mean, s = central$sd)) %>%
        cbind(central) %>%
        mutate(shape = as.numeric(.data[["shape"]]),
               scale = as.numeric(.data[["scale"]]))

      params <- rbind(params, central[, colnames(params)])


      ### assume missing values ####

      # assume residential biomass value for commercial
      params <- params %>%
        rbind(params %>%
                filter(.data[["subsector"]] == "Res",
                       .data[["hs"]] == "biom") %>%
                mutate(subsector = "Com"))

      # assume biomass values for coal
      params <- params %>%
        rbind(params %>%
                filter(.data[["hs"]] == "biom") %>%
                mutate(hs = "sobo"))

      # all technologies included?
      hs <- toolGetMapping("heatingSystem.csv",
                           type = "sectoral", where = "brick")
      params <- params %>%
        right_join(hs["hs"], by = "hs")
      if (any(is.na(params))) {
        stop("Incomplete mapping of heating technologies.")
      }



      ### map to building types ####
      typMap <- toolGetMapping("buildingType.csv",
                               type = "sectoral", where = "brick") %>%
        rbind(data.frame(typ = "Com", subsector = "Com")) # TODO: remove once dim maps are dynamic # nolint
      typMap <- stats::setNames(typMap[["subsector"]], typMap[["typ"]])

      params <- params %>%
        pivot_longer(c("scale", "shape"), names_to = "variable") %>%
        as.magpie(datacol = "value")

      params <- do.call(mbind, lapply(names(typMap), function(typ) {
        params %>%
          mselect(subsector = typMap[typ], collapseNames = TRUE) %>%
          add_dimension(add = "typ", nm = typ)
      }))

      description <- "Weibull lifetime distribution parameters for heating systems"
    },




    ## Building shell ====

    buildingShell = {

      # taken from Skarning et al. 2017
      params <- inline.data.frame(
        "from; to",
        "  40; 60"
      )

      # we assume probabilities of the lower and upper value respectively
      prob <- c(from = 0.25, to = 0.75)
      params <- params %>%
        mutate(shape = log(log(1 - prob[["to"]]) / log(1 - prob[["from"]]),
                           .data[["to"]] / .data[["from"]]),
               scale = .data[["from"]] /
                 (-log(1 - prob[["from"]]))^(1 / .data[["shape"]])) %>%
        select(-"from", -"to") %>%
        pivot_longer(everything(), names_to = "variable") %>%
        as.magpie(datacol = "value")

      description <- "Weibull lifetime distribution parameters for the building shell"
    },


    stop("Invalid subtype: ", subtype)
  )



  # RETURN ---------------------------------------------------------------------

  # fill missing regions
  params <- toolCountryFillAvg(params, verbosity = 2, no_remove_warning = "GLO")

  # weight: FE demand
  feBuildings <- calcOutput("WeightFeBuildings", aggregate = FALSE) %>%
    mselect(period = "y2020", collapseNames = TRUE)
  if ("typ" %in% getSets(params)) {
    feBuildings <- feBuildings %>%
      mselect(typ = getItems(params, "typ"))
  } else {
    feBuildings <- dimSums(feBuildings)
  }


  return(list(x = params,
              weight = feBuildings,
              min = 0,
              unit = "[scale] = yr; [shape] = 1",
              description = description))
}
