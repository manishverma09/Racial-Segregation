modified.localG = function(black_population, total_population,  listw, method='kershaw', zero.policy=NULL, spChk=NULL, return_internals=FALSE, GeoDa=FALSE)
  # This function is based on the original script from the SPDEP package. I am modifying it so that I can use it with ratios.
  # I am leaving a number of parameters here, although as of July 25 we do not use them. But I do intend to later expand the code and use them.

  # Arguments:
  #          black_population: Population of blacks in a geography. Numeric.
  #          total_population: Total population in a geography. Numeric.
  #          method = It could be 'kershaw' i.e. the method used in Kershaw's paper, or other (still to be developed as on July 25th)
  # Returns:
  #          gstar: Z-score for Getis-Ord G
  #
  #
  # The key challenge, as I see it, is the following:
  #
  # Getis-Ord calculates deviation of 'local mean' from the global mean. The global mean in the usual case is the mean of the entire data.
  # However, when we use ratios (e.g. proportions of blacks in the total population) we would like to estimate the difference between the
  # local proportion and the global mean proportion. The global mean proportion is not the mean of the distribution of the data, rather it has a
  # nonlinear realtionship with the data. 
  
  # Note that using global mean can also produce negative underroot in the denominator. 
  
  # So we are interested in deviation from m(X), i.e.  local(x) - m(X) where m(X) is not mean, median etc. but some nonlinear function of all x

  #  In the code below I calculate the difference between the local ratio and the global proportion


  ### 1. CHECKS borrowed from the SPDEP package

  # Type of objects check
  {
  if (!inherits(listw, "listw"))
  stop(paste(deparse(substitute(listw)), "is not a listw object"))

  if (!is.numeric(black_population))
    stop(paste(deparse(substitute(black_population)), "is not a numeric vector"))

  if (!is.numeric(total_population))
    stop(paste(deparse(substitute(total_population)), "is not a numeric vector"))

  stopifnot(is.vector(black_population))
  stopifnot(is.vector(total_population))

  # Size consistency between different arguments
  n = length(listw$neighbours)
  if (n != length(black_population))stop("Different numbers of observations")

  if (length(total_population) != length(black_population))stop("Different numbers of observations")

  # Check for NAs
  if (any(is.na(black_population))) stop(paste("NA in ", deparse(substitute(black_population))))
  if (any(is.na(total_population))) stop(paste("NA in ", deparse(substitute(total_population))))


  if (is.null(spChk)) spChk = get.spChkOption()

  if (spChk && !chkIDs(black_population, listw))
    stop("Check of data and weights ID integrity failed")

  if (is.null(zero.policy))
    zero.policy = get("zeroPolicy", envir = .spdepOptions)

  stopifnot(is.logical(zero.policy))


  ### 2. PROCESSING OPTIONS (Notations here are a mix of the original SPDEP code, the two papers from Getis-Ord publiched in 1992 and 1995
  #     and the ArcGIS site https://pro.arcgis.com/en/pro-app/tool-reference/spatial-statistics/h-how-hot-spot-analysis-getis-ord-gi-spatial-stati.htm (since most people use this))

  gstari = FALSE
  if (!is.null(attr(listw$neighbours, "self.included")) &&
      attr(listw$neighbours, "self.included")) gstari = TRUE

  if (method == 'kershaw'){

  x_bar  = sum(black_population, na.rm = T) / sum(total_population, na.rm = T)   # This is the global mean proportion

  x = black_population /total_population                                         # This is black proportion in each tract
  x[is.infinite(x)] = 0                           # We are not removing tracts with zero population, becasue we sum it does not create any degenerate cases


  # Calculate the numerator of G-star
  lx = lag.listw(listw, x, zero.policy=zero.policy)
  wij.xbar = lapply(listw$weights, function (x) sum(x)*x_bar)

  numerator = lx - unlist(wij.xbar)

  # Calculate the denominator of G-star
  capitalS = sqrt((sum(x^2)/length(x)) - x_bar^2)                             # this could be divided by (n-1)

  den.part1 = sapply(listw$weights, function (x) sum(x^2))                    # sum (wij^2)
  den.part1 = length(x)*den.part1

  den.part2 = sapply(listw$weights, function (x) (sum(x))^2)

  denominator = (den.part1 - den.part2) / (length(x)-1)
  denominator = capitalS*sqrt(denominator)

  # Calculate gstar
  gstar = numerator/denominator
  } else {
    gstar = 'The function currently executes Kershaw method. I will later add my method'
  }
  return (gstar)
  }

   
