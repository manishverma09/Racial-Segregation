tract.gstar = function (census.tract, method = 'kershaw')
  # The function calculates Getis_Ord gstar at tract level.
  # Arguments:
  #        census.tract: SF object with tracts information.
  #        method: 'Kershaw', see how the weight matrix is defined below
  # Returns:
  #         cbsa.index: SF object with the zscore using Getis-Ord based on Kershaw et al. (2017)

  # Example:
  #         cb.tr = tract.gstar (cbsa.tract, 'kershaw')

{
   nearNeigh = poly2nb(census.tract, queen=FALSE)

  # For Kershaw we want to include self and weigh it as '1', the weight of the neighbors also total to 1

  if (method == 'kershaw')
  {
    nearNeigh = include.self(nearNeigh)       # Include the cell itself like Kershaw

    weight.list = lapply(nearNeigh, function (x) rep(1, times= length(x)))    # Create a list with one for each neighbor and self
    weight.list = lapply(weight.list, function (x) x / (length(x)-1))         # Divide by the total no. of neighbours not including self as in Kerhsaw

    # Weight self equal to 1 as in Kershaw
    for (i in 1:length(nearNeigh))
    {
      index = nearNeigh[[i]] == i
      weight.list[[i]][index] = 1
    }
    nearNeigh.w = nb2listw(nearNeigh, style = "W")     # zero.policy, see variance stabilizing scheme 'S'
    nearNeigh.w$weights = weight.list                  # Replace SPDEP's weights with Kershaw's weights
    g.star = modified.localG(census.tract$black_population, census.tract$total_population, nearNeigh.w, zero.policy = FALSE)
    census.tract$gstar = g.star

  } else {
    nearNeigh.w = nb2listw(nearNeigh, style = "W")
    g.star = modified.localG(census.tract$black_population, census.tract$total_populationl, nearNeigh.w, zero.policy = FALSE)
    census.tract$gstar = g.star
  }

  colnames(census.tract)[colnames(census.tract) == 'GEOID10.1'] = 'GEOID.Tract'
  return (census.tract)

}
