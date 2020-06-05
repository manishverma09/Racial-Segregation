tract.blackprop = function (state, county, tract.data.dir = NULL)

{
  # This function downloads census information (black population, total population, black proportion) and joins with tiger data read locally.
  # Arguments:
  #         state: Two digit state fips code. It is a charactor vector
  #        county: Three digit county fips code. It is a character vector
  #         tract.data.dir: Directory where tract data is stored. Downloading geometry takes time so you might like to store it locally. 
  
  # Returns:
  #         tract.blackprop: Tract level population info for the state and counties

  # Example:
  #         t.b = tract.blackprop ('26', c('123', '234', '012'), 'census-tracts-2010/)

  ###
  projString = "+proj=aea +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +units=m +no_defs"
  #options(tigris_use_cache = T)

  tract.population = get_decennial(geography = 'tract', variables = 'P001001', state = state,
                                   county =county, year=2010)
  tract.population = tract.population[c('GEOID', 'value')]
  setnames(tract.population, old = 'value', new = 'total_population')

  # There could be tracts with zero population - I am not removing it
  tract.black = get_decennial(geography = 'tract', variables = 'P003003', state = state,
                              county =county, year=2010)

  tract.white = get_decennial(geography = 'tract', variables = 'P003002', state = state,
                              county =county, year=2010)

  tract.black = tract.black[c('GEOID', 'value')]
  setnames(tract.black, old = 'value', new = 'black_population')

  tract.white = tract.white[c('GEOID', 'value')]
  setnames(tract.white, old = 'value', new = 'white_population')

  black.prop = left_join(tract.population, tract.black, by = 'GEOID')
  black.prop = left_join(black.prop, tract.white, by = 'GEOID')
  black.prop['black_proportion_total'] =  black.prop$black_population/black.prop$total_population
  black.prop['black_proportion_white'] =  black.prop$black_population/black.prop$white_population

  # If we do not provide the local shape file directory tract.data.dir
  if (is.null (tract.data.dir))  {

  # We can get the census geography with geometry=T in 'get_decennial' above, but I prefer it this way. Get tract boundaries

  tc = tracts(state = state, county =county, year=2010, class = 'sf')
  tc = tc['GEOID10']
  } else {

  # If you have it locally 
  tract.geog.dir1 = paste(tract.data.dir, 'tl_2010_', state, '_tract10/', sep='')
  file.name = paste('tl_2010_', state, '_tract10', sep='')
  tc = st_read(tract.geog.dir1, file.name, stringsAsFactors = F)
  index = tc$COUNTYFP10 %in%county
  tc = tc[index,]
  tc = tc['GEOID10']
  }

  ### NOTE

  # There are some errors here in tract geometry, for example in tc[741,] for ALABAMA there is a self intersection.
  # So tc is coming back as multipolygon. Try the code below for 'AL' state
  # p1 = st_cast(tc[741,], 'POLYGON')
  # g1 = ggplot() + geom_sf(data = p1[1,], col = 'red') + geom_sf(data = p1[2,])
  # g1

  ### CLOSE

  tract.blackprop = left_join(tc, black.prop, by=c('GEOID10' = 'GEOID'))
  tract.blackprop = st_transform(tract.blackprop, projString)

  # Remove NA  - we will have to be careful with this step, please see the note below.

  ### NOTE (I do not remember the original source)
  
  # As originally designed, spatial autocorrelation tests assumed there are no neighborless units in the
  # study area.  When this assumption is violated, the size of n may be adjusted (reduced) to reflect
  # the fact that some units are effectively being ignored.  Not doing so will generally bias the
  # absolute value for the autocorrelation statistic upward and the variance downward.

  ### CLOSE

  tract.blackprop =  tract.blackprop[!is.na( tract.blackprop$black_proportion_total),]

  return(tract.blackprop)
}

