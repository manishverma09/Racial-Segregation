library(spdep)
library(english)
library(sf)
library(tidycensus)
library(USAboundaries)
library(tigris)
library(dplyr)
library(data.table)

# API
census_api_key = 'your census API key here as a string'
readRenviron("~/.Renviron")       


calculate.gstar = function (state.fip, county.fip, census.var){
   
  # This function calculates G-star.
  # Arguments:
  #         state.fip: Two digit state fips code. It is a charactor vector
  #        county.fip: Three digit county fips code. It is a character vector
  #         census.var: Code of the tract level census variable ('P053001') 
  # Returns:
  #         calculate.gstar: Tract level gstar

  # Example:
  #         c.g = calculate.gstar ('26', '123','P053001')

   
   # I am not very careful about tryCatch below. I just want to take care of the known issues. 
   tryCatch({
   var1 = get_decennial(geography = 'tract', variables = census.var, year = 2000 , 
                        state = state.fip, county = county.fip, sumfile = 'sf3', geometry = TRUE)
   
   # For some reason there are empty geometries (e.g. try state = '34' and county = '011'), remove them
   geom.dim = is.na(st_dimension(var1))
   var1 = var1[!geom.dim,]
   
   if (dim(var1)[1]>1){                   # There are counties with only one tract  
   var1 = var1[c('GEOID', 'value')]
   setnames(var1, 'value', 'income')
   nearNeigh = poly2nb(var1, queen = TRUE)   # With queen = FALSE we get tracts with no neighbor in small counties
   nearNeigh = include.self(nearNeigh) 
   nearNeigh.w = nb2listw(nearNeigh, style = "W")
   local.g = localG(var1$income, nearNeigh.w, zero.policy = TRUE)
   var1['gstar'] = local.g
   return (var1)
   } else {
      print('Only nas produced')
      #var1 = error.df
   }
   
   }, error = function (e){
      print (e)
   })
}
