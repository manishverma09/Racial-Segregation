library(spdep)
library(english)
library(sf)
library(tidycensus)
library(USAboundaries)
library(tigris)
library(dplyr)
library(data.table)

# API
#apiKey = 'your API key here as a string'
# readRenviron("~/.Renviron")       # Yon need to do this once


# Get subject location data
projString = "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"

location.dir = "/Volumes/kines-epl/EPL-GEO_BSERE/Data/WorkingData/Manish/subject_location/"

location.original = st_read(location.dir, 'subject_location_conus', stringsAsFactors = FALSE)
location.original = st_transform(location.original, projString)

# Attach state and county information - be careful about year

# For SF3 table we want counties for the year 2000

county.data = st_read('/Volumes/kines-epl/EPL-GEO_BSERE/Data/OriginalData/Census/census-counties/2000/ tl_2010_us_county00/tl_2010_us_county00.shp')
county.data = county.data[c('STATEFP00', 'COUNTYFP00')]
#county.data['GEOID10'] = as.character(county.data$GEOID10)
county.data['STATEFP00'] = as.character(county.data$STATEFP00)
county.data['COUNTYFP00'] = as.character(county.data$COUNTYFP00)

county.data = st_transform(county.data, projString)
location.county = st_intersection(location.original, county.data)
remove(list = c('county.data', 'location.original'))

# We want to calculate for each county at census tract level
st.county = location.county[c('STATEFP00', 'COUNTYFP00')]
st_geometry(st.county) = NULL
uq.st.county = distinct(st.county)
# uq.st.county['STATEFP10'] = as.character(uq.st.county$STATEFP10)
# uq.st.county['COUNTYFP10'] = as.character(uq.st.county$COUNTYFP10)

# Income information
var.name = load_variables(year = '2000', dataset = 'sf3', cache = TRUE)
index = grepl('Median household income', var.name$label)
income.var = var.name[index,] # We want fourth 'P053001'

#### 1. Calculate for SF3 table from Census 2000 (I have obtained results with 2010 and 2000 and saved the results)
error.df = data.frame("GEOID10" = NA, "income"= NA, "geometry"= NA, "gstar"= NA )

error.list = list()


calculate.gstar = function (state.fip, county.fip){
   
   tryCatch({
   var1 = get_decennial(geography = 'tract', variables = 'P053001', year = 2000 , 
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


applyFun = function (state.fip, county.fip) {
      
      try (
      {t.gstar = calculate.gstar (state.fip, county.fip)}, silent = FALSE)
      
      if (class (t.gstar) == 'try-error') {
      print ('error he')
      } else {return (t.gstar)}
      
}

gstar.tracts = apply(uq.st.county, 1, function (x) applyFun(x[1], x[2]))

# Find the ones where there was an error and so we do not get dataframe
valid.result = lapply(gstar.tracts, is.data.frame)
valid.result = do.call(rbind, valid.result)
print(which(!valid.result)) # There are five without results and have errors


gstar.valid = gstar.tracts[valid.result]
gstar.valid = do.call(rbind, gstar.valid)

gstar.valid = st_transform(gstar.valid, projString)

location.gstar.county = st_intersection(location.original, gstar.valid)
location.gstar.county = location.gstar.county %>% group_by(move_id) %>% summarise()

# Some move_id do not have gstar data - the possible reasons could be
# There is only one tract in the county
# More than one tracts but the tracts are embedded (contained in ) other tracts
# There was geometry or data error from census site
# Let us probe a bit

st.county.error = uq.st.county[!valid.result,]




moveid.notcovered = location.county$move_id %in% location.gstar.county$move_id
moveid.notcovered = location.county[!moveid.notcovered,]

# Let us try again for these moveids
uq.st.county1 = moveid.notcovered[c('STATEFP10')]




## There are NAs in the results

index = lapply(gstar.tracts, function (x) sum(rowSums(is.na(x)))>0 )
index = do.call(rbind, index)



