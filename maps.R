# Last week, we listened to a podcast about climate models and read about one
# particular report. We're going to visualize the output of a different climate
# model, the CMIP6.

# You can read about the CMIP6 here: https://www.carbonbrief.org/cmip6-the-next-generation-of-climate-models-explained/

# Climate models are run to model different "scenarios": predictions about the
# future given certain assumed levels of emissions, public policies, etc.

# We're going to analyze data from CMIP that varies two parameters.

################################################################################
# Param 1: Different "Shared Socioeconomic Pathways". The different pathways are
# as follows.
################################################################################
# (https://www.carbonbrief.org/explainer-how-shared-socioeconomic-pathways-explore-future-climate-change/)

#### SSP1 Sustainability – Taking the Green Road (Low challenges to mitigation and
                                        # adaptation)
# ---
# The world shifts gradually, but pervasively, toward a more
# sustainable path, emphasizing more inclusive development that respects
# perceived environmental boundaries. Management of the global commons slowly
# improves, educational and health investments accelerate the demographic
# transition, and the emphasis on economic growth shifts toward a broader
# emphasis on human well-being. Driven by an increasing commitment to achieving
# development goals, inequality is reduced both across and within
# countries. Consumption is oriented toward low material growth and lower
# resource and energy intensity.

### SSP2 Middle of the Road (Medium challenges to mitigation and adaptation)
# ---
#The world follows a path in which social, economic, and technological trends do
# not shift markedly from historical patterns. Development and income growth
# proceeds unevenly, with some countries making relatively good progress while
# others fall short of expectations. Global and national institutions work
# toward but make slow progress in achieving sustainable development
# goals. Environmental systems experience degradation, although there are some
# improvements and overall the intensity of resource and energy use
# declines. Global population growth is moderate and levels off in the second
# half of the century. Income inequality persists or improves only slowly and
# challenges to reducing vulnerability to societal and environmental changes
# remain.

# SSP3 Regional Rivalry – A Rocky Road (High challenges to mitigation and
                                        # adaptation)
# ---
# A resurgent nationalism, concerns about competitiveness and security, and
# regional conflicts push countries to increasingly focus on domestic or, at
# most, regional issues. Policies shift over time to become increasingly
# oriented toward national and regional security issues. Countries focus on
# achieving energy and food security goals within their own regions at the
# expense of broader-based development. Investments in education and
# technological development decline. Economic development is slow, consumption
# is material-intensive, and inequalities persist or worsen over
# time. Population growth is low in industrialized and high in developing
# countries. A low international priority for addressing environmental concerns
# leads to strong environmental degradation in some regions.

# SSP4 Inequality – A Road Divided (Low challenges to mitigation, high
                                        # challenges to adaptation)
# ---
# Highly unequal investments in human capital, combined with increasing
# disparities in economic opportunity and political power, lead to increasing
# inequalities and stratification both across and within countries. Over time, a
# gap widens between an internationally-connected society that contributes to
# knowledge- and capital-intensive sectors of the global economy, and a
# fragmented collection of lower-income, poorly educated societies that work in
# a labor intensive, low-tech economy. Social cohesion degrades and conflict and
# unrest become increasingly common. Technology development is high in the
# high-tech economy and sectors. The globally connected energy sector
# diversifies, with investments in both carbon-intensive fuels like coal and
# unconventional oil, but also low-carbon energy sources. Environmental policies
# focus on local issues around middle and high income areas.

# SSP5 Fossil-fueled Development – Taking the Highway (High challenges to
                                        # mitigation, low challenges to adaptation)
# ---
# This world places increasing faith in competitive markets, innovation and
# participatory societies to produce rapid technological progress and
# development of human capital as the path to sustainable development. Global
# markets are increasingly integrated. There are also strong investments in
# health, education, and institutions to enhance human and social capital. At
# the same time, the push for economic and social development is coupled with
# the exploitation of abundant fossil fuel resources and the adoption of
# resource and energy intensive lifestyles around the world. All these factors
# lead to rapid growth of the global economy, while global population peaks and
# declines in the 21st century. Local environmental problems like air pollution
# are successfully managed. There is faith in the ability to effectively manage
# social and ecological systems, including by geo-engineering if necessary.

################################################################################
# Param 2: What is the ratio between the energy absorbed by the Earth's
# atmosphere and the energy reflected back into space (termed 'radiative forcing').
################################################################################
# (https://www.climate.gov/maps-data/climate-data-primer/predicting-climate/climate-forcing)

## 0.0: Radiative forcing reference value at the start of the industrial revolution in the year 1750
## 2.3: Radiative forcing in the year 2011
## 2.6: A target for radiative forcing in the year 2100 that is considered ideal, but improbable
## 4.5: A target for radiative forcing by 2100 that is considered practical
## 8.5: A probable level of radiative forcing by 2100 if no significant actions are taken to mitigate CO2 emissions

###########
# MAPPING #
###########

# Step 1: Downloading the data

# In a browser, go to https://esgf-node.llnl.gov/search/cmip6/

# Click 'Experiment ID'

# ID 'ssp126' corresponds to SSP Scenario 1, with 2.6 radiative forcing;
# 'ssp245' corresponds to scenario 2 with 4.5 radiative forcing, etc

# Under variable, select the variables that you want to download.
# These are described here: https://pcmdi.llnl.gov/mips/cmip3/variableList.html#Table_A1a

# To narrow the number of redundant files, we'll select 'NOAA-GFDL' (the US
# National Oceanic and Atmospheric Administration's Geophysical Fluid Dynamics
# Laboratory) as our institution ID.

# For this example, I'll downoad:
#    - tas: daily-mean near-surface (usually, 2 meter) air temperature (K)

# Despite selecting a single institution, you'll get multiple potential files to
# download. This is because the simulation inherently has some variability in
# it: if you run it multiple times, you'll get slightly different results due to
# the randomness in the simulation. By running the simulation multiple times,
# researchers can assess whether features of the observed scenario are largely
# stable despite this randomness, or whether there is a great degree of noise in
# the predictions.

# Select a run (e.g., CMIP6.ScenarioMIP.NOAA-GFDL.GFDL-CM4.ssp245.r1i1p1f1.Amon.tas.gr1)
# Click 'list files'
# Click 'HTTP download'
# Move the file to a reasonable working directory

# Step 2: Load and process the data
library(RNetCDF)

# NetCDF files are another way to store GIS data
# These are large files, this only estbalishes a connection to the file
nc <- open.nc('C:/Users/silag/Downloads/tas_Amon_GISS-E2-1-G_ssp245-covid_r5i1p5f2_gn_202001-202412.nc')

# A tutorial on how to work with NetCDF files with this particular library is
# here: https://journal.r-project.org/archive/2013-2/michna-woods.pdf

# To see what's in our nc file:
print.nc(nc)

# To extract a date:
dates = as.Date(var.get.nc(nc, "time"), origin="2020-01-01 00:00:00")

# This goes up to 12/14/2024
max(dates)
print(dates)

# We can select the temperature for a single observation as follows
grid = var.get.nc(nc, "tas", start=c(NA, NA, 37), count=c(NA, NA, 1))

# By default, this is a matrix with the following dimensions:
dim(grid)
typeof(grid)

# This is the temperature in K for a grid over the entire world at the following
# date: 2023-01-15
dates[37]

# 'rasters' are another way to work with spatial data. In this case, we're going
# to represent the data as grid cells where each cell is a point on earth with
# a simulated temperature.
library(raster)
sim.2023 <- raster(grid, xmn=-90, xmx=90, ymn=0, ymx=360,
                   crs="+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
plot(sim.2023)

# Oops we need to rotate earth.
sim.2023 = rotate(flip(t(sim.2023), 2))
plot(sim.2023)

# Let's chop off the extra whitespace
plot(sim.2023, ylim = c(-100, 100))



# Make a loop

for(i in 1:60){
  grid = var.get.nc(nc, "tas", start=c(NA, NA, i), count=c(NA, NA, 1))

  
  # Convert Kelvin to Fahrenheit
  for(j in 1:length(grid)){
    grid[j] = (grid[j]-273.15)*(9/5)+32
  }
  
  sim <- raster(grid, xmn=-90, xmx=90, ymn=0, ymx=360,
                     crs="+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
  
  month = format(dates[i], "%b-%y")
  
  sim = rotate(flip(t(sim), 2))
  plot(sim, ylim=c(-100,100), zlim=c(-20, 120), axes=FALSE, main=month)
  
}





# Figure out what the overall min and max temperature values are

# curmin = 0
# curmax = 0
# for(i in 1:60){
# 
#   grid = var.get.nc(nc, "tas", start=c(NA, NA, i), count=c(NA, NA, 1))
#   
#   # Convert Kelvin to Fahrenheit
#   for(j in 1:length(grid)){
#     grid[j] = (grid[j]-273.15)*(9/5)+32
#   }
#   
#   newmin = min(grid)
#   newmax = max(grid)
#   if(newmin < curmin){
#     curmin=newmin
#   }
#   if(newmax>curmax){
#     curmax=newmax
#   }
# }
# print(curmin)
# print(curmax)
