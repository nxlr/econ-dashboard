# Add data switch logic here - for india and states


#shapeFile <- read_sf(dsn = "./www/Layers_DISTRICTS-polygon.shx")
districtGDP <- read_excel("./datasets/gdp/districts_gdp.xlsx")
stateGDP <- read_excel("./datasets/gdp/state_gdp_2011_12_prices.xlsx")
indusData <- read_excel("./datasets/industry/industry_haryana.xlsx")
sectoralGDP <- read_excel("./datasets/gdp/sectoral_gdp.xlsx")
allLabor_data_tmp <- read_excel("./datasets/labor/labor_overall_haryana.xlsx")
allLabor_data <- melt(allLabor_data_tmp, id=c("Year","Age","Sector","Gender"))
edLabor_data_tmp <- read_excel("./datasets/labor/labor_education_haryana.xlsx")
edLabor_data <- melt(edLabor_data_tmp, id=c("Year", "Age", "Sector", "Gender"))
sectoralLabor_data_tmp <- read_excel("./datasets/labor/labor_sectoral_haryana.xlsx")
sectoralLabor_data <- melt(sectoralLabor_data_tmp, id=c("Year", "Age", "Sector", "Gender"))
irrigation_data <- read_excel("./datasets/agriculture/Irrigation.xlsx")

literacyData <- read_excel("./datasets/education/literacy_rate.xlsx")
#indus1 <- as.data.frame(read_excel("./datasets/industry/indus1.xlsx"))
