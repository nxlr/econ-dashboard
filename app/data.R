# Add data switch logic here - for india and states


#shapeFile <- read_sf(dsn = "./www/Layers_DISTRICTS-polygon.shx")
districtGDP <- read_excel("./datasets/gdp/districts_gdp.xlsx")
stateGDP <- read_excel("./datasets/gdp/state_gdp.xlsx")
gdp_melt <- melt(stateGDP, id=c("Year"))
colnames(gdp_melt)[2] <- c("Variable")
colnames(gdp_melt)[3] <- c("Value")
indusData <- read_excel("./datasets/industry/industry_haryana.xlsx")
sectoralGDP <- read_excel("./datasets/gdp/sectoral_gdp_transformed.xlsx")
sectoralAll <- read_excel("./datasets/gdp/sectoral_gdp_constant.xlsx")

allLabor_data_tmp <- read_excel("./datasets/labor/labor_overall_haryana.xlsx")
allLabor_data <- melt(allLabor_data_tmp, id=c("Year","Age","Sector","Gender"))
colnames(allLabor_data)[5] <- c("Variable")
colnames(allLabor_data)[6] <- c("Value")
edLabor_data <- read_excel("./datasets/labor/labor_education_haryana.xlsx")
sectoralLabor_data_tmp <- read_excel("./datasets/labor/labor_sectoral_haryana.xlsx")
sectoralLabor_data <- melt(sectoralLabor_data_tmp, id=c("Year", "Age", "Sector", "Gender"))
colnames(sectoralLabor_data)[5] <- c("Variable")
colnames(sectoralLabor_data)[6] <- c("Value")

irrigation_data <- read_excel("./datasets/agriculture/Irrigation.xlsx")
financeData <- read_excel("./datasets/finance/finance_overall.xlsx")
literacyData <- read_excel("./datasets/education/literacy_rate.xlsx")
healthData <- read_excel("./datasets/health/Health.xlsx")
#indus1 <- as.data.frame(read_excel("./datasets/industry/indus1.xlsx"))
