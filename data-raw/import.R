## Import and save various source files
##
## Data Description : LSOA/Postcode
## Location         : http://ons.maps.arcgis.com/home/item.html?id=ef72efd6adf64b11a2228f7b3e95deea
lsoa_postcode_2011 <- read.csv('csv/PCD11_OA11_LSOA11_MSOA11_LAD11_EW_LU_aligned_v2.csv')
names(lsoa_postcode_2011) <- names(lsoa_postcode_2011) %>% tolower()
names(lsoa_postcode_2011) <- gsub('lsoa11cd', 'lsoa_code_2011', names(lsoa_postcode_2011))
names(lsoa_postcode_2011) <- gsub('lsoa11nm', 'lsoa_name_2011', names(lsoa_postcode_2011))
save(lsoa_postcode_2011,
     file = '../data/lsoa_postcode.RData')
