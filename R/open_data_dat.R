library(retroharmonize)
library(dplyr)

gesis_dir <- file.path("C:/Users/Daniel Antal/OneDrive - Visegrad Investments",
                       "_data", "gesis")

dir ( gesis_dir )

pollutant_files <- c("ZA5877_v2-0-0.sav", "ZA6595_v3-0-0.sav",  "ZA6861_v1-2-0.sav", 
                     "ZA7488_v1-0-0.sav", "ZA7572_v1-0-0.sav")

pollutant_files %in% dir (gesis_dir)

eb_waves <- read_surveys(file.path(gesis_dir, pollutant_files), .f='read_spss')

documented_eb_waves <- document_waves (eb_waves) 

eb_climate_metadata <- lapply ( X = eb_waves, FUN = metadata_create )
eb_climate_metadata <- do.call(rbind, eb_climate_metadata)
#let's keep the example managable:
eb_trust_metadata2  <- eb_climate_metadata 

eb_demography_metadata  <- eb_climate_metadata %>%
  filter ( grepl( "rowid|isocntry|^d8$|^d7$|^wex|^w1$|d25|^d15a", .data$var_name_orig) )

eb_regional_metadata <- eb_climate_metadata %>%
  filter ( grepl( "rowid|isocntry|p7", .data$var_name_orig))

climate_awareness_metadata <- eb_climate_metadata %>%
  mutate ( var_label_std = var_label_normalize(.data$label_orig) ) %>%
  filter ( .data$var_label_std  %in% c("serious world problems first", 
                                       "serious world problems climate change") |
             .data$var_name_orig == "rowid"
           ) 


hww <- harmonized_eb_waves <- harmonize_waves ( 
  waves = hw, 
  .f = harmonize_serious_problems )

as_character ( hww$serious_world_problems_first)
