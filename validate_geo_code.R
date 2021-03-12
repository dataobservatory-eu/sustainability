

geo  <- p1$geo
valideate_geo_code <- function ( geo, nuts_year = 2016 ) {
  
  assertthat::assert_that(
    any ( c("character", "factor") %in% class(geo) ),
    msg = "geo must be a character or factor vector."
  )
  
  geo <- as.character(geo)
  
  assertthat::assert_that(
    nuts_year %in% c(1999,2003,2006,2010,2013,2016,2021), 
    msg = glue::glue ( "nuts_year={nuts_year} is an invalid parameter setting.")
  )
  
  utils::data (all_valid_nuts_codes, package ="regions", 
               envir = environment())
  
  get_country_code( all_valid_nuts_codes,
    geo          = geo, 
    typology     = "NUTS" )
  
  exceptions <- all_valid_nuts_codes %>%
    mutate ( 
      country_code = get_country_code( 
        geo          = .data$geo, 
        typology     = "NUTS" )
    ) %>%
    filter ( .data$country_code %in% c("IS", "LI", "NO", "AL",
                                       "CH", "MK", "RS", "TR", 
                                       "ME"))  %>%
    distinct ( geo, typology ) %>%
    mutate ( typology = glue::glue ( "non_eu_{typology}") ) %>%
    select ( all_of(c("geo", "typology"))) %>%
    bind_rows ( tibble (
      geo = c("GB", "GR", "XK"), 
      typology = c(rep("iso_country", 2), "non_eu_country")
    ))
  
  filtering <- grepl( as.character(nuts_year), 
                      all_valid_nuts_codes$nuts )
  
  filtered_nuts_data_frame <- all_valid_nuts_codes[filtering, ] %>%
    select ( all_of(c("geo", "typology"))) %>%
    full_join ( exceptions, by = c("geo", "typology") )
  
  tibble::tibble ( 
    geo = geo) %>%
    left_join ( filtered_nuts_data_frame, by = 'geo' ) %>%
    mutate ( typology = if_else ( condition = is.na(typology), 
                                  true = "invalid", 
                                  false = as.character(typology) )
    ) %>%
    select ( all_of("typology")) %>%
    unlist () %>%
    as.character()
  
}
