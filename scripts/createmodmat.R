load(here("data", "processed", "all_validated_rr.RData"))
load(here("data", "processed", "meteo_seasonality.RData"))

all_validated_rr_split <- lapply(all_validated_rr, FUN=function(x){split(x, x$Pollutant)})
all_validated_rr_flat <- purrr::flatten(all_validated_rr_split)

merg <- function(aq.dat, meteo.data){
  
  aq.dat_m <- aq.dat %>% rename(date = DateTime) %>%
  select(-Year, -Month, -Day, -Hour) %>%
  merge(meteo.data, by="date", all.x=TRUE) %>%
  select(-Year, -Month, -Day,
         -date, -Classification, -Site_combine,
         -Latitude, -Longitude) %>% 
  tidyr::drop_na()
  
  return(aq.dat_m)
}

all_validated_rr_modmat <- lapply(all_validated_rr_flat, FUN=merg, meteo_seasonality)

save(all_validated_rr_modmat, file=here("data", "processed", "all_validated_rr_modmat.RData"))
