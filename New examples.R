
setwd("C:/Users/SmithAC/Documents/GitHub/bbsBayes2_workshop")

library(bbsBayes2)
# latlong example ---------------------------------------------------------

s <- stratify("latlong","Baird's Sparrow")
p <- prepare_data(s, min_n_routes = 1)
map<-load_map(stratify_by = "latlong")
sp<-prepare_spatial(p,map)

# first difference
mp_first_diff <- prepare_model(sp, model = "first_diff",model_variant = "spatial",
                               calculate_log_lik = TRUE)
m_first_diff <- run_model(mp_first_diff,
                          iter_sampling = 2000)
saveRDS(m_first_diff,"output/BASP_latlong_first_diff_spatial.rds")

# gamye
mp_gamye <- prepare_model(sp, model = "gamye",model_variant = "spatial",
                          calculate_log_lik = TRUE)
m_gamye <- run_model(mp_gamye,
                     iter_sampling = 2000)
saveRDS(m_gamye,"output/BASP_latlong_gamye_spatial.rds")


m_gamye <- readRDS("output/BASP_latlong_gamye_spatial.rds")

m_first_diff <- readRDS("output/BASP_latlong_first_diff_spatial.rds")


sum_gamye <- get_summary(m_gamye)
sum_first_diff <- get_summary(m_first_diff)

loo_gamye <- m_gamye$model_fit$loo()
loo_first_diff <- m_first_diff$model_fit$loo()

# animated maps

# Cross-validation

s <- stratify("latlong","Baird's Sparrow")
p <- prepare_data(s, min_n_routes = 1)
map<-load_map(stratify_by = "latlong")
sp<-prepare_spatial(p,map)

m_hier <- prepare_model(p,"gamye",
                        calculate_cv = TRUE)

m_spat <- prepare_model(ps,"gamye",
                        calculate_cv = TRUE)

for(k in 1:10){
  m_tmp <- run_model(m_hier,
                     refresh = 500,
                     k = k)
  save_model_run(m_tmp,
                 path = paste0("output/m_hier_",k,"_rds"))




}
# fit a model with the calc_loo = TRUE


# Covariate example (prairie climate)

# overlay latlong strata and july 3-month SPEI
library(sf)
library(raster)
library(tidyverse)

spei3 <- raster::raster("data/spei03.nc")
latlong_map <- bbsBayes2::load_map("latlong")

sf::st_crs(spei3)
crs_map <- sf::st_crs(latlong_map)
st_transform(spei3,crs_map)

# model modification example (different prior or different derived parameter)







