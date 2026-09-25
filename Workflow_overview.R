# Workflow Overview

library(bbsBayes2)
library(tidyverse)
library(sf)

stratification <- "latlong"

s <- stratify(by = stratification, species = "Baird's Sparrow")
p <- prepare_data(s, min_n_routes = 1)
sp <-  prepare_spatial(p,
                       queen = TRUE,
                       strata_map = load_map(stratification))


# exploring the realised spatial neighbours -------------------------------


neighbour_graph <- sp$spatial_data$map
neighbour_graph

extra_layer <- load_map("bbs")

extent <- filter(load_map(stratification),
                      strata_name %in% sp$meta_strata$strata_name)
bb <- sf::st_bbox(extent)

neighbour_map <- neighbour_graph +
  geom_sf(data = extra_layer,
          fill = NA)+
  coord_sf(xlim = bb[c("xmin","xmax")],
           ylim = bb[c("ymin","ymax")])

neighbour_map


md <- prepare_model(sp,
                    model = "first_diff",
                    model_variant = "spatial")


# fitting the model -------------------------------------------------------


# m <- run_model(md,
#                  refresh = 500,
#                  output_basename = "BASP_fd_spatial",
#                  output_dir = "output")
m <- readRDS("output/BASP_fd_spatial.rds")


# Convergence -------------------------------------------------------------


# converge <- get_summary(m)
# saveRDS(converge,"output/BASP_fd_spatial_convergence.rds")
converge <- readRDS("output/BASP_fd_spatial_convergence.rds")




# Indices -----------------------------------------------------------------

i <- generate_indices(m,
                      hpdi = TRUE)

names(i)


p <- plot_indices(indices = i,
                  add_observed_means = TRUE,
                  add_number_routes = TRUE)


p[["continent"]]




# exploring changes in observer and route effects -------------------------

# extracting the mean observer intercepts
obs_effs <- converge %>%
  filter(grepl("obs_raw[",variable,fixed = TRUE)) %>%
  mutate(observer = row_number())%>%
  mutate(obs_effect = mean) %>% #*sdobs$mean) %>%
  select(observer,obs_effect)

# extracting the mean route (site) intercepts
ste_effs <- converge %>%
  filter(grepl("ste_raw[",variable,fixed = TRUE)) %>%
  mutate(site = row_number())%>%
  mutate(ste_effect = mean) %>% #*sdobs$mean) %>%
  select(site,ste_effect)

# extracting the mean route (site) intercepts
strat_effs <- converge %>%
  filter(grepl("strata_raw[",variable,fixed = TRUE)) %>%
  mutate(strata = row_number())%>%
  mutate(strata_effect = mean) %>% #*sdobs$mean) %>%
  select(strata,strata_effect)

# joining with the raw data of observations to explore
# the observer and route intercepts through time
raw_data <- m$raw_data %>%
  inner_join(.,obs_effs,
             by = "observer") %>%
  inner_join(ste_effs,
             by = "site") |>
  inner_join(strat_effs,
             by = "strata") |>
  group_by(year)  |>
  summarise(mean_obs_eff = mean(obs_effect),
            q75_obs = mean_obs_eff + 2*(sd(obs_effect)/sqrt(n())),
            q25_obs = mean_obs_eff - 2*(sd(obs_effect)/sqrt(n())),
            mean_ste_eff = mean(ste_effect),
            q75_ste = mean_ste_eff + 2*(sd(ste_effect)/sqrt(n())),
            q25_ste = mean_ste_eff - 2*(sd(ste_effect)/sqrt(n())),
            mean_strata_eff = mean(strata_effect),
            q75_strata = mean_strata_eff + 2*(sd(strata_effect)/sqrt(n())),
            q25_strata = mean_strata_eff - 2*(sd(strata_effect)/sqrt(n())))

library(patchwork) # allows mising of multiple plots

routes_obs_time <- ggplot()+
  geom_hline(yintercept = 0)+
  geom_pointrange(data = raw_data,
                  aes(x = year, y = mean_obs_eff,
                      ymin = q25_obs, ymax = q75_obs),
                  colour = "red",
                  alpha = 0.5)+
  geom_pointrange(data = raw_data,
                  aes(x = year, y = mean_ste_eff,
                      ymin = q25_ste, ymax = q75_ste),
                  colour = "blue",
                  alpha = 0.5)+
  geom_pointrange(data = raw_data,
                  aes(x = year, y = mean_strata_eff,
                      ymin = q25_strata, ymax = q75_strata),
                  colour = "purple",
                  alpha = 0.5)+
  ylab("Mean and 50% quantiles of observer (red)\nroute (blue) and strata (purple) intercepts by year")+
  theme_bw()

routes_obs_time

p[["continent"]] / routes_obs_time

p[["49_-109"]]



p <- plot_indices(indices = i,
                  spaghetti = TRUE,
                  n_spaghetti = 20,
                  alpha_spaghetti = 0.8)
print(p[["continent"]])



# Trends ------------------------------------------------------------------


t <- generate_trends(i)

t_10 <- generate_trends(i,
                        min_year = 2015,
                        max_year = 2025)


t_first_10 <- generate_trends(i,
                        min_year = 1970,
                        max_year = 1975)

names(t)
names(t$trends)




trend_map <- plot_map(t)
trend_map
trend_map+
        geom_sf(data = extra_layer,
                fill = NA)+
        coord_sf(xlim = bb[c("xmin","xmax")],
                 ylim = bb[c("ymin","ymax")])

trend_map <- plot_map(t_10)
trend_map

trend_map <- plot_map(t_first_10)
trend_map

# gam smooth indices ------------------------------------------------------




# i_gam_smooth <- generate_indices(m,
#                       hpdi = TRUE,
#                       gam_smooths = TRUE)
#
#saveRDS(i_gam_smooth,"output/BASP_fd_gam_smooth_indices.rds")

i_gam_smooths <- readRDS("output/BASP_fd_gam_smooth_indices.rds")
names(i_gam_smooth)

t_smooth <- generate_trends(i_gam_smooth,
                        min_year = 1970,
                        max_year = 2025,
                        gam = TRUE)
trend_map <- plot_map(t_smooth) +
  geom_sf(data = extra_layer,
          fill = NA)+
  coord_sf(xlim = bb[c("xmin","xmax")],
           ylim = bb[c("ymin","ymax")])
print(trend_map)


abund_map <- plot_map(t_10,
                      alternate_column = "rel_abundance")
abund_map

abund_map <- plot_map(t_first_10,
                      alternate_column = "rel_abundance")
abund_map




# Bobolink latlong trends -------------------------------------------------


s <- stratify("latlong",
              "Bobolink")


ps <- prepare_data(s,
                   min_year = 1970,
                   min_n_routes = 1,
                   min_span = 10) |>
  prepare_spatial(strata_map = load_map("latlong"))

m_data <- prepare_model(ps,
                  model = "first_diff",
                  model_variant = "spatial")

# m2 <- run_model(m_data,
#                 refresh = 500,
#                 output_basename = "bobo_fd_spatial",
#                 output_dir = "output")
#
# converge_m2 <- get_summary(m2)

saveRDS(converge_m2,"output/bobo_fd_spatial_convergence.rds")

i_2 <- generate_indices(m2,hpdi = TRUE)

t_2 <- generate_trends(i_2)

map_2 <- plot_map(t_2)


# Coarse comparison with eBird trend map ----------------------------------

#  https://science.ebird.org/status-and-trends/species/boboli/trends-map?showAllTrends=true


t_2_ebird <- generate_trends(i_2,
                             min_year = 2012,
                             max_year = 2022)
map_2_eb <- plot_map(t_2_eb,
                     alternate_column = "percent_change",
                     col_ebird = TRUE)
map_2_eb





# alternative regional summary from fitted data ---------------------------




stratification <- "bbs"

s <- stratify(by = stratification, species = "Purple Martin")
p <- prepare_data(s)
sp <-  prepare_spatial(p,
                       queen = TRUE,
                       strata_map = load_map(stratification))

mp <- prepare_model(sp,
                    model = "first_diff",
                    model_variant = "spatial")

m <- run_model(mp,
               refresh = 500,
               output_basename = "PUMA_fd_spatial",
               output_dir = "output")

i <- generate_indices(m, hpdi = TRUE,
                      regions = c("country"))

trajs <- plot_indices(i, add_observed_means = TRUE)


mp2 <- prepare_model(sp,
                    model = "gamye",
                    model_variant = "spatial")


m2 <- run_model(mp2,
               refresh = 500,
               output_basename = "PUMA_gamye_spatial",
               output_dir = "output")

i2 <- generate_indices(m2, hpdi = TRUE,
                      regions = c("country"))

trajs2 <- plot_indices(i2, add_observed_means = TRUE)



