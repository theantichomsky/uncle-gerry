required_packages <- c("sf", "tigris", "tidycensus", "dplyr", "ggplot2", "cowplot", "classInt")
missing_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]

if (length(missing_packages) > 0) {
  tryCatch({
    install.packages(missing_packages, dependencies = TRUE, repos = "https://cloud.r-project.org")
  }, error = function(e) {
    stop("Required packages not installed.")
  })
}

library(sf)
library(tigris)
library(tidycensus)
library(dplyr)
library(ggplot2)
library(cowplot)
library(classInt)

options(tigris_use_cache = TRUE)
options(tigris_class = "sf")

target_counties <- c(
  "Alameda", "Contra Costa", "Marin", "Napa", "San Francisco", 
  "San Mateo", "Santa Clara", "Solano", "Sonoma",
  "Sacramento", "San Joaquin", "Yolo", "Placer", "El Dorado", 
  "Yuba", "Sutter", "Butte", "Glenn", "Colusa", "Lake",
  "Santa Cruz", "San Benito", "Stanislaus", "Calaveras", 
  "Amador", "Sierra", "Plumas",
  "Mendocino", "Tehama", "Nevada"
)

ca_counties_boundaries <- counties(state = "CA", cb = TRUE, year = 2020) %>%
  filter(NAME %in% target_counties) %>%
  st_transform(crs = 3310)

ca_tracts <- tracts(state = "CA", cb = FALSE, year = 2020) %>%
  st_transform(crs = 3310)

samnam_tracts <- ca_tracts %>%
  st_filter(ca_counties_boundaries, .predicate = st_intersects) %>%
  st_make_valid()

tryCatch({
  ca_pop <- get_acs(
    geography = "tract",
    variables = "B01003_001",
    state = "CA",
    year = 2020,
    geometry = FALSE
  )
  
  samnam_tracts <- samnam_tracts %>%
    left_join(
      ca_pop %>% 
        select(GEOID, estimate) %>%
        rename(POPULATION = estimate),
      by = "GEOID"
    )
}, error = function(e) {
  stop("Could not fetch population data. Please set your Census API key.")
})

samnam_tracts <- samnam_tracts %>%
  filter(!is.na(POPULATION) & POPULATION > 0) %>%
  st_transform(crs = 3310)

samnam_tracts <- samnam_tracts %>%
  st_join(
    ca_counties_boundaries %>% select(NAME) %>% rename(COUNTY = NAME),
    left = FALSE,
    largest = TRUE
  )

ca_places <- places(state = "CA", cb = FALSE, year = 2020) %>%
  st_transform(crs = 3310)

sf_city_boundary <- ca_places %>% filter(NAME == "San Francisco")
sac_city_boundary <- ca_places %>% filter(NAME == "Sacramento")
sj_city_boundary <- ca_places %>% filter(grepl("^San Jose", NAME, ignore.case = TRUE))

if (nrow(sj_city_boundary) == 0) {
  sj_city_boundary <- ca_places %>%
    filter(NAME == "San Jose" | NAME == "San José")
}

samnam_tracts <- samnam_tracts %>%
  mutate(
    IN_SF_CITY = st_intersects(geometry, sf_city_boundary, sparse = FALSE)[,1],
    IN_SAC_CITY = st_intersects(geometry, sac_city_boundary, sparse = FALSE)[,1],
    IN_SJ_CITY = st_intersects(geometry, sj_city_boundary, sparse = FALSE)[,1]
  )

total_pop <- sum(samnam_tracts$POPULATION, na.rm = TRUE)
target_districts <- 269
ideal_pop_per_district <- total_pop / target_districts

county_stats <- samnam_tracts %>%
  st_drop_geometry() %>%
  group_by(COUNTY) %>%
  summarise(
    POPULATION = sum(POPULATION, na.rm = TRUE),
    N_TRACTS = n(),
    .groups = "drop"
  ) %>%
  mutate(TARGET_DISTRICTS = round(POPULATION / ideal_pop_per_district))

sf_city_pop <- sum(samnam_tracts$POPULATION[samnam_tracts$IN_SF_CITY], na.rm = TRUE)
sf_city_target <- 8
sf_county_pop <- county_stats$POPULATION[county_stats$COUNTY == "San Francisco"]
sf_county_rest_pop <- sf_county_pop - sf_city_pop

if (sf_county_rest_pop > 0) {
  sf_county_rest_target <- max(1, round(sf_county_rest_pop / ideal_pop_per_district))
} else {
  sf_county_rest_target <- 0
}

sf_county_total_target <- sf_city_target + sf_county_rest_target
county_stats$TARGET_DISTRICTS[county_stats$COUNTY == "San Francisco"] <- sf_county_total_target

sac_city_pop <- sum(samnam_tracts$POPULATION[samnam_tracts$IN_SAC_CITY], na.rm = TRUE)
sac_city_target <- 8
sac_county_pop <- county_stats$POPULATION[county_stats$COUNTY == "Sacramento"]
sac_county_rest_pop <- sac_county_pop - sac_city_pop

if (sac_county_rest_pop > 0) {
  sac_county_rest_target <- max(1, round(sac_county_rest_pop / ideal_pop_per_district))
} else {
  sac_county_rest_target <- 0
}

sac_county_total_target <- sac_city_target + sac_county_rest_target
county_stats$TARGET_DISTRICTS[county_stats$COUNTY == "Sacramento"] <- sac_county_total_target

sj_city_pop <- sum(samnam_tracts$POPULATION[samnam_tracts$IN_SJ_CITY], na.rm = TRUE)
sj_city_target <- round(sj_city_pop / ideal_pop_per_district)
sc_county_pop <- county_stats$POPULATION[county_stats$COUNTY == "Santa Clara"]
sc_county_rest_pop <- sc_county_pop - sj_city_pop

if (sc_county_rest_pop > 0) {
  sc_county_rest_target <- max(1, round(sc_county_rest_pop / ideal_pop_per_district))
} else {
  sc_county_rest_target <- 0
}

sc_county_total_target <- sj_city_target + sc_county_rest_target
county_stats$TARGET_DISTRICTS[county_stats$COUNTY == "Santa Clara"] <- sc_county_total_target

total_allocated <- sum(county_stats$TARGET_DISTRICTS)
difference <- target_districts - total_allocated

if (difference != 0) {
  other_counties <- county_stats %>%
    filter(COUNTY != "San Francisco" & COUNTY != "Sacramento") %>%
    arrange(desc(POPULATION))
  
  if (difference > 0) {
    for (i in 1:min(abs(difference), nrow(other_counties))) {
      county_stats$TARGET_DISTRICTS[county_stats$COUNTY == other_counties$COUNTY[i]] <- 
        county_stats$TARGET_DISTRICTS[county_stats$COUNTY == other_counties$COUNTY[i]] + 1
    }
  } else {
    other_counties <- other_counties %>% arrange(POPULATION)
    for (i in 1:min(abs(difference), nrow(other_counties))) {
      current <- county_stats$TARGET_DISTRICTS[county_stats$COUNTY == other_counties$COUNTY[i]]
      if (current > 1) {
        county_stats$TARGET_DISTRICTS[county_stats$COUNTY == other_counties$COUNTY[i]] <- current - 1
      }
    }
  }
}

district_within_county <- function(county_name, target_districts_county, all_tracts, ideal_pop, pre_filtered_tracts = NULL) {
  cat(sprintf("\nDistricting %s (target: %d districts)...\n", county_name, target_districts_county))
  
  if (!is.null(pre_filtered_tracts)) {
    county_tracts <- pre_filtered_tracts
  } else {
    county_tracts <- all_tracts %>% filter(COUNTY == county_name)
  }
  
  n_tracts_county <- nrow(county_tracts)
  if (n_tracts_county == 0) return(NULL)
  
  district_assignment <- 1:n_tracts_county
  ideal_pop_county <- sum(county_tracts$POPULATION, na.rm = TRUE) / target_districts_county
  adjacency_sparse <- st_touches(county_tracts, sparse = TRUE)
  
  find_adjacent_districts_county <- function(dist_id, assignment) {
    tracts_in_dist <- which(assignment == dist_id)
    adjacent_tract_ids <- unique(unlist(adjacency_sparse[tracts_in_dist]))
    if (length(adjacent_tract_ids) > 0) {
      unique(assignment[adjacent_tract_ids])
    } else {
      integer(0)
    }
  }
  
  calc_district_pops_county <- function(assignment) {
    county_tracts %>%
      st_drop_geometry() %>%
      mutate(DISTRICT = assignment) %>%
      group_by(DISTRICT) %>%
      summarise(POP = sum(POPULATION, na.rm = TRUE), .groups = "drop") %>%
      pull(POP)
  }
  
  current_districts <- length(unique(district_assignment))
  max_iterations <- 10000
  iteration <- 0
  
  while (current_districts > target_districts_county && iteration < max_iterations) {
    iteration <- iteration + 1
    
    if (iteration %% 200 == 0) {
      cat(sprintf("  Iteration %d: %d districts remaining\n", iteration, current_districts))
    }
    
    district_pops <- calc_district_pops_county(district_assignment)
    unique_districts <- unique(district_assignment)
    
    district_df <- data.frame(
      DISTRICT = unique_districts,
      POP = district_pops
    ) %>%
      filter(POP < ideal_pop_county * 1.2) %>%
      arrange(POP)
    
    if (nrow(district_df) == 0) break
    
    best_merge <- NULL
    best_score <- Inf
    district_df_sorted <- district_df %>% arrange(POP)
    
    for (i in 1:min(200, nrow(district_df_sorted))) {
      dist1_id <- district_df_sorted$DISTRICT[i]
      dist1_pop <- district_df_sorted$POP[i]
      
      if (dist1_pop >= ideal_pop_county * 0.9) next
      
      adj_districts <- find_adjacent_districts_county(dist1_id, district_assignment)
      adj_districts <- adj_districts[adj_districts != dist1_id]
      
      if (length(adj_districts) > 0) {
        all_adj_pops <- data.frame(
          DISTRICT = unique_districts,
          POP = district_pops
        ) %>%
          filter(DISTRICT %in% adj_districts) %>%
          arrange(POP)
        
        for (j in 1:nrow(all_adj_pops)) {
          dist2_id <- all_adj_pops$DISTRICT[j]
          dist2_pop <- all_adj_pops$POP[j]
          combined_pop <- dist1_pop + dist2_pop
          
          if (combined_pop <= ideal_pop_county * 2.0) {
            pop_score <- abs(combined_pop - ideal_pop_county)
            size_penalty <- (dist1_pop + dist2_pop) / ideal_pop_county
            score <- pop_score + (size_penalty * 0.1)
            
            if (score < best_score) {
              best_score <- score
              best_merge <- list(dist1 = dist1_id, dist2 = dist2_id)
            }
          }
        }
      }
    }
    
    if (is.null(best_merge)) {
      smallest_dist <- district_df$DISTRICT[1]
      adj_districts <- find_adjacent_districts_county(smallest_dist, district_assignment)
      adj_districts <- adj_districts[adj_districts != smallest_dist]
      
      if (length(adj_districts) > 0) {
        adj_pops <- district_df %>%
          filter(DISTRICT %in% adj_districts) %>%
          arrange(POP)
        
        if (nrow(adj_pops) > 0) {
          best_merge <- list(
            dist1 = smallest_dist,
            dist2 = adj_pops$DISTRICT[1]
          )
        } else {
          break
        }
      } else {
        break
      }
    }
    
    district_assignment[district_assignment == best_merge$dist2] <- best_merge$dist1
    current_districts <- length(unique(district_assignment))
  }
  
  while (current_districts > target_districts_county) {
    district_pops <- calc_district_pops_county(district_assignment)
    unique_districts <- unique(district_assignment)
    
    district_df <- data.frame(
      DISTRICT = unique_districts,
      POP = district_pops
    ) %>%
      arrange(POP)
    
    smallest_dist <- district_df$DISTRICT[1]
    adj_districts <- find_adjacent_districts_county(smallest_dist, district_assignment)
    adj_districts <- adj_districts[adj_districts != smallest_dist]
    
    if (length(adj_districts) == 0) break
    
    adj_pops <- district_df %>%
      filter(DISTRICT %in% adj_districts) %>%
      arrange(POP)
    
    if (nrow(adj_pops) == 0) break
    
    district_assignment[district_assignment == adj_pops$DISTRICT[1]] <- smallest_dist
    current_districts <- length(unique(district_assignment))
    
    if (current_districts %% 5 == 0) {
      cat(sprintf("  Final merge: %d districts remaining\n", current_districts))
    }
  }
  
  county_tracts %>%
    mutate(DISTRICT = district_assignment)
}

all_county_districts <- list()
district_counter <- 1

for (i in 1:nrow(county_stats)) {
  county_name <- county_stats$COUNTY[i]
  target_county <- county_stats$TARGET_DISTRICTS[i]
  
  if (county_name == "San Francisco") {
    sf_county_tracts <- samnam_tracts %>% filter(COUNTY == "San Francisco")
    sf_city_tracts <- sf_county_tracts %>% filter(IN_SF_CITY == TRUE)
    sf_rest_tracts <- sf_county_tracts %>% filter(IN_SF_CITY == FALSE)
    
    if (nrow(sf_city_tracts) > 0) {
      sf_city_result <- district_within_county(
        "San Francisco City", 
        sf_city_target, 
        samnam_tracts,
        ideal_pop_per_district,
        pre_filtered_tracts = sf_city_tracts
      )
      
      if (!is.null(sf_city_result)) {
        unique_districts <- unique(sf_city_result$DISTRICT)
        for (dist_id in unique_districts) {
          sf_city_result$DISTRICT[sf_city_result$DISTRICT == dist_id] <- district_counter
          district_counter <- district_counter + 1
        }
        all_county_districts[[length(all_county_districts) + 1]] <- sf_city_result
      }
    }
    
    if (nrow(sf_rest_tracts) > 0 && sf_county_rest_target > 0) {
      sf_rest_result <- district_within_county(
        "San Francisco County Rest", 
        sf_county_rest_target, 
        samnam_tracts,
        ideal_pop_per_district,
        pre_filtered_tracts = sf_rest_tracts
      )
      
      if (!is.null(sf_rest_result)) {
        unique_districts <- unique(sf_rest_result$DISTRICT)
        for (dist_id in unique_districts) {
          sf_rest_result$DISTRICT[sf_rest_result$DISTRICT == dist_id] <- district_counter
          district_counter <- district_counter + 1
        }
        all_county_districts[[length(all_county_districts) + 1]] <- sf_rest_result
      }
    }
  } else if (county_name == "Sacramento") {
    sac_county_tracts <- samnam_tracts %>% filter(COUNTY == "Sacramento")
    sac_city_tracts <- sac_county_tracts %>% filter(IN_SAC_CITY == TRUE)
    sac_rest_tracts <- sac_county_tracts %>% filter(IN_SAC_CITY == FALSE)
    
    if (nrow(sac_city_tracts) > 0) {
      sac_city_result <- district_within_county(
        "Sacramento City", 
        sac_city_target, 
        samnam_tracts,
        ideal_pop_per_district,
        pre_filtered_tracts = sac_city_tracts
      )
      
      if (!is.null(sac_city_result)) {
        unique_districts <- unique(sac_city_result$DISTRICT)
        for (dist_id in unique_districts) {
          sac_city_result$DISTRICT[sac_city_result$DISTRICT == dist_id] <- district_counter
          district_counter <- district_counter + 1
        }
        all_county_districts[[length(all_county_districts) + 1]] <- sac_city_result
      }
    }
    
    if (nrow(sac_rest_tracts) > 0 && sac_county_rest_target > 0) {
      sac_rest_result <- district_within_county(
        "Sacramento County Rest", 
        sac_county_rest_target, 
        samnam_tracts,
        ideal_pop_per_district,
        pre_filtered_tracts = sac_rest_tracts
      )
      
      if (!is.null(sac_rest_result)) {
        unique_districts <- unique(sac_rest_result$DISTRICT)
        for (dist_id in unique_districts) {
          sac_rest_result$DISTRICT[sac_rest_result$DISTRICT == dist_id] <- district_counter
          district_counter <- district_counter + 1
        }
        all_county_districts[[length(all_county_districts) + 1]] <- sac_rest_result
      }
    }
  } else if (county_name == "Santa Clara") {
    sc_county_tracts <- samnam_tracts %>% filter(COUNTY == "Santa Clara")
    sj_city_tracts <- sc_county_tracts %>% filter(IN_SJ_CITY == TRUE)
    sc_rest_tracts <- sc_county_tracts %>% filter(IN_SJ_CITY == FALSE)
    
    if (nrow(sj_city_tracts) > 0) {
      sj_city_result <- district_within_county(
        "San Jose City", 
        sj_city_target, 
        samnam_tracts,
        ideal_pop_per_district,
        pre_filtered_tracts = sj_city_tracts
      )
      
      if (!is.null(sj_city_result)) {
        unique_districts <- unique(sj_city_result$DISTRICT)
        for (dist_id in unique_districts) {
          sj_city_result$DISTRICT[sj_city_result$DISTRICT == dist_id] <- district_counter
          district_counter <- district_counter + 1
        }
        all_county_districts[[length(all_county_districts) + 1]] <- sj_city_result
      }
    }
    
    if (nrow(sc_rest_tracts) > 0 && sc_county_rest_target > 0) {
      sc_rest_result <- district_within_county(
        "Santa Clara County Rest", 
        sc_county_rest_target, 
        samnam_tracts,
        ideal_pop_per_district,
        pre_filtered_tracts = sc_rest_tracts
      )
      
      if (!is.null(sc_rest_result)) {
        unique_districts <- unique(sc_rest_result$DISTRICT)
        for (dist_id in unique_districts) {
          sc_rest_result$DISTRICT[sc_rest_result$DISTRICT == dist_id] <- district_counter
          district_counter <- district_counter + 1
        }
        all_county_districts[[length(all_county_districts) + 1]] <- sc_rest_result
      }
    }
  } else {
    county_result <- district_within_county(
      county_name, 
      target_county, 
      samnam_tracts, 
      ideal_pop_per_district
    )
    
    if (!is.null(county_result)) {
      unique_districts <- unique(county_result$DISTRICT)
      for (dist_id in unique_districts) {
        county_result$DISTRICT[county_result$DISTRICT == dist_id] <- district_counter
        district_counter <- district_counter + 1
      }
      all_county_districts[[length(all_county_districts) + 1]] <- county_result
    }
  }
}

all_districts <- bind_rows(all_county_districts)

final_districts <- all_districts %>%
  group_by(DISTRICT) %>%
  summarise(
    POPULATION = sum(POPULATION, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(DISTRICT_NUM = row_number()) %>%
  st_make_valid()

county_union <- ca_counties_boundaries %>% 
  st_union() %>%
  st_make_valid()

final_districts <- final_districts %>%
  st_intersection(county_union) %>%
  st_make_valid() %>%
  filter(st_is_valid(geometry) & as.numeric(st_area(geometry)) > 0)

cat(sprintf("\n=== DISTRICTING COMPLETE ===\n"))
cat(sprintf("Final district count: %d (target: %d)\n", nrow(final_districts), target_districts))

ca_counties <- ca_counties_boundaries

bbox <- st_bbox(final_districts)

sac_city_districts_main <- st_filter(final_districts, sac_city_boundary, .predicate = st_intersects)
sj_city_districts_main <- st_filter(final_districts, sj_city_boundary, .predicate = st_intersects)

other_districts_main <- final_districts %>%
  filter(!DISTRICT_NUM %in% c(sac_city_districts_main$DISTRICT_NUM,
                               sj_city_districts_main$DISTRICT_NUM))

main_map <- ggplot() +
  geom_sf(data = sj_city_districts_main, 
          fill = "#EFEFEF", 
          color = "white", 
          linewidth = 0.1) +
  geom_sf(data = sac_city_districts_main, 
          fill = "#EFEFEF", 
          color = "white", 
          linewidth = 0.1) +
  geom_sf(data = other_districts_main, 
          fill = "#EFEFEF", 
          color = "white", 
          linewidth = 0.1) +
  geom_sf(data = ca_counties, 
          fill = NA, 
          color = "white", 
          linewidth = 0.3) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA)
  ) +
  coord_sf(expand = FALSE, 
           xlim = c(bbox[["xmin"]], bbox[["xmax"]]),
           ylim = c(bbox[["ymin"]], bbox[["ymax"]]))

sf_city_bbox <- st_bbox(sf_city_boundary)
sf_city_districts <- st_filter(final_districts, sf_city_boundary, .predicate = st_intersects)

sf_inset <- ggplot() +
  geom_sf(data = sf_city_districts, 
          fill = "#EFEFEF", 
          color = "white", 
          linewidth = 0.15) +
  geom_sf(data = sf_city_boundary, 
          fill = NA, 
          color = "white", 
          linewidth = 0.4) +
  coord_sf(xlim = c(sf_city_bbox["xmin"], sf_city_bbox["xmax"]),
           ylim = c(sf_city_bbox["ymin"], sf_city_bbox["ymax"]),
           expand = FALSE) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "white", color = "white", linewidth = 0.5),
    panel.background = element_rect(fill = "white", color = NA)
  )

sj_city_bbox <- st_bbox(sj_city_boundary)
sj_city_districts <- st_filter(final_districts, sj_city_boundary, .predicate = st_intersects)

sj_inset <- ggplot() +
  geom_sf(data = sj_city_districts, 
          fill = "#EFEFEF", 
          color = "white", 
          linewidth = 0.15) +
  geom_sf(data = sj_city_boundary, 
          fill = NA, 
          color = "white", 
          linewidth = 0.4) +
  coord_sf(xlim = c(sj_city_bbox["xmin"], sj_city_bbox["xmax"]),
           ylim = c(sj_city_bbox["ymin"], sj_city_bbox["ymax"]),
           expand = FALSE) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "white", color = "white", linewidth = 0.5),
    panel.background = element_rect(fill = "white", color = NA)
  )

sac_city_bbox <- st_bbox(sac_city_boundary)
sac_city_districts <- st_filter(final_districts, sac_city_boundary, .predicate = st_intersects)

sac_inset <- ggplot() +
  geom_sf(data = sac_city_districts, 
          fill = "#EFEFEF", 
          color = "white", 
          linewidth = 0.15) +
  geom_sf(data = sac_city_boundary, 
          fill = NA, 
          color = "white", 
          linewidth = 0.4) +
  coord_sf(xlim = c(sac_city_bbox["xmin"], sac_city_bbox["xmax"]),
           ylim = c(sac_city_bbox["ymin"], sac_city_bbox["ymax"]),
           expand = FALSE) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "white", color = "white", linewidth = 0.5),
    panel.background = element_rect(fill = "white", color = NA)
  )

combined_map <- ggdraw() +
  draw_plot(main_map, x = 0, y = 0, width = 1, height = 1) +
  draw_plot(sf_inset, x = 0.02, y = 0.70, width = 0.22, height = 0.22) +
  draw_plot(sj_inset, x = 0.02, y = 0.47, width = 0.22, height = 0.22) +
  draw_plot(sac_inset, x = 0.02, y = 0.24, width = 0.22, height = 0.22)

ggsave("samnam_electoral_map.png", 
       combined_map, 
       width = 16, 
       height = 12, 
       dpi = 300,
       bg = "white")

cat("\n=== MAP GENERATION COMPLETE ===\n")
cat("Map saved as 'samnam_electoral_map.png'\n")
print(combined_map)
