# Instalação do pacote
install.packages("greenR")
library(greenR)


# Identificação da localidade de interesse
teresina <- get_osm_data("Teresina, Brazil")    

# Salvar o objeto teresina (será útil no script 4)
saveRDS(teresina, "teresina.rds")

# Areas verdes

area_verde_teresina <- teresina$green_areas

visualize_green_spaces(area_verde_teresina)

green_space_clustering(area_verde_teresina, num_clusters = 3) # com 3 clusters

green_space_clustering(area_verde_teresina, num_clusters = 5) # com 5 clusters


# Acessibilidade
resultado <- analyze_green_accessibility(
  network_data = teresina$highways$osm_lines,
  green_areas = teresina$green_areas$osm_polygons,
  mode = "walking",
  grid_size = 300 
  
)

str(resultado)

viz <- create_accessibility_visualizations(
  accessibility_analysis = resultado,
  green_areas = teresina$green_areas$osm_polygons,
  mode = "walking"
)

print(viz$distance_map)

print(viz$coverage_plot)

print(viz$directional_plot)

print(viz$combined_plot)

viz$leaflet_map

cat(viz$summary)

print(viz$directional_table)

# Para saber mais, ver a documentação do pacote:
# https://cran.r-project.org/web/packages/greenR/greenR.pdf

