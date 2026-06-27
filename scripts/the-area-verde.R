# =========================================================
# ANÁLISE DE ÁREAS VERDES EM TERESINA COM O PACOTE greenR
# =========================================================

# 1. Pacotes
install.packages("greenR")
library(greenR)

# =========================================================
# 2. Coleta dos dados do OpenStreetMap
# =========================================================

teresina <- get_osm_data("Teresina, Brazil")

# Salvar objeto para reutilização
saveRDS(teresina, "teresina.rds")

# Em análises futuras, usar:
# teresina <- readRDS("teresina.rds")

# =========================================================
# 3. Áreas verdes
# =========================================================

area_verde_teresina <- teresina$green_areas

# Visualização interativa das áreas verdes
visualize_green_spaces(area_verde_teresina)

# Agrupamento espacial das áreas verdes
green_space_clustering(area_verde_teresina, num_clusters = 3)
green_space_clustering(area_verde_teresina, num_clusters = 5)

# =========================================================
# 4. Acessibilidade às áreas verdes
# =========================================================

resultado <- analyze_green_accessibility(
  network_data = teresina$highways$osm_lines,
  green_areas = teresina$green_areas$osm_polygons,
  mode = "walking",
  grid_size = 300
)

# Ver estrutura do resultado
str(resultado)

# =========================================================
# 5. Visualizações da acessibilidade
# =========================================================

viz <- create_accessibility_visualizations(
  accessibility_analysis = resultado,
  green_areas = teresina$green_areas$osm_polygons,
  mode = "walking"
)

print(viz$distance_map)
print(viz$coverage_plot)
print(viz$directional_plot)
print(viz$combined_plot)

# Mapa interativo
viz$leaflet_map

# Resumo textual
cat(viz$summary)

# Tabela direcional
print(viz$directional_table)
