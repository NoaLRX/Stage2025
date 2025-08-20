import geopandas as gpd

# Charger le fichier des codes postaux
code_postal_path = "/Users/noa/Desktop/PRISM/Data/MISC/Codes postaux - Communes/codes_postaux_region.shp"
code_postal_gdf = gpd.read_file(code_postal_path)

# Afficher les noms des colonnes
print("Colonnes disponibles dans le fichier des codes postaux:")
print(code_postal_gdf.columns.tolist())

# Afficher les premières lignes pour voir le contenu
print("\nAperçu des données:")
print(code_postal_gdf.head()) 