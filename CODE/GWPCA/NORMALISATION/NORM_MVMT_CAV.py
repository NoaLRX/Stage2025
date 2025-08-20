import os
import pandas as pd
import geopandas as gpd
from shapely.geometry import Point
import glob

# Paths
input_dir = "/Users/noa/Desktop/PRISM/Data/Indicateurs_PRISM/Mouvement2Terrain/MVT_1_CAV"
output_dir = "/Users/noa/Desktop/PRISM/Data/Indicateurs_PRISM/Mouvement2Terrain"
postal_codes_path = "/Users/noa/Desktop/PRISM/Data/MISC/Codes postaux - Communes/codes_postaux_region.shp"

# Nature types to exclude
excluded_types = [
    "orifice_artificiel_horizontal",
    "orifice_artificiel_vertical",
    "ouvrage civil",
    "ouvrage linéaire",
    "ouvrage linéaire et surfacique",
    "ouvrage surfacique",
    "souterrain refuge",
    "tunnel ferroviaire",
    "tunnel routier"
]

# Create output directory if it doesn't exist
os.makedirs(output_dir, exist_ok=True)

def process_csv_files(apply_filters=True):
    # Get all CSV files in the directory
    csv_files = glob.glob(os.path.join(input_dir, "*.csv"))
    print(f"Found {len(csv_files)} CSV files to process.")
    
    # Initialize an empty dataframe to store all valid records
    all_data = []
    
    # Process each CSV file
    for csv_file in csv_files:
        try:
            print(f"Processing {os.path.basename(csv_file)}...")
            # Read the CSV file
            df = pd.read_csv(csv_file, sep=';', encoding='utf-8', low_memory=False)
            
            # Skip if the file is empty or doesn't have the required columns
            if df.empty or 'xouvl2e' not in df.columns or 'youvl2e' not in df.columns or 'id' not in df.columns or 'natureCavite' not in df.columns:
                print(f"Skipping {os.path.basename(csv_file)} - Missing required columns")
                continue
                
            # Filter out excluded nature types if apply_filters is True
            if apply_filters:
                df = df[~df['natureCavite'].isin(excluded_types)]
            
            # Select only required columns
            df = df[['id', 'natureCavite', 'xouvl2e', 'youvl2e']]
            
            # Convert coordinates to numeric, dropping any rows with invalid coordinates
            df['xouvl2e'] = pd.to_numeric(df['xouvl2e'], errors='coerce')
            df['youvl2e'] = pd.to_numeric(df['youvl2e'], errors='coerce')
            df = df.dropna(subset=['xouvl2e', 'youvl2e'])
            
            all_data.append(df)
            
        except Exception as e:
            print(f"Error processing {os.path.basename(csv_file)}: {str(e)}")
    
    # Combine all dataframes
    if all_data:
        combined_df = pd.concat(all_data, ignore_index=True)
        print(f"Combined data contains {len(combined_df)} valid records.")
        return combined_df
    else:
        print("No valid data found in any CSV files.")
        return None

def create_geopackage(df, filtered=True):
    # Create a GeoDataFrame with Point geometry from coordinates
    # EPSG:27572 is the correct projection for "Lambert II étendu" coordinates
    geometry = [Point(x, y) for x, y in zip(df['xouvl2e'], df['youvl2e'])]
    gdf = gpd.GeoDataFrame(df, geometry=geometry, crs="EPSG:27572")
    
    # Drop the original coordinate columns
    gdf = gdf.drop(['xouvl2e', 'youvl2e'], axis=1)
    
    # Reproject to a more standard CRS (EPSG:4326 - WGS84)
    gdf = gdf.to_crs("EPSG:4326")
    
    # Set output filename based on whether data is filtered or not
    output_suffix = "_l2e"
    if filtered:
        output_gpkg = os.path.join(output_dir, f"cavites_filtrees{output_suffix}.gpkg")
    else:
        output_gpkg = os.path.join(output_dir, f"cavites_completes{output_suffix}.gpkg")
    
    # Save as geopackage
    gdf.to_file(output_gpkg, driver="GPKG")
    print(f"Geopackage saved to {output_gpkg}")
    
    return gdf

def count_cavities_by_postal_code(filtered_cavities_gpkg):
    # Le chemin vers le fichier filtré des cavités
    filtered_cavities_path = os.path.join(output_dir, "cavites_filtrees_l2e.gpkg")
    print(f"Counting cavities by postal code using filtered data from: {filtered_cavities_path}")
    
    # Lire le fichier GPKG filtré
    cavities_gdf = gpd.read_file(filtered_cavities_path)
    
    # Read the postal code shapefile
    postal_codes = gpd.read_file(postal_codes_path)
    
    # Make sure both GeoDataFrames are in the same CRS
    postal_codes = postal_codes.to_crs(cavities_gdf.crs)
    
    print(f"Spatial join between {len(cavities_gdf)} cavities and {len(postal_codes)} postal code areas")
    
    # Spatial join to count cavities in each postal code area
    joined = gpd.sjoin(cavities_gdf, postal_codes, how="inner", predicate="within")
    
    # Count cavities per postal code using 'ID' as the postal code column
    cavity_counts = joined.groupby('ID')[['id']].count()
    cavity_counts.columns = ['cavity_count']
    
    # Merge the counts back to the postal code data
    postal_codes_with_counts = postal_codes.set_index('ID')
    postal_codes_with_counts = postal_codes_with_counts.join(cavity_counts)
    postal_codes_with_counts = postal_codes_with_counts.reset_index()
    
    # Fill NaN counts with 0
    postal_codes_with_counts['cavity_count'] = postal_codes_with_counts['cavity_count'].fillna(0).astype(int)
    
    # Save result as geopackage
    output_counts = os.path.join(output_dir, "cavites_par_code_postal_l2e.gpkg")
    postal_codes_with_counts.to_file(output_counts, driver="GPKG")
    print(f"Cavity counts by postal code saved to {output_counts}")
    
    return postal_codes_with_counts

def main():
    print("Starting cave data processing using xouvl2e/youvl2e coordinates (Lambert II étendu)...")
    
    # Process all CSV files with filters and combine them
    print("\n--- Processing filtered data ---")
    filtered_data = process_csv_files(apply_filters=True)
    if filtered_data is None or filtered_data.empty:
        print("No filtered data to process. Exiting.")
        return
    
    # Create geopackage with the filtered and combined data
    create_geopackage(filtered_data, filtered=True)
    
    # Process all CSV files without filters and combine them
    print("\n--- Processing complete data (without filters) ---")
    complete_data = process_csv_files(apply_filters=False)
    if complete_data is None or complete_data.empty:
        print("No complete data to process. Exiting.")
        return
    
    # Create geopackage with the complete unfiltered data
    create_geopackage(complete_data, filtered=False)
    
    # Count cavities by postal code using the filtered data GPKG
    print("\n--- Counting cavities by postal code ---")
    postal_code_counts = count_cavities_by_postal_code("cavites_filtrees_l2e.gpkg")
    
    print("\nProcessing complete!")

if __name__ == "__main__":
    main()
