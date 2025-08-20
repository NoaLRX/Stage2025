#!/bin/bash

# Script pour générer les données normalisées DRIAS

echo "Génération des données normalisées DRIAS..."
echo "Ce processus peut prendre du temps selon la quantité de données à traiter."

# Vérifier si le script R existe
if [ ! -f "normalize_drias_data.R" ]; then
  echo "Erreur: Le fichier normalize_drias_data.R n'existe pas!"
  exit 1
fi

# Rendre le script R exécutable
chmod +x normalize_drias_data.R

# Exécuter le script R
echo "Lancement du script de normalisation..."
Rscript normalize_drias_data.R

echo "Traitement terminé!" 