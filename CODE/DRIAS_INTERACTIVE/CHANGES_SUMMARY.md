# Résumé des Modifications pour la Fonctionnalité de Normalisation des Données DRIAS

## Fichiers créés

1. **normalize_drias_data.R**
   - Script R qui calcule les pourcentages de variation par rapport à REF pour les variables spécifiées
   - Traite à la fois les fichiers GPKG et Excel
   - Crée une structure de dossiers identique dans Data/DRIAS_NORM

2. **generate_normalized_data.sh**
   - Script shell pour exécuter le script de normalisation
   - Vérifie la présence du script R et le rend exécutable
   - Facilite l'exécution du processus de normalisation

3. **README_NORMALISATION.md**
   - Documentation détaillée de la fonctionnalité de normalisation
   - Explique ce qu'est la normalisation et comment l'utiliser
   - Liste les variables supportées et les avantages de cette approche

## Modifications dans app.R

1. **Ajout des chemins vers les données normalisées**
   ```r
   path_indicateurs_saisonniers_norm <- "Data/DRIAS_NORM/INDICATEURS_SAISONNIERS_ETE/Resultats/"
   path_indicateurs_annuels_norm <- "Data/DRIAS_NORM/INDICATEURS_ANNUELS_HORIZONS/Resultats/"
   path_feux_indicateurs_norm <- "Data/DRIAS_NORM/FEUX_INDICATEURS_ANNUELS_HORIZONS/Resultats/"
   path_agri_indicateurs_norm <- "Data/DRIAS_NORM/AGRI_INDICATEURS_ANNUELS_HORIZONS/Resultats/"
   ```

2. **Définition des variables qui peuvent être normalisées**
   ```r
   normalized_variables <- c(
     "NORTAV", "NORSD", "NORTX35", "NORTR", "NORTXHWD", "NORTNCWD", 
     "NORTNFD", "NORRR", "NORRR1MM", "NORFFQ98", "NORFF98"
   )
   ```

3. **Modification de la fonction `selected_folder_path()`**
   - Désormais réactive au choix de normalisation
   - Sélection intelligente entre les dossiers normaux et normalisés
   - Vérification de l'existence des dossiers normalisés

4. **Ajout d'une case à cocher pour la normalisation dans l'interface**
   ```r
   checkboxInput("use_normalized", "Afficher les variations en % par rapport à REF", value = FALSE)
   ```

5. **Ajout d'un texte d'aide conditionnel**
   - Visible uniquement lorsque la normalisation est activée
   - Explique comment interpréter les données normalisées

6. **Création d'un observateur pour les changements de normalisation**
   - Vérifie que la variable sélectionnée peut être normalisée
   - Affiche des notifications appropriées
   - Réinitialise les données pour forcer un rechargement

7. **Modification de la logique d'affichage des cartes**
   - Adaptation des palettes de couleurs pour les données normalisées
   - Utilisation d'une palette divergente (RdBu) pour les pourcentages
   - Échelle fixe de -100% à +100% pour les données normalisées

8. **Amélioration des popups et labels**
   - Adaptation pour afficher "Variation: X%" au lieu de "Valeur: X" pour les données normalisées
   - Création de fonctions spécifiques pour générer popups et labels

9. **Adaptation de la légende**
   - Ajout du titre "Variation en %" pour les données normalisées

## Instructions d'utilisation

1. Exécuter le script de génération des données normalisées :
   ```bash
   ./generate_normalized_data.sh
   ```

2. Lancer l'application Shiny :
   ```r
   shiny::runApp()
   ```

3. Dans l'interface, cocher la case "Afficher les variations en % par rapport à REF" pour utiliser les données normalisées

## Remarques

- La normalisation n'est active que pour H1, H2 et H3. Les données REF restent inchangées
- Si une variable non normalisable est sélectionnée avec la normalisation activée, un avertissement s'affiche
- Les fichiers normalisés sont générés automatiquement à partir des données originales et stockés dans Data/DRIAS_NORM
- Cette fonctionnalité rend les tendances climatiques plus faciles à interpréter 