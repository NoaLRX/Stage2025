# Fonctionnalité de Normalisation des Données DRIAS

Ce document explique la nouvelle fonctionnalité de normalisation des données DRIAS qui a été ajoutée à l'application interactive.

## Qu'est-ce que la normalisation des données?

La normalisation implémentée dans cette application consiste à calculer le **pourcentage de variation** des indicateurs climatiques par rapport à la période de référence (REF). 

Par exemple, si pour une commune donnée:
- La température moyenne (NORTAV) pour REF est de 12°C
- La température moyenne pour H1 est de 14°C

Alors la valeur normalisée pour H1 sera de: ((14 - 12) / 12) * 100 = +16.67%

Cette normalisation permet de mieux visualiser l'évolution relative du climat plutôt que les valeurs absolues.

## Variables normalisées

La normalisation a été implémentée pour les variables suivantes:

- NORTAV (Température moyenne)
- NORSD (Nombre de journées d'été)
- NORTX35 (Nombre de jours de forte chaleur)
- NORTR (Nombre de nuits tropicales)
- NORTXHWD (Nombre de jours de vague de chaleur)
- NORTNCWD (Nombre de jours de vague de froid)
- NORTNFD (Nombre de jours de gel)
- NORRR (Cumul de précipitations)
- NORRR1MM (Nombre de jours de pluie)
- NORFFQ98 (Vent fort)
- NORFF98 (Nombre de jours de vent > Q98)

## Comment utiliser la fonctionnalité de normalisation

1. Dans l'interface de l'application, cochez la case "Afficher les variations en % par rapport à REF"
2. Sélectionnez un scénario, un horizon (H1, H2 ou H3) et une variable normalisable
3. Cliquez sur "Confirmer et charger la carte"

La carte affichera alors les pourcentages de variation par rapport à REF. Notez que:
- Pour l'horizon REF, les données restent inchangées (non normalisées)
- Pour les variables non normalisables, un message d'avertissement s'affichera

## Interprétation des données normalisées

- Les **valeurs positives** (en rouge) indiquent une **augmentation** par rapport à REF
- Les **valeurs négatives** (en bleu) indiquent une **diminution** par rapport à REF
- Plus la couleur est intense, plus la variation est importante

Par exemple, une valeur de +50% pour NORTAV en H3 signifie que la température moyenne augmentera de 50% par rapport à la période de référence.

## Génération des données normalisées

Les données normalisées doivent être générées avant de pouvoir utiliser cette fonctionnalité. Pour cela:

1. Vérifiez que le script `normalize_drias_data.R` est présent à la racine du projet
2. Exécutez le script de génération: `./generate_normalized_data.sh`

Le script créera un répertoire `Data/DRIAS_NORM` avec une structure identique à celle des données d'origine, mais contenant les versions normalisées des fichiers.

Note: Ce processus peut prendre du temps selon la quantité de données à traiter.

## Avantages de la normalisation

- Permet de mieux visualiser les tendances climatiques
- Facilite la comparaison entre différentes régions (les valeurs relatives peuvent être plus informatives que les valeurs absolues)
- Met en évidence les changements les plus significatifs
- Permet une interprétation plus intuitive des projections climatiques futures

## Remarques techniques

- Les valeurs normalisées sont exprimées en pourcentage (%)
- Une palette de couleurs divergente est utilisée pour les données normalisées (bleu pour les diminutions, rouge pour les augmentations)
- Pour les valeurs de référence (REF) proches de zéro, la normalisation peut donner des résultats extrêmes. Dans ce cas, les valeurs sont considérées comme non disponibles (NA) 