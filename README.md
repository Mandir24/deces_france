# Analyse des décès en France (2019-2022)

## 📝 Présentation du projet
Ce projet réalise une **analyse exploratoire de données (EDA)** portant sur la mortalité en France sur une période de quatre ans. L'étude traite un volume de **2 621 178 observations** pour dégager des tendances nationales et une analyse détaillée de la région **Normandie**.

## 👥 Auteurs
* **Nathan Boucher**
* **Mandir Diop**
* **Maxime Gamondele**
* *Établissement :* IUT Grand Ouest Normandie, Campus de Lisieux (BUT Science des Données)
* *Année universitaire :* 2023-2024

## 📊 Contenu de l'Analyse

### 1. Échelle Nationale
* **Répartition par sexe :** Étude montrant une répartition stable d'environ 50% d'hommes et 50% de femmes sur toute la période.
* **Répartition géographique :** Classement des régions par nombre de décès (Île-de-France en tête, Corse en fin de liste).
* **Analyse des âges :** Mise en évidence d'un âge moyen de décès à 80 ans et d'une médiane à 85 ans.

### 2. Focus Normandie
* **Analyse départementale :** Comparaison des décès entre la Seine-Maritime, le Calvados, la Manche, l'Eure et l'Orne.
* **Longévité :** Comparaison par sexe confirmant que les femmes vivent en moyenne plus longtemps que les hommes dans tous les départements normands.

## 🛠️ Méthodologie et Variables
Le projet s'appuie sur des fichiers de données à largeur fixe comprenant les informations suivantes :
* Nom, prénoms et sexe de l'individu.
* Dates et lieux de naissance.
* Dates et lieux de décès.

### Outils Statistiques
* **V de Cramer :** Utilisé pour mesurer l'association entre variables qualitatives (sexe, année, région).
* **Rapport de corrélation :** Appliqué pour analyser l'influence des régions sur l'âge de décès.
* **Visualisations :** Utilisation de diagrammes à barres, histogrammes et boxplots (boîtes à moustaches).

## 📈 Résultats Clés
* L'association entre le sexe et l'année de décès est quasi nulle (0,56%).
* La région de résidence n'influence pas significativement l'âge de décès (association de 0,57%).
* En Normandie, le classement des départements par nombre de décès suit globalement leur poids démographique.