# Age-related trends in Niche Position and Specialization in Neotropical Vertebrates

This repository contains **R scripts, functions, and processed datasets** used to reproduce the analyses presented in the manuscript:

> *Age-related trends in Niche Position and Specialization in Neotropical vertebrates*

---

## ⚠️ Computational Requirements

Some scripts are computationally intensive and are intended to run on a **multi-core server environment**. These are explicitly marked with `#server`.

---

## 📁 Repository Structure

The repository is organized into **four analytical modules and one auxiliary functions module**. All scripts generate outputs in:

data/processed/ (which directly contains the input data of the main analyses and the phylogenetic signal final results)

which is subdivided into:

- `weighted_enfa/` → Results from weighted ENFA analyses (VIF < 10)  
- `biomes/` → Results from biome-specific ENFA analyses (VIF < 3)  
- `Sensitivity_analyses/` → Outputs from spatial scale and variable selection tests  

---

## ⚙️ Environment Setup and Dependencies

All analyses rely on a central setup script that loads required libraries, defines directory structure, and sources auxiliary functions.

**Script location:** `results/scripts/source.R`

This script performs the following tasks:

- Loads all required R packages for spatial, phylogenetic, and statistical analyses  
- Defines relative directory paths used throughout the repository  
- Sources all auxiliary functions from `results/scripts/functions/`  
- Sets graphical themes used in figure generation  

The script is intended to be run at the beginning of each session to ensure a consistent computational environment.

### R version

Analyses were developed using:

- **R version:** 4.0.2  

Compatibility with newer R versions is expected but not explicitly tested.

### Required packages

Key packages include:

- Phylogenetics: `ape`, `phytools`, `geiger`, `caper`, `diversitree`, `TreeSim`  
- Spatial analysis: `sf`, `raster`, `rgdal`, `spatstat`  
- Data manipulation: `tidyverse`, `dplyr`, `tidyr`  
- Visualization: `ggplot2`, `cowplot`, `ggpubr`, `ggridges`  
- ENFA analysis: `CENFA`, `usdm`  

> Users should install all dependencies prior to running the workflow.

---

## 📦 Data Sources

Raw input data were obtained from:

- International Union for Conservation of Nature (IUCN Red List) – species distribution data  
- VertLife – tetrapod phylogenies  
- BirdTree – avian phylogenies  

> Due to licensing restrictions, original shapefiles and phylogenetic trees are **not redistributed** in this repository. Users must obtain these data directly from the original sources.

---
## 📁 Data Repository Structure

All processed data used in the analyses are stored in:

`data/processed/`

This directory serves as the main entry point for all datasets included in this repository. It contains both **primary datasets** and **subdirectories with analysis-specific outputs**.

---

### Top-level contents

- `processed_data_dictionary_filled.csv`  
  Data dictionary describing all variables contained in `vert_enfa_ages.csv`.

- `vert_enfa_ages.csv`  
  Species-level dataset (across the four tetrapod groups: amphibians, reptiles, birds, and mammals) including:
  - ENFA metrics (weighted and biome-specific)  
  - Estimated species ages  
  - Corrected species ages under different extinction scenarios  

- `vert_enfa_ages_list.RData`  
  R data object containing a hierarchical list of datasets used in phylogenetic comparative analyses.

  **Structure:**
  - The object is organized by tetrapod group (amphibians, reptiles, birds, mammals).  
  - Each group contains **100 datasets**, each corresponding to a different randomly sampled phylogenetic tree (from VertLife or BirdTree).  
  - Each dataset represents one phylogenetic replicate.

  **Contents of each dataset:**
  - Species identity and taxonomic information  
  - Estimated species ages derived from the corresponding phylogeny  
  - Corrected species ages under different extinction scenarios (low, intermediate, high)  
  - ENFA-derived niche metrics:
    - Marginality and specialization (biome-based ENFA)  
    - Weighted marginality and specialization (weighted ENFA)  

  **Purpose:**
  This object captures phylogenetic uncertainty by storing replicated datasets across 100 alternative phylogenetic trees per tetrapod group. These datasets were used to run PGLS models and to compute summary statistics (e.g., mean, median, confidence intervals) across phylogenetic replicates.

  **Notes:**
  - Each dataset corresponds to an independent phylogenetic hypothesis.  
  - No phylogenetic trees are included; only derived species-level attributes are stored.  

---

### Subdirectories

#### `biomes_enfa/`
Contains ENFA PGLS results calculated separately for each biome (Variance Inflation Factor, VIF < 10).

- PGLS results for each tetrapod group, ENFA metric, and each biome across 100 phylogenetic trees (i.e., alternative species age estimates).
  - `tables/`
    - PGLS summaries for each biome, ENFA metric, and each tetrapod group (mean values across 100 trees). 
- Includes a data dictionary describing all variables.

---

#### `phylogenetic_signal/`
Contains outputs of phylogenetic signal analyses for ENFA metrics.

- Includes statistics such as Pagel’s λ and Blomberg’s K.  
- Contains summary tables and intermediate outputs.  
- Includes a data dictionary describing all variables.

---

#### `sensitivity_analyses/`
Contains results from sensitivity analyses assessing robustness of ENFA metrics.

**Spatial scale analyses:**
- ENFA metrics calculated across the entire Neotropics (i.e., without biome stratification, VIF < 10) from each tetrapod group.  
- Includes species-level ENFA metrics and estimated/corrected species ages across 100 phylogenetic trees.
  - `pgls_10th/`
     - PGLS results and summaries for each tetrapod group and ENFA metric (Neotropical marginality and specialization).

**Bioclimatic variable selection:**
- Contains subdirectories evaluating the effect of variable collinearity thresholds.

  - `biomes_3th/`  
    - Includes an R object with a hierarchical structure analogous to `vert_enfa_ages_list.RData`, but with biome-ENFA metrics calculated with a VIF < 3. 
    - Includes PGLS summary results for tetrapod groups in which biome-based ENFA metrics derived under VIF < 3 and VIF < 10 show low agreement (Pearson correlation < 0.7). 

  - `whole_neotropics_3th/`  
    - ENFA metrics, for each tetrapod group, calculated across the entire Neotropics with a VIF < 3.
    - `pgls_3th/`
       - Includes PGLS summary results for tetrapod groups in which whole-based ENFA metrics derived under VIF < 3 and VIF < 10 show low agreement (Pearson correlation < 0.7). 

- Each subdirectory includes its own data dictionary.

---

#### `weighted_enfa/`
Contains ENFA results where marginality and specialization are weighted by species’ biome occupancy.

- Species-level weighted ENFA metrics (w.marginality and w.specialization with a VIF < 10).  
- PGLS general results and summaries for each tetrapod group and each weighted ENFA metric across 100 phylogenetic trees.  
- Includes a data dictionary describing all variables.

---

### 📑 Data dictionaries and metadata system

This repository uses a **hierarchical and standardized metadata system**:

1. **Dataset-specific dictionaries**  
   Each main directory (e.g., `biomes_enfa/`, `weighted_enfa/`, `sensitivity_analyses/`) includes a data dictionary describing all variables in its datasets.

2. **Standardized variable definitions**  
   All data dictionaries were generated using a standardized internal mapping of variable names to descriptions and units, implemented in:

   `results/scripts/functions/functions.R`

   This mapping ensures consistency across all datasets and includes definitions for:
   - Taxonomic variables (species, genus, family, etc.)  
   - ENFA metrics (marginality, specialization)  
   - Phylogenetic signal metrics (Pagel’s λ, Blomberg’s K)  
   - Regression outputs (estimates, p-values, R²)  
   - Species age estimates (million years, Myr)  
   - Categorical variables (biome, extinction scenario)

As a result, identical column names across different files always share the same meaning, units, and interpretation.

---

### ⚠️ Notes

All datasets included here are **derived products** from ENFA and phylogenetic analyses.

- No original shapefiles or phylogenetic trees are included.  
- Data are provided in a form that prevents reconstruction of restricted original datasets.

---

## 🧭 Analytical Workflow

### 1. Spatial Data Processing and ENFA  
**Folder:** `results/scripts/ENFA`

- `dsm.enfa.function.R` → Prepares biome, climate, and species distribution data  
- `ENFA.function.R` → Performs ENFA analyses  
- `weight.enfa.function.R` → Computes weighted ENFA metrics  
- `execution_enfa.R` → Runs the full ENFA workflow  

---

### 2. Phylogenetic Data Preparation  
**Folder:** `results/scripts/phylo_analyses/phylo_manipulation`

- `multiphylo_data_enfa.R` → Generates 100 phylogenetic trees per tetrapod group  
- `comparative_data.R` → Creates `comparative.data` objects using the `caper` package  

---

### 3. Phylogenetic Comparative Analyses  
**Folder:** `results/scripts/phylo_analyses/pgls`

- `phylo_signal.R` → Estimates phylogenetic signal (`#server`)  
- `anova_kruskal.R` → Kruskal–Wallis tests across tetrapod classes  
- `weighted_pgls.R` → Runs PGLS models on weighted ENFA metrics (`#server`)  
- `plots_weighted.R` → Generates plots for weighted ENFA results  
- `biomes_pgls.R` → Runs biome-specific PGLS models (`#server`)  
- `plots_biomes.R` → Generates biome-level plots  
- `final_tables.R` → Produces summary tables  

---

### 4. Sensitivity Analyses  
**Folder:** `results/scripts/sensitivity_analyses`

#### (i) Spatial Scale  
**Subfolder:** `spatial_scale`

- `whole_neotropic_ENFA.R` → ENFA across the Neotropics (`#server`)  
- `whole_neo_data.R` → Merges ENFA metrics with species age estimates  
- `pgls_sensitivity_whole_neo.R` → Runs PGLS models (`#server`)  
- `whole_neotropical_plots_results.R` → Generates plots  

#### (ii) Bioclimatic Variable Selection  
**Subfolder:** `biovariable_selection`

- `sensitivity_biome_climate_3th.R` → Environmental filtering (VIF < 3)  
- `neotropic_ENFA_3th.R` → ENFA with reduced variables  
- `correlation_neotropica_10th_3th.R` → Correlation analyses  
- `biome_ENFA_3th.R` → Biome ENFA (VIF < 3) (`#server`)  
- `correlation_biome_3th.R` → Correlation of biome ENFA metrics  
- `plots_biomes_3th.R` → Generates plots  

---

### 5. Auxiliary Functions  
**Folder:** `results/scripts/functions`

- `functions.R` → Species age estimation, phylogenetic manipulation, PGLS output extraction, metadata dictionary  
- `corrective_age.R` → Species age correction framework based on extinction scenarios  
  (adapted from: https://github.com/thauffe/SpeciesAge)

---

## 📑 Metadata and Data Dictionary

To facilitate data reuse and interpretation, a **data dictionary file (`...data_dictionary.csv`)** was generated for each main data directory within `data/processed/`.

Each `..._data_dictionary.csv` file provides a structured description of all variables contained in the corresponding datasets, including:

- **dataset** (name of the dataset)
- **Column name** (as it appears in the data files)  
- **Variable description** (biological or statistical meaning)  
- **Units of measurement** (e.g., million years, none, categorical)  
- **Data type** (numeric, categorical, etc.)  
- **Additional notes** (e.g., transformations, model context, or category definitions)  

These metadata files are intended to ensure that all variables can be interpreted by users without prior knowledge of the study.

> Data dictionaries are organized at the folder level (e.g., `weighted_enfa/`, `biomes/`, `Sensitivity_analyses/`) and describe all `.csv` files contained within each directory.

---

## 📬 Contact

**Carlos Calderón del Cid**  
Email: caldecid@gmail.com