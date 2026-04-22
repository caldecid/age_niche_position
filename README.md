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