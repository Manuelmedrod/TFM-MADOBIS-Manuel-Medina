library(tidyverse)
library(FactoMineR)
library(factoextra)

# Load data
df <- read.csv("tabla_PCA.csv", sep = ";")
colnames(df) <- str_replace_all(colnames(df), " ", ".")

# Pivot
mic_wide <- df %>%
  select(Strain, Group, Antibiotic, MIC) %>%
  pivot_wider(names_from = Antibiotic, values_from = MIC, names_prefix = "MIC_")

halo_wide <- df %>%
  select(Strain, Group, Antibiotic, Halo.diameter) %>%
  pivot_wider(names_from = Antibiotic, values_from = Halo.diameter, names_prefix = "Halo_")

combined <- inner_join(mic_wide, halo_wide, by = c("Strain", "Group"))

# Factor Group label
combined$Group <- factor(combined$Group)

# PCA
data_for_pca <- combined %>%
  column_to_rownames("Strain") %>%
  select(-Group)

res.pca <- PCA(data_for_pca, scale.unit = TRUE, graph = FALSE)

# PCA Graphic for first tow components
fviz_pca_ind(res.pca,
             label = "all",                 # Mostrar etiquetas
             geom = "text",                # Solo texto
             habillage = combined$Group,   # Colorear por grupo (debe ser factor)
             palette = c( "hotpink", "darkorchid"),
             repel = TRUE)
