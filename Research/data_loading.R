# Read the TSV/CSV file for Datasets

# https://www.cbioportal.org/study/clinicalData?id=luad_tcga_pan_can_atlas_2018
luad_data1 <- read_tsv("www/luad_tcga_pan_can_atlas_2018_clinical_data.tsv")
# https://www.cancerimagingarchive.net/collection/nsclc-radiomics/
nsclc_data2 <- read.csv("www/manifest1603198545583/NSCLC-Radiomics-Lung1.clinical-version3-Oct-2019.csv")
# https://www.cbioportal.org/study/clinicalData?id=luad_tcga
luad_data3 <- read_tsv("www/luad_tcga_firehose_legacy_clinical_data.tsv")
# https://www.cbioportal.org/study/clinicalData?id=luad_oncosg_2020
luad_data4 <- read_tsv("www/luad_oncosg_2020_clinical_data.tsv")

datasets_clinical <- list("LUAD TCGA" = luad_data1, "NSCLC Radiomics" = nsclc_data2, "LUAD TCGA Firehose" = luad_data3, "LUAD OncoSG" = luad_data4)

# Datasets of images
datasets <- list(
  "NSCLC LUNG1-001" = "www/0.000000-NA-20785",
  "NSCLC LUNG1-002" = "www/0.000000-NA-82046",
  "NSCLC LUNG1-003" = "www/1.000000-NA-28595",
  "NSCLC LUNG1-004" = "www/1.000000-NA-61228")