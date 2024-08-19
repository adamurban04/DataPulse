# Research: Lung adenocarcinoma (LUAD)

<br>

## Abstract

This report outlines the development and implementation of a research project aimed at identifying novel biomarkers for LUAD. The project utilized a comprehensive data analysis approach, integrating clinical, genomic, and imaging data. This study was conducted in collaboration by the researchers Adam Urban and James Conolly as part of the UPSTaRT (S&E Undergraduate Programme of Summer Training and Research Track) initiative at UL (University of Limerick), under the supervision of Prof. Conor Ryan and Dr. Meghana Kshiragar.

## Introduction

Each year, an estimated 42,000 people in Ireland are diagnosed with cancer, including both 
invasive and non-invasive tumors [3]. Recent research indicates over 24,000 invasive cases 
annually, with 13,075 in men and 11,349 in women were reported during 2019-2021 [1, 3], among 
which lung cancer was the third most common cancer in males and the second most common in 
females [3].

Lung cancer, in particular, is the most deadly in Ireland, making it the leading cause of cancer
fatalities for both sexes [3]. The future presents an even more alarming picture, with 
projections indicating a rapid increase in lung cancer incidence, especially among women, with 
rates expected to rise by 105% between 2015 and 2045, by 131% in men, and by 119% combined [2].

In light of these statistics, our research focuses on Lung adenocarcinoma (LUAD), the most 
prevalent type of primary lung cancer within the non-small cell lung cancer (NSCLC) category 
[4]. This choice is informed by the dataset’s popularity and distinctive characteristics.

Our goal is to offer meaningful contributions to the global effort against lung cancer, 
enhancing our collective understanding and potentially leading to more effective treatment 
strategies.

## Methods

### Research Motivation and Objective

The primary objective of this research was to develop a tool capable of identifying data biases and providing summary statistics across various data modalities, including clinical, genomic, and imaging datasets [5]. This tool is designed to enhance the understanding of LUAD and contribute to the broader effort to combat lung cancer by identifying novel biomarkers.

### Development Process

#### Initial Setup and Familiarization

The project began with a comprehensive onboarding process, where the research team familiarized themselves with the necessary tools and technologies, including R, RStudio, and ShinyApp [6]. The team also conducted a thorough review of the relevant literature to understand the biological context of LUAD and the goals of the research.

#### Data Selection and Integration

The research utilized a variety of datasets, including a clinical dataset from cBioPortal and an image dataset containing CT scans of non-small cell lung cancer patients [5]. The data was integrated into an R Shiny App, which was developed to provide an interactive platform for data exploration and analysis.

#### Tool Development and Enhancement

The Shiny App was continuously refined over the course of the project. Key features included:
- Interactive Plots: Developed using the Plotly package to visualize clinical data and identify potential relationships [7].
- Correlation Matrix: Implemented for numeric data in clinical datasets to facilitate the identification of correlations [8].
- DICOM Viewer: Integrated for the visualization of CT scans, enhancing the tool’s capability to analyze imaging data.
- Stratified Analysis: Enabled using the table1 R package, allowing users to perform stratified analyses of clinical data based on selected variables [9].

### Advanced Analytical Techniques

In the later stages of the project, the research team explored advanced analysis techniques using the RadioGx and PharmacoGx R Bioconductor packages [10, 11]. These tools were used to conduct radiogenomic and pharmacogenomic analyses, providing insights into cell line responses to radiation and drug treatments. This analysis is critical for identifying potential biomarkers and informing treatment strategies for LUAD.

## Results

The research culminated in the development of a comprehensive Shiny App that provides a robust platform for the exploration of LUAD data. The app’s functionalities include:
- Interactive Visualization: Users can explore clinical datasets through interactive plots, allowing for a detailed analysis of demographic and clinical variables.
- Correlation and Statistical Analysis: The app features a correlation matrix and T-test functionalities, enabling users to identify relationships between variables and assess drug sensitivity.
- Radiogenomic and Pharmacogenomic Analysis: The app supports the analysis of cell line responses to radiation and drugs, offering valuable insights into potential biomarkers and treatment strategies.

## Discussion

The development of this Shiny App represents a significant contribution to the field of lung cancer research, particularly in the context of LUAD. The integration of clinical, genomic, and imaging data into a single platform provides a comprehensive tool for researchers to explore potential biomarkers and identify data biases. The inclusion of advanced analytical techniques further enhances the app’s utility, offering new avenues for the study of LUAD.

## Conclusion

This research has successfully developed a tool that integrates multiple data modalities and offers a range of analytical functionalities. The Shiny App is a valuable resource for lung cancer research, providing insights that could lead to the identification of novel biomarkers and more effective treatment strategies.

## References

[1] Cancer statistics — cancer.ie. https://www.cancer.ie/ cancer-information-and-support/cancer-
information/ about-cancer/cancer-statistics. [Accessed 28-05-2024].

[2] cancer.ie. https://www.cancer.ie/sites/default/files/2020-02/
Irish%20Cancer%20Society%20Lung%20Action%20Plan%202019.pdf. [Ac- cessed 28-05-2024]. 

[3] ncri.ie. https://www.ncri.ie/sites/ncri/files/pubs/NCRI\ _AnnualStatisticalReport\_2023.pdf.
[Accessed 28-05-2024]. 

[4] XiaoCong Wang, YanMei Li, HuiHua Hu, FangZheng Zhou, Jie Chen, and DongSheng Zhang.
Comprehensive analysis of gene expression and dna methylation data identifies potential
biomarkers and functional epige- netic modules for lung adenocarcinoma. Genetics and Molecular
Biology, 43:e20190164, 2020.

[5] Datasets Used:

1. LUAD clinical dataset. https://www.cbioportal.org/study/clinicalData?id=luad_tcga_pan_can_atlas_2018
2. NSCLC clinical and image dataset. https://www.cancerimagingarchive.net/collection/nsclc-radiomics/
3. LUAD clinical dataset. https://www.cbioportal.org/study/clinicalData?id=luad_tcga
4. LUAD clinical dataset. https://www.cbioportal.org/study/clinicalData?id=luad_oncosg_2020
5. Drug data. https://www.cancerrxgene.org/downloads/drug_data?pathway=All&tissue=LUAD
6. Drug data. https://www.cancerrxgene.org/downloads/drug_data?screening_set=GDSC1&tissue=LUAD

[6] Shiny package. https://cran.r-project.org/web/packages/shiny/index.html

[7] Plotly package. https://cran.r-project.org/web/packages/plotly/index.html

[8] Corrplot package. https://cran.r-project.org/web/packages/corrplot/vignettes/corrplot-intro.html

[9] Table1 package. https://cran.r-project.org/web/packages/table1/vignettes/table1-examples.html

[10] RadioGx package. https://www.bioconductor.org/packages/release/bioc/html/RadioGx.html

[11] PharmacoGx package. https://www.bioconductor.org/packages/release/bioc/html/PharmacoGx.html
