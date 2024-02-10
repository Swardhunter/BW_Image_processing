# Historical Aerial Images for Land Use Classification in Norway

This README outlines the framework for the semi-automated classification of land use in Norway based on historical aerial imagery. By leveraging advanced textural analysis and modern software tools, this project aims to provide accurate and efficient land-use classification.

## Introduction

Historical Aerial Images are an invaluable asset for constructing historical land use (LU) maps. These maps are instrumental in detecting changes over time and evaluating the impact of human activities on land use. However, the development of thematic LU maps from historical aerial photographs is a complex task. This complexity primarily arises from the limited spectral variability in these images, as they are predominantly monochromatic. Despite this limitation, the high resolution of these images facilitates the differentiation between various land classes based on textural differences.

Norway's "Norge i Bilder" offers a comprehensive database of historical aerial images dating back to 1930, covering a substantial portion of the country. These images are pivotal for analyzing land-use dynamics changes resulting from various activities. With resolutions reaching up to 20 cm per pixel, these images provide a detailed view of different land types.

Leveraging data from Norge I Bilder, we aim to create a semi-automated land-use classification for the Norwegian HP schemes’ areas, based on the available data. Five primary classes have been identified for a Level I Land-Use Classification:

- **Forest Land**: Includes all types of forests and small marshlands.
- **Water**: Encompasses water bodies.
- **Urban**: Comprises roads and built-up areas.
- **Bareland**: Consists of exposed rocks and various barren land areas.
- **Agricultural**: Pertains to cultivated and crop lands.

## Methods

### Using Textural Features:

- **Completed Local Binary Pattern (CLBP)**: As introduced by Guo et al. in 2010, this method is utilized for textural analysis.
- **Grey Level Co-Occurrence Matrix (GLCM)**: Developed by Haralick et al. in 1973, it includes various measures such as Mean, Variance, Inertia (Contrast), Entropy, Correlation, and Cluster Shade.
- **Median Filter**: Applied to reduce noise in the images.
- **Object-Based Image Analysis (OBIA)**: Combines all features where segmentation parameters are set to 100/50, with Shape and Compactness values at 0.1 and 0.8, respectively.

## Tools

- **CLBP**: Implemented using Matlab source code provided by the authors.
- **GLCM**: Utilized through the OTB toolbox for Haralick feature extraction.
- **Images Preprocessing**: Performed using GDAL with Python.
- **Object-Based Classification & Refinement**: Executed in eCognition Software by Trimble.
- **Accuracy Assessment & Visualisation**: Carried out using ArcGIS Pro.

## Results

An overview of the classified areas is presented in the following story map: [View Story Map](https://storymaps.arcgis.com/stories/9b54d4a4e52d4474ac8b71ecce7deecf)
