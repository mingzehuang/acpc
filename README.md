*Github Account:*
<a href="https://github.com/mingzehuang" class="uri">https://github.com/mingzehuang</a>

acpc: Area Classification on Dimension Reduced Micro-level Census Data
======================================================================

**Acknowledgment:** This package is inspired by several similar
assignments by [Irina Gaynanova](https://irinagain.github.io/) when I’m
taking Statistical Computation at Texas A&M University as a graduate
student.

The R package `acpc` implements robust PCA, then sparse PCA, then
K-means classification for Micro-level census data. The original idea
was inspired by

[Vickers D. and Phil R. (2007). “Creating the UK National Statistics
2001 output area classification”. *J. R. Statist. Soc.
A*](https://rss.onlinelibrary.wiley.com/doi/epdf/10.1111/j.1467-985X.2007.00466.x).

The input data matrix is expected to be n rows, p + 1 columns data
frame, which is commonly used in census data. The first column contains
name IDs for all n observations, the rest p columns are features. The
column names for features are feature IDs. All features are supposed to
be numeric so that robust sparse PCA and K-means can be processed
meaningful.

The package will omit those observations with missing values.

Output list includes the cluster labels for all (non-missing)
observations corresponding to observation names; the scores for
clustering centers; the scores for observations; the loading coeffcients
for corresponding principle components and features.

Installation
------------

    devtools::install_github("mingzehuang/acpc")

Example for function acpc()
---------------------------

    library(acpc)
    # Demographic data for each county in US
    demo_data = readRDS(url(
    "https://shiny.rstudio.com/tutorial/written-tutorial/lesson5/census-app/data/counties.rds",
      "rb"))

    # Set number of clusters
    K = 3

    # Classification outputs
    acpcresult <- acpc(demo_data, K)
    acpcresult$Y # Cluster label for each observation.
    acpcresult$center # Scores for cluster centers.
    acpcresult$U # Scores for observations.
    acpcresult$V # Loadings for corresponding principle components and original features.

Example by default data for function visualization()
====================================================

Don’t run this chunk in Rmd/md file if you don’t like warning. Shinny
App requires continuous serving, you will see warning if you stop
serving.

You should run it in your console.

    # Shinny App visualization (only works for two principle components case!)
    if(exists("data_for_acpc")) {
    rm(data_for_acpc)
    } # Clean your global environment to avoid name conflict!

    # Generate App for default dataset

    visualization()

Use your own data for function visualization()
==============================================

Don’t run this chunk it in Rmd/md file if you don’t like warning. Shinny
App requires continuous serving, you will see warning if you stop
serving.

You should run it in your console.

    # Generate App for your own dataset

    your_own_dataset = readRDS(url(
    "https://shiny.rstudio.com/tutorial/written-tutorial/lesson5/census-app/data/counties.rds",
    "rb")) # Load your own dataset

    data_for_acpc = your_own_dataset # Rename your own dataset as "data_for_acpc"!
    visualzation()
