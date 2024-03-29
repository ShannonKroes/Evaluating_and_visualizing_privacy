# Evaluating_and_visualizing_privacy
This repository contains the R code for our tool to evaluate and visualize privacy as presented in 'Evaluating privacy of individuals in medical data' by S.K.S. Kroes, M.P. Janssen, R.H.H. Groenwold and M. van Leeuwen. 

Basisfunctions.R in the 'Manuscript' folder contains the functions needed to compute and visualize the privacy measures in the paper. For users who want to evaluate and visualize privacy of data containing identifiable individuals, we recommend using the functions in this file on their own device. 

Additionally, the 'Manuscript' folder also contains all the data used in the article, as well as Produce_figures_manuscript.R, which contains the R code used to perform the analyses and generate the figures in the article. 

App.R in the 'App' folder is the code for the online tool we created: https://skskroes.shinyapps.io/Evaluating_and_visualizing_privacy/. When using this tool, the uploaded data will be processed on the R shiny server. A csv file can be uploaded, where the first row contains the variable names and the format is as stated in the paper. Note that computation time increases factorially with the number of variables and evaluating privacy for all combinations of auxiliary information could take days for data sets with 10 variables. In that case, it may be more suited to run the algorithm for maximum auxiliary information only. Additionally, if the data consists of many cells, it may not be possible to generate a picture that depicts a color for every cell. In that case, we recommend using the smooth_heatmap function in the Basis_functions.R file. 

The R packages utils, gplots, and RColorBrewer are required and the app requires shiny.

For any questions, please send an email to S.kroes@sanquin.nl. 

