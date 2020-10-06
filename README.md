# Evaluating_and_visualizing_privacy
This repository contains the code for our tool to evaluate and visualize privacy as presented in 'Evaluating privacy of individuals in medical data' by S.K.S. Kroes, M.P. Janssen, R.H.H Groenwold and M. van Leeuwen. 

Basisfunctions.R contains the functions needed to compute and visualize the privacy measures in the paper. For users who want to evaluate and visualize privacy of data containing identifiable individuals, we recommend using the functions in this file on their own device. 

App.R is the code for the online tool we created: https://skskroes.shinyapps.io/Evaluating_and_visualizing_privacy/. When using this tool, the uploaded data will be processed on the R shiny server. A csv file can be uploaded, where the first row contains the variable names and the format is as stated in the paper. 

The 'Manuscript' folder contains all the data used in the article, as well as Produce_figures_manuscript.R, which contains the R code used to perform the analyses and generate the figures in the article. 

For any questions, please send an email to S.kroes@sanquin.nl. 

