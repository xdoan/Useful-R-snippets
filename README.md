# Useful-R-snippets
R functions and commands I want to reuse and keep track of. 

# Functions.R
Helpful when working with human WGS data that you want to analyze by chromosome
1) ggplot2 bargraph ordered by chromosome with the UCSC "chr1...chrY" naming system. Not alphanumeric ordering. Could probably use mixedsort() but this allows me to control the order of things (like if I wanted genome first, instead of last). 
2) paired bargraph of function #1 for paired samples, e.g. Tumor/Normal, treatment/no-treatment. 
3) normalize count data by average coverage per chromosome 
