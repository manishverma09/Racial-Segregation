# racial-segregation
Some R code I used to estimate racial segregation, essentially Getis-Ord G*, for pblic health research. The orignial function is from the spdep package. I modified it a bit to match Kershaw et al. (2017; JAMA Intern Med). You have to be careful before you use ratios that are not additive with Getis-Ord G*. See my comments in the code. You can use the code to examine any census variable such as income segregation, where it will be more appropriate since incomes are additive.  

The two functions with 'gstar' in their name are the top level functions and call the other two functions. Make sure you load the required libraries. 
