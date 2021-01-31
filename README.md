# Test-function-generator-
The code generates test functions including categorical variables with interaction effects. 

This function generator is motivated by complex engineering applications where there is a mix of continuous and categorical features involved, and the response has a non-linear relation with the features. The description of the different parameters of the function is given below. <br />

Non Linearity : Based on the common test functions used for optimization in the literature (https://www.sfu.ca/~ssurjano/optimization.html) , the common structures observed are <br />
1) polynomials up to 4th order   2) trignometric functions (sin and cos)   3) log, exp, and logistic functions <br />
This function creates "families" of non linear functions. The non linear family consists fuctions for 
NL1 - Only polynomial terms (will create a function with only polynomial terms)
NL2 - Polynomial and Trignometric terms
NL3 - Mix of polynomial,logistic ,log and exp terms.

The various arguments for the functions are described below

dim : The dimension of the dataset. Currently the only possible value the function takes are 12,36 and 60
split : Split ration between continuos and categorical features in the problem. 
       Split =1 25% continuous and 75% categorical
       Split=2  equal number of continuous and categorical features
       Split =3 75% continuous and 25% categorical
imp : Proportion of true features in the problem. Possible values are 0.25, 0.5 and 0.75
resptype : Defines the structure of true model. 
          1 - Purely additive model
          2 - Interactions between continuous features.
          3 - Interactions between continuous and categorical features.
          4 - Interactions between categorical features. 





