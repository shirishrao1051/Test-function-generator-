# Test-function-generator-
The code generates test functions including categorical variables with interaction effects. 

This function generator is motivated by complex engineering applications where there is a mix of continuous and categorical features involved, and the response has a non-linear relation with the features. The description of the different parameters of the function is given below. <br />

Non Linearity : Based on the common test functions used for optimization in the literature (https://www.sfu.ca/~ssurjano/optimization.html) , the common structures observed are <br />
1) polynomials up to 4th order <br /> 2) trignometric functions (sin and cos) <br />   3) log, exp, and logistic functions <br />
This function creates "families" of non linear functions. The non linear family consists fuctions for <br />
NL1 - Only polynomial terms (will create a function with only polynomial terms)
NL2 - Polynomial and Trignometric terms
NL3 - Mix of polynomial,logistic ,log and exp terms. <br />

The various arguments for the functions are described below <br />

dim : The dimension of the dataset. Currently the only possible value the function takes are 12,36 and 60 <br />
split : Split ration between continuos and categorical features in the problem.
       1 = 25% continuous and 75% categorical.
       2 = equal number of continuous and categorical features.
       3 = 75% continuous and 25% categorical <br />
imp : Proportion of true features in the problem. Possible values are 0.25, 0.5 and 0.75 <br />
resptype : Defines the structure of true model. <br />
          1 - Purely additive model <br />
          2 - Interactions between continuous features. <br />
          3 - Interactions between continuous and categorical features. <br />
          4 - Interactions between categorical features. <br />





