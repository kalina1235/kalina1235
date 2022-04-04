#__________Full Factorial Design With All Interactions__________#

n=9  
#number of runs
m=5  
#number of replications
k=3  
#number of factors
p=7  
#number of parameters,  Interactions between factors also count:  A, B, C, AB, BC, AC, ABC

data_new = read.csv("/Users/tech1/Downloads/data_new.csv")  
#reads CSV file

constants = seq(1,1,length.out=9)    
#creates a column of nine 1's for the LSE

data_matrix = as.matrix(cbind(constants, data_new$x1, data_new$x2, data_new$x3, data_new$x1*data_new$x2, data_new$x1*data_new$x3, data_new$x2*data_new$x3, data_new$x1*data_new$x2*data_new$x3,data_new$y1, data_new$y2, data_new$y3, data_new$y4, data_new$y5)) 
#converted the DataFrame into a Matrix to do Matrix Math

X_unrep = cbind(data_matrix[,1], data_matrix[,2], data_matrix[,3], data_matrix[,4], data_matrix[,5], data_matrix[,6], data_matrix[,7], data_matrix[,8]) 
#created X_unrep

X = rbind(X_unrep, X_unrep, X_unrep, X_unrep, X_unrep)

y = as.matrix(c(data_matrix[,9], data_matrix[,10], data_matrix[,11], data_matrix[,12], data_matrix[,13])) 
#combining response values

colnames(y) <- c("response")  
#changed Response Column name

y_bar = sum(y)/(n*m)

Beta_Hat <- solve(t(X)%*%X)%*%(t(X)%*%y)  
#Parameter Estimation (Beta-Hat)

#__________Factorial Effects__________#

eff_A = 2*Beta_Hat[2]
eff_B = 2*Beta_Hat[3]
eff_C = 2*Beta_Hat[4]

INT_AB = 2*Beta_Hat[5]
INT_AC = 2*Beta_Hat[6]
INT_BC = 2*Beta_Hat[7]
INT_ABC = 2*Beta_Hat[8]

x1 = -1:1
x2 = -1:1
x3 = -1:1
optimal_equation = 10000
optimal_x1 = 2
optimal_x2 = 2
optimal_x3 = 2

for(x1 in seq(-1,1,by = .1)){
  for(x2 in seq(-1,1,by = .1)){
    for(x3 in seq(-1,1,by = .1)){
      mod_equation = Beta_Hat[1] + (Beta_Hat[2]*x1) + (Beta_Hat[3]*x2) + (Beta_Hat[4]*x3) + (Beta_Hat[5]*x1*x2) + (Beta_Hat[6]*x1*x3) + (Beta_Hat[7]*x2*x3) + (Beta_Hat[8]*x1*x2*x3)
      if(mod_equation < optimal_equation){
        optimal_equation = mod_equation
        optimal_x1 = x1
        optimal_x2 = x2
        optimal_x3 = x3
      }
    }
  }
}

print(c("x1 is =", optimal_x1, "x2 is =", optimal_x2, "x3 is =", optimal_x3, "predictied scrap is", optimal_equation))
