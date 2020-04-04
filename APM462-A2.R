install.packages("powerplus")
library(powerplus)
row1 <- c(0.8,0.1,0.1,0)
row2 <- c(0.1,0.5,0.2,0.2)
row3 <-c(0.1,0.3,0.3,0.3)
row4 <-c(0,0,0,1)
mat <- rbind(row1,row2,row3,row4)
Matpow(mat,2) # call the function Matpow to calculate power of the matrix,
              # where mat is the matrix P in assginment, 2 is the power we want
              # to calculate
Matpow(mat,1/12) # 1/12 is the power we want to calculate in this case