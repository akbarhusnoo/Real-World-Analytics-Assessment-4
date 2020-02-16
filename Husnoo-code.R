################################# CASE STUDY 2 (b) #########################################################

library(lpSolveAPI)
#initialise the cloth factory model with 0 initial constraint and 9 decision variables
clothFactoryModel <- make.lp(0,9) 
#including that the aim is to maximiize the objective function (total profit)
lp.control(clothFactoryModel, sense = "maximize")
#creating the objective function
#Order of decision variables: Xc1, Xc2, Xc3, Xw1, Xw2, Xw3, Xs1, Xs2, Xs3
set.objfn(clothFactoryModel, c(25,21,25,10,6,10,5,1,5))
#addition of demand AND materials constraints
add.constraint(clothFactoryModel, c(1,0,0,1,0,0,1,0,0), "<=", 4200) #XC1 + XW1 + XS1 ≤ 4200 
add.constraint(clothFactoryModel, c(0,1,0,0,1,0,0,1,0), "<=", 3200) #XC2 + XW2 + XS2 ≤ 3200
add.constraint(clothFactoryModel, c(0,0,1,0,0,1,0,0,1), "<=", 3500) #XC3 + XW3 + XS3 ≤ 3500 
add.constraint(clothFactoryModel, c(0.5,0,0,-0.5,0,0,-0.5,0,0), ">=", 0) #0.5XC1 - 0.5XW1 - 0.5XS1 ≥ 0 
add.constraint(clothFactoryModel, c(-0.4,0,0,0.6,0,0,0.4,0,0), ">=", 0) #0.6XW1 – 0.4XC1 – 0.4 XS1 ≥ 0 
add.constraint(clothFactoryModel, c(0,0.4,0,0,-0.6,0,0,-0.6,0), ">=", 0) #0.4XC2 – 0.6XW2 – 0.6XS2 ≥ 0 
add.constraint(clothFactoryModel, c(0,-0.4,0,0,0.6,0,0,-0.4,0), ">=", 0) #0.6XW2 – 0.4XC2 – 0.4 XS2 ≥ 0 
add.constraint(clothFactoryModel, c(0,0,0.6,0,0,-0.4,0,0,-0.4), ">=", 0) #0.6XC3 – 0.4XW3 – 0.4XS3 ≥ 0 
add.constraint(clothFactoryModel, c(0,0,-0.5,0,0,0.5,0,0,-0.5), ">=", 0) #0.5XW3 – 0.5XC3 – 0.5 XS3 ≥ 0 
#addition of implicit constraints
add.constraint(clothFactoryModel, c(1,0,0,0,0,0,0,0,0), ">=", 0) #XC1 ≥ 0 
add.constraint(clothFactoryModel, c(0,1,0,0,0,0,0,0,0), ">=", 0) #XC2 ≥ 0 
add.constraint(clothFactoryModel, c(0,0,1,0,0,0,0,0,0), ">=", 0) #XC3 ≥ 0
add.constraint(clothFactoryModel, c(0,0,0,1,0,0,0,0,0), ">=", 0) #XW1 ≥ 0
add.constraint(clothFactoryModel, c(0,0,0,0,1,0,0,0,0), ">=", 0) #XW2 ≥ 0
add.constraint(clothFactoryModel, c(0,0,0,0,0,1,0,0,0), ">=", 0) #XW3 ≥ 0
add.constraint(clothFactoryModel, c(0,0,0,0,0,0,1,0,0), ">=", 0) #XS1 ≥ 0
add.constraint(clothFactoryModel, c(0,0,0,0,0,0,0,1,0), ">=", 0) #XS2 ≥ 0
add.constraint(clothFactoryModel, c(0,0,0,0,0,0,0,0,1), ">=", 0) #XS3 ≥ 0
all.constraint.names <- c("Demand Constraint for Spring", "Demand Constraint for Autumn", "Demand Constraint for Winter", 
                          "Proportion of cotton in Spring","Proportion of wool in Spring", "Proportion of cotton in Autumn",
                          "Proportion of wool in Autumn", "Proportion of cotton in Winter","Proportion of wool in Winter", "XC1", "XC2",
                          "XC3", "XW1", "XW2", "XW3","XS1", "XS2", "XS3")
decision.variable.names <- c("XC1", "XC2", "XC3", "XW1", "XW2", "XW3","XS1", "XS2", "XS3")
dimnames(clothFactoryModel) <- list(all.constraint.names, decision.variable.names)
solve(clothFactoryModel)
get.objective(clothFactoryModel)
get.variables(clothFactoryModel)
get.constraints(clothFactoryModel) 
clothFactoryModel

################################# CASE STUDY # 3(e) #########################################################

library(lpSolveAPI)
#initialise the cloth factory model with 0 initial constraint and 9 decision variables
bidding.Company.Model <- make.lp(0,6) 
#including that the aim is to maximiize the objective function (total profit)
lp.control(bidding.Company.Model, sense = "maximize")
#creating the objective function
#Order of decision variables: X1, X2, X3, X4, X5, V
set.objfn(bidding.Company.Model, c(0,0,0,0,0,1)) #max V
#addition of constraints
add.constraint(bidding.Company.Model, c(-1,-1,-1,-1,1,1), "<=", 0) #v – X1 – X2 – X3 – X4 + X5 ≤0
add.constraint(bidding.Company.Model, c(1,-1,-1,-1,1,1), "<=", 0) #v + X1 – X2 – X3 – X4 + X5 ≤0
add.constraint(bidding.Company.Model, c(1,1,-1,-1,1,1), "<=", 0) #v + X1 + X2 – X3 – X4 + X5 ≤0
add.constraint(bidding.Company.Model, c(1,1,1,-1,1,1), "<=", 0) #v + X1 + X2 + X3 – X4 + X5 ≤0
add.constraint(bidding.Company.Model, c(1,1,1,1,-1,1), "<=", 0) #v + X1 + X2 + X3 +X4 – X5 ≤0
#addition of implicit constraints
add.constraint(bidding.Company.Model, c(1,1,1,1,1,0), "=", 1) #X1 + X2 + X3 + X4 + X5 = 1
add.constraint(bidding.Company.Model, c(1,0,0,0,0,0), ">=", 0) #X1≥0
add.constraint(bidding.Company.Model, c(0,1,0,0,0,0), ">=", 0) #X2≥0
add.constraint(bidding.Company.Model, c(0,0,1,0,0,0), ">=", 0) #X3≥0
add.constraint(bidding.Company.Model, c(0,0,0,1,0,0), ">=", 0) #X4≥0
add.constraint(bidding.Company.Model, c(0,0,0,0,1,0), ">=", 0) #X5≥0

Row.names <- c("Row 1", "Row 2", "Row 3", "Row 4", "Row 5", "Row 6")
Column.names <- c("X1", "X2", "X3", "X4", "X5", "V")
dimnames(bidding.Company.Model) <- list(Row.names, Column.names)
solve(bidding.Company.Model)
get.objective(bidding.Company.Model)
get.variables(bidding.Company.Model)
get.constraints(bidding.Company.Model) 
bidding.Company.Model

