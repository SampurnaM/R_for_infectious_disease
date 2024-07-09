## Source Material
#https://sbfnk.github.io/modelling/intro_to_r_gk.html
#reading in data from csv files
mydata<- read.table("data.csv",header=TRUE, sep = ",")
mydata
#the data.sv file contains information about flu in Hammersmith hospita;
#using the read.csv function
flu_hh<-read.csv("data.csv")
flu_hh
#getting dataframe dimensions
dim(flu_hh)
head(flu_hh)
tail(flu_hh)
#accessing data in dataframes
flu_hh[,1] #first column
#using $and column name
flu_hh$num_inf
#getting column names
colnames(flu_hh)
#time for infection is the first column, so,
times<- flu_hh[,1]
times
#subseeting time for the second value
times[2]
##find the maximum time value
max_time<-max(times)
#now find what was the time in which number of flu cases was maximum
time_flu_max<- 
flu_hh[which(flu_hh[,"num_inf"]==max(flu_hh$num_inf)),"time"]
time_flu_max
###adding a column to dataframes
sum<-sum(flu_hh[,"num_inf"])
sum
#now adding a percentage column to flu numbers
flu_hh$perc <- 100*flu_hh$num_inf/sum
flu_hh$perc
#### Plotting functions
#using the in-built plot functions
plot(flu_hh[,1],flu_hh[,2])
#adding labels
plot(flu_hh[,"time"],flu_hh[,"num_inf"])
#adding lines to the points in the plot
lines(flu_hh[,"time"],flu_hh[,"num_inf"])
##only lines to make a plot
plot(flu_hh[,"time"],flu_hh[,"num_inf"],type ="l")
#changing color of the line to red from black
plot(flu_hh[,"time"],flu_hh[,"num_inf"],type ="l",col ="red")
##adding X and Y labels
plot(flu_hh[,"time"],flu_hh[,"num_inf"],type ="l",
     col = "red",xlab = "Time",ylab = "Number")
##conditionals: in-built functions to compare elements in R
#which gives me index of values that match a condition
which(flu_hh$time>60)
#if want to search for just values across different columns
which(flu_hh>60, arr.ind = TRUE)
#if else conditionals
if(any(flu_hh$num_inf<0)){
  print("Error: negative numbers")
}
#for loops
for (i in 1:10) {
  print(2*i)
}
## writing my own functions
#syntax : name_of_function<-function(inputs){
# code
#}
#### Solving for ordinary differential equations
#installing package
install.packages("deSolve")
#loading package into library
library(deSolve)
#help for ode fuction
?ode
## We are using odes to model SIR outbreak
data_sir <- read.csv("data_sir.csv")
##plotting the SIR outbreak data
dim(data_sir)
names(data_sir)
##plot
plot(data_sir[,1],data_sir[,2])
###Now creating parameters for ODE
init<- c(S=0,I=0,R=0) # initial vector
init
# Now expanding this vector to 100,00 where one is I(infected) and rest are susceptible(S)
populn = 100000
init[1] = 99999
init[2] =1
##getting the time points
times <- seq(0,40,by =1 )
### Now the function for ODE
sir<- function(time,state,parameters) {
  with(as.list(c(state,parameters)),{
    dS = -beta * S * I #change in S
    dI= beta * S * I - gamma * I
    dR = gamma * I
    return(list(c(dS,dI,dR)))
    
    })
}
## writing parameters
parameters <- c(beta = 2/1e+05, gamma = 0.5)
# init
init <- c(S = 1e+05 -1 , I =1 , R =0)
#Now running ODE
out<- ode(y = init, func = sir, times = times, parms = parameters)
head(out)


for(i in 1:populn){
   init[]
}