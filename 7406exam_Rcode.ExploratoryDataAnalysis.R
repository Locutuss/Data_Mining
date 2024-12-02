##### The following R codes might be useful
##### when you conduct this take-home exam of ISyE 7406
#####
### Read Training Data
## Assume you save the training data in the folder "C:/temp" in your local laptop
traindata <- read.table(file = "C:/temp/7406train.csv", sep=",");
dim(traindata);
## dim=10000*202
## The first two columns are X1 and X2 values, and the last 200 columns are the Y valus

### Some example plots for exploratory data analysis
### please feel free to add more exploratory analysis
X1 <- traindata[,1];
X2 <- traindata[,2];

## note that muhat = E(Y) and Vhat = Var(Y)
muhat <- apply(traindata[,3:202], 1, mean);
Vhat  <- apply(traindata[,3:202], 1, var);

## You can construct a dataframe in R that includes all crucial
##    information for our exam
data0 = data.frame(X1 = X1, X2=X2, muhat = muhat, Vhat = Vhat);

## we can plot 4 graphs in a single plot
par(mfrow = c(2, 2));
plot(X1, muhat);
plot(X2, muhat);
plot(X1, Vhat);
plot(X2, Vhat);


## Or you can first create an initial plot of one line
##         and then iteratively add the lines
##
##   below is an example to plot X1 vs. muhat for different X2 values
##
## let us reset the plot
dev.off()
##
## now plot the lines one by one for each fixed X2
##
flag <- which(data0$X2 == 0);
plot(data0[flag,1], data0[flag, 3], type="l",
     xlim=range(data0$X1), ylim=range(data0$muhat), xlab="X1", ylab="muhat");
for (j in 1:99){
  flag <- which(data0$X2 == 0.01*j);
  lines(data0[flag,1], data0[flag, 3]);
}

## You can also plot figures for each fixed X1 or for Vhat

### Then you need to build two models:
##  (1) predict muhat from X1 and X2
##  (2) predict Vhat from X1 and X2.

## Testing Data: first read testing X variables
testX  <- read.table(file = "C:/temp/7406test.csv", sep=",");
dim(testX)
## This should be a 2500*2 matrix

## Next, based on your models, you predict muhat and Vhat for (X1, X2) in textX.
## Suppose that will lead you to have a new data.frame
##   "testdata" with 4 columns, "X1", "X2", "muhat", "Vhat"

## Then you can write the object "testdata" in the csv file as follows:
## Then you can upload the .csv file to the Canvas
## (please use your own Last Name and First Name)
## 
write.table(testdata, file="C:/temp/1.LastName.FirstName.csv",
            sep=",",  col.names=F, row.names=F)

## Note that in your final answers, you essentially add two columns for your estimation of
##     $mu(X1,X2)=E(Y)$ and $V(X1, X2)=Var(Y)$
##  to the testing  X data file "7406test.csv".
##
## Please double check whether your .csv file has 2500 rows and 4 columns or not,
##    whether it has "NA" or other unreadable values. These are typical small  
##    mistakes that will severely affect your grade! 


##### In the auto-grading, we run loops, one loop for each student
#####  In each loop, we first generate the filename as name1 = "1.LastName.FirstName.csv",
#####  Next, we compare your answers with those Monte Carlo based values,
#####     "muhatestMC" and "VhatestMC", which was computed as in the training data
#####  Also if somehow the auto-grading program failed (e.g., 
#####   due to inconsistent file names), we will manually compute your prediction, 
#####   as we want to make sure to have a fair grading to everyone.
#####

rest01 <- read.table(file = name1, sep=",");
muhatmp  <- round(rest01[,3], 6);  ## Your predicted values for \mu in 6 digits
Vhatmp   <- round(rest01[,4],6);   ## Your predicted value of Vhat in 6 digits
MSEmu   <-  mean((muhatestMC - muhatmp)^2);
MSEV   <-   mean((VhatestMC - Vhatmp)^2);
##### Your technical scores will be based on MSEmu and MSEV values
##### In general, the smaller MSEs, the better.
##### However, there is no universal answer on how small is small.