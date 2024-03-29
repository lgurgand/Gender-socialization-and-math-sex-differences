####################
####IMPUTATION ######
library(tidyverse)
library(mice)
library(miceadds)


### LOAD YOUR DATA HERE ########
data <- read.csv("database.csv")

#we want to do multiple imputation on the control but not on the other variables 

set.seed(123)

## Configure parallelization
## Parallel backend for foreach (also loads foreach and parallel; includes doMC)
library(doParallel)
## Reproducible parallelization
library(doRNG)
## Detect core count
nCores <- min(parallel::detectCores(), 8)
## Used by parallel::mclapply() as default
options(mc.cores = nCores)
## Used by doParallel as default
options(cores = nCores)
## Register doParallel as the parallel backend with foreach
## http://stackoverflow.com/questions/28989855/the-difference-between-domc-and-doparallel-in-r
doParallel::registerDoParallel(cores = nCores)
## Report multicore use
cat("### Using", foreach::getDoParWorkers(), "cores\n")
cat("### Using", foreach::getDoParName(), "as backend\n")

## imputation of only ctrl variables method https://rpubs.com/kaz_yos/mice-exclude ###

## Create a before-MI dataset
df_before <- data

## Extract all variable names in dataset
allVars <- names(df_before)


####### PUT HERE THE NAME OF THE VARIABLES HAVUING MISSING VALUES########
missVars <-c("A3_TOTREVEN", "A5_TOTREVEN", "meduc_5y", "feduc_5y") 

## mice predictorMatrix
## A square matrix of size ncol(data) containing 0/1
## data specifying the set of predictors to be used for each
## target column. Rows correspond to target variables (i.e.
## variables to be imputed), in the sequence as they appear in
## data. A value of '1' means that the column variable is used
## as a predictor for the target variable (in the rows). The
## diagonal of predictorMatrix must be zero. The default for
## predictorMatrix is that all other columns are used as
## predictors (sometimes called massive imputation). Note: For
## two-level imputation codes '2' and '-2' are also allowed.
##
predictorMatrix <- matrix(0, ncol = length(allVars), nrow = length(allVars))
rownames(predictorMatrix) <- allVars
colnames(predictorMatrix) <- allVars


###  Specify Variables informing imputation
## These can be either complete variables or variables with missingness.
## Those with missingness must be imputed.
## Explicitly specify.

###### HERE YOU PUT THE VARIABLES THAT YOU WANT TO USE TO INFORM THE IMPUTATION. USUALLY ITS THE SAME VARIABLES AS THE ONE YOU IMPUTE######
imputerVars <- c("A3_TOTREVEN", "A5_TOTREVEN", "meduc_5y", "feduc_5y") 

## Keep variables that actually exist in dataset
imputerVars <- intersect(unique(imputerVars), allVars)
imputerVars
imputerMatrix <- predictorMatrix
imputerMatrix[,imputerVars] <- 1
imputerMatrix

###  Specify variables with missingness to be imputed
## Could specify additional variables that are imputed,
## but does not inform imputation.

###### HERE YOU PUT THE VARIABLES THAT YOU WANT TO IMPUTE. USUALLY ITS THE SAME VARIABLES AS THE ONE YOU USE TO INFORM IMPUTATION ######
imputedOnlyVars <- c( "meduc_5y", "feduc_5y","A3_TOTREVEN", "A5_TOTREVEN")

## Imputers that have missingness must be imputed.
imputedVars <- intersect(unique(c(imputedOnlyVars, imputerVars)), missVars)
imputedVars
imputedMatrix <- predictorMatrix
imputedMatrix[imputedVars,] <- 1
imputedMatrix

###  Construct a full predictor matrix (rows: imputed variables; cols: imputer variables)\n")
## Keep correct imputer-imputed pairs only
predictorMatrix <- imputerMatrix * imputedMatrix
## Diagonals must be zeros (a variable cannot impute itself)
diag(predictorMatrix) <- 0
predictorMatrix


###  Dry-run mice for imputation methods\n")
dryMice <- mice(data = df_before, m = 1, predictorMatrix = predictorMatrix, maxit = 0)
## Update predictor matrix
predictorMatrix <- dryMice$predictorMatrix
cat("###   Imputers (non-zero columns of predictorMatrix)\n")
imputerVars <- colnames(predictorMatrix)[colSums(predictorMatrix) > 0]
imputerVars
cat("###   Imputed (non-zero rows of predictorMatrix)\n")
imputedVars <- rownames(predictorMatrix)[rowSums(predictorMatrix) > 0]
imputedVars
cat("###   Imputers that are complete\n")
setdiff(imputerVars, imputedVars)
cat("###   Imputers with missingness\n")
intersect(imputerVars, imputedVars)
cat("###   Imputed-only variables without being imputers\n")
setdiff(imputedVars, imputerVars)
cat("###   Variables with missingness that are not imputed\n")
setdiff(missVars, imputedVars)
cat("###   Relevant part of predictorMatrix\n")
predictorMatrix[rowSums(predictorMatrix) > 0, colSums(predictorMatrix) > 0]

## Empty imputation method to really exclude variables
## http://www.stefvanbuuren.nl/publications/MICE%20in%20R%20-%20Draft.pdf
##
## MICE will automatically skip imputation of variables that are complete.
## One of the problems in previous versions of MICE was that all incomplete
## data needed to be imputed. In MICE 2.0 it is possible to skip imputation
## of selected incomplete variables by specifying the empty method "".
## This works as long as the incomplete variable that is skipped is not being
## used as a predictor for imputing other variables.
## Note: puttting zeros in the predictorMatrix alone is NOT enough!
##
dryMice$method[setdiff(allVars, imputedVars)] <- ""
cat("###   Methods used for imputation\n")
dryMice$method[sapply(dryMice$method, nchar) > 0]
dryMice$method

cat("
###  Run mice\n")
M <- 20
cat("### Imputing", M, "times\n")

## Set seed for reproducibility
set.seed(3561126)

## Parallelized execution
miceout <- foreach(i = seq_len(M), .combine = ibind) %dorng% {
  cat("### Started iteration", i, "\n")
  miceout <- mice::mice(data = df_before, m = 1, print = TRUE,
                  predictorMatrix = predictorMatrix, method = dryMice$method,
                  MaxNWts = 2000)
  cat("### Completed iteration", i, "\n")
  ## Make sure to return the output
  miceout
}


cat("
###  Show mice results\n")
## mice object ifself
miceout
## Variables that no longer have missingness after imputation
cat("###   Variables actually imputed\n")
actuallyImputedVars <-
  setdiff(names(df_before)[colSums(is.na(df_before)) > 0],
          names(complete(miceout, action = 1))[colSums(is.na(complete(miceout, action = 1))) > 0])
actuallyImputedVars

## Examine discrepancies
cat("###   Variables that were unexpectedly imputed\n")
setdiff(actuallyImputedVars, imputedVars)
cat("###   Variables that were planned for MI but not imputed\n")
setdiff(imputedVars, actuallyImputedVars)

## Still missing variables
cat("###   Variables still having missing values\n")
names(complete(miceout, action = 1))[colSums(is.na(complete(miceout, action = 1))) > 0]

# To save the imputed dataset run the following line : 
#write.mice.imputation(mi.res = miceout, name = "ALL_elfe_ecole_imp", mids2spss=FALSE)

# To load the dataset we do the following
#elfe_mids <- load.Rdata2("ALL_elfe_ecole_imp/ALL_elfe_ecole_imp.Rdata", path=getwd())

