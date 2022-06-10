# START ----
.Directory <- ''
.Packages <- 'Pareto, ggplot2, gridExtra'; {
    if(! exists('.oldValues')){
        .oldValues <- mget(setdiff(objects(all.names=TRUE), c('.Directory', '.Packages'))); rm(list=names(.oldValues))
        .oldDirectory <- getwd()
        .oldPackages <- setdiff(.packages(), c('base', 'datasets', 'graphics', 'grDevices', 'methods', 'stats', 'utils'))
        if(length(.oldPackages)) lapply(paste0('package:', .oldPackages), detach, character.only=TRUE, unload=TRUE)
    }
    
    if(exists('.Directory') && .Directory != '') setwd(.Directory) else setwd(dirname(rstudioapi::getSourceEditorContext()$path))
    
    invisible(lapply(.Packages <- strsplit(.Packages, ', ')[[1]], function(p){if(! is.element(p, rownames(installed.packages()))) install.packages(p); suppressPackageStartupMessages(require(p, quietly=TRUE, character.only=TRUE))}))
}




# COMMANDS ----
#Q.1
## 1 & 2
set.seed(100)
Data <- data.frame(x.n=rnorm(50000),x.p=rPareto(50000,t=1,alpha=2) )
attach(Data)

pBase <- ggplot(data=Data, mapping=aes(x.n))
pHist <- pBase + geom_histogram(bins=20,color="black",fill="pink")
pBox <- pBase + geom_boxplot(fill='pink')

grid.arrange(pHist, pBox)

mean(x.n); sd(x.n)

## Yes bc with rnorm with expect to have mean= 0 and sd=1 and for xn the mean is -0.000208 which is close to 0 and sd = 0.9989 which is close to 1


## 3 Since the variable is not skewed because it is from a standard normal distribution, the sample mean can be used to summarize the variable. (...t-distribution...)

## 4
mean(x.p); sd(x.p)

analyzePareto <- function(n){
    iPrev <- 1
    nUp <- n + 1
    for(i in seq(min(x.p), max(x.p), length.out=nUp)[2:nUp]){
        currData <- dplyr::filter(Data, iPrev <= x.p & x.p <= i)
        pHistP <- ggplot(data=currData,mapping=aes(x=x.p)) + 
            geom_histogram(bins=20,color="black",fill="grey")
        
        pBoxP <- ggplot(data=Data, mapping=aes(x.p)) +
            geom_boxplot()
        
        grid.arrange(pHistP, pBoxP)
        
        iPrev <- i
    }
}

# it is skewed so mean is less informative for x.p

detach(Data)


#Q.2
rm(list=ls())

Data <- read.table("DataAssignment1.txt",header=FALSE, sep=",")
 
# we will remove 1 negative value (i = 416) from the data
logData <- data.frame(V1=log(Data$V1[Data$V1 > 0]))

# Data
pBase <- ggplot(data=Data, mapping=aes(V1))
pHist <- pBase + geom_histogram(bins=20,color="black",fill="grey")
pBox <- pBase + geom_boxplot()

grid.arrange(pHist, pBox)

# logData
pBase <- ggplot(data=logData, mapping=aes(V1))
pHist <- pBase + geom_histogram(bins=20,color="black",fill="grey")
pBox <- pBase + geom_boxplot()

grid.arrange(pHist, pBox)

quantiles <- quantile(logData$V1, seq(0, 1, 0.25))
IQR <- quantiles[4] - quantiles[2]
lower <- quantiles[2] - 1.5*IQR
upper <- quantiles[4] + 1.5*IQR
outliers <- logData$V1[(logData$V1 < lower) | (upper < logData$V1)]

summary(Data)
summary(logData)




# END ----
if(0){
    setwd(.oldDirectory)
    
    if(length(.Packages)) lapply(paste0('package:', .Packages), detach, character.only=TRUE, unload=TRUE)
    
    lapply(.oldPackages, function(p) suppressPackageStartupMessages(require(p, quietly=TRUE, character.only=TRUE)))
    
    for(name in names(.oldValues)) assign(name, .oldValues[[name]]); rm(list=objects(all.names=TRUE))
}
