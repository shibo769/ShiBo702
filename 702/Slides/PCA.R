# PCA for treasury yield

data<-read.csv("/Users/haoxing/Documents/Work/Teaching/BU MF728/tyc_06_19.csv")
head(data)
tail(data)
data <- as.matrix(data[,-1]) # remove date
head(data)
X <- diff(data) # calculate increments
head(X)
dim(X)
S<-cov(X) # covariance matrix

# Calculate principal components
# princomp() returns a list including loadings
out <- princomp(X)
summary(out)
# See the loadings
out$loadings

# Check that sum of variances of PCs equals sum of variances of original data
PC.variances <- out$sdev^2
sum.PCvariances <- sum(PC.variances)
sum.PCvariances
traceS <- sum(diag(S))
traceS
# Cumulative contribution to total variance
cumsum(PC.variances)/sum(PC.variances)

# Plot variances
plot(out)

# loading of the first 4 principal components
par(mfrow=c(2,2))
plot(out$loadings[,1], ylim=c(-0.7, 0.7))
plot(out$loadings[,2], ylim=c(-0.7, 0.7))
plot(out$loadings[,3], ylim=c(-0.7, 0.7))
plot(out$loadings[,4], ylim=c(-0.7, 0.7))
