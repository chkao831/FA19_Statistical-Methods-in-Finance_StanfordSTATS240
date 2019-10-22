##################################################
## STATS 240 HW1 Autumn19-20
## Date: Oct 21
## Author: Chih-Hsuan (Carolyn) Kao
##################################################

# 2.3 PCA

#retdata contains the daily log returns of 12 stocks over time
retdata <-read.table("Desktop/STATS240/d_logret_12stocks.txt", header=TRUE)
attach(retdata)
X <- cbind(aapl,adbe,adp,amd,dell,gtw,hp,ibm,msft,orcl,sunw,yhoo)
summary(X)
cor(X)
cov(X)

#plot formatting 2 by 2
par(mfrow=c(2,2))

# 2.3 (b) Correlation
#this performs a principle components analysis on X
#calculation uses the correlation matrix
pca_corr = princomp(X,cor = TRUE)
summary(pca_corr)
plot(pca_corr)
screeplot(pca_corr,type="line",main="Scree Plot CORR")
# Eigenvalues using correlation matrix
eig_corr <- (pca_corr$sdev)^2
cat(eig_corr)

# 2.3 (a) Covariance
#this performs a principle components analysis on X
#calculation uses the covariance matrix
pca_cov = princomp(X, cor = FALSE)
summary(pca_cov)
plot(pca_cov)
screeplot(pca_cov,type="line",main="Scree Plot COV")
eig_cov <- (pca_cov$sdev)^2
cat(eig_cov)

#factor loadings
pca_corr$loadings
pca_cov$loadings

