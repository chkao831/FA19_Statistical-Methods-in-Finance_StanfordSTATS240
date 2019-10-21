retdata <-read.table("Desktop/STATS240/d_logret_12stocks.txt", header=TRUE)
attach(retdata)
X <- cbind(aapl,adbe,adp,amd,dell,gtw,hp,ibm,msft,orcl,sunw,yhoo)
summary(X)
cor(X)
cov(X)

par(mfrow=c(2,2))
pca_corr = princomp(X,cor = TRUE)
summary(pca_corr)
plot(pca_corr)
screeplot(pca_corr,type="line",main="Scree Plot CORR")
# Eigenvalues
eig_corr <- (pca_corr$sdev)^2
eig_corr
pca_cov = princomp(X, cor = FALSE)
summary(pca_cov)
plot(pca_cov)
screeplot(pca_cov,type="line",main="Scree Plot COV")
eig_cov <- (pca_cov$sdev)^2
eig_cov


pca_corr$loadings
pca_cov$loadings
