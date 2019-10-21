install.packages("foreign")
install.packages("ggplot2")
library("ggplot2")
library("foreign")

stock = read.table("Desktop/STATS240/m_logret_10stocks.txt",
               sep="\t",
               header = TRUE,
               fill=FALSE, 
               strip.white=TRUE)

stock <- stock[2:length(stock)] #strip out date
stock <- stock[-c(157,158,159), ] 

sp500ret = read.table("Desktop/STATS240/m_sp500ret_3mtcm.txt",
                   skip = 1,
                   sep="\t",
                   header = TRUE,
                   fill=FALSE, 
                   strip.white=TRUE)
sp500ret$X3mTCM <- sp500ret$X3mTCM*0.01


sp500ret$risk_premium <- sp500ret$sp500 - sp500ret$X3mTCM


length(stock$AAPL)
length(sp500ret$X3mTCM)
par(mfrow=c(1,1))
plot(sp500ret$risk_premium,stock$AAPL-sp500ret$X3mTCM,main="Linear Regression_AAPL",xlab="Market Risk Premium",
     ylab="AAPL Risk Premium")
AAPL_model <- lm(stock$AAPL-sp500ret$X3mTCM~sp500ret$risk_premium)
summary(AAPL_model)
AAPL_alpha_CI_lower <- summary(AAPL_model)$coefficients[1,1] - 2* summary(AAPL_model)$coefficients[1,2]
AAPL_alpha_CI_upper <- summary(AAPL_model)$coefficients[1,1] + 2* summary(AAPL_model)$coefficients[1,2]
AAPL_beta_CI_lower <- summary(AAPL_model)$coefficients[2,1] - 2* summary(AAPL_model)$coefficients[2,2]
AAPL_beta_CI_upper <- summary(AAPL_model)$coefficients[2,1] + 2* summary(AAPL_model)$coefficients[2,2]
AAPL_alpha_CI_lower
AAPL_alpha_CI_upper
AAPL_beta_CI_lower
AAPL_beta_CI_upper
abline(AAPL_model,col=2,lwd=3)

ADBE_model <- lm(stock$ADBE-sp500ret$X3mTCM~sp500ret$risk_premium)
ADBE_alpha_CI_lower <- summary(ADBE_model)$coefficients[1,1] - 2* summary(ADBE_model)$coefficients[1,2]
ADBE_alpha_CI_upper <- summary(ADBE_model)$coefficients[1,1] + 2* summary(ADBE_model)$coefficients[1,2]
ADBE_beta_CI_lower <- summary(ADBE_model)$coefficients[2,1] - 2* summary(ADBE_model)$coefficients[2,2]
ADBE_beta_CI_upper <- summary(ADBE_model)$coefficients[2,1] + 2* summary(ADBE_model)$coefficients[2,2]

ADP_model <- lm(stock$ADP-sp500ret$X3mTCM~sp500ret$risk_premium)
ADP_alpha_CI_lower <- summary(ADP_model)$coefficients[1,1] - 2* summary(ADP_model)$coefficients[1,2]
ADP_alpha_CI_upper <- summary(ADP_model)$coefficients[1,1] + 2* summary(ADP_model)$coefficients[1,2]
ADP_beta_CI_lower <- summary(ADP_model)$coefficients[2,1] - 2* summary(ADP_model)$coefficients[2,2]
ADP_beta_CI_upper <- summary(ADP_model)$coefficients[2,1] + 2* summary(ADP_model)$coefficients[2,2]

AMD_model <- lm(stock$AMD-sp500ret$X3mTCM~sp500ret$risk_premium)
AMD_alpha_CI_lower <- summary(AMD_model)$coefficients[1,1] - 2* summary(AMD_model)$coefficients[1,2]
AMD_alpha_CI_upper <- summary(AMD_model)$coefficients[1,1] + 2* summary(AMD_model)$coefficients[1,2]
AMD_beta_CI_lower <- summary(AMD_model)$coefficients[2,1] - 2* summary(AMD_model)$coefficients[2,2]
AMD_beta_CI_upper <- summary(AMD_model)$coefficients[2,1] + 2* summary(AMD_model)$coefficients[2,2]

DELL_model <- lm(stock$DELL-sp500ret$X3mTCM~sp500ret$risk_premium)
DELL_alpha_CI_lower <- summary(DELL_model)$coefficients[1,1] - 2* summary(DELL_model)$coefficients[1,2]
DELL_alpha_CI_upper <- summary(DELL_model)$coefficients[1,1] + 2* summary(DELL_model)$coefficients[1,2]
DELL_beta_CI_lower <- summary(DELL_model)$coefficients[2,1] - 2* summary(DELL_model)$coefficients[2,2]
DELL_beta_CI_upper <- summary(DELL_model)$coefficients[2,1] + 2* summary(DELL_model)$coefficients[2,2]

GTW_model <- lm(stock$GTW-sp500ret$X3mTCM~sp500ret$risk_premium)
GTW_alpha_CI_lower <- summary(GTW_model)$coefficients[1,1] - 2* summary(GTW_model)$coefficients[1,2]
GTW_alpha_CI_upper <- summary(GTW_model)$coefficients[1,1] + 2* summary(GTW_model)$coefficients[1,2]
GTW_beta_CI_lower <- summary(GTW_model)$coefficients[2,1] - 2* summary(GTW_model)$coefficients[2,2]
GTW_beta_CI_upper <- summary(GTW_model)$coefficients[2,1] + 2* summary(GTW_model)$coefficients[2,2]

HP_model <- lm(stock$HP-sp500ret$X3mTCM~sp500ret$risk_premium)
HP_alpha_CI_lower <- summary(HP_model)$coefficients[1,1] - 2* summary(HP_model)$coefficients[1,2]
HP_alpha_CI_upper <- summary(HP_model)$coefficients[1,1] + 2* summary(HP_model)$coefficients[1,2]
HP_beta_CI_lower <- summary(HP_model)$coefficients[2,1] - 2* summary(HP_model)$coefficients[2,2]
HP_beta_CI_upper <- summary(HP_model)$coefficients[2,1] + 2* summary(HP_model)$coefficients[2,2]

IBM_model <- lm(stock$IBM-sp500ret$X3mTCM~sp500ret$risk_premium)
IBM_alpha_CI_lower <- summary(IBM_model)$coefficients[1,1] - 2* summary(IBM_model)$coefficients[1,2]
IBM_alpha_CI_upper <- summary(IBM_model)$coefficients[1,1] + 2* summary(IBM_model)$coefficients[1,2]
IBM_beta_CI_lower <- summary(IBM_model)$coefficients[2,1] - 2* summary(IBM_model)$coefficients[2,2]
IBM_beta_CI_upper <- summary(IBM_model)$coefficients[2,1] + 2* summary(IBM_model)$coefficients[2,2]

MSFT_model <- lm(stock$MSFT-sp500ret$X3mTCM~sp500ret$risk_premium)
MSFT_alpha_CI_lower <- summary(MSFT_model)$coefficients[1,1] - 2* summary(MSFT_model)$coefficients[1,2]
MSFT_alpha_CI_upper <- summary(MSFT_model)$coefficients[1,1] + 2* summary(MSFT_model)$coefficients[1,2]
MSFT_beta_CI_lower <- summary(MSFT_model)$coefficients[2,1] - 2* summary(MSFT_model)$coefficients[2,2]
MSFT_beta_CI_upper <- summary(MSFT_model)$coefficients[2,1] + 2* summary(MSFT_model)$coefficients[2,2]

ORCL_model <- lm(stock$ORCL-sp500ret$X3mTCM~sp500ret$risk_premium)
ORCL_alpha_CI_lower <- summary(ORCL_model)$coefficients[1,1] - 2* summary(ORCL_model)$coefficients[1,2]
ORCL_alpha_CI_upper <- summary(ORCL_model)$coefficients[1,1] + 2* summary(ORCL_model)$coefficients[1,2]
ORCL_beta_CI_lower <- summary(ORCL_model)$coefficients[2,1] - 2* summary(ORCL_model)$coefficients[2,2]
ORCL_beta_CI_upper <- summary(ORCL_model)$coefficients[2,1] + 2* summary(ORCL_model)$coefficients[2,2]


Rme <- sp500ret$risk_premium #excess of the S&P 500 index

AAPL_Rte <- stock$AAPL-sp500ret$X3mTCM #excess of the stock AAPL
AAPL_mod <- lm(AAPL_Rte ~ I(rownames(stock) < 86)*Rme + Rme)
summary(AAPL_mod)
summary(AAPL_mod)$coefficients[4,1]

ADBE_Rte <- stock$ADBE-sp500ret$X3mTCM 
ADBE_mod <- lm(ADBE_Rte ~ I(rownames(stock) < 86)*Rme + Rme)
summary(ADBE_mod)$coefficients[4,1]

ADP_Rte <- stock$ADP-sp500ret$X3mTCM 
ADP_mod <- lm(ADP_Rte ~ I(rownames(stock) < 86)*Rme + Rme)
summary(ADP_mod)$coefficients[4,1]

AMD_Rte <- stock$AMD-sp500ret$X3mTCM 
AMD_mod <- lm(AMD_Rte ~ I(rownames(stock) < 86)*Rme + Rme)
summary(AMD_mod)$coefficients[4,1]

DELL_Rte <- stock$DELL-sp500ret$X3mTCM 
DELL_mod <- lm(DELL_Rte ~ I(rownames(stock) < 86)*Rme + Rme)
summary(DELL_mod)$coefficients[4,1]

GTW_Rte <- stock$GTW-sp500ret$X3mTCM 
GTW_mod <- lm(GTW_Rte ~ I(rownames(stock) < 86)*Rme + Rme)
summary(GTW_mod)$coefficients[4,1]

HP_Rte <- stock$HP-sp500ret$X3mTCM 
HP_mod <- lm(HP_Rte ~ I(rownames(stock) < 86)*Rme + Rme)
summary(HP_mod)$coefficients[4,1]

IBM_Rte <- stock$IBM-sp500ret$X3mTCM 
IBM_mod <- lm(IBM_Rte ~ I(rownames(stock) < 86)*Rme + Rme)
summary(IBM_mod)$coefficients[4,1]

MSFT_Rte <- stock$MSFT-sp500ret$X3mTCM 
MSFT_mod <- lm(MSFT_Rte ~ I(rownames(stock) < 86)*Rme + Rme)
summary(MSFT_mod)$coefficients[4,1]

ORCL_Rte <- stock$ORCL-sp500ret$X3mTCM 
ORCL_mod <- lm(ORCL_Rte ~ I(rownames(stock) < 86)*Rme + Rme)
summary(ORCL_mod)$coefficients[4,1]

#3.6.g

Rme <- sp500ret$risk_premium #excess of the S&P 500 index
AAPL_Rte <- stock$AAPL-sp500ret$X3mTCM
AAPL_mod <- lm(AAPL_Rte ~ I(rownames(stock) <87 )*Rme + Rme)
x = nrow(summary(AAPL_mod)$coefficients)
x
summary(AAPL_mod)
minimum = 20000000000
minimal_t0 = 200

for (t in 1:156){
  AAPL_mod <- lm(AAPL_Rte ~ I(rownames(stock) < t)*Rme + Rme)
  if(nrow(summary(AAPL_mod)$coefficients) == 4){
    if(!is.null(summary(AAPL_mod)$coefficients[3,1]) && !is.null(summary(AAPL_mod)$coefficients[4,1])){
    beta2 = summary(AAPL_mod)$coefficients[3,1]
    beta1 = summary(AAPL_mod)$coefficients[4,1] - beta2
    }
    RSS = sum(beta1^2,beta2^2,t^2)
    print(RSS)
    
    if(RSS<minimum){
      minimum = RSS
      minimal_t0 = t
    }
  }
}
minimum #RSS over beta1, beta2, t0
minimal_t0
