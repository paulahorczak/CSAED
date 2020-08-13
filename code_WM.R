##### Case studies in the analysis of experimental data #####
##### Replication of MPlus analyses using lavaan ( https://doi.org/10.3758/s13423-020-01737-4 )#####
##### May - August, 2020; Paula Horczak #####

### Load packages
library(readr)
library(lavaan)
library(semTools)
library(corrplot)

### Load data
Reading_Header <- read_csv("Desktop/Case studies/WMC and MW/Reading_Header.csv")
data<-Reading_Header


### Handle misssing value (code = 999999)
data[data == 999999] <- as.numeric(NA)


### Creating correlation matrix for manifest variables (Table 4 in the original paper)
cor.mani <- subset(data, select = c("INT1", "INT2", "INT3",
                                    "FAM1", "FAM2","FAM3", 
                                    "OSPAN", "SSPAN",
                                    "VOL", "INV",
                                    "COM1", "COM2", "COM3"))

cor.m <- cor(cor.mani, use = "pairwise.complete.obs", method = "pearson")
print(cor.m, digits = 2)

# Creating b/w plot to describe the correlation matrix
col1 <- colorRampPalette(c( "white", "gold","red"))
corrplot(cor.m, method = "color", outline = TRUE, col = col1(10), tl.col = "black", tl.cex = 1, cl.cex = 1)


### Creating correlation matrix for latent variables (Table 1 in the original paper)
COM<- (data$COM1+data$COM2+data$COM3)
INT<- (data$INT1+data$INT2+data$INT3)
FAM<- (data$FAM1+data$FAM2+data$FAM3)
WMC<- (data$OSPAN+data$SSPAN)

lat.vbs<-data.frame(INT,FAM,WMC,data$VOL,data$INV,COM)


cor.l <- cor(lat.vbs,use="pairwise.complete.obs", method = "pearson") 
print(cor.l, digits = 2)


### Specify the model

model<-'

#latent variables

COM=~COM1+COM2+COM3
INT=~INT1+INT2+INT3
FAM=~FAM1+FAM2+FAM3
WMC=~OSPAN+SSPAN

# regressions

COM ~ a* WMC + INT + FAM + d*VOL + e*INV
VOL ~ f * WMC + g * INT + h * FAM
INV ~ i * WMC + j * INT + k * FAM


#covariances

VOL~~INV
INT~~WMC
INT~~FAM
FAM~~WMC


# indirect effects

WMCVOL:= f*d
WMCINVOL:= i*e

INTVOL:= g*d
INTINVOL:=j*e

FAMVOL:=h*d
FAMINVOL:=k*e

# total indirect effect
#TIE := WMCVOL + WMCINVOL

# total effect WMC on COM
#TE := TIE + a
'

### Fit the model (and make sure that familiarity and interest variables are treated as categorical predictors)
fit <- sem(model, data = data, cluster = 'SUBJECT', ordered = c("FAM1", "FAM2", "FAM3",
                                           "INT1", "INT2", "INT3"), missing = "pairwise")

### Display the results (Tables 2 and 3 in the paper)
summary(fit, fit.measures=TRUE, standardized = TRUE) 