library(readr)
install.packages("corrplot")
library(corrplot)
options(scipen = 999)
#GDP PER CAPITA (2023 USD)
USJAPANGERMANYGDPPERCAP = read_csv("C:/Users/fishe/OneDrive/Desktop/MACRO THEORY/GDPPerCap.csv")

#Population
USJAPANGERMANYPOP = read_csv("C:/Users/fishe/OneDrive/Desktop/MACRO THEORY/Population.csv")

#Net Foreign Direct Investment
USJAPANGERFORDIRINV = read_csv("C:/Users/fishe/OneDrive/Desktop/MACRO THEORY/FORDIRINV.csv")

#Fixed Capital Formation
USJAPANGERFIXEDCAPFORM = read_csv("C:/Users/fishe/OneDrive/Desktop/MACRO THEORY/GrossFixedCapitalFormation.csv")

#Yearly Average Interest Rates
USJAPANGERITRRATES = read_csv("C:/Users/fishe/OneDrive/Desktop/MACRO THEORY/USGERJAPITRRATES.csv")

#Total Factor Productivity
USJAPANGERTFP = read_csv("C:/Users/fishe/OneDrive/Desktop/MACRO THEORY/USGERJAPTFP.csv")

MergedData=merge(USJAPANGERMANYGDPPERCAP, USJAPANGERMANYPOP, by = "Year")
colnames(MergedData)[colnames(MergedData)=="United States.x"]="USGDPPerCap"
colnames(MergedData)[colnames(MergedData)=="United States.y"]="USPop"
colnames(MergedData)[colnames(MergedData)=="Germany.x"]="GERGDPPerCap"
colnames(MergedData)[colnames(MergedData)=="Germany.y"]="GERPop"
colnames(MergedData)[colnames(MergedData)=="Japan.x"]="JAPGDPPerCap"
colnames(MergedData)[colnames(MergedData)=="Japan.y"]="JAPPop"

MergedData1=merge(MergedData,USJAPANGERFORDIRINV, by = "Year")
colnames(MergedData1)[colnames(MergedData1)=="United States"]="USFORDIRINV"
colnames(MergedData1)[colnames(MergedData1)=="Germany"]="GERFORDIRINV"
colnames(MergedData1)[colnames(MergedData1)=="Japan"]="JAPFORDIRINV"

MergedData2=merge(MergedData1,USJAPANGERFIXEDCAPFORM, by = "Year")
colnames(MergedData2)[colnames(MergedData2)=="United States"]="USFIXEDCAPFORM"
colnames(MergedData2)[colnames(MergedData2)=="Germany"]="GERFIXEDCAPFORM"
colnames(MergedData2)[colnames(MergedData2)=="Japan"]="JAPFIXEDCAPFORM"

MergedData3=merge(MergedData2,USJAPANGERITRRATES, by = "Year")

#Final Dataset 
USGERMANYJAPANDATA1 = merge(MergedData3, USJAPANGERTFP, by = "Year")

#Annual GDP Per Capita Growth Rate Calculations
USGDPPC_yearlychange = diff(USGERMANYJAPANDATA$USGDPPerCap)
USGDPPCGR = (USGDPPC_yearlychange / USGERMANYJAPANDATA$USGDPPerCap[-length(USGERMANYJAPANDATA$USGDPPerCap)]) * 100
USGDPPCGR

GERGDPPC_yearlychange = diff(USGERMANYJAPANDATA$GERGDPPerCap)
GERGDPPCGR = (GERGDPPC_yearlychange / USGERMANYJAPANDATA$GERGDPPerCap[-length(USGERMANYJAPANDATA$GERGDPPerCap)]) * 100
GERGDPPCGR

JAPGDPPC_yearlychange = diff(USGERMANYJAPANDATA$JAPGDPPerCap)
JAPGDPPCGR = (JAPGDPPC_yearlychange / USGERMANYJAPANDATA$JAPGDPPerCap[-length(USGERMANYJAPANDATA$JAPGDPPerCap)]) * 100
JAPGDPPCGR

# Adding a "0" to each growth rate vector because there is no growth rate for year 1 (1970)
USGDPPCGR=c(0,USGDPPCGR)
JAPGDPPCGR=c(0,JAPGDPPCGR)
GERGDPPCGR=c(0,GERGDPPCGR)

USGERMANYJAPANDATA = cbind(USGERMANYJAPANDATA1, USGDPPCGR, JAPGDPPCGR, GERGDPPCGR)

USGERMANYJAPANGROWTHDATA = subset(USGERMANYJAPANDATA, select = c(Year, USGDPPCGR, JAPGDPPCGR, GERGDPPCGR))

Germany20042023growth = ((GERGDPPerCap[54]-GERGDPPerCap[35])/GERGDPPerCap[35])*100
AVGGERgrowth20042023 = exp((log(1.572)/19))

US20042023growth = ((USGDPPerCap[54]-USGDPPerCap[35])/USGDPPerCap[35])*100
AVGUSgrowth20042023 = exp((log(1.984)/19))

JAP19821995growth = ((JAPGDPPerCap[26]-JAPGDPPerCap[13])/JAPGDPPerCap[13])*100
JAPCAGRgrowth19821995 = exp((log(3.519)/13))

#Standard Deviations of Growth Rate
sd(USGDPPCGR)
sd(GERGDPPCGR)
sd(JAPGDPPCGR)

#Germany Average Annual GDP Per Capita Growth 1970-1995
AVGGERgrowth19701995 = ((GERGDPPerCap[26]/GERGDPPerCap[1])^(1/25)) - 1

#Germany Average Annual GDP Per Capita Decline 1995-2003
Germany19952002decline = ((GERGDPPerCap[33]/GERGDPPerCap[26]))^(1/7) - 1

#Germany Average Annual GDP Per Capita Growth 2003-2023
AVGGERgrowth20022023 = ((GERGDPPerCap[54]/GERGDPPerCap[33])^(1/21)) - 1

#Plot of German Foreign Direct Investment
GERFORDIRINVtenmillions = GERFORDIRINV/10000000

plot(Year, GERFORDIRINVtenmillions, type="l", col="red", lwd=3, main="German Net Foreign Direct Investment, Ten Millions 2023 USD")
abline(v=1995, col="black", lwd=3)
abline(v=2002, col="black", lwd=3)
abline(h=0, col="forestgreen", lwd=3)

#Creation of Correlation Matrices

USsubset = USGERMANYJAPANDATA[, c("USGDPPCGR","USPop","USFORDIRINV","USFIXEDCAPFORM","USTFP","USAverageItrRate")]
cor_matrix_us = cor(USsubset, use = "pairwise.complete.obs")

corrplot(cor_matrix_us, method = "circle")

JAPsubset = USGERMANYJAPANDATA[, c("JAPGDPPCGR","JAPPop","JAPFORDIRINV","JAPFIXEDCAPFORM","JAPTFP","JAPAverageItrRate")]
cor_matrix_jap = cor(JAPsubset, use = "pairwise.complete.obs")

corrplot(cor_matrix_jap, method = "circle")

GERsubset = USGERMANYJAPANDATA[, c("GERGDPPCGR","GERPop","GERFORDIRINV","GERFIXEDCAPFORM","GERTFP","GERAverageItrRate")]
cor_matrix_ger = cor(GERsubset, use = "pairwise.complete.obs")

corrplot(cor_matrix_ger, method = "circle")
 