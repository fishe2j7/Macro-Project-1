library(ggplot2)
attach(USGERMANYJAPANDATA)
plot(Year,USGDPPCGR, type="l", col="red", ylim=c(-20,50), lwd=3, ylab = "Growth Rates", main="GDP Per Capita Growth Rates, 1970-2023 (%)")
lines(Year, JAPGDPPCGR, col="blue", lwd=3)
lines(Year, GERGDPPCGR, col="forestgreen", lwd=3)
abline(h = 0, col = "black", lty = 1, lwd = 2)
legend("topleft", legend=c("US GDP Per Capita Growth", "Japan GDP Per Capita Growth", "Germany GDP Per Capita Growth", "Zero Growth"), col=c("red", "blue", "forestgreen", "black"), cex=.66, lty = 1)


plot(Year,USGDPPerCap, type="l", col="red", lwd=3, ylab = "GDP Per Capita", main="GDP Per Capita (2023 USD)", yaxt="n")
lines(Year, JAPGDPPerCap, col="blue", lwd=3)
lines(Year, GERGDPPerCap, col="forestgreen", lwd=3)
legend("topleft", legend=c("US GDP Per Capita", "Japan GDP Per Capita", "Germany GDP Per Capita"), col=c("red", "blue", "forestgreen", "black"), cex=.66, lty = 1)
axis(2, at = c(10000, 20000, 30000, 40000, 50000, 60000, 70000, 80000), labels = c("$10,000", "$20,000", "$30,000", "$40,000", "$50,000", "$60,000", "$70,000", "$80,000"), cex.axis=.7, las=2)
