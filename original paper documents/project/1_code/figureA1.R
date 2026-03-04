##############################################################
## figureA1.R
##
## Figure A1: Ethnic diversity measures by logged constituency
## population density
##############################################################

rm(list=ls())

load("../0_data/prim.dat.Rdata")

d2 <- prim.dat[prim.dat$party=="NPP",]

pdf(file="../2_output/1_figs/figA1_appendix_popdensethfrac.pdf", height=6, width=11)
par(mfrow=c(1,2))
plot(log(d2$popdens), d2$ethfrac_10group, 
     main="Ethnic fractionalization, by population density\nConstituency level", 
     ylab="Ethnic fractionalization", xlab="Ln(Population density (1000s/sq km))", 
     pch=16, cex=0.8, col="dodgerblue2")
abline(lm(d2$ethfrac_10group ~ log(d2$popdens)), lwd=1.5, col="firebrick")

plot(log(prim.dat $popdens), prim.dat $ethfrac_ownparty, 
     main="Ethnic frac. w/in own party, by population density\nConstituency level", 
     ylab="Ethnic frac. w/in own party", xlab="Ln(Population density (1000s/sq km))", 
     pch=16, cex=0.8, col="dodgerblue2")
abline(lm(prim.dat $ethfrac_ownparty ~ log(prim.dat $popdens)), lwd=1.5, col="firebrick")

dev.off()
