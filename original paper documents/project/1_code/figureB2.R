###################################################################
## figureB2.R
##
## plot for explaining full matching
## show the same but with weights from full matching, add lines 
## showing sets - for just previous total number of aspirants ==3
###################################################################

rm(list=ls())

load("../2_output/0_results/results.RData") # from analysis.R

x<- result.num_aspirants_total.2016.main

pdf(file="../2_output/1_figs/figB2_matchedsets_example.pdf", height=6, width=10)
par(mar=c(4.25,10,2.8,1))

setlist <- unique(x$dat.weights$em)
setlist <- sort(setlist)[54:72]

symbols(x$dat.weights$propscore[x$dat.weights$ndc==1&x$dat.weights$em %in% setlist], 
        x$dat.weights$ndc[x$dat.weights$ndc==1&x$dat.weights$em %in% setlist], 
        bg="white", xlim=c(0,1), ylim=c(-.5,1.5), yaxt="n", ylab="", xlab="Propensity Score",
        circles=.035*sqrt(x$dat.weights$weight[x$dat.weights$ndc==1&x$dat.weights$em %in% setlist]/pi), 
        inches=FALSE)


for(i in 1:length(setlist)){
  segments(
    x0=rep(x$dat.weights$propscore[x$dat.weights$ndc==1&x$dat.weights$em==setlist[i]], 
           length(x$dat.weights$propscore[x$dat.weights$ndc==0&x$dat.weights$em==setlist[i]])),
    y0=rep(1, length(x$dat.weights$propscore[x$dat.weights$ndc==0&x$dat.weights$em==setlist[i]])),
    x1=x$dat.weights$propscore[x$dat.weights$ndc==0&x$dat.weights$em==setlist[i]],
    y1=rep(0, length(x$dat.weights$propscore[x$dat.weights$ndc==0&x$dat.weights$em==setlist[i]]))
  ) 
}

symbols(x$dat.weights$propscore[x$dat.weights$ndc==1&x$dat.weights$em %in% setlist], 
        x$dat.weights$ndc[x$dat.weights$ndc==1&x$dat.weights$em %in% setlist], 
        bg=adjustcolor("darkgreen", alpha=0.5), add=TRUE, 
        circles=.035*sqrt(x$dat.weights$weight[x$dat.weights$ndc==1&x$dat.weights$em %in% setlist]/pi), 
        inches=FALSE)
symbols(x$dat.weights$propscore[x$dat.weights$ndc==0&x$dat.weights$em %in% setlist], 
        x$dat.weights$ndc[x$dat.weights$ndc==0&x$dat.weights$em %in% setlist], 
        bg=adjustcolor("blue", alpha=0.5), add=TRUE,
        circles=.035*sqrt(x$dat.weights$weight[x$dat.weights$ndc==0&x$dat.weights$em %in% setlist]/pi), 
        inches=FALSE)

axis(2, at=c(0,1), labels=c("Control (NPP)", "Treated (NDC)"), las=1, cex=2)

dev.off()

