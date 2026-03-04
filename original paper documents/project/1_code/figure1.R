##############################################################
## Figure1.R
##
## Figure 1: Women aspirants and nominees, by year and party
##############################################################

rm(list=ls())
library(ggplot2)

load("../0_data/prim.dat.Rdata")

df1 <- data.frame(
  year=c(2012,2012,2012,2012,2016,2016,2016,2016),
  party=c("NPP", "NPP", "NDC", "NDC", "NPP", "NPP", "NDC","NDC"),
  gender=c("women", "men", "women", "men", "women", "men", "women", "men"),
  numasp= round(c(
    mean(na.omit(prim.dat$female.sum.2012[prim.dat$party=="NPP"])),
    mean(na.omit(prim.dat$num_aspirants_total.2012[prim.dat$party=="NPP"]))-mean(na.omit(prim.dat$female.sum.2012[prim.dat$party=="NPP"])),
    mean(na.omit(prim.dat$female.sum.2012[prim.dat$party=="NDC"])),
    mean(na.omit(prim.dat$num_aspirants_total.2012[prim.dat$party=="NDC"]))-mean(na.omit(prim.dat$female.sum.2012[prim.dat$party=="NDC"])),
    mean(na.omit(prim.dat$female.sum.2016[prim.dat$party=="NPP"])),
    mean(na.omit(prim.dat$num_aspirants_total.2016[prim.dat$party=="NPP"]))-mean(na.omit(prim.dat$female.sum.2016[prim.dat$party=="NPP"])),
    mean(na.omit(prim.dat$female.sum.2016[prim.dat$party=="NDC"])),
    mean(na.omit(prim.dat$num_aspirants_total.2016[prim.dat$party=="NDC"]))-mean(na.omit(prim.dat$female.sum.2016[prim.dat$party=="NDC"]))
  ),2)
)

numasp= round(c(
  mean(na.omit(prim.dat$female.sum.2012[prim.dat$party=="NPP"])),
  mean(na.omit(prim.dat$num_aspirants_total.2012[prim.dat$party=="NPP"]))-mean(na.omit(prim.dat$female.sum.2012[prim.dat$party=="NPP"])),
  mean(na.omit(prim.dat$female.sum.2012[prim.dat$party=="NDC"])),
  mean(na.omit(prim.dat$num_aspirants_total.2012[prim.dat$party=="NDC"]))-mean(na.omit(prim.dat$female.sum.2012[prim.dat$party=="NDC"])),
  mean(na.omit(prim.dat$female.sum.2016[prim.dat$party=="NPP"])),
  mean(na.omit(prim.dat$num_aspirants_total.2016[prim.dat$party=="NPP"]))-mean(na.omit(prim.dat$female.sum.2016[prim.dat$party=="NPP"])),
  mean(na.omit(prim.dat$female.sum.2016[prim.dat$party=="NDC"])),
  mean(na.omit(prim.dat$num_aspirants_total.2016[prim.dat$party=="NDC"]))-mean(na.omit(prim.dat$female.sum.2016[prim.dat$party=="NDC"]))
),2)

numasp_lab = paste(numasp, c("women", "men"), sep=" ")

numasp_y= round(c(
  mean(na.omit(prim.dat$female.sum.2012[prim.dat$party=="NPP"])),
  mean(na.omit(prim.dat$num_aspirants_total.2012[prim.dat$party=="NPP"])),
  mean(na.omit(prim.dat$female.sum.2012[prim.dat$party=="NDC"])),
  mean(na.omit(prim.dat$num_aspirants_total.2012[prim.dat$party=="NDC"])),
  mean(na.omit(prim.dat$female.sum.2016[prim.dat$party=="NPP"])),
  mean(na.omit(prim.dat$num_aspirants_total.2016[prim.dat$party=="NPP"])),
  mean(na.omit(prim.dat$female.sum.2016[prim.dat$party=="NDC"])),
  mean(na.omit(prim.dat$num_aspirants_total.2016[prim.dat$party=="NDC"]))
),2)

dat_text <- data.frame(
  label=numasp_lab,
  year=c(2012, 2012,2012,2012,2016,2016,2016,2016),
  party=c("NPP", "NPP", "NDC", "NDC", "NPP", "NPP", "NDC","NDC"),
  gender=c("women", "men", "women", "men", "women", "men", "women", "men"),
  y=numasp_y+0.4
)

pdf(file="../2_output/1_figs/fig1a_asp_gender_bar.pdf", height=6, width=6)
ggplot(data=df1, aes(x=party, y=numasp, fill=gender)) +
  geom_bar(stat="identity") + 
  facet_grid(~year) +
  scale_fill_manual(values=c("lightblue", "salmon")) +
  labs(y="Average Number of Aspirants") +
  geom_text(data=dat_text, label=dat_text$label, y=numasp_y+0.1)
dev.off()


df <- data.frame(
  year=c(2012,2012,2016,2016),
  party=c("NPP", "NDC", "NPP", "NDC"),
  nom= round(c(
    mean(na.omit(prim.dat$female.nominee.2012[prim.dat$party=="NPP"])),
    mean(na.omit(prim.dat$female.nominee.2012[prim.dat$party=="NDC"])),
    mean(na.omit(prim.dat$female.nominee.2016[prim.dat$party=="NPP"])),
    mean(na.omit(prim.dat$female.nominee.2016[prim.dat$party=="NDC"]))
  ),2)
)

numnom_y= round(c(
  mean(na.omit(prim.dat$female.nominee.2012[prim.dat$party=="NPP"])),
  mean(na.omit(prim.dat$female.nominee.2012[prim.dat$party=="NDC"])),
  mean(na.omit(prim.dat$female.nominee.2016[prim.dat$party=="NPP"])),
  mean(na.omit(prim.dat$female.nominee.2016[prim.dat$party=="NDC"]))
),2)

dat_text <- data.frame(
  label=numnom_y,
  year=c(2012,2012,2016,2016),
  party=c("NPP", "NDC", "NPP", "NDC"),
  nom= round(c(
    mean(na.omit(prim.dat$female.nominee.2012[prim.dat$party=="NPP"])),
    mean(na.omit(prim.dat$female.nominee.2012[prim.dat$party=="NDC"])),
    mean(na.omit(prim.dat$female.nominee.2016[prim.dat$party=="NPP"])),
    mean(na.omit(prim.dat$female.nominee.2016[prim.dat$party=="NDC"]))
  ),2),
  y=numnom_y
)

pdf(file="../2_output/1_figs/fig1b_nom_female_bar.pdf", height=6, width=6)
ggplot(data=df, aes(x=party, y=nom, fill=party)) + geom_bar(stat="identity") +
  facet_grid(~year) +
  ylim(0,0.2) +
  scale_fill_manual(values=c("darkgreen", "darkblue")) +
  labs(y="Share of Nominees Who are Women") +
  geom_text(data=dat_text, label=dat_text$label, y=numnom_y +0.02)
dev.off()

