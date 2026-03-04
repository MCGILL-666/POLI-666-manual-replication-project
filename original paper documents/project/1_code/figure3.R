##############################################################
## Figure3.R
##
## Figure 3: Aspirants' and nominees' membership in their 
## parties' core ethnic coalition, by party and year
##############################################################

rm(list=ls())
library(ggplot2)

load("../0_data/prim.dat.Rdata")


df <- data.frame(
  year=c(2012,2012,2016,2016),
  party=c("NPP", "NDC", "NPP", "NDC"),
  nom= round(c(
    mean(na.omit(prim.dat$owngroup.nominee.2012[prim.dat$party=="NPP"])),
    mean(na.omit(prim.dat$owngroup.nominee.2012[prim.dat$party=="NDC"])),
    mean(na.omit(prim.dat$owngroup.nominee.2016[prim.dat$party=="NPP"])),
    mean(na.omit(prim.dat$owngroup.nominee.2016[prim.dat$party=="NDC"]))
  ),2)
)

numnom_y= round(c(
  mean(na.omit(prim.dat$owngroup.nominee.2012[prim.dat$party=="NPP"])),
  mean(na.omit(prim.dat$owngroup.nominee.2012[prim.dat$party=="NDC"])),
  mean(na.omit(prim.dat$owngroup.nominee.2016[prim.dat$party=="NPP"])),
  mean(na.omit(prim.dat$owngroup.nominee.2016[prim.dat$party=="NDC"]))
),2)

dat_text <- data.frame(
  label=numnom_y,
  year=c(2012,2012,2016,2016),
  party=c("NPP", "NDC", "NPP", "NDC"),
  nom= round(c(
    mean(na.omit(prim.dat$owngroup.nominee.2012[prim.dat$party=="NPP"])),
    mean(na.omit(prim.dat$owngroup.nominee.2012[prim.dat$party=="NDC"])),
    mean(na.omit(prim.dat$owngroup.nominee.2016[prim.dat$party=="NPP"])),
    mean(na.omit(prim.dat$owngroup.nominee.2016[prim.dat$party=="NDC"]))
  ),2),
  y=numnom_y+0.1
)

pdf(file="../2_output/1_figs/fig3b_nom_owngroup_bar.pdf", height=6, width=6)
ggplot(data=df, aes(x=party, y=nom, fill=party)) + geom_bar(stat="identity") +
  facet_grid(~year) +
  ylim(0,0.75) +
  scale_fill_manual(values=c("darkgreen", "darkblue")) +
  labs(y="Share of Nominees who are from the Core Group") +
  geom_text(data=dat_text, label=dat_text$label, y=numnom_y+0.05)
dev.off()




###
df <- data.frame(
  year=c(2012,2012,2012,2012,2016,2016,2016,2016),
  party=c("NPP", "NPP", "NDC", "NDC", "NPP", "NPP", "NDC","NDC"),
  ethnicity=c("non-core", "core", "non-core", "core", "non-core", "core", "non-core", "core"),
  numasp= round(c(
    mean(na.omit(prim.dat$not_owngroups.sum.2012[prim.dat$party=="NPP"])),
    mean(na.omit(prim.dat$cand_owngroups.sum.2012[prim.dat$party=="NPP"])),
    mean(na.omit(prim.dat$not_owngroups.sum.2012[prim.dat$party=="NDC"])),
    mean(na.omit(prim.dat$cand_owngroups.sum.2012[prim.dat$party=="NDC"])),
    mean(na.omit(prim.dat$not_owngroups.sum.2016[prim.dat$party=="NPP"])),
    mean(na.omit(prim.dat$cand_owngroups.sum.2016[prim.dat$party=="NPP"])),
    mean(na.omit(prim.dat$not_owngroups.sum.2016[prim.dat$party=="NDC"])),
    mean(na.omit(prim.dat$cand_owngroups.sum.2016[prim.dat$party=="NDC"]))
  ),2)
)


numasp= round(c(
  mean(na.omit(prim.dat$not_owngroups.sum.2012[prim.dat$party=="NPP"])),
  mean(na.omit(prim.dat$cand_owngroups.sum.2012[prim.dat$party=="NPP"])),
  mean(na.omit(prim.dat$not_owngroups.sum.2012[prim.dat$party=="NDC"])),
  mean(na.omit(prim.dat$cand_owngroups.sum.2012[prim.dat$party=="NDC"])),
  mean(na.omit(prim.dat$not_owngroups.sum.2016[prim.dat$party=="NPP"])),
  mean(na.omit(prim.dat$cand_owngroups.sum.2016[prim.dat$party=="NPP"])),
  mean(na.omit(prim.dat$not_owngroups.sum.2016[prim.dat$party=="NDC"])),
  mean(na.omit(prim.dat$cand_owngroups.sum.2016[prim.dat$party=="NDC"]))
),2)

numasp_y= c(0.5, 1.8, 0.5, 2, 0.5, 2, 1, 2.7)


dat_text <- data.frame(
  label=numasp,
  year=c(2012, 2012,2012,2012,2016,2016,2016,2016),
  party=c("NPP", "NPP", "NDC", "NDC", "NPP", "NPP", "NDC","NDC"),
  gender=c("non-core", "core", "non-core", "core", "non-core", "core", "non-core", "core"),
  ethnicity=c("non-core", "core", "non-core", "core", "non-core", "core", "non-core", "core"),
  y=numasp_y
)

pdf(file="../2_output/1_figs/fig3a_asp_owngroup_bar.pdf", height=6, width=6)
ggplot(data=df, aes(x=party, y=numasp, fill=ethnicity)) +geom_bar(stat="identity") + 
  facet_grid(~year) +
  scale_fill_manual(values=c("darkolivegreen3", "darkgoldenrod1")) +
  labs(y="Average Number of Aspirants") +
  geom_text(data=dat_text, label=dat_text$label, y=numasp_y)
dev.off()


