##############################################################
## Figure4.R
##
## Figure 4: Political experience of nominees, by party and year
##############################################################

rm(list=ls())
library(ggplot2)

load("../0_data/prim.dat.Rdata")


df <- data.frame(
  year=c(2012,2012,2016,2016),
  party=c("NPP", "NDC", "NPP", "NDC"),
  nom= round(c(
    mean(na.omit(prim.dat$private_sector.ONLY.nominee.2012[prim.dat$party=="NPP"])),
    mean(na.omit(prim.dat$private_sector.ONLY.nominee.2012[prim.dat$party=="NDC"])),
    mean(na.omit(prim.dat$private_sector.ONLY.nominee.2016[prim.dat$party=="NPP"])),
    mean(na.omit(prim.dat$private_sector.ONLY.nominee.2016[prim.dat$party=="NDC"]))
  ),2)
)

numnom_y= round(c(
  mean(na.omit(prim.dat$private_sector.ONLY.nominee.2012[prim.dat$party=="NPP"])),
  mean(na.omit(prim.dat$private_sector.ONLY.nominee.2012[prim.dat$party=="NDC"])),
  mean(na.omit(prim.dat$private_sector.ONLY.nominee.2016[prim.dat$party=="NPP"])),
  mean(na.omit(prim.dat$private_sector.ONLY.nominee.2016[prim.dat$party=="NDC"]))
),2)

dat_text <- data.frame(
  label=numnom_y,
  year=c(2012,2012,2016,2016),
  party=c("NPP", "NDC", "NPP", "NDC"),
  nom= round(c(
    mean(na.omit(prim.dat$private_sector.ONLY.nominee.2012[prim.dat$party=="NPP"])),
    mean(na.omit(prim.dat$private_sector.ONLY.nominee.2012[prim.dat$party=="NDC"])),
    mean(na.omit(prim.dat$private_sector.ONLY.nominee.2016[prim.dat$party=="NPP"])),
    mean(na.omit(prim.dat$private_sector.ONLY.nominee.2016[prim.dat$party=="NDC"]))
  ),2),
  y=numnom_y+0.1
)

pdf(file="../2_output/1_figs/fig4a_nom_privatesectoronly_bar.pdf", height=6, width=6)
ggplot(data=df, aes(x=party, y=nom, fill=party)) + geom_bar(stat="identity") +
  facet_grid(~year) +
  ylim(0,0.2) +
  scale_fill_manual(values=c("darkgreen", "darkblue")) +
  labs(y="Share of Nominees who are Wealthy Newcomers") +
  geom_text(data=dat_text, label=dat_text$label, y=numnom_y+0.02)
dev.off()


##
df <- data.frame(
  year=c(2012,2012,2016,2016),
  party=c("NPP", "NDC", "NPP", "NDC"),
  nom= round(c(
    mean(na.omit(prim.dat$incumbent.nominee.2012[prim.dat$party=="NPP"])),
    mean(na.omit(prim.dat$incumbent.nominee.2012[prim.dat$party=="NDC"])),
    mean(na.omit(prim.dat$incumbent.nominee.2016[prim.dat$party=="NPP"])),
    mean(na.omit(prim.dat$incumbent.nominee.2016[prim.dat$party=="NDC"]))
  ),2)
)

numnom_y= round(c(
  mean(na.omit(prim.dat$incumbent.nominee.2012[prim.dat$party=="NPP"])),
  mean(na.omit(prim.dat$incumbent.nominee.2012[prim.dat$party=="NDC"])),
  mean(na.omit(prim.dat$incumbent.nominee.2016[prim.dat$party=="NPP"])),
  mean(na.omit(prim.dat$incumbent.nominee.2016[prim.dat$party=="NDC"]))
),2)

dat_text <- data.frame(
  label=numnom_y,
  year=c(2012,2012,2016,2016),
  party=c("NPP", "NDC", "NPP", "NDC"),
  nom= round(c(
    mean(na.omit(prim.dat$incumbent.nominee.2012[prim.dat$party=="NPP"])),
    mean(na.omit(prim.dat$incumbent.nominee.2012[prim.dat$party=="NDC"])),
    mean(na.omit(prim.dat$incumbent.nominee.2016[prim.dat$party=="NPP"])),
    mean(na.omit(prim.dat$incumbent.nominee.2016[prim.dat$party=="NDC"]))
  ),2),
  y=numnom_y+0.1
)

pdf(file="../2_output/1_figs/fig4b_nom_incumbent_bar.pdf", height=6, width=6)
ggplot(data=df, aes(x=party, y=nom, fill=party)) + geom_bar(stat="identity") +
  facet_grid(~year) +
  ylim(0,0.5) +
  scale_fill_manual(values=c("darkgreen", "darkblue")) +
  labs(y="Share of Nominees who are Incumbents") +
  geom_text(data=dat_text, label=dat_text$label, y=numnom_y+0.05)
dev.off()
