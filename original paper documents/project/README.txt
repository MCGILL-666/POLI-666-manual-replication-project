README

The folder replication/ contains the replication data and code for "Democratizing the Party:  The Effects of Primary Election Reforms in Ghana" by Nahomi Ichino and Noah L. Nathan.  Everything in /2_output is produced by the data in /0_data and code in /1_code.


0_data/
  prim.dat.RData
  
1_code/
  analysis.R -- produces ../2_output/0_results/results.RData
  analysis_altmethods.R -- produces ../2_output/0_results/results.altmethods.RData
  analysis_by_competitiveness.R -- produces ../2_output/0_results/results.comp.RData
  analysis_by_mpopshare.R -- produces ../2_output/0_results/results.mpopshare.RData
  analysis_excl_dropout.R -- produces ../2_output/0_results/results.minusdropout.RData
  analysis_with_calipers.R -- produces ../2_output/0_results/results.caliper.RData
  analysis_without_em.R -- produces ../2_output/0_results/results.noexact.RData
  figure1.R
  figure2.R
  figure3.R
  figure4.R
  figureA1.R
  figureB1B4B5B6.R
  figureB2.R
  figureB3.R
  figureC1.R
  figureC2.R
  figureC3.R
  figureC4.R
  table1.R
  tableA1.R
  tableA2.R
  tableB1.R
  tableB2.R
  tableB3.R
  tableC1.R
  tableC2.R
  tableC3.R
  tableC5.R
  tableC6.R


2_output/
  0_results/
    results.RData
    results.altmethods.RData
    results.caliper.RData
    results.comp.RData
    results.mpopshare.RData
    results.minusdropout.RData
    results.noexact.RData
  1_figs/
    all the figs for the article and online appendix
  2_tables/
    all the tables for the article and online appendix in csv and tex formats
    tex files formatted before being included in the manuscript



