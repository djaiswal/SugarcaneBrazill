SP_rcp_data <- read.csv("./saopaolo_onepoint_summary.csv")
# Finding relative yield with respect to RCP8P5

SP_rcp_data$refY_rcp8p5 <-c(rep(SP_rcp_data$HarvestedStem[4],4),rep(SP_rcp_data$HarvestedStem[8],4),rep(SP_rcp_data$HarvestedStem[12],4),
                            rep(SP_rcp_data$HarvestedStem[16],4),rep(SP_rcp_data$HarvestedStem[20],4))

SP_rcp_data$Y_rel_rcp8p5 <- SP_rcp_data$HarvestedStem*1/SP_rcp_data$refY_rcp8p5

#Renaming scenario level names
levels(SP_rcp_data$scenario)[levels(SP_rcp_data$scenario)=="rcp2p6"] <- "RCP2.6"
levels(SP_rcp_data$scenario)[levels(SP_rcp_data$scenario)=="rcp4p5"] <- "RCP4.5"
levels(SP_rcp_data$scenario)[levels(SP_rcp_data$scenario)=="rcp6p0"] <- "RCP6.0"
levels(SP_rcp_data$scenario)[levels(SP_rcp_data$scenario)=="rcp8p5"] <- "RCP8.5"

#Renamingmodel level names to be consistent with NCLIM manuscript
levels(SP_rcp_data$modelname)[levels(SP_rcp_data$modelname)=="GFDL"] <- "GFDL-ESM2M"
levels(SP_rcp_data$modelname)[levels(SP_rcp_data$modelname)=="HadGEM"] <- "HadGEM2-ES"
levels(SP_rcp_data$modelname)[levels(SP_rcp_data$modelname)=="IPSL"] <- "IPSL-CM5A-LR"
levels(SP_rcp_data$modelname)[levels(SP_rcp_data$modelname)=="MIROC"] <- "MIROC-ESM-CHEM"
levels(SP_rcp_data$modelname)[levels(SP_rcp_data$modelname)=="NorESM"] <- "NorESM1-M"

library(ggplot2)

rcp_plot <- ggplot(data=SP_rcp_data, aes(x=scenario,y=Y_rel_rcp8p5)) +
            geom_bar(stat="identity") +
            facet_grid(. ~ modelname) +
            geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
            scale_y_continuous(name="Normalized Stem Yield",breaks=c(0,0.25,0.5,0.75,1.0)) +
            scale_x_discrete(name=" Representative Concentration Pathways (RCPs)") +
            theme(axis.text.x=element_text(angle=45, hjust=1))
  
ggsave(filename = "./Normalized_yield_different_RCPs.png", dpi = 800)
