# -------------------------------------------------------------------------
# Title: Means-Tested Transfers, Asset Limits, and Universal Basic Income
# Purpose: Replication codes for FRBC's Economic Commentary
# File: Script to generate Figure A.3 in the appendix
# Authors: André  Victor D. Luduvice and Cornelius Johnson
# Cleveland, July 20th, 2022
# -------------------------------------------------------------------------


# Directory and Calibration -----------------------------------------------
# All previous calibration settings carry over, we simply change wealth_trim from 0.95 to 1
wealth_trim <- 1


# Resetting pu2018 by re-running IO operations
# Data Input --------------------------------------------------------------
id_dem <- c("shhadid", "spanel", "ssuid", "swave", "pnum", "monthcode", "wpfinwgt", "rfamref", "erelrpe")
id_dem <- c(id_dem, "tage", "tage_ehc", "tdob_byear", "esex", "erace", "eeduc", "rged", "tceb")
id_dem <- c(id_dem, "rhnumper", "rhnumu18")
earnings_inc <- c("thtotinc", "thinc_ast", "thincpov", "thinc_bank", "thinc_bond", "thinc_oth", "thinc_rent", "thinc_stmf", "tptotinc", "tpearn")
cash_trnf <- c("tptrninc", "tga_amt", "tsnap_amt", "tssi_amt", "ttanf_amt", "twic_amt")
eeitc_sts <- c("eeitc", "efstatus")
assets <- c("thnetworth", "thval_ast", "thval_bank", "thval_bond", "thval_bus", 
            "thval_esav", "thval_home", "thval_oth", "thval_re", "thval_rent")
assets <- c(assets, "thval_ret", "thval_rmu", "thval_stmf", "thval_veh")
debt <- c("thdebt_ast", "thdebt_bus", "thdebt_cc", "thdebt_ed", "thdebt_home", 
          "thdebt_ot", "thdebt_re", "thdebt_rent", "thdebt_sec", "thdebt_usec", 
          "thdebt_veh", "tmed_amt")
equity <- c("theq_bus", "theq_home", "theq_re", "theq_rent", "theq_veh")

pu2018 <- fread(paste0(getwd(),"/Data/pu2018.csv"), select = c(id_dem, earnings_inc, cash_trnf, eeitc_sts, assets, debt, equity))

pu2018$ssuid <- as.character(pu2018$ssuid)
pu2018$shhadid <- as.character(pu2018$shhadid)
pu2018$spanel <- as.character(pu2018$spanel)
pu2018$eeduc <- as.numeric(pu2018$eeduc)
pu2018$theq_veh <- as.numeric(pu2018$theq_veh)

# Data Transformations ----------------------------------------------------
source(paste0(getwd(),"/Scripts/AL_CJ_2022_Replication_Transformations.R"), local = TRUE)

# Figure A3 ----------------------------------------------------------------
data <- dataGraphs_thinc_q_mean[,.(thinc_quintile, recipient, thnetworth)]

# Formatting x-axis labels in $ notation
x1 <- paste0("\u2264 $", big_mark(round(thinc_quints[2],-2)))
x2 <- paste(paste0("$",big_mark(round(thinc_quints[2], -2)+1)), paste0("$",big_mark(round(thinc_quints[3],-2))), sep = " - ")
x3 <- paste(paste0("$",big_mark(round(thinc_quints[3], -2)+1)), paste0("$",big_mark(round(thinc_quints[4],-2))), sep = " - ")
x4 <- paste(paste0("$",big_mark(round(thinc_quints[4], -2)+1)), paste0("$",big_mark(round(thinc_quints[5],-2))), sep = " - ")
x5 <- paste0(paste0("$",big_mark(round(thinc_quints[5], -2)+1)), "+")
thinc_quintiles <- c(
  paste0(x1, " \n (0%-20%)"),
  paste0(x2, " \n (21%-40%)"),
  paste0(x3, "\n (41%-60%)"), 
  paste0(x4, "\n (61%-80%)"), 
  paste0(x5, "\n (81%-100%)"))

# Plotting Figure A3
ggplot(data, aes(x = factor(thinc_quintile, labels = thinc_quintiles), y = thnetworth, 
                 fill = as.factor(recipient)))+
  geom_bar(position="dodge", stat="identity")+
  theme_classic()+
  theme(axis.title = element_text(size=15, color="black"),
        axis.text.y=element_text(size=20, color="black"),
        axis.text.x=element_text(size=15, color="black"),
        strip.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(colour = "black", size=1),
        plot.title=element_text(size=20, color="black"),
        legend.key.size = unit(.5, 'cm'),
        legend.text=element_text( size=15),
        legend.text.align = 1,
        legend.title=element_blank(),
        legend.key = element_rect(colour = "transparent", fill="transparent"),
        legend.background = element_rect(linetype = 1, size = 0.5, colour = 1),
        legend.justification = c(0,1),
        legend.position=c(0,1))+
  scale_y_continuous(labels = big_mark)+
  scale_x_discrete()+
  scale_fill_manual(values=c(color3, color2), labels = c("Household not receiving benefits", "Household receiving benefits"))+
  labs(x="Household Income", y = "Mean Wealth ($)")

ggsave(paste0(Chart_Path, "Figure_A3.png"), width = 12, height=5, unit="in")
