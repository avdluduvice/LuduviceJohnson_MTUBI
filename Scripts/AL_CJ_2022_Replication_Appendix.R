# -------------------------------------------------------------------------
# Title: Means-Tested Transfers, Asset Limits, and Universal Basic Income
# Purpose: Replication codes for FRBC's Economic Commentary
# File: Script to generate Tables A.1 and A.2 and Figures A.1 and A.2
# Authors: André  Victor D. Luduvice and Cornelius Johnson
# Cleveland, July 20th, 2022
# -------------------------------------------------------------------------


# Generating Summary Statistics -------------------------------------------
x <- dataGraphs[,.(recipient, thhearn_annual, thtotinc_annual, thinc_quintile, thnetworth,
                   thhresources, 
                   thhtransfers_annual, tsnap_amt_annual_hh,
                   ttanf_amt_annual_hh, eitc_value, tssi_amt_annual_hh, thval_bank,
                   thval_bond, thval_re, thval_rent, thval_ret, thval_stmf,
                   thval_veh)]
labs <- data.frame(varnames = colnames(x),
                   tabnames = c("Recipient", "Earnings", "Income", 
                                "Income Quintile", "Net Worth", "Resources", 
                                "Transfers", "SNAP", "TANF", 
                                "EITC", "SSI","Bank", "Bonds", 
                                "Real Estate", "Rentals", "Retirement", "Investments",
                                "Vehicles"))

# Summary Statistics - By Benefit Receipt
weights_nonrecip <- dataGraphs[recipient == 0]$wpfinwgt
weights_recip <- dataGraphs[recipient == 1]$wpfinwgt


# Nonrecipient
st(dataGraphs[recipient == 0,.(thhearn_annual, thtotinc_annual, thnetworth, thhresources, tsnap_amt_annual_hh,
                               ttanf_amt_annual_hh, eitc_value, tssi_amt_annual_hh, thval_bank,
                               thval_bond, thval_re, thval_rent, thval_ret, thval_stmf,
                               thval_veh)], out = "csv", file = "Tables/Table_A1.1.csv",
   summ = c('min(x)', 'max(x)', 
            'wtd.mean(x, weights = weights_nonrecip)', 
            'wtd.quantile(x, weights = weights_nonrecip, probs = 0.25)', 
            'wtd.quantile(x, weights = weights_nonrecip, probs = 0.5)',
            'wtd.quantile(x, weights = weights_nonrecip, probs = 0.75)','notNA(x)'),
   summ.names = c("Min", "Max", "Mean", "0.25", "0.5", "0.75", "N"),
   title = "Summary Statistics - Households not receiving benefits",
   labels = labs,
   digits = 2)

# Recipient
st(dataGraphs[recipient == 1,.(thhearn_annual, thtotinc_annual, thnetworth,thhresources, tsnap_amt_annual_hh,
                               ttanf_amt_annual_hh, eitc_value, tssi_amt_annual_hh, thval_bank,
                               thval_bond, thval_re, thval_rent, thval_ret, thval_stmf,
                               thval_veh)], out = "csv", file = "Tables/Table_A1.2.csv",
   summ = c('min(x)', 'max(x)', 
            'wtd.mean(x, weights = weights_recip, na.rm = T)', 
            'wtd.quantile(x, weights = weights_recip, probs = 0.25)', 
            'wtd.quantile(x, weights = weights_recip, probs = 0.5)',
            'wtd.quantile(x, weights = weights_recip, probs = 0.75)','notNA(x)'),
   summ.names = c("Min", "Max", "Mean", "0.25", "0.5", "0.75", "N"),
   title = "Summary Statistics - Households receiving benefits",
   labels = labs,
   digits = 2)


# Summary Statistics - By Income Quintile - Recipients Only
weights_quint1 <- dataGraphs[recipient == 1 & thinc_quintile == "1st Quintile"]$wpfinwgt
weights_quint2 <- dataGraphs[recipient == 1 & thinc_quintile == "2nd Quintile"]$wpfinwgt
weights_quint3 <- dataGraphs[recipient == 1 & thinc_quintile == "3rd Quintile"]$wpfinwgt
weights_quint4 <- dataGraphs[recipient == 1 & thinc_quintile == "4th Quintile"]$wpfinwgt
weights_quint5 <- dataGraphs[recipient == 1 & thinc_quintile == "5th Quintile"]$wpfinwgt

# 1st Quintile
st(dataGraphs[recipient == 1 & thinc_quintile == "1st Quintile",.(thhearn_annual, thtotinc_annual, 
                                                                  thnetworth, thhresources, thhtransfers_annual)], 
   out = "csv", file = "Tables/TableA2.1.csv",
   summ = c('min(x)', 'max(x)', 
            'wtd.mean(x, weights = weights_quint1, na.rm = T)', 
            'wtd.quantile(x, weights = weights_quint1, probs = 0.25)', 
            'wtd.quantile(x, weights = weights_quint1, probs = 0.5)',
            'wtd.quantile(x, weights = weights_quint1, probs = 0.75)','notNA(x)'),
   summ.names = c("Min", "Max", "Mean", "0.25", "0.5", "0.75", "N"),
   title = "Summary Statistics - Households receiving benefits - 1st Income Quintile",
   labels = labs,
   digits = 2)

# 2nd Quintile
st(dataGraphs[recipient == 1 & thinc_quintile == "2nd Quintile",.(thhearn_annual, thtotinc_annual, 
                                                                  thnetworth, thhresources, thhtransfers_annual)], 
   out = "csv", file = "Tables/TableA2.2.csv",
   summ = c('min(x)', 'max(x)', 
            'wtd.mean(x, weights = weights_quint2, na.rm = T)', 
            'wtd.quantile(x, weights = weights_quint2, probs = 0.25)', 
            'wtd.quantile(x, weights = weights_quint2, probs = 0.5)',
            'wtd.quantile(x, weights = weights_quint2, probs = 0.75)','notNA(x)'),
   summ.names = c("Min", "Max", "Mean", "0.25", "0.5", "0.75", "N"),
   title = "Summary Statistics - Households receiving benefits - 2nd Income Quintile",
   labels = labs,
   digits = 2)

# 3rd Quintile
st(dataGraphs[recipient == 1 & thinc_quintile == "3rd Quintile",.(
  thhearn_annual, thtotinc_annual,     
  thnetworth, thhresources, thhtransfers_annual)], out = "csv", file = "Tables/TableA2.3.csv",
  summ = c('min(x)', 'max(x)', 
           'wtd.mean(x, weights = weights_quint3, na.rm = T)', 
           'wtd.quantile(x, weights = weights_quint3, probs = 0.25)', 
           'wtd.quantile(x, weights = weights_quint3, probs = 0.5)',
           'wtd.quantile(x, weights = weights_quint3, probs = 0.75)','notNA(x)'),
  summ.names = c("Min", "Max", "Mean", "0.25", "0.5", "0.75", "N"),
  title = "Summary Statistics - Households receiving benefits - 3rd Income Quintile",
  labels = labs,
  digits = 2)

# 4th Quintile
st(dataGraphs[recipient == 1 & thinc_quintile == "4th Quintile",.(
  thhearn_annual, thtotinc_annual,    
  thnetworth, thhresources, thhtransfers_annual)], out = "csv", file = "Tables/TableA2.4.csv",
  summ = c('min(x)', 'max(x)', 
           'wtd.mean(x, weights = weights_quint4, na.rm = T)', 
           'wtd.quantile(x, weights = weights_quint4, probs = 0.25)', 
           'wtd.quantile(x, weights = weights_quint4, probs = 0.5)',
           'wtd.quantile(x, weights = weights_quint4, probs = 0.75)','notNA(x)'),
  summ.names = c("Min", "Max", "Mean", "0.25", "0.5", "0.75", "N"),
  title = "Summary Statistics - Households receiving benefits - 4th Income Quintile",
  labels = labs,
  digits = 2)

# 5th Quintile
st(dataGraphs[recipient == 1 & thinc_quintile == "5th Quintile",.(
  thhearn_annual, thtotinc_annual, 
  thnetworth, thhresources, thhtransfers_annual)], out = "csv", file = "Tables/TableA2.5.csv",
  summ = c('min(x)', 'max(x)', 
           'wtd.mean(x, weights = weights_quint5, na.rm = T)', 
           'wtd.quantile(x, weights = weights_quint5, probs = 0.25)', 
           'wtd.quantile(x, weights = weights_quint5, probs = 0.5)',
           'wtd.quantile(x, weights = weights_quint5, probs = 0.75)','notNA(x)'),
  summ.names = c("Min", "Max", "Mean", "0.25", "0.5", "0.75", "N"),
  title = "Summary Statistics - Households receiving benefits - 5th Income Quintile",
  labels = labs,
  digits = 2)


# Figure A1 -------------------------------------------------------------
dataGraphs <- as.data.table(dataGraphs)
dataGraphs[efstatus !=2, efstatus := 1]
dataGraphs[rhnumu18 >=3, rhnumu18 := 3]

ggplot(dataGraphs[eitc_value !=0], aes(x = thhearn_annual, y = eitc_value)) + 
  geom_point(aes(color = factor(efstatus, labels = c("Single, Head of Household, or Widowed", "Married Filing Jointly"))), size = 2) + 
  theme_classic() + 
  theme(axis.title = element_text(size=15, color="black"),
        axis.text.y=element_text(size=20, color="black"),
        axis.text.x=element_text(size=20, color="black"),
        strip.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(colour = "black", size=1),
        plot.title=element_text(size=20, color="black"),
        legend.key.size = unit(.5, 'cm'),
        legend.text=element_text( size=15),
        legend.text.align = 0,
        legend.title=element_blank(),
        legend.key = element_rect(colour = "transparent", fill="transparent"),
        legend.background = element_rect(linetype = 1, size = 0.5, colour = 1),
        legend.justification = c(-1.61,1),
        legend.position=c(0,1)) + 
  scale_y_continuous(labels = big_mark) + 
  scale_x_continuous(labels = big_mark)+
  scale_fill_manual(values = c(color5, color2), labels = c("1", "2"))+
  labs(x = "Household Earnings ($)", y = "Imputed EITC Value ($)")

ggsave(paste0(Chart_Path, "Figure_A1.png"), width = 12, height=5, unit="in")


# Figure A2 ---------------------------------------------------------------
# Figure A2.1
data <- dataGraphs_thinc_q_mean[,.(thinc_quintile, recipient, thhearn_annual)]

x1 <- paste0("\u2264 $", big_mark(round(thinc_quints[2],-2)))
x2 <- paste(paste0("$",big_mark(round(thinc_quints[2], -2)+1)), paste0("$",big_mark(round(thinc_quints[3],-2))), sep = " - ")
x3 <- paste(paste0("$",big_mark(round(thinc_quints[3], -2)+1)), paste0("$",big_mark(round(thinc_quints[4],-2))), sep = " - ")
x4 <- paste(paste0("$",big_mark(round(thinc_quints[4], -2)+1)), paste0("$",big_mark(round(thinc_quints[5],-2))), sep = " - ")
x5 <- paste0(paste0("$",big_mark(round(thinc_quints[5], -2)+1)), "+")
new <- c(
  paste0(x1, " \n (0%-20%)"),
  paste0(x2, " \n (21%-40%)"),
  paste0(x3, "\n (41%-60%)"), 
  paste0(x4, "\n (61%-80%)"), 
  paste0(x5, "\n (81%-100%)"))

ggplot(data, aes(x = factor(thinc_quintile, labels = new), y = thhearn_annual, 
                 fill = as.factor(recipient)))+
  geom_bar(position="dodge", stat="identity")+
  theme_classic()+
  theme(axis.title = element_text(size=15, color="black"),
        axis.text.y=element_text(size=20, color="black"),
        axis.text.x=element_text(size=13, color="black"),
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
  scale_fill_manual(values=c(color3, color2), labels = c("Household not receiving benefits", "Household receiving benefits"))+
  labs(x="Household Income", y = "Mean Earnings ($)")

ggsave(paste0(Chart_Path, "Figure A2.1.png"), width = 12, height=5, unit="in")

# Figure A2.2
data <- dataGraphs_thinc_q_mean[,.(thinc_quintile, recipient, thtotinc_annual)]

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
ggplot(data, aes(x = factor(thinc_quintile, labels = thinc_quintiles), 
                 y = thtotinc_annual, fill = as.factor(recipient)))+
  geom_bar(position="dodge", stat="identity")+
  theme_classic()+
  theme(axis.title = element_text(size=15, color="black"),
        axis.text.y=element_text(size=20, color="black"),
        axis.text.x=element_text(size=13, color="black"),
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
  scale_fill_manual(values=c(color3, color2), labels = c("Household not receiving benefits", "Household receiving benefits"))+
  labs(x="Household Income", y = "Mean Household Income ($)")

ggsave(paste0(Chart_Path, "Figure A2.2.png"), width = 12, height=5, unit="in")

