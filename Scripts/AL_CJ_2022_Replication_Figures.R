# -------------------------------------------------------------------------
# Title: Means-Tested Transfers, Asset Limits, and Universal Basic Income
# Purpose: Replication codes for FRBC's Economic Commentary
# File: Script to generate Figures 1 to 4
# Authors: André  Victor D. Luduvice and Cornelius Johnson
# Cleveland, July 20th, 2022
# -------------------------------------------------------------------------


# Defining Color Scheme ---------------------------------------------------

color1 = "#599871"   #green
color2 = "#2875a8"   #blue
color3 = "#e67a17"   #orange
color4 = "#581f54"   #maroon
color5 = "#960909"   #red
color6 = "#bd9e39"   #gold
color7 = "#12b3d1"   #baby blue
color8 = "#9467bd"   #purple
color9 = "#93795c"   #brown
color10 = "#103c62"  #navy

# Figure 1 ----------------------------------------------------------------
data <- dataGraphs_thinc_q_mean[,.(thinc_quintile, recipient, thnetworth)]

big_mark <-  function(x) format(x, big.mark = ",", scientific = FALSE)

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


# Plotting Figure 1
ggplot(data, aes(x = factor(thinc_quintile, labels = thinc_quintiles), y = thnetworth, 
                 fill = as.factor(recipient)))+
  geom_vline(aes(xintercept = 3.5)) + 
  annotate(geom = "text", x = 3, y = 400000, label = "91% of recipients")+
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



ggsave(paste0(Chart_Path, "Figure_1.png"), width = 12, height=5, unit="in")


# Figure 2 ----------------------------------------------------------------
data <- dataGraphs_thinc_d_mean[recipient == 1,.(thinc_decile, thtotinc_annual, tsnap_amt_annual_hh, tssi_amt_annual_hh, ttanf_amt_annual_hh, eitc_value)]

data <- data[, .(thinc_decile,
                 tsnap_amt_annual_hh = (tsnap_amt_annual_hh / thtotinc_annual)*100, 
                 tssi_amt_annual_hh = (tssi_amt_annual_hh / thtotinc_annual)*100, 
                 ttanf_amt_annual_hh = (ttanf_amt_annual_hh / thtotinc_annual)*100, 
                 eitc_value = (eitc_value / thtotinc_annual)*100)]

data <- pivot_longer(data, !thinc_decile, names_to = "variable", values_to = "value")
data <- as.data.table(data)

# Formatting x-axis labels in $ notation
x1 <- paste0("\u2264 $", big_mark(round(thinc_decs[2],-2)))
x2 <- paste(paste0("$",big_mark(round(thinc_decs[2], -2)+1)), paste0("$",big_mark(round(thinc_decs[3],-2))), sep = "-")
x3 <- paste(paste0("$",big_mark(round(thinc_decs[3], -2)+1)), paste0("$",big_mark(round(thinc_decs[4],-2))), sep = "-")
x4 <- paste(paste0("$",big_mark(round(thinc_decs[4], -2)+1)), paste0("$",big_mark(round(thinc_decs[5],-2))), sep = "-")
x5 <- paste(paste0("$",big_mark(round(thinc_decs[5], -2)+1)), paste0("$",big_mark(round(thinc_decs[6],-2))), sep = "-")
x6 <- paste(paste0("$",big_mark(round(thinc_decs[6], -2)+1)), paste0("$",big_mark(round(thinc_decs[7],-2))), sep = "-")
x7 <- paste(paste0("$",big_mark(round(thinc_decs[7], -2)+1)), paste0("$",big_mark(round(thinc_decs[8],-2))), sep = "-")
x8 <- paste(paste0("$",big_mark(round(thinc_decs[8], -2)+1)), paste0("$",big_mark(round(thinc_decs[9],-2))), sep = "-")
x9 <- paste(paste0("$",big_mark(round(thinc_decs[9], -2)+1)), paste0("$",big_mark(round(thinc_decs[10],-2))), sep = "-")
x10 <- paste0(paste0("$",big_mark(round(thinc_decs[10], -2)+1)), "+")

thinc_deciles <- c(
  paste0(x1, "\n (0%-10%)"), 
  paste0(x2, "\n (11%-20%)"), 
  paste0(x3, "\n (21%-30%)"), 
  paste0(x4, "\n (31%-40%)"), 
  paste0(x5, "\n (41%-50%)"), 
  paste0(x6, "\n (51%-60%)"), 
  paste0(x7, "\n (61%-70%)"), 
  paste0(x8, "\n (71%-80%)"), 
  paste0(x9, "\n (81%-90%)"), 
  paste0(x10, "\n (91%-100%)"))

# Plotting Figure 2
ggplot(data, aes(fill = variable, 
                 x = factor(thinc_decile, labels = thinc_deciles), y = as.integer(value))) + 
  geom_bar(position = "dodge", stat = "identity") +
  theme_classic()+
  theme(axis.title = element_text(size=15, color="black"),
        axis.text.y=element_text(size=20, color="black"),
        axis.text.x=element_text(size=9, color="black"),
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
        legend.justification = c(-10.29,1),
        legend.position=c(0,1))+
  scale_y_continuous()+
  scale_fill_manual(values=c(color1, color2, color3, color4), labels = c("EITC", "SNAP", "SSI", "TANF"))+
  labs(x="Household Income", y= "Annual Benefits (% Income)")

ggsave(paste0(Chart_Path, "Figure_2.png"), width = 12, height=5, unit="in")


# Figure 3 ----------------------------------------------------------------
data <- dataGraphs_thinc_d_mean[(
  thinc_decile == as.character(unique(dataGraphs_thinc_d_mean$thinc_decile)[1:4])[1] 
  | thinc_decile == as.character(unique(dataGraphs_thinc_d_mean$thinc_decile)[1:4])[2] 
  | thinc_decile == as.character(unique(dataGraphs_thinc_d_mean$thinc_decile)[1:4])[3] 
  | thinc_decile == as.character(unique(dataGraphs_thinc_d_mean$thinc_decile)[1:4])[4] 
  | thinc_decile == as.character(unique(dataGraphs_thinc_d_mean$thinc_decile)[1:5])[5] 
),.(thinc_decile, recipient, thhresources)]

x1 <- paste0("\u2264 $", big_mark(round(thinc_decs[2],-2)))
x2 <- paste(paste0("$",big_mark(round(thinc_decs[2], -2)+1)), paste0("$",big_mark(round(thinc_decs[3],-2))), sep = " - ")
x3 <- paste(paste0("$",big_mark(round(thinc_decs[3], -2)+1)), paste0("$",big_mark(round(thinc_decs[4],-2))), sep = " - ")
x4 <- paste(paste0("$",big_mark(round(thinc_decs[4], -2)+1)), paste0("$",big_mark(round(thinc_decs[5],-2))), sep = " - ")
x5 <- paste0(paste0("$",big_mark(round(thinc_decs[5], -2)+1)), "+")
thinc_deciles <- c(
  paste0(x1, "\n (0%-10%)"), 
  paste0(x2, "\n (11%-20%)"), 
  paste0(x3, "\n (21%-30%)"), 
  paste0(x4, "\n (31%-40%)"), 
  paste0(x5, "\n (41%-50%)"))


# Plotting Figure 3 ----------------------------------------------------------------
ggplot(data, aes(x = factor(thinc_decile, labels = thinc_deciles), y = thhresources, 
                 fill = as.factor(recipient)))+
  geom_bar(position="dodge", stat="identity")+
  theme_classic()+
  theme(axis.title = element_text(size=15, color="black"),
        axis.text.y=element_text(size=20, color="black"),
        # axis.text.y.right = element_text(size = 15, color = "black"),
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
  scale_y_continuous(labels = big_mark, breaks = seq(0,150000, by=30000), limits = c(0, 150000))+
  scale_fill_manual(values=c(color3, color2), labels = c("Household not receiving benefits", "Household receiving benefits"))+
  labs(x="Household Income", y = "Mean Resources ($)") + 
  geom_hline(yintercept = 3250, size = 1.5, linetype = 2, color=color6) + 
  geom_hline(yintercept = (19730/12)*3, size = 1.5, linetype = 2, color = color5)+ #19730 is the federal poverty threshold for 2 adults and 1 child - https://www2.census.gov/programs-surveys/cps/tables/time-series/historical-poverty-thresholds/thresh17.xls
  geom_hline(yintercept = 10000, size = 1.5, linetype = 2)

ggsave(paste0(Chart_Path, "Figure_3.png"), width = 12, height=5, unit="in")


# Figure 4 ----------------------------------------------------------------
data <- dataGraphs_thinc_q_mean  %>% 
  mutate(
    thval_bank_pct = (thval_bank/thnetworth)*100, 
    thval_bond_pct = (thval_bond/thnetworth)*100,
    thval_re_pct = (thval_re/thnetworth)*100,
    thval_rent_pct = (thval_rent/thnetworth)*100, 
    thval_ret_pct = (thval_ret/thnetworth)*100,
    thval_stmf_pct = (thval_stmf/thnetworth)*100, 
    thval_veh_pct = (thval_veh/thnetworth)*100, 
    thval_oth_pct = (thval_oth/thnetworth)*100,
    thval_bus_pct = (thval_bus/thnetworth)*100,
    thval_esav_pct = (thval_esav/thnetworth)*100,
    thval_home_pct = (thval_home/thnetworth)*100
  )

data <- data %>% mutate(
  recipient = case_when(
    recipient == 0 ~ "Household not receiving benefits", 
    recipient == 1 ~ "Household receiving benefits"
  )
  
)


data <- as.data.table(data)
data <- data[thinc_quintile == "1st Quintile" | thinc_quintile == "2nd Quintile"]

data <- data[,.(thinc_quintile, recipient, thval_bank_pct, thval_bond_pct, thval_re_pct, thval_rent_pct,
                thval_ret_pct, thval_stmf_pct, thval_veh_pct, thval_oth_pct, thval_bus_pct, 
                thval_esav_pct)]
data <- pivot_longer(data,!recipient & !thinc_quintile, names_to = "variable", values_to = "value")

fc_levels <- c( "thval_veh_pct",
                "thval_ret_pct",
                "thval_bus_pct",
                "thval_oth_pct", 
                "thval_bank_pct",
                "thval_rent_pct",
                "thval_stmf_pct", 
                "thval_re_pct",
                "thval_esav_pct",
                "thval_bond_pct")

assets <- c("Vehicles",
            "Retirement",
            "Businesses",
            "Other",
            "Checkings & Savings",
            "Rental Property",
            "Stocks & Mutual Funds",
            "Real Estate",
            "Educational Savings",
            "Bonds")

ggplot(data, aes(x = factor(variable, levels = fc_levels, labels = assets), y = value, fill = recipient))+
  geom_bar(position="dodge", stat="identity")+
  theme_classic()+
  theme(axis.title = element_text(size=15, color="black"),
        axis.text.y=element_text(size=20, color="black"),
        axis.text.x=element_text(size=18, color="black"),
        strip.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(colour = "black", size=1),
        plot.title=element_text(size=20, color="black"),
        legend.key.size = unit(.5, 'cm'),
        legend.text=element_text( size=30),
        legend.text.align = 1,
        legend.title=element_blank(),
        legend.key = element_rect(colour = "transparent", fill="transparent"),
        legend.background = element_rect(linetype = 1, size = 0.5, colour = 1),
        legend.justification = c(1,1),
        legend.position=c(1,1))+
  scale_y_continuous()+
  labs(x = "",y="Share of Portfolio (%)")

ggsave(paste0(Chart_Path, "Figure_4.png"), width = 24, height=10, unit="in")


