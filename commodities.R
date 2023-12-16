#install.packages("ggplot2")
install.packages("vctrs")


library(readr)
library(dplyr)
library(tidyverse)
library(ggplot2)
#rm(list = ls())
#tiff('C:/mccci/manuf_comb.jpeg', units="in", width=8, height=4, res=300, compression = 'lzw')
#grid.arrange(p1, p2, ncol = 2)
#ggplot(aes(corp_tax_lcu/1000000, man_va_gdp))+ 
#geom_point(color="RED")+
#geom_smooth(method=lm, se=F)+
#theme_light()+
#ylab("Manufacturing Value-Addition (%GDP)")+
#xlab("Taxes on Profits (Millions MWK)")
#dev.off()


# Create a list of file paths
file_paths <- paste0("C:/tradedatafeb23/TradeData_21FEB_", 1990:2022, ".csv")

# Read all the files and combine them into a single data frame
commodities_data <- lapply(file_paths, function(file) read.csv(file)) %>%
  bind_rows()

View(commodities_data)

#Replace values
# Define the levels and labels to replace
levels_to_replace <- c("Ethiopia (...1992)", "Dem. Rep. of Germany (...1990)", "Czechoslovakia (...1992)", "Other Asia, nes", "R\xe9union (Overseas France)", "Yugoslavia (...1991)", "T\xfcrkiye", "C\xf4te d'Ivoire")
labels_to_replace <- c("Ethiopia", "Germany", "Czechia", "Other Asia, nes", "Réunion (Overseas France)", "Yugoslavia", "Türkiye", "Côte d'Ivoire")

# Use ifelse and %in% to replace only the matched values in ReporterDesc
commodities_data$ReporterDesc <- ifelse(commodities_data$ReporterDesc %in% levels_to_replace, 
                                        labels_to_replace[match(commodities_data$ReporterDesc, levels_to_replace)], 
                                        commodities_data$ReporterDesc)

#replacing reporter code for South Sudan alone
commodities_data$ReporterDesc <- ifelse(commodities_data$ReporterDesc == "Sudan (...2011)", "South Sudan",commodities_data$ReporterDesc)

#replacing the partner code
commodities_data$PartnerDesc <- ifelse(commodities_data$PartnerDesc == "C\xf4te d'Ivoire", "Côte d'Ivoire", 
                                       ifelse(commodities_data$PartnerDesc == "T\xfcrkiye", "Türkiye",
                                              ifelse(commodities_data$PartnerDesc == "R\xe9union (Overseas France)", "Réunion (Overseas France)",
                                                     ifelse(commodities_data$PartnerDesc == "Cura\xe7ao", "Curaçao",
                                                            ifelse(commodities_data$PartnerDesc == "Saint Barth\xe9lemy", "Saint Barthélemy",
                                                                   ifelse(commodities_data$PartnerDesc == "Sudan (...2011)", "South Sudan",
                                                                          commodities_data$PartnerDesc))))))



#load Agg_trade_8822
library(readxl)
agg_trade_1988_2004 <- read_excel("C:/Users/Michael.MICHAEL-NOTEBOO/Dropbox/songwe/agg_trade_1988_2004.xlsx")
agg_trade_2005_2022 <- read_excel("C:/Users/Michael.MICHAEL-NOTEBOO/Dropbox/songwe/agg_trade_2005_2022.xlsx")
agg_trade_8822<- rbind(agg_trade_1988_2004, agg_trade_2005_2022)

# Extract unique values of reporter, rep_cont, partner, and part_cont from agg_trade_8822
unique_reporter <- unique(agg_trade_8822[, c("reporter", "rep_cont")])
unique_partner <- unique(agg_trade_8822[, c("partner", "part_cont")])

# create a data frame with the new values
new_values <- data.frame(reporter = c("Other Asia, nes", "Yugoslavia"), rep_cont = c("Asia", "Europe"))

# append the new values to unique_reporter
unique_reporter <- rbind(unique_reporter, new_values)


# Assign rep_cont to ReporterDesc in commodities dataset
commodities_data$rep_cont <- NA
commodities_data$rep_cont <- unique_reporter$rep_cont[match(commodities_data$ReporterDesc, unique_reporter$reporter)]

# Assign part_cont to PartnerDesc in commodities dataset
commodities_data$part_cont <- NA
commodities_data$part_cont <- unique_partner$part_cont[match(commodities_data$PartnerDesc, unique_partner$partner)]


unique(commodities_data %>% filter(is.na(rep_cont)) %>% select(ReporterDesc))

unique(commodities_data %>% filter(is.na(part_cont)) %>% select(PartnerDesc))


#RENAMING
commodities_sub <- commodities_data %>%
  rename(year = Period,
         reporter = ReporterDesc,
         rep_cont = rep_cont,
         partner = PartnerDesc,
         part_cont = part_cont,
         commodity = CmdDesc,
          flow = FlowCode,
         imporex=FlowDesc,
         trade_value = PrimaryValue)

#CREATE DATASET FOR WORKING ON
commods_8822<-commodities_sub

#GENERATE AFRICA AS A REPORTER
commods_8822$afrirep<-ifelse(commods_8822$rep_cont=="Africa","Africa", commods_8822$reporter )
commods_8822$afripart<-ifelse(commods_8822$part_cont=="Africa","Africa", commods_8822$partner)


#GENERATE BRICS AS A VARIABLE AND NEW_BRICS
#unique(commods_8822$reporter)
# Create a new variable "bricship"
commods_8822$brics_rep <- ifelse(commods_8822$reporter %in% c("Russian Federation", "China", "India", "South Africa", "Brazil"), "BRICS",
                                ifelse(commods_8822$reporter %in% c("Algeria", "Argentina", "Egypt", "Iran", "Mexico", "Saudi Arabia", "Türkiye"), "POTENTIAL_BRICS",
                                       "NON-BRICS"))

commods_8822$brics_part <- ifelse(commods_8822$partner %in% c("Russian Federation", "China", "India", "South Africa", "Brazil"), "BRICS",
                                 ifelse(commods_8822$partner %in% c("Algeria", "Argentina", "Egypt", "Iran", "Mexico", "Saudi Arabia", "Türkiye"), "POTENTIAL_BRICS",
                                        "NON-BRICS"))

unique(commods_8822$partner[commods_8822$brics_part=="BRICS"])


#unique(commodities_sub$year)

# Create year bands

commods_8822 <- commods_8822 %>%
  mutate(year_bands = case_when(
    year >= 1990 & year <= 1991 ~ "1990-1991",
    year >= 1992 & year <= 2001 ~ "1992-2001",
    year >= 2002 & year <= 2011 ~ "2002-2011",
    year >= 2012 & year <= 2021 ~ "2012-2021",
    year == 2022 ~ "2022",
    TRUE ~ NA_character_
  ))

colnames(commods_8822)


library(dplyr)

unique(commods_8822$partner)


#EXAMINE TRADE BETWEEN India, USA, Japan, China, Germany AND AFRICA

# Filter the data to only include the countries of interest
countries_of_interest <- c("China", "India", "France", "Japan", "Germany", "USA")
filtered_2 <- commods_8822 %>%
  filter(reporter %in% countries_of_interest, part_cont == "Africa",
         imporex=="Export")

filtered_2 <-filtered_2 %>% 
  group_by(year, reporter) %>% 
  summarize(total_trade_volume=sum(trade_value), trade_mill=total_trade_volume/1000000)



# Create a line plot of trade volume over time, with a different color for each reporter
subset_df <- filtered_2[filtered_2$year >= 1992 & filtered_2$year <= 2021, ]


# Convert year to a factor variable to ensure proper ordering of x-axis
subset_df$year <- factor(subset_df$year)


color_mapping <- c("China" = "blue", "USA" = "red", "India" = "green", "Japan" = "orange", "France" = "purple", "Germany" = "black")


ggplot(subset_df, aes(x = year, y = trade_mill, color = reporter, group=reporter)) +
  geom_line(size = 2) +
  scale_color_manual(values = color_mapping) +
  scale_y_continuous(
    name = "Trade Volume (Millions of USD)",
    sec.axis = sec_axis(~ . * 1000, name = "Trade Volume (Billions of USD)", labels = scales::comma)
  ) +
  theme(
    legend.position = "none",
    plot.title = element_text(size = 14),
    axis.text.x = element_text(size = 8, angle = 45, hjust = 1)
  ) +
  labs(x = "Year", color = "Reporter") +
  ggtitle("Trade Volume with Africa by Reporting country (Reporter) (1992-2021)") +
  theme_ipsum() +
  theme(axis.text.x = element_text(size = 8))




#EUROPE, CHINA AND USA AS PARTNERS
library(dplyr)

# List of European countries (according to Eurostat)
europe_list <- c("Austria", "Belgium", "Bulgaria", "Croatia", "Republic of Cyprus", "Czechia", "Denmark",
                 "Estonia", "Finland", "France", "Germany", "Greece", "Hungary", "Ireland", "Italy",
                 "Latvia", "Lithuania", "Luxembourg", "Malta", "Netherlands", "Poland", "Portugal",
                 "Romania", "Slovakia", "Slovenia", "Spain", "Sweden")

commods_8822 <- commods_8822 %>%
  mutate(us_china_eur = case_when(
    partner %in% europe_list ~ "Europe",
    partner %in% c("China", "USA") ~ partner,
    TRUE ~ "ROW"
  ))

unique(commods_8822$us_china_eur)

# Filter the data by the specified conditions and group and summarize the data
commodities_data_us_china_eur <- commods_8822 %>% 
  filter(afrirep == "Africa", imporex == "Export", year >= 1995, year <= 2020) %>% 
  group_by(us_china_eur, afrirep, year) %>% 
  summarize(trade_mill = sum(trade_value)/1000000000)

# Create the plot
tiff('C:/songwe_graphs/africa_exports_us_china_eur_trend.jpeg', units="cm", width=25, height=14, res=300, compression = 'lzw')
ggplot(commodities_data_us_china_eur, aes(x = year, y = trade_mill, group = us_china_eur, color = us_china_eur)) +
  geom_line(linewidth=1.2) +
  labs(title = "Africa Exports to US, China, and Europe",
       x = "Year",
       y = "Trade (Millions of USD)") +
  scale_color_manual(values = c("USA" = "red", "China" = "blue", "Europe" = "green", "ROW" = "purple")) +
  theme_light()+
  scale_x_continuous(name = "Year", breaks = unique(commodities_data_us_china_eur$year), limits = c(1995, 2020), expand = c(0, 0))+
  theme(axis.text.x = element_text(size = 5),
        legend.position = "bottom")
dev.off()


# Create the plot
tiff('C:/songwe_graphs/africa_exports_us_china_eur_trend2.jpeg', units="cm", width=25, height=14, res=300, compression = 'lzw')
ggplot(commodities_data_us_china_eur %>% filter(us_china_eur != "ROW"), aes(x = year, y = trade_mill, group = us_china_eur, color = us_china_eur)) +
  geom_line(linewidth=1.2) +
  labs(title = "Africa Exports to US, China, and Europe",
       x = "Year",
       y = "Trade (Billions of USD)") +
  scale_color_manual(values = c("USA" = "red", "China" = "blue", "Europe" = "green", "ROW" = "purple")) +
  theme_light()+
  scale_x_continuous(name = "Year", breaks = unique(commodities_data_us_china_eur$year), limits = c(1995, 2020), expand = c(0, 0))+
  theme(axis.text.x = element_text(size = 5),
        legend.position = "bottom")
dev.off()



#AFRICA EXPORTS TO THE BRICS
# Filter the data by the specified conditions and group and summarize the data
commodities_data_brics <- commods_8822 %>% 
  filter(afripart == "Africa", imporex == "Import", year >= 1992, year <= 2021) %>% 
  group_by(brics_rep, afripart, year) %>% 
  summarize(trade_mill = sum(trade_value)/1000000)

# Create the plot
tiff('C:/songwe_graphs/brics_trend_afexp.jpeg', units="cm", width=25, height=14, res=300, compression = 'lzw')
ggplot(commodities_data_brics, aes(x = year, y = trade_mill, group = brics_rep, color = brics_rep)) +
  geom_line(linewidth=1.2) +
  labs(title = "Africa Exports to the BRICS, NON-BRICS, and POTENTIAL_BRICS",
       x = "Year",
       y = "Trade (Millions of USD)") +
  scale_color_manual(values = c("BRICS" = "red", "POTENTIAL_BRICS" = "blue", "NON-BRICS" = "green")) +
  theme_light()+
  scale_x_continuous(name = "Year", breaks = unique(commodities_data_brics$year), limits = c(1992, 2021), expand = c(0, 0))+
  theme(axis.text.x = element_text(size = 5),
        legend.position = "bottom")
dev.off()


#AFRICA EXPORTS TO THE BRICS (GROWTH RATES)
# Filter the data by the specified conditions and group and summarize the data
commodities_data_brics <- commods_8822 %>% 
  filter(afripart == "Africa", imporex == "Import", year >= 1992, year <= 2021) %>% 
  group_by(brics_rep, afripart, year) %>% 
  summarize(trade_mill = sum(trade_value)/1000000) %>% 
  mutate(yoy_growth = 100*(trade_mill - lag(trade_mill, default = first(trade_mill))) / lag(trade_mill, default = first(trade_mill))) %>%
  ungroup()

# Create the plot
tiff('C:/songwe_graphs/brics_trend_afexp_growth.jpeg', units="cm", width=25, height=14, res=300, compression = 'lzw')
ggplot(commodities_data_brics, aes(x = year, y = yoy_growth, group = brics_rep, color = brics_rep)) +
  geom_line(linewidth=1.2) +
  labs(title = "Growth of Africa Exports to the BRICS, NON-BRICS, and POTENTIAL_BRICS",
       x = "Year",
       y = "Growth Rate (Percentage)") +
  scale_color_manual(values = c("BRICS" = "red", "POTENTIAL_BRICS" = "blue", "NON-BRICS" = "green")) +
  theme_light()+
  scale_x_continuous(name = "Year", breaks = unique(commodities_data_brics$year), limits = c(1992, 2021), expand = c(0, 0))+
  theme(axis.text.x = element_text(size = 5),
        legend.position = "bottom")
dev.off()


#AFRICA EXPORTS TO THE BRICS
# Filter the data by the specified conditions and group and summarize the data
commodities_data_brics <- commods_8822 %>% 
  filter(afripart == "Africa", imporex == "Export", year >= 1992, year <= 2021) %>% 
  group_by(brics_rep, afripart, year) %>% 
  summarize(trade_mill = sum(trade_value)/1000000)

# Create the plot
tiff('C:/songwe_graphs/brics_trend_afimp.jpeg', units="cm", width=25, height=14, res=300, compression = 'lzw')
ggplot(commodities_data_brics, aes(x = year, y = trade_mill, group = brics_rep, color = brics_rep)) +
  geom_line(linewidth=1.2) +
  labs(title = "Africa Imports from the BRICS, NON-BRICS, and POTENTIAL_BRICS",
       x = "Year",
       y = "Trade (Millions of USD)") +
  scale_color_manual(values = c("BRICS" = "red", "POTENTIAL_BRICS" = "blue", "NON-BRICS" = "green")) +
  theme_light()+
  scale_x_continuous(name = "Year", breaks = unique(commodities_data_brics$year), limits = c(1992, 2021), expand = c(0, 0))+
  theme(axis.text.x = element_text(size = 5),
        legend.position = "bottom")
dev.off()


#AFRICA IMPORTS TO THE BRICS (GROWTH RATES)
# Filter the data by the specified conditions and group and summarize the data
commodities_data_brics <- commods_8822 %>% 
  filter(afripart == "Africa", imporex == "Export", year >= 1992, year <= 2021) %>% 
  group_by(brics_rep, afripart, year) %>% 
  summarize(trade_mill = sum(trade_value)/1000000) %>% 
  mutate(yoy_growth = 100*(trade_mill - lag(trade_mill, default = first(trade_mill))) / lag(trade_mill, default = first(trade_mill))) %>%
  ungroup()

# Create the plot
tiff('C:/songwe_graphs/brics_trend_afimp_growth.jpeg', units="cm", width=25, height=14, res=300, compression = 'lzw')
ggplot(commodities_data_brics, aes(x = year, y = yoy_growth, group = brics_rep, color = brics_rep)) +
  geom_line(linewidth=1.2) +
  labs(title = "Growth of Africa Imports from the BRICS, NON-BRICS, and POTENTIAL_BRICS",
       x = "Year",
       y = "Growth Rate (Percentage)") +
  scale_color_manual(values = c("BRICS" = "red", "POTENTIAL_BRICS" = "blue", "NON-BRICS" = "green")) +
  theme_light()+
  scale_x_continuous(name = "Year", breaks = unique(commodities_data_brics$year), limits = c(1992, 2021), expand = c(0, 0))+
  theme(axis.text.x = element_text(size = 5),
        legend.position = "bottom")
dev.off()


#TRADE WITH EACH OF THE BRICS

# Filter the data by the specified conditions and group and summarize the data
commodities_data_brics <- commods_8822 %>% 
  filter(afripart == "Africa", imporex == "Import", year >= 1992, year <= 2021, reporter %in% c("Brazil", "Russian Federation", "India", "China", "South Africa")) %>% 
  group_by(reporter, afripart, year) %>% 
  summarize(trade_mill = sum(trade_value)/1000000)

# Create the plot
tiff('C:/songwe_graphs/brics_trend_each_exp.jpeg', units="cm", width=25, height=14, res=300, compression = 'lzw')
ggplot(commodities_data_brics, aes(x = year, y = trade_mill, group = reporter, color = reporter)) +
  geom_line(linewidth=1.2) +
  labs(title = "Africa Exports to BRICS Countries",
       x = "Year",
       y = "Trade (Millions of USD)") +
  scale_color_manual(values = c("Brazil" = "red", "Russian Federation" = "blue", "India" = "green", "China" = "purple", "South Africa" = "orange")) +
  theme_light() +
  scale_x_continuous(name = "Year", breaks = unique(commodities_data_brics$year), limits = c(1992, 2021), expand = c(0, 0)) +
  theme(axis.text.x = element_text(size = 5),
        legend.position = "bottom")
dev.off()


#TRADE WITH EACH OF THE BRICS (GROWTH)

# Filter the data by the specified conditions and group and summarize the data
commodities_data_brics <- commods_8822 %>% 
  filter(afripart == "Africa", imporex == "Import", year >= 1992, year <= 2021, reporter %in% c("Brazil", "Russian Federation", "India", "China", "South Africa")) %>% 
  group_by(reporter, afripart, year) %>% 
  summarize(trade_mill = sum(trade_value)/1000000) %>% 
  mutate(yoy_growth = 100*(trade_mill - lag(trade_mill, default = first(trade_mill))) / lag(trade_mill, default = first(trade_mill))) %>%
  ungroup()

# Create the plot
tiff('C:/songwe_graphs/brics_trend_each_exp_growth.jpeg', units="cm", width=25, height=14, res=300, compression = 'lzw')
ggplot(commodities_data_brics, aes(x = year, y = yoy_growth, group = reporter, color = reporter)) +
  geom_line(linewidth=1.2) +
  labs(title = "Growth of Africa Exports to BRICS Countries",
       x = "Year",
       y = "Growth rate") +
  scale_color_manual(values = c("Brazil" = "red", "Russian Federation" = "blue", "India" = "green", "China" = "purple", "South Africa" = "orange")) +
  theme_light() +
  scale_x_continuous(name = "Year", breaks = unique(commodities_data_brics$year), limits = c(1992, 2021), expand = c(0, 0)) +
  theme(axis.text.x = element_text(size = 5),
        legend.position = "bottom")
dev.off()



#AFRICA-CHINA, AFRICA-USA EXPORTS
commods_8822 %>%
  filter(afrirep %in% c("Africa", "Africa") & 
           imporex %in% c("Export") &
           partner %in% c("China", "USA") & 
           year_bands %in% c("1992-2001", "2002-2011", "2012-2021")) %>%
  group_by(year_bands, commodity, partner) %>%
  summarise(trade_mill = sum(trade_value)/1000000) %>%
  arrange(desc(trade_mill)) %>%
  group_by(year_bands, partner) %>%
  slice_head(n = 5)%>%
  ggplot(aes(x = year_bands, y = trade_mill, fill = commodity)) +
  geom_bar(stat = "identity", position = "stack") +
  facet_wrap(~partner, scales = "free_y") +
  labs(title = "Top-5 Exported commodities from Africa-China and Africa-USA",
       x = "Year Bands", y = "Trade Value",
       fill = "Commodity") +
  theme_bw()


library(tidyverse)

# filter data for Africa-China and Africa-USA trade between 1992 and 2021
commodities_data_filtered <- commods_8822 %>% 
  filter(afrirep %in% c("Africa") & 
           partner %in% c("China", "USA") & 
           year >= 1992 & year <= 2021)

# calculate year-on-year growth rates for Africa-China and Africa-USA trade
commodities_data_filtered_growth <- commodities_data_filtered %>%
  group_by(afrirep, partner, year, commodity) %>%
  summarise(trade_value = sum(trade_value)) %>%
  group_by(afrirep, partner, commodity) %>%
  mutate(yoy_growth = (trade_value - lag(trade_value, default = first(trade_value))) / lag(trade_value, default = first(trade_value))) %>%
  ungroup()

# plot the year-on-year growth rates as trend lines
ggplot(commodities_data_filtered_growth, aes(x = year, y = yoy_growth, color = partner, group = partner)) +
  geom_line() +
  facet_wrap(~commodity, scales = "free_y", ncol = 2) +
  labs(x = "Year", y = "Year-on-Year Growth Rate", title = "Year-on-Year Growth Rates of Commodities between Africa-China and Africa-USA Trade") +
  theme_bw()



#REDUCE CASES
# filter data for Africa-China and Africa-USA trade between 1992 and 2021
commodities_data_filtered <- commods_8822 %>% 
  filter(afrirep %in% c("Africa") & 
           partner %in% c("China", "USA") & 
           year >= 1992 & year <= 2021)

# calculate year-on-year growth rates for Africa-China and Africa-USA trade
commodities_data_filtered_growth <- commodities_data_filtered %>%
  group_by(afrirep, partner, year, commodity) %>%
  summarise(trade_value = sum(trade_value)) %>%
  group_by(afrirep, partner, commodity) %>%
  mutate(yoy_growth = (trade_value - lag(trade_value, default = first(trade_value))) / lag(trade_value, default = first(trade_value))*100) %>%
  filter(all(!is.na(yoy_growth) & year %in% 1992:2021)) %>%
  ungroup()

# plot the year-on-year growth rates as trend lines
ggplot(commodities_data_filtered_growth, aes(x = year, y = yoy_growth, color = partner, group = partner)) +
  geom_line() +
  facet_wrap(~commodity, scales = "free_y", ncol = 2) +
  labs(x = "Year", y = "Year-on-Year Growth Rate", title = "Year-on-Year Growth Rates of Top 5 Most Traded Commodities between Africa-China and Africa-USA Trade") +
  theme_bw()

#HEAT MAP
# filter data for Africa-China and Africa-USA trade between 1992 and 2021
commodities_data_filtered <- commods_8822 %>% 
  filter(afrirep %in% c("Africa") & 
           partner %in% c("China", "USA") & 
           year >= 1992 & year <= 2021)

# calculate year-on-year growth rates for Africa-China and Africa-USA trade
commodities_data_filtered_growth <- commodities_data_filtered %>%
  group_by(afrirep, partner, year, commodity) %>%
  summarise(trade_value = sum(trade_value)) %>%
  group_by(afrirep, partner, commodity) %>%
  mutate(yoy_growth = (trade_value - lag(trade_value, default = first(trade_value))) / lag(trade_value, default = first(trade_value))*100) %>%
  filter(!is.na(yoy_growth) & year %in% 1992:2021) %>%
  ungroup()

# create separate datasets for each combination of partner and commodity
commodities_data_filtered_growth_china <- commodities_data_filtered_growth %>%
  filter(partner == "China")
commodities_data_filtered_growth_usa <- commodities_data_filtered_growth %>%
  filter(partner == "USA")

plots <- list()

for (commodity in unique(commodities_data_filtered_growth$commodity)) {
  plot_data_china <- commodities_data_filtered_growth_china %>%
    filter(commodity == .data$commodity)
  plot_data_usa <- commodities_data_filtered_growth_usa %>%
    filter(commodity == .data$commodity)
  
  p <- ggplot() +
    geom_point(data = plot_data_china, aes(x = yoy_growth, y = lag(yoy_growth), color = "Africa-China")) +
    geom_point(data = plot_data_usa, aes(x = yoy_growth, y = lag(yoy_growth), color = "Africa-USA")) +
    geom_smooth(method =lm, se=FALSE )
    labs(x = "YoY Growth Rate: Africa-USA", y = "YoY Growth Rate: Africa-China", title = paste("Trade in", commodity)) +
    theme_bw()
  
  plots[[commodity]] <- p
}

# combine the plots into one using the patchwork package
library(patchwork)
wrap_plots <- wrap_plots(plots[[1]], plots[[2]], plots[[3]], plots[[4]], plots[[5]], ncol = 2)
wrap_plots


#MODIFY CODE
library(dplyr)

# filter data for Africa-USA trade between 1992 and 2021
africa_usa_trade <- commods_8822 %>% 
  filter(afrirep == "Africa" & partner == "USA" & year >= 1992 & year <= 2021)

# group data by commodity and calculate total trade value for each commodity
commodity_totals <- africa_usa_trade %>% 
  group_by(commodity) %>% 
  summarise(total_trade_value = sum(trade_value))

# sort data in descending order of trade value and select top 5 commodities
top_5_commodities <- commodity_totals %>% 
  arrange(desc(total_trade_value)) %>% 
  slice(1:5) %>% 
  pull(commodity)

top_5_commodities



#NEW GRAPH, REDUCED
# filter data for Africa-China and Africa-USA trade between 1992 and 2021
commodities_data_filtered <- commods_8822 %>% 
  filter(afrirep %in% c("Africa") & 
           partner %in% c("China", "USA") & 
           year >= 1992 & year <= 2021)

# calculate year-on-year growth rates for Africa-China and Africa-USA trade
commodities_data_filtered_growth <- commodities_data_filtered %>%
  group_by(afrirep, partner, year, commodity) %>%
  summarise(trade_value = sum(trade_value)) %>%
  group_by(afrirep, partner, commodity) %>%
  mutate(yoy_growth = (trade_value - lag(trade_value, default = first(trade_value))) / lag(trade_value, default = first(trade_value))*100) %>%
  ungroup()

# extract top 5 commodities
top_5_commodities <- commodities_data_filtered_growth %>%
  group_by(commodity) %>%
  summarise(total_trade_value = sum(trade_value)) %>%
  arrange(desc(total_trade_value)) %>%
  slice(1:5) %>%
  pull(commodity)

# plot the year-on-year growth rates for the top 5 commodities as trend lines
ggplot(commodities_data_filtered_growth %>% filter(commodity %in% top_5_commodities), aes(x = year, y = yoy_growth, color = partner, group = partner)) +
  geom_line() +
  facet_wrap(~commodity, scales = "free_y", ncol = 2) +
  labs(x = "Year", y = "Year-on-Year Growth Rate (%)", title = "Year-on-Year Growth Rates of Top 5 Commodities between Africa-China and Africa-USA Trade") +
  theme_bw()


#AFRICA-FOCUSED ANALYSIS
#colnames(commods_8822)

library(tidyverse)

# Filter for the relevant data
commods_8822_filtered <- commods_8822 %>% 
  filter(rep_cont == "Africa", part_cont == "Africa", year >= 1992)

# Create a new column for year-on-year growth rates
commods_8822_filtered_growth_imp <- commods_8822_filtered %>% 
  filter(imporex=="Import") %>% 
  group_by(imporex, year) %>% 
  summarise(trade_value = sum(trade_value)) %>% 
  mutate(yoy_growth = ((trade_value/lag(trade_value, default = first(trade_value)))-1)*100)

# Create a new column for year-on-year growth rates
commods_8822_filtered_growth_exp <- commods_8822_filtered %>% 
  filter(imporex=="Export") %>% 
  group_by(imporex, year) %>% 
  summarise(trade_value = sum(trade_value)) %>% 
  mutate(yoy_growth = ((trade_value/lag(trade_value, default = first(trade_value)))-1)*100)


# Create the plot
# Combine the data frames
commods_8822_filtered_growth <- rbind(commods_8822_filtered_growth_imp, commods_8822_filtered_growth_exp)

# Create the plot
tiff('C:/songwe_graphs/intraafricayoy.jpeg', units="cm", width=25, height=14, res=400, compression = 'lzw')
ggplot(data = commods_8822_filtered_growth, aes(x = year, y = yoy_growth, color = imporex)) +
  geom_line(size = 1.2) +
  scale_color_manual(values = c("blue", "red")) +
  labs(x = "Year", 
       y = "Year-on-Year Growth Rate (%)", 
       color = "Trade Direction",
       title="Growth of Intra-African Imports and Exports 1992-2021") +
  theme_light()+
  scale_x_continuous(name = "Year", breaks = unique(commods_8822_filtered_growth$year), limits = c(1992, 2021), expand = c(0, 0))+
  theme(axis.text.x = element_text(size = 5),
        legend.position = "bottom")
  dev.off()

  
  
#AFRICAN COUNTRIES-CIRCULAR PLOT  
  library(tidyverse)
  
  # Load dataset

  # Subset dataset for African countries and imports
  df_africa_imp <- subset(commods_8822, rep_cont == "Africa" & part_cont=="Africa" & imporex == "Import")
  df_africa_world <- subset(commods_8822, rep_cont == "Africa" & partner=="World" & imporex == "Import")
  
  # Summarize trade values by reporter
  df_sum <- aggregate(trade_value ~ reporter, data = df_africa_imp, sum)
  colnames(df_sum)[2] <- "import_africa"
  
  df_wld <- aggregate(trade_value ~ reporter, data = df_africa_world, sum)
  colnames(df_wld)[2] <- "import_world"
  
  # bind the dataset
  df_share_af <- cbind(df_sum["reporter"], df_sum["import_africa"], df_wld["import_world"]) %>% 
    mutate(africa_share=(import_africa/import_world)*100)

  # Add an id column
  df_share_af$id <- seq(1, nrow(df_share_af))
  
  # Get the name and the y position of each label
  label_data <- df_share_af
  number_of_bar <- nrow(label_data)
  angle <- 90 - 360 * (label_data$id - 0.5) / number_of_bar # Substract 0.5 to center the labels
  label_data$hjust <- ifelse(angle < -90, 1, 0)
  label_data$angle <- ifelse(angle < -90, angle+180, angle)
  
  # Create circular barplot
  # Make the plot
  tiff('C:/songwe_graphs/circle_intraafrica.jpeg', units="cm", width=12, height=12, res=400, compression = 'lzw')
  ggplot(df_share_af, aes(x=as.factor(id), y=africa_share)) +       # Note that id is a factor. If x is numeric, there is some space between the first bar
    geom_bar(stat="identity", fill=alpha("#00BFFF", 0.7)) +
    ylim(-100,120) +
    theme_minimal() +
    theme(
      axis.text = element_blank(),
      axis.title = element_blank(),
      panel.grid = element_blank(),
      plot.margin = unit(rep(-1,4), "cm") 
    ) +
    coord_polar(start = 0) + 
    geom_text(data=label_data, aes(x=id, y=africa_share, label=reporter, hjust=hjust), color="black", fontface="bold",alpha=0.6, size=2.5, angle= label_data$angle, inherit.aes = FALSE )+
    annotate("text", x=0, y=-99, label="Volume of \n Intra-African \n Trade", size=2, color="black", fontface="bold")+ 
    labs(title = "Circular Barplot of Import Trade Value for African Countries", x = NULL, y = NULL)
    dev.off()
  
#STACKED BAR LETS SEE  
  # Summarize trade values by reporter for import from Africa
  df_sum <- aggregate(trade_value ~ reporter, data = df_africa_imp, sum) %>% 
    mutate(variable="import_africa")
  
  # Summarize trade values by reporter for import from the world
  df_wld <- aggregate(trade_value ~ reporter, data = df_africa_world, sum) %>% 
    mutate(variable="import_world")
  
  # Bind the two datasets
  df_bound <- rbind(df_sum, df_wld)
  
  # Add an id column
  df_bound$id <- seq(1, nrow(df_bound))
  
  # Get the name and the y position of each label
  label_data <- df_bound
  number_of_bar <- nrow(label_data)
  angle <- 90 - 360 * (label_data$id - 0.5) / number_of_bar # Substract 0.5 to center the labels
  label_data$hjust <- ifelse(angle < -90, 1, 0)
  label_data$angle <- ifelse(angle < -90, angle+180, angle)
  
  # Create circular barplot
  # Make the plot
p<-ggplot(df_bound, aes(x=as.factor(reporter), y=trade_value, fill=variable)) +
    geom_bar(stat="identity", position="fill", alpha=0.7) +
    theme_minimal() +
    theme(
      axis.text = element_blank(),
      axis.title = element_blank(),
      panel.grid = element_blank(),
      plot.margin = unit(rep(-1, 4), "cm") 
    )  +
    coord_polar(start=0) + 
    geom_text(data=label_data, aes(x=id, y=trade_value, label=reporter, hjust=hjust), color="black", fontface="bold", alpha=0.6, size=2.5, angle=label_data$angle, inherit.aes=FALSE) +
    annotate("text", x=0, y=-99, label="Circle", size=8, color="black", fontface="bold") +
    scale_fill_manual(values=c("#00BFFF", "#bfbfbf"), name=NULL) # Define fill colors and legend title
  
  # Add plot title and axis labels
  p + labs(title="Circular Barplot of Import Trade Value for African Countries", x=NULL, y=NULL)
  
  
  
  #STACKED PLOTS WITH FILL
  africa_imp<- subset(commods_8822, rep_cont == "Africa" & part_cont=="Africa" & imporex == "Import")
  africa_exp<- subset(commods_8822, rep_cont == "Africa" & part_cont=="Africa" & imporex == "Export")

  
  
  # Summarize trade values by reporter for import from Africa
  df_imp <- africa_imp %>% 
    select(reporter, year_bands, trade_value) %>% 
    group_by(reporter, year_bands) %>% 
    summarise(trade_value=sum(trade_value), variable="import_africa")
    
  
  # Summarize trade values by reporter for import from the world
  df_exp <- africa_exp %>% 
    select(reporter, year_bands, trade_value) %>% 
    group_by(reporter, year_bands) %>% 
    summarise(trade_value=sum(trade_value), variable="export_africa")
  
bound_africa<-rbind(df_imp,df_exp)
  
  # Create stacked barplot
tiff('C:/songwe_graphs/intraafrica_all.jpeg', units="cm", width=29, height=16, res=400, compression = 'lzw')
bound_africa %>% 
  filter(year_bands!="1990-1991" & year_bands!="2022" ) %>% 
  ggplot(aes(x = as.factor(reporter), y = trade_value/1000000, fill = variable)) +
    geom_bar(stat = "identity", position = "stack") +
    theme_minimal() +
    labs(title = "Stacked Barplot of Intra-African Trade, with RSA",
         x = "Reporter", y = "Trade Value",
         fill = "Trade Type")+
    coord_flip()+
   facet_wrap(~year_bands)
dev.off()


#STACK PLOT WITHOUT RSA
tiff('C:/songwe_graphs/intraafrica_norsa.jpeg', units="cm", width=29, height=16, res=400, compression = 'lzw')
bound_africa %>%
  filter(year_bands != "1990-1991" & year_bands != "2022" & reporter != "South Africa") %>%
  ggplot(aes(x = as.factor(reporter), y = trade_value/1000000, fill = variable)) +
  geom_bar(stat = "identity", position = "stack") +
  theme_minimal() +
  labs(title = "Stacked Barplot of Intra-African Trade-Excl. RSA",
       x = "Reporter", y = "Trade Value (in millions)",
       fill = "Trade Type") +
  coord_flip() +
  facet_wrap(~year_bands)
dev.off()

# Create stacked barplot using percentage shares
tiff('C:/songwe_graphs/intraafricaper.jpeg', units="cm", width=29, height=16, res=400, compression = 'lzw')
bound_africa %>% 
  filter(year_bands!="1990-1991" & year_bands!="2022" ) %>% 
  ggplot(aes(x = as.factor(reporter), y = trade_value/1000000, fill = variable)) +
  geom_bar(stat = "identity", position = "fill") +
  theme_minimal() +
  labs(title = "Stacked Barplot of Intra-African Trade, Proportions of Imports and Exports",
       x = "Reporter", y = "Trade Value",
       fill = "Trade Type")+
  coord_flip()+
  facet_wrap(~year_bands)
dev.off()


#EXCLUDING NAMIBIA AND NIGERIA
tiff('C:/songwe_graphs/intraafrica_excl_nam_nig.jpeg', units="cm", width=29, height=16, res=400, compression = 'lzw')
bound_africa %>%
  filter(year_bands != "1990-1991" & year_bands != "2022" & reporter != "South Africa" & reporter != "Nigeria" & reporter != "Namibia") %>%
  ggplot(aes(x = as.factor(reporter), y = trade_value/1000000, fill = variable)) +
  geom_bar(stat = "identity", position = "stack") +
  theme_minimal() +
  labs(title = "Stacked Barplot of Intra-African Trade-Excl. RSA, Nigeria & Namibia",
       x = "Reporter", y = "Trade Value (in millions)",
       fill = "Trade Type") +
  coord_flip() +
  facet_wrap(~year_bands)
dev.off()


afr_wld_exp<-commods_8822 %>% 
         filter(rep_cont=="Africa" & part_cont=="World" & imporex=="Export") %>% 
         select(reporter, year_bands, rep_cont, part_cont, imporex, trade_value) %>% 
         group_by(reporter, year_bands) %>% 
         summarise(trade_value=sum(trade_value), imporex="Africa_Exports")

wrld_afr_imp<-commods_8822 %>% 
  filter(part_cont=="Africa" & imporex=="Import") %>% 
  select(reporter, year_bands, rep_cont, part_cont, partner, imporex, trade_value) %>% 
  group_by(partner, year_bands) %>% 
  summarise(trade_value=sum(trade_value), imporex="World_Imports", reporter=partner)

africa_mirror<-rbind(afr_wld_exp, wrld_afr_imp)


#INTRA-AFRICAN GROSS TRADE
plot_dat_top5<- commods_8822 %>%
  filter(rep_cont == "Africa" & part_cont == "Africa" & year_bands != "1990-1991" & year_bands != "2022") %>%
  group_by(year_bands, commodity) %>%
  summarize(total_trade_value = sum(trade_value)) %>%
  ungroup() %>%
  arrange(desc(total_trade_value)) %>%
  group_by(year_bands) %>%
  top_n(5, total_trade_value)


tiff('C:/songwe_graphs/intraafrica_commodities.jpeg', units="cm", width=29, height=16, res=400, compression = 'lzw')
  ggplot(plot_dat_top5, aes(x = year_bands, y = total_trade_value/1000000, fill = commodity)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = "Top 5 Commodities Traded between Africa Countries",
       x = "Year", y = "Trade Value (in millions)",
       fill = "Commodity") +
  facet_wrap(~year_bands, scales = "free_x") +
  theme(legend.position = "right")
  dev.off()
  
  
  #INTRA-AFRICAN EXPORT
  plot_exp_top5<- commods_8822 %>%
    filter(rep_cont == "Africa" & part_cont == "Africa" & year_bands != "1990-1991" & year_bands != "2022" & imporex=="Export") %>%
    group_by(year_bands, commodity) %>%
    summarize(total_trade_value = sum(trade_value)) %>%
    ungroup() %>%
    arrange(desc(total_trade_value)) %>%
    group_by(year_bands) %>%
    top_n(5, total_trade_value)
  
  
  tiff('C:/songwe_graphs/intraafrica_exports.jpeg', units="cm", width=29, height=16, res=400, compression = 'lzw')
  ggplot(plot_exp_top5, aes(x = year_bands, y = total_trade_value/1000000, fill = commodity)) +
    geom_bar(stat = "identity") +
    theme_light() +
    labs(title = "Top 5 Commodities Exported from African countries to Africa",
         x = "Year", y = "Trade Value (in millions)",
         fill = "Commodity") +
    facet_wrap(~year_bands, scales = "free_x") +
    theme(legend.position = "right")
  dev.off()

  
  
  #AFRICAN EXPORTS TO USA
  plot_exp_top5usa<- commods_8822 %>%
    filter(rep_cont == "Africa" & partner == "USA" & year_bands != "1990-1991" & year_bands != "2022" & imporex=="Export") %>%
    group_by(year_bands, commodity) %>%
    summarize(total_trade_value = sum(trade_value)) %>%
    ungroup() %>%
    arrange(desc(total_trade_value)) %>%
    group_by(year_bands) %>%
    top_n(5, total_trade_value)
  
  
  tiff('C:/songwe_graphs/africa_usa_exports.jpeg', units="cm", width=29, height=16, res=400, compression = 'lzw')
  ggplot(plot_exp_top5usa, aes(x = year_bands, y = total_trade_value/1000000, fill = commodity)) +
    geom_bar(stat = "identity") +
    theme_light() +
    labs(title = "Top 5 Commodities Exported from African countries to USA",
         x = "Year", y = "Trade Value (in millions)",
         fill = "Commodity") +
    facet_wrap(~year_bands, scales = "free_x") +
    theme(legend.position = "right")
  dev.off()
  
  
  #AFRICAN EXPORTS TO CHINA
  plot_exp_top5<- commods_8822 %>%
    filter(rep_cont == "Africa" & partner == "China" & year_bands != "1990-1991" & year_bands != "2022" & imporex=="Export") %>%
    group_by(year_bands, commodity) %>%
    summarize(total_trade_value = sum(trade_value)) %>%
    ungroup() %>%
    arrange(desc(total_trade_value)) %>%
    group_by(year_bands) %>%
    top_n(5, total_trade_value)
  
  
  tiff('C:/songwe_graphs/africa_china_exports.jpeg', units="cm", width=29, height=16, res=400, compression = 'lzw')
  ggplot(plot_exp_top5, aes(x = year_bands, y = total_trade_value/1000000, fill = commodity)) +
    geom_bar(stat = "identity") +
    theme_light() +
    labs(title = "Top 5 Commodities Exported from African countries to China",
         x = "Year", y = "Trade Value (in millions)",
         fill = "Commodity") +
    facet_wrap(~year_bands, scales = "free_x") +
    theme(legend.position = "right")
  dev.off()
  
  
  #SCATTERPLOT OF AFRICAN EXPORTS TO CHINA vs USA
  library(tidyverse)
  
  scatt_usa_china<- commods_8822 %>%
    filter(commodity == "Mineral fuels, lubricants and related materials", imporex == "Export", 
           rep_cont == "Africa", partner %in% c("USA", "China")) %>%
    group_by(year_bands, reporter, commodity, partner) %>%
    summarise(trade_value = sum(trade_value))
  
  africa_usa_exports<-scatt_usa_china %>% 
                      filter(partner=="USA") %>% 
                      summarise(trade_value_usa=trade_value/1000000) %>% 
                      unite(year_reporter, year_bands, reporter, sep = "_", remove = FALSE)
  
  africa_china_exports<-scatt_usa_china %>% 
                      filter(partner=="China") %>% 
                      summarise(trade_value_china=trade_value/1000000)  %>% 
                      unite(year_reporter, year_bands, reporter, sep = "_", remove = FALSE)
  
china_usa_combined <- left_join(africa_usa_exports, africa_china_exports, by = "year_reporter")



 
    ggplot(china_usa_combined, aes(x = trade_value_china, y = trade_value_usa)) +
    geom_point() +
    theme_minimal() +
    labs(title = "Export of Mineral fuels, lubricants and related materials",
         x = "Export to China (in millions)",
         y = "Export to USA (in millions)")
    
    
    
    #BILATERAL TRADE PATTERNS
    #Examine bilateral trade patterns for India, USA, Japan, China, Germany, France:
    # Load necessary libraries
    library(ggplot2)
    library(dplyr)
    library(lubridate)
    library(gridExtra)

    # Filter data based on the given conditions and create a new variable
    bilateral_data <- commods_8822 %>% 
      filter(imporex == "Export",
             year >= 1992,
             year <= 2021,
             reporter %in% c("India", "USA", "Japan", "China", "Germany", "France"),
             partner %in% c("India", "USA", "Japan", "China", "Germany", "France")) %>%
      mutate(trade_bill = trade_value / 1000000000)
    
    # Sum trade_value by reporter, partner, and year
    bilateral_sum_data <- bilateral_data %>% 
      group_by(reporter, partner, year) %>% 
      summarise(trade_sum = sum(trade_bill))
    
    #USA DATA TRENDS
    usa_data <- commods_8822 %>% 
      filter(imporex == "Import",
             year >= 2013,
             year <= 2021,
             reporter %in% c("India", "Japan", "China", "Germany", "France"),
             partner %in% c("USA"))  %>%
      mutate(trade_bill = trade_value / 1000000000)
    
    # Sum trade_value by reporter, partner, and year
    bilateral_usa_data <- usa_data %>% 
      group_by(partner, reporter, year) %>% 
      summarise(trade_sum = sum(trade_bill)) %>%  
      ungroup()  # ungroup to ensure rename works properly
    
    # Now swap the 'partner' and 'reporter' names
    bilateral_usa_data <- bilateral_usa_data %>%
      rename(reporter = partner,
             partner = reporter)
    
    #merge bilateral data
    merged_bilateral <- rbind(bilateral_sum_data, bilateral_usa_data) %>% 
      arrange(reporter, partner, year)
    
    # Create the facet grid plot
    ggplot(merged_bilateral, aes(x=year, y=trade_sum, color=partner)) +
      geom_line() +
      facet_grid(partner~reporter, scales = "free") +
      labs(x = "Year",
           y = "Export Value (USD Billions)",
           color = "Destination Country \n (Partner)") +
      scale_x_continuous(breaks = seq(1992, 2022, 5))+
      theme_light()+
      theme(axis.text.x = element_text(angle =0, vjust = 0.5, hjust=1, size=5))
  
    
    
   