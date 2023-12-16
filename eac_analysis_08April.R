#install.packages("ggplot2")
install.packages("vctrs")


library(readr)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(magrittr)
library(gridExtra)
library(stargazer)
library(xtable)
library(ggpubr)
library(tidyr)
library(ggrepel)

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

unique(commods_8822$reporter)

#GENERATE AFRICA AS A REPORTER
commods_8822$afrirep<-ifelse(commods_8822$rep_cont=="Africa","Africa", commods_8822$reporter )
commods_8822$afripart<-ifelse(commods_8822$part_cont=="Africa","Africa", commods_8822$partner)


#GENERATE EAC AS A VARIABLE AND NON-EAC
#unique(commods_8822$reporter)

# Create a vector of EAC countries
eac_countries <- c("Kenya", "United Rep. of Tanzania", "Uganda", "Rwanda", "Burundi", "South Sudan", "Dem. Rep. of the Congo")

# Create the rep_eac variable
commods_8822 <- commods_8822 %>%
  mutate(rep_eac = ifelse(reporter %in% eac_countries, "EAC", "NON-EAC"))


#Create the part_EAC variable
commods_8822 <- commods_8822 %>%
  mutate(part_eac = ifelse(partner %in% eac_countries, "EAC", "NON-EAC"))


# Create a dataset of EAC countries and their joining years
eac_joining_years <- data.frame(
  country = c("Kenya", "United Rep. of Tanzania" , "Uganda", "Rwanda", "Burundi", "South Sudan", "Dem. Rep. of the Congo"),
  joining_year = c(1999, 1999, 1999, 2007, 2007, 2016, 2022)
)

# Add the joining_year variable to commods_8822
commods_8822 <- commods_8822 %>%
  left_join(eac_joining_years, by = c("reporter" = "country"))

# Create the join_eac variable
commods_8822 <- commods_8822 %>%
  mutate(join_eac = ifelse(rep_eac == "EAC" & year == joining_year, 
                           "Joined EAC",
                           "Not applicable"))


unique(commods_8822$reporter[commods_8822$rep_eac=="EAC"])


#PLOTTING TREND OF EAC AND AFRICA WITH JOINING YEAR

# Filter the dataset
filtered_data <- commods_8822 %>%
  filter(rep_eac == "EAC", part_cont == "Africa", part_eac != "EAC", imporex == "Export") %>% 
  group_by(rep_eac, reporter, join_eac, year) %>% 
  summarize(trade_mill = sum(trade_value)/1000000)

# Plot the trend graph
ggplot(filtered_data, aes(x = year, y = trade_mill, color = reporter)) +
  geom_line(size = 1) +
  geom_point(data = filtered_data %>% filter(join_eac == "Joined EAC"),
             aes(shape = join_eac), size = 3) +
  scale_shape_manual(values = c("Joined EAC" = 16)) +
    labs(
    title = "EAC Countries' Exports to Non-EAC African Countries",
    x = "Year",
    y = "Export Value",
    shape = "Join EAC"
  ) +
  theme_minimal()


# Create the first plot (excluding DRC)
plot1 <- ggplot(filtered_data %>% filter(reporter != "Dem. Rep. of the Congo"),
                aes(x = year, y = trade_mill, color = reporter)) +
  geom_line(size=1) +
  geom_point(data = filtered_data %>% filter(join_eac == "Joined EAC" & reporter != "Dem. Rep. of the Congo"),
             aes(shape = join_eac), size = 3) +
    scale_shape_manual(values = c("Joined EAC" = 1)) +
  labs(
    title = "EAC Countries' Exports to Non-EAC African Countries (Excluding DRC)",
    x = "Year",
    y = "Export Value (Millions of USD)",
    shape = "Join EAC"
  ) +
  theme_minimal()

# Create the second plot (DRC only)
plot2 <- ggplot(filtered_data %>% filter(reporter == "Dem. Rep. of the Congo"),
                aes(x = year, y = trade_mill, color = reporter)) +
  geom_line() +
  geom_point(data = filtered_data %>% filter(join_eac == "Joined EAC" & reporter == "Dem. Rep. of the Congo"),
             aes(shape = join_eac), size = 3) +
  scale_shape_manual(values = c("Joined EAC" = 1)) +
  labs(
    title = "DRC Exports to Non-EAC African Countries",
    x = "Year",
    y = "Export Value (Millions of USD)",
    shape = "Join EAC"
  ) +
  theme_minimal()

# Arrange the plots side by side
grid.arrange(plot1, plot2, ncol = 2)





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



# Filter data for the three categories
eac_data <- commods_8822 %>%
  filter(rep_eac == "EAC", imporex=="Export") %>%
  mutate(category = case_when(
    part_eac == "EAC" ~ "EAC",
    part_cont == "Africa" ~ "Africa",
    TRUE ~ "World"
  ))


# Calculate the sum of the export values for each category by year
eac_summary <- eac_data %>%
  group_by(category,year) %>%
  summarise(trade_mill = sum(trade_value)/1000000) %>%
  ungroup()

# Create the line plot
ggplot(eac_summary, aes(x = year, y = trade_mill, color = category)) +
  geom_line(size = 1) +
  geom_vline(xintercept = 1999, linetype = "dotted", size = 1) +
  geom_text(aes(x = 2001, y = max(trade_mill), label = "EAC Established by \n Kenya, Uganda, Tanzania"), hjust = 1.2, vjust = 1, size = 4) +
  geom_vline(xintercept = 2007, linetype = "dotted", size = 1) +
  geom_text(aes(x = 2009, y = max(trade_mill), label = "Two-members join \n Rwanda & Burundi"), hjust = 1.2, vjust = 1, size = 4) +
  geom_vline(xintercept = 2016, linetype = "dotted", size = 1) +
  geom_text(aes(x = 2018, y = max(trade_mill), label = "South Sudan joins"), hjust = 1.2, vjust = 1, size = 4) +
  geom_vline(xintercept = 2022, linetype = "dotted", size = 1) +
  geom_text(aes(x = 2023, y = max(trade_mill), label = "DRC joins"), hjust = 1.2, vjust = 1, size = 4) +
  labs(
    title = "EAC Exports to EAC, Africa, and the World",
    x = "Year",
    y = "Export Value"
  ) +
  theme_minimal()



#STACKED PLOTS WITH FILL
eac_imp<- subset(commods_8822, rep_eac == "EAC" & part_eac=="EAC" & imporex == "Import")
eac_exp<- subset(commods_8822, rep_eac == "EAC" & part_eac=="EAC" & imporex == "Export")

# Summarize trade values by reporter for import from EAC
df_imp_eac <- eac_imp %>% 
  select(reporter, year_bands, trade_value) %>% 
  group_by(reporter, year_bands) %>% 
  summarise(trade_value=sum(trade_value), variable="import_eac")

# Summarize trade values by reporter for export to EAC
df_exp_eac <- eac_exp %>% 
  select(reporter, year_bands, trade_value) %>% 
  group_by(reporter, year_bands) %>% 
  summarise(trade_value=sum(trade_value), variable="export_eac")

bound_eac <- rbind(df_imp_eac, df_exp_eac)

# Create stacked barplot
tiff('C:/eac_graphs/intraeac_fill.jpeg', units="cm", width=29, height=16, res=400, compression = 'lzw')
bound_eac %>% 
  filter(year_bands != "1990-1991" & year_bands != "2022") %>% 
  ggplot(aes(x = as.factor(reporter), y = trade_value/1000000, fill = variable)) +
  geom_bar(stat = "identity", position = "stack") +
  theme_minimal() +
  labs(title = "Stacked Barplot of Intra-EAC Trade",
       x = "Reporter", y = "Trade Value",
       fill = "Trade Type") +
  coord_flip() +
  facet_wrap(~year_bands)+
  scale_fill_manual(values = c("import_eac" = "gold", "export_eac" = "blue"))
dev.off()


#TOP 10 COMMODITIES
# Filter the data for intra-EAC trade
intra_eac_trade <- commods_8822 %>%
  filter(rep_eac == "EAC" & part_eac == "EAC" & imporex == "Export")

# Group by commodity and calculate the sum of trade_value
commodity_exports <- intra_eac_trade %>%
  group_by(commodity) %>%
  summarise(export_value = sum(trade_value)/1000000)

# Order by descending trade_value and select the top 10 commodities
top10_commodity_exports <- commodity_exports %>%
  arrange(desc(export_value)) %>%
  head(10)

# Calculate the total EAC trade value
total_eac_trade_value <- sum(commodity_exports$export_value)

# Create a table with the required columns
top10_exports_table <- top10_commodity_exports %>%
  mutate(total_eac_trade_value = total_eac_trade_value,
         trade_share = (export_value / total_eac_trade_value) * 100)


top10_exports_table_rounded <- top10_exports_table %>%
  mutate(commodity = commodity,
         export_value = round(export_value, 2),
         total_eac_trade_value = round(total_eac_trade_value, 2),
         trade_share = round(trade_share, 2))


stargazer(top10_exports_table_rounded, summary = FALSE, title = "Top 10 EAC Exports", 
          label = "tab:top10_exports", 
          header = FALSE, 
          type = "latex", 
          align = TRUE, 
          column.sep.width = "5pt", 
          digits = 2,
          column.labels = c("Commodity", "Export Value (Millions of USD)", "Total EAC Trade Value (Millions of USD)", "Trade Share (%)"))


#TOP 10 COMMODITIES
# Filter the data for EAC to Africa trade
eac_to_africa_export <- commods_8822 %>%
  filter(rep_eac == "EAC" & part_cont == "Africa" & part_eac != "EAC" & imporex == "Export")

# Group by commodity and calculate the sum of trade_value
commodity_exports <- eac_to_africa_export %>%
  group_by(commodity) %>%
  summarise(export_value = sum(trade_value)/1000000)

# Order by descending trade_value and select the top 10 commodities
top10_commodity_exports <- commodity_exports %>%
  arrange(desc(export_value)) %>%
  head(10)

# Calculate the total EAC to Africa trade value
total_eac_to_africa_trade_value <- sum(commodity_exports$export_value)

# Create a table with the required columns
top10_exports_table <- top10_commodity_exports %>%
  mutate(total_eac_trade_value = total_eac_to_africa_trade_value,
         trade_share = (export_value / total_eac_to_africa_trade_value) * 100)

top10_exports_table_rounded <- top10_exports_table %>%
  mutate(commodity = commodity,
         export_value = round(export_value, 2),
         total_eac_trade_value = round(total_eac_trade_value, 2),
         trade_share = round(trade_share, 2))

stargazer(top10_exports_table_rounded, summary = FALSE, title = "Top 10 EAC to Africa Exports", 
          label = "tab:top10_exports", 
          header = FALSE, 
          type = "latex", 
          align = TRUE, 
          column.sep.width = "5pt", 
          digits = 2,
          column.labels = c("Commodity", "Export Value (Millions of USD)", "Total EAC to Africa Trade Value (Millions of USD)", "Trade Share (%)"))




#TOP 10 COUNTRIES TRADING WITH EAC
# filter the data to include only relevant rows
df_filtered <- commods_8822 %>%
  filter(rep_eac == "EAC", imporex == "Export") 

df_filtered <- df_filtered[df_filtered$year >= 1992 & df_filtered$year <= 2021, ]

# group the data by year_band and partner, and calculate the total trade_value
df_grouped <- df_filtered %>%
  group_by(year_bands, partner) %>%
  summarise(total_trade_value = sum(trade_value), trade_mill=total_trade_value/1000000)

# find the top 10 countries for each year_band based on total_trade_value
df_top10 <- df_grouped %>%
  group_by(year_bands) %>%
  top_n(10, trade_mill)

# create the bubble chart
ggplot(df_top10, aes(x = year_bands, y = partner, size = trade_mill, color = partner)) +
  geom_point() +
  scale_size(range = c(5, 25), labels = scales::unit_format(unit = "M", scale = 1e-3)) +
  scale_color_viridis_d() +
  labs(title = "",
       x = "Year Band", y = "Partner",
       size = "Merchandise Trade (Millions USD)", color = "Partner") +
  theme_minimal()

commods_8822 %>% 
  filter(rep_cont=="Africa") %>% 
unique(reporter)

#SCATTERPLOTS OF INTRA-EAC VERSUS EAC-AFRICA TRADE
# Filter data for EAC to EAC and EAC to Africa exports
eac_to_eac <- commods_8822 %>% filter(rep_eac == "EAC", part_eac == "EAC", imporex == "Export")
eac_to_africa <- commods_8822 %>% filter(rep_eac == "EAC", part_cont == "Africa", part_eac != "EAC", imporex == "Export")

# Group data by commodity and year_bands, and compute trade_mill
eac_to_eac_grouped <- eac_to_eac %>% 
  group_by(commodity, year_bands, year, reporter) %>% 
  summarise(trade_value = sum(trade_value), trade_mill = trade_value / 1000000)

eac_to_africa_grouped <- eac_to_africa %>% 
  group_by(commodity, year_bands, year, reporter) %>% 
  summarise(trade_value = sum(trade_value), trade_mill = trade_value / 1000000)

# Merge the datasets
scatterplot_data <- merge(eac_to_eac_grouped, eac_to_africa_grouped, by = c("commodity", "year_bands", "reporter", "year"), suffixes = c("_eac", "_africa"))

# Create scatterplots facet_wrapped by year_bands
scatterplot_data %>% 
  filter(trade_mill_eac <= 400, trade_mill_africa <= 2000) %>%
  ggplot(aes(x = trade_mill_eac, y = trade_mill_africa)) +
  geom_point(color="RED")+
  geom_smooth(method=lm, se=F)+
  #stat_cor(method="pearson")+
  labs(title = "Scatterplots of EAC to EAC vs EAC to Africa Exports",
       x = "EAC to EAC Exports (Millions USD)",
       y = "EAC to Africa Exports (Millions USD)") +
  facet_wrap(~year_bands) +
  theme_light()


unique(commods_8822$reporter)



#TFTA SCHENANIGANS

sadc_countries <- c("Angola", "Botswana", "Comoros", "Dem. Rep. of the Congo", "Eswatini", "Lesotho", "Madagascar", "Malawi", "Mauritius", "Mozambique", "Namibia", "Seychelles", "South Africa", "United Rep. of Tanzania", "Zambia", "Zimbabwe")
comesa_countries <- c("Burundi", "Comoros", "Dem. Rep. of the Congo", "Djibouti", "Egypt", "Eritrea", "Eswatini", "Ethiopia", "Kenya", "Libya", "Madagascar", "Malawi", "Mauritius", "Rwanda", "Seychelles", "Sudan", "Tunisia", "Uganda", "Zambia", "Zimbabwe")

commods_8822$sadc <- ifelse(commods_8822$partner %in% sadc_countries, "SADC", "NON-SADC")
commods_8822$comesa <- ifelse(commods_8822$partner %in% comesa_countries, "COMESA", "NON-COMESA")

tfta_countries <- c(sadc_countries, comesa_countries, "Burundi", "Kenya", "Rwanda", "United Rep. of Tanzania", "Uganda", "Dem. Rep. of the Congo")
commods_8822$tfta <- ifelse(commods_8822$partner %in% tfta_countries, "TFTA", "NON-TFTA")

#GRAPH FOR THE TFTA
#CREATING REGION_GROUP
commods_8822$tfta <- ifelse(commods_8822$sadc == "SADC", "TFTA",
                                    ifelse(commods_8822$comesa == "COMESA", "TFTA",
                                           ifelse(commods_8822$eac == "EAC", "TFTA", "NON-TFTA")))

#

#FILTERING THE DATA
# Create separate data frames for each region
sadc_data <- commods_8822 %>%
  filter(rep_eac == "EAC", part_cont == "Africa", imporex == "Export", sadc == "SADC") %>%
  group_by(sadc, year) %>%
  summarize(trade_mill = sum(trade_value)/1000000)

eac_data <- commods_8822 %>%
  filter(rep_eac == "EAC", part_cont == "Africa", imporex == "Export", part_eac == "EAC") %>%
  group_by(part_eac, year) %>%
  summarize(trade_mill = sum(trade_value)/1000000)

comesa_data <- commods_8822 %>%
  filter(rep_eac == "EAC", part_cont == "Africa", imporex == "Export", comesa == "COMESA") %>%
  group_by(comesa, year) %>%
  summarize(trade_mill = sum(trade_value)/1000000)

tfta_data <- commods_8822 %>%
  filter(rep_eac == "EAC", part_cont == "Africa", imporex == "Export", tfta == "TFTA") %>%
  group_by(tfta, year) %>%
  summarize(trade_mill = sum(trade_value)/1000000)

# Merge the data frames together
combined_data <- bind_rows(sadc_data, eac_data, comesa_data, tfta_data)

#combined_data$recs <- ifelse(combined_data$sadc == "SADC", "SADC",
                          #ifelse(combined_data$part_eac == "EAC", "EAC",
                            #ifelse(combined_data$comesa == "COMESA", "COMESA",
                               #ifelse(combined_data$tfta == "TFTA", "TFTA", "NON-TFTA"))))


combined_data$recs <- case_when(combined_data$sadc == "SADC"~ "SADC",
                                combined_data$part_eac == "EAC" ~ "EAC",
                                combined_data$comesa == "COMESA"~ "COMESA",
                                combined_data$tfta == "TFTA" ~ "TFTA")


#PLOT THE DATA
ggplot(combined_data, aes(x = year, y = trade_mill, color = recs)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  labs(
    title = "EAC Countries' Exports to SADC, COMESA, and TFTA Countries",
    x = "Year",
    y = "Export Value (Million USD)",
    color = "Region"
  ) +
  theme_minimal()



