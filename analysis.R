##Figures for PROFOR review report

setwd("~/Documents/github/forest_gov_pov/")
library(dplyr)
library(tidyr)
library(ggplot2)
library(reshape)
library(geojsonio)
library(maps)

data <- read.csv("SR_FinalDataset_190415_pr_updated2.csv",header=TRUE,stringsAsFactors = FALSE)

##Figure 7- direction of outcome vs. type of outcome (heatmap)
sub <- data %>% select(case_id,OutcomeDirection_income,OutcomeDirection_assets) %>% distinct()
colnames(sub) <- c("case_id","Income","Capital/Assets")
sub2 <- melt(sub,id="case_id")
sub2 <- sub2 %>% filter(!is.na(value))
df <- count(sub2, variable, value)

pdf("Figure7_Poverty_x_Direction.pdf",height=6,width=11)
ggplot(df, aes(value,variable)) +
  geom_tile(aes(fill=n), color="white") +
  geom_text(aes(label=n)) +
  scale_fill_gradient(name="Number\nof cases",low="white",high="steelblue") +
  ylab("Poverty dimension") +
  xlab("Direction of outcome") +
  theme(legend.title=element_text(size=10),
        legend.text=element_text(size=10),
        axis.title=element_text(size=16))
dev.off()

##Figure 7a - individual heat maps by funder type
fund <- data %>% select(case_id,International.funded..0.1.,Domestic.funded..0.1.,Mix.funded..0.1.) %>% distinct()
colnames(fund) <- c("case_id","International","Domestic","Mix")
fund2 <- melt(fund,id="case_id") %>% filter(value != 0) %>% distinct() %>% select(-value)
colnames(fund2) <- c("case_id","Funder")
df2 <- full_join(sub2,fund2,by="case_id")
plot <- count(df2,variable,value,Funder) %>% complete(variable,value,Funder)

pdf("Figure7a_Poverty_x_Direction_x_Funder.pdf",height=6,width=11)
ggplot(plot, aes(value,variable)) +
  geom_tile(aes(fill=n), color="white") +
  geom_text(aes(label=n)) +
  scale_fill_continuous(na.value="lightgray") +
  scale_fill_gradient(name="Number\nof cases",low="white",high="steelblue") +
  ylab("Poverty dimension") +
  xlab("Direction of outcome") +
  theme(legend.title=element_text(size=10),
        legend.text=element_text(size=10),
        axis.title=element_text(size=16)) +
  facet_grid(. ~ Funder)
dev.off()

#Figure X - bundle of rights x direction of outcome
##Assign bundle type
bundle <- data %>% select(case_id,Access,Withdrawal,Management,Exclusion,Alienation) %>% distinct()
bundle$type <- c("")

row <- c(1:nrow(bundle))
for(i in row){
  if(bundle$Access[i] == -1 & bundle$Withdrawal[i] == -1 & bundle$Management[i] == 0 & bundle$Exclusion[i] == 0 & bundle$Alienation[i] == 0){
    bundle$type[i] <- c("Access and withdrawal restricted")
  } else if (bundle$Access[i] == 1 & bundle$Withdrawal[i] == -1 & bundle$Management[i] == 0 & bundle$Exclusion[i] == 0 & bundle$Alienation[i] == 0){
    bundle$type[i] <- c("Access but with restricted withdrawal")
  } else if (bundle$Access[i] == 1 & bundle$Withdrawal[i] == 1 & bundle$Management[i] == 0 & bundle$Exclusion[i] == 0 & bundle$Alienation[i] == 0){
    bundle$type[i] <- c("Access+Withdrawal")
  } else if (bundle$Access[i] == 1 & bundle$Withdrawal[i] == 1 & bundle$Management[i] == 1 & bundle$Exclusion[i] == 0 & bundle$Alienation[i] == 0){
    bundle$type[i] <- c("Access+Withdrawal+Management")
  } else if (bundle$Access[i] == 1 & bundle$Withdrawal[i] == 1 & bundle$Management[i] == 1 & bundle$Exclusion[i] == 1 & bundle$Alienation[i] == 0){
    bundle$type[i] <- c("Access+Withdrawal+Management+Exclusion")
  } else if (bundle$Access[i] == 1 & bundle$Withdrawal[i] == 1 & bundle$Management[i] == 1 & bundle$Exclusion[i] == 1 & bundle$Alienation[i] == 1) {
    bundle$type[i] <- c("Full bundle")
  } else
    bundle$type[i] <- c("None")
}

##Create dataframe for plotting various heatmaps
bundle_type <- bundle %>% select(case_id,type) %>% distinct()
df3 <- full_join(df2,bundle_type,by="case_id")

plot1 <- count(df3,variable,value,type) %>% complete(variable,value,type)
plot2 <- count(df3,value,type,Funder) %>% complete(value,type,Funder)
plot3 <- count(df3,variable,type) %>% complete(variable,type)

##Figure 5 Poverty dimension x Bundle of rights
pdf("FigureX_Poverty_x_Bundle_060319.pdf", height=4.5, width=12)
ggplot(plot3, aes(type,variable)) +
  geom_tile(aes(fill=n), color="white") +
  geom_text(aes(label=n)) +
  scale_fill_continuous(na.value="lightgray") +
  scale_fill_gradient(name="Number\nof cases",low="white",high="steelblue") +
  scale_x_discrete(breaks=c("Access and withdrawal restricted","Access but with restricted withdrawal","Access+Withdrawal","Access+Withdrawal+Management","Access+Withdrawal+Management+Exclusion","Full bundle"), 
                   labels=c("None","Access + restricted withdrawal","Withdrawal","Management","Exclusion","Alienation")) +
  scale_y_discrete(breaks=c("Income","Capital/Assets"),
                   limits=c("Capital/Assets","Income"),
                   labels=c("Income/Consumption","Capital/Assets")) +
  ylab("Poverty dimension") +
  xlab("Property rights affected") +
  theme(legend.title=element_text(size=10),
        legend.text=element_text(size=10),
        axis.title=element_text(size=16),
        axis.text.x = element_text(angle=45,hjust=1))
dev.off()

pdf("FigureX_Poverty_x_Direction_x_Bundle_042619.pdf",height=4.5,width=12)
ggplot(plot1, aes(value,type)) +
  geom_tile(aes(fill=n), color="white") +
  geom_text(aes(label=n)) +
  scale_fill_continuous(na.value="lightgray") +
  scale_fill_gradient(name="Number\nof cases",low="white",high="steelblue") +
  ylab("Bundle of rights") +
  xlab("Direction of outcome") +
  theme(legend.title=element_text(size=10),
        legend.text=element_text(size=10),
        axis.title=element_text(size=16)) +
  facet_grid(. ~ variable)
dev.off()

pdf("FigureXa_Funder_x_Direction_x_Bundle_042619.pdf",height=4.5,width=12)
ggplot(plot2, aes(value,type)) +
  geom_tile(aes(fill=n), color="white") +
  geom_text(aes(label=n)) +
  scale_fill_continuous(na.value="lightgray") +
  scale_fill_gradient(name="Number\nof cases",low="white",high="steelblue") +
  ylab("Bundle of rights") +
  xlab("Direction of outcome") +
  theme(legend.title=element_text(size=10),
        legend.text=element_text(size=10),
        axis.title=element_text(size=16)) +
  facet_grid(. ~ Funder)
dev.off()

pdf("FigureXa_Funder_x_Direction_x_Bundle_042619.pdf",height=4.5,width=12)
ggplot(plot2, aes(value,type)) +
  geom_tile(aes(fill=n), color="white") +
  geom_text(aes(label=n)) +
  scale_fill_continuous(na.value="lightgray") +
  scale_fill_gradient(name="Number\nof cases",low="white",high="steelblue") +
  ylab("Bundle of rights") +
  xlab("Direction of outcome") +
  theme(legend.title=element_text(size=10),
        legend.text=element_text(size=10),
        axis.title=element_text(size=16)) +
  facet_grid(. ~ Funder)
dev.off()
##Figure 2 - distribution geographic

##Prepare data - convert to geojson
worldmap <- map_data("world")

country_dat <- data %>% select(case_id,Study_Country) %>% distinct()
s <- strsplit(country_dat$Study_Country, split=", ")
cf <- data.frame(case_id=rep(country_dat$case_id, sapply(s, length)), Study_Country = unlist(s))
colnames(cf) <- c("case_id","region")
plot4 <- count(cf,region)
light <- worldmap %>% select(region) %>% distinct()
mismatch <- anti_join(plot4,light,by="region")

cf2 <- data.frame(lapply(cf, function(x) {
  gsub("Phillippines","Philippines", x)
}))

cf3 <- data.frame(lapply(cf2, function(x) {
  gsub("Viet Nam","Vietnam", x)
}))

cf_count <- count(cf3,region)

worldmap_plot <- left_join(worldmap,cf_count,by="region")

pdf("Figure2_Geo.pdf",height=8.5,width=11)
ggplot(worldmap_plot, aes(long,lat,group=group)) +
  geom_polygon(aes(fill=n)) +
  scale_fill_continuous(na.value="lightgray") +
  scale_fill_gradient(name="Number\nof cases",low="#deebf7",high="#08306b")
dev.off()

library(leaflet)
library(leaflet.esri)
library(mapview)

centroids <- read.csv("country_centroids_az8.csv",header=TRUE,stringsAsFactors = FALSE)
coord <- centroids %>% select(geounit,Longitude,Latitude) %>% distinct()
colnames(coord) <- c("region","Long","Lat")

df <- anti_join(cf_count,coord,by="region")

cf_plot <- data.frame(lapply(cf_count, function(x) {
  gsub("USA","United States of America", x)
}))

cf_plot <- left_join(cf_plot,coord,by="region")
cf_plot$n <- as.numeric(cf_plot$n)

m <- leaflet(cf_plot) %>%
  addTiles() %>%
  setView(lng = 0, lat = 0, zoom = 0) %>%
  addEsriTiledMapLayer(
    url = "http://gis-treecover.wri.org/arcgis/rest/services/ForestLoss_2000_2012_map/MapServer") %>%
  addCircleMarkers(
    radius = ~(n*1.25),
    fillColor = "black",
    stroke = FALSE, fillOpacity = 0.75
  )
mapshot(m, file="FigureX_countrycounts_forestcover.png")
