
library(tidyverse)
library(readxl)
library(lubridate)
library(ggplot2)
library(ggthemes)
library(bbplot)
library(extrafont)
library(plotly)


#### CHARTING SETUP
loadfonts()

pal <- c("#2274A5", "#F0C808","#E56399","#3BB273","#2E294E","#EEEEEE","#FFBA22")

bbc_style <- function() {
  font <- "Arial"
  
  ggplot2::theme(
    
    #Text format:
    #This sets the font, size, type and colour of text for the chart's title
    plot.title = ggplot2::element_text(family=font,
                                       size=28,
                                       face="bold",
                                       color="#222222"),
    #This sets the font, size, type and colour of text for the chart's subtitle, as well as setting a margin between the title and the subtitle
    plot.subtitle = ggplot2::element_text(family=font,
                                          size=18,
                                          margin=ggplot2::margin(9,1,9,1)),
    plot.caption = ggplot2::element_text(family=font,
                                         size=12,
                                         face="italic",
                                         color="black"),
    #This leaves the caption text element empty, because it is set elsewhere in the finalise plot function
    
    #Legend format
    #This sets the position and alignment of the legend, removes a title and backround for it and sets the requirements for any text within the legend. The legend may often need some more manual tweaking when it comes to its exact position based on the plot coordinates.
    legend.position = "top",
    legend.text.align = 0,
    legend.background = ggplot2::element_blank(),
    legend.title = ggplot2::element_blank(),
    legend.key = ggplot2::element_blank(),
    legend.text = ggplot2::element_text(family=font,
                                        size=18,
                                        color="#222222"),
    
    #Axis format
    #This sets the text font, size and colour for the axis test, as well as setting the margins and removes lines and ticks. In some cases, axis lines and axis ticks are things we would want to have in the chart - the cookbook shows examples of how to do so.
    axis.title = ggplot2::element_blank(),
    axis.text = ggplot2::element_text(family=font,
                                      size=18,
                                      color="#222222"),
    axis.text.x = ggplot2::element_text(margin=ggplot2::margin(5, b = 10)),
    axis.ticks = ggplot2::element_blank(),
    axis.line = ggplot2::element_blank(),
    
    #Grid lines
    #This removes all minor gridlines and adds major y gridlines. In many cases you will want to change this to remove y gridlines and add x gridlines. The cookbook shows you examples for doing so
    panel.grid.minor = ggplot2::element_blank(),
    panel.grid.major.y = ggplot2::element_line(color="#cbcbcb"),
    panel.grid.major.x = ggplot2::element_blank(),
    
    #Blank background
    #This sets the panel background as blank, removing the standard grey ggplot background colour from the plot
    panel.background = ggplot2::element_blank(),
    
    #Strip background (#This sets the panel background for facet-wrapped plots to white, removing the standard grey ggplot background colour and sets the title size of the facet-wrap title to font size 22)
    strip.background = ggplot2::element_rect(fill="white"),
    strip.text = ggplot2::element_text(size  = 22,  hjust = 0)
  )
}

setwd("C:/Users/fintan.smith/Desktop/Prisons")


prison_uk_dt<-read_csv("data.csv")
population_UK <- read_csv("pop_overall_CSV.csv")



prison_uk_dt_age <- prison_uk_dt%>%
  filter(str_detect(`Age / Custody / Nationality / Offence Group`," - "))%>%
  rename(age.bracket= `Age / Custody / Nationality / Offence Group`)


# Define list of offence categories taken from unique() 
offences <- c("03 Robbery","07 Possession of weapons","01 Violence against the person","05 Criminal damage and arson","09 Miscellaneous crimes against society","12 Offence not recorded","04 Theft Offences","11 Summary offences","02 Sexual offences","06 Drug offences","10 Fraud Offences","08 Public order offences")

# filter so only rows with offence categories are read
prison_uk_dt_offence <- prison_uk_dt%>%
  filter(`Age / Custody / Nationality / Offence Group` %in% offences)%>%
  rename(offence = `Age / Custody / Nationality / Offence Group`)%>%
  mutate(offence= as.factor(offence))

prison_uk_dt_offence$offence <- gsub("^.{0,3}", "", prison_uk_dt_offence$offence)

# Calculate total imprisonments
total_imprisonments= sum(prison_uk_dt_offence$Population, na.rm = TRUE)

imprisonment_by_offence <- prison_uk_dt_offence%>%
  group_by(offence)%>%
  summarise(Population = sum(Population, na.rm = TRUE))%>%
  mutate(proportion_of_offences= Population/total_imprisonments*100)





imprisonment_by_offence_date <- prison_uk_dt_offence%>%
  mutate(offence_R= case_when(str_detect(offence,"Misc")~ "Other",
                              str_detect(offence,"Offence not") ~"Other",
                              str_detect(offence,"Summa")~ "Other",
                              str_detect(offence,"Criminal damage") ~ "Other",
                              str_detect(offence,"Fraud")~ "Other",
                              str_detect(offence,"Robber") ~"Robbery",
                              str_detect(offence,"Public order") ~ "Other",
                              str_detect(offence,"Theft") ~ "Theft",
                              str_detect(offence,"Sexual") ~ "Sexual offences",
                              str_detect(offence,"Violence")~ "Violence against the person",
                              str_detect(offence,"Drug")~"Drug offences",
                              str_detect(offence,"Possession")~ "Possession of weapons"))

  imprisonment_by_offence_date$offence_R <- factor(imprisonment_by_offence_date$offence_R  ,
                                                   levels=c("Violence against the person", "Sexual offences", "Drug offences", "Theft"
                                                            , "Robbery","Possession of weapons", "Other") )
  
imprisonment_by_offence_date<- imprisonment_by_offence_date%>%
  group_by(offence_R,Date)%>%
  summarise(Population = sum(Population, na.rm = TRUE))%>%
  ungroup(.)%>%
  group_by(Date)%>%
  mutate(total=sum(Population))%>%
  ungroup(.)%>%
  group_by(offence_R,Date)%>%
  mutate(proportion_of_offences= Population/total*100)

imprisonment_by_offence_date$Date <- lubridate::as_date(ym(imprisonment_by_offence_date$Date))


## PLOT OFFENCE TYPE OVER TIME
offence_plot<- ggplot(imprisonment_by_offence_date,aes(x= Date,y=Population, fill=offence_R))+
  geom_area(stat="align")+
  labs(x = "Date", 
       y = "", 
       title = "People who have committed violent, drug, or sex related\noffences make up the bulk of England And Wales'\nprison population",
       subtitle = "Number of prisoners in England and Wales serving time for offences fitting into each category over\ntime",
       caption = "Source: Ministry of Justice")+
  bbc_style()+
  scale_fill_manual(values = c("#2274A5", "#F0C808","#E56399","#3BB273","#2E294E","#DB504A","#EEEEEE"))

ggsave("offence_chart.jpeg",offence_plot,width = 30, height = 20,units="cm")  

ggplotly(offence_time_p, tooltip = c("y"))







imprisonment_by_date <- prison_uk_dt_offence%>%
  group_by(Date)%>%
  summarise(Population = sum(Population, na.rm = TRUE))

imprisonment_by_date$Date <- lubridate::as_date(ym(imprisonment_by_date$Date))

# READ IN PRISON POP YEARLY 1900-2023
prison_pop <- read.csv("population_1900_2023.csv")

prison_pop_full <- prison_pop%>%
  mutate(Year= gsub(pattern = "[^0-9.-]", replacement = '',Year))%>%
  mutate(Total= gsub(pattern = "[^0-9.-]", replacement = '',Total))%>%
  rename(YEAR=Year)%>%
  mutate(YEAR=as.numeric(YEAR))%>%
  mutate(Prison.pop=as.numeric(Total))%>%  
  mutate(Prison.pop= ifelse(YEAR==2023, (87576+85851+83281)/3,Prison.pop))%>% # ADJUST AVERAGE TO INCORPORATE LATEST FIGURE (pop update 26 october 2023)
  filter(YEAR>=1970)

prison_pop_gen <- left_join(prison_pop_full, population_UK,by="YEAR")%>%
  filter(YEAR>=1970)

prison_pop_gen_prop <- prison_pop_gen%>%
  mutate(prop= Prison.pop / Population )%>%
  mutate(prisoner_per_100= prop*100000)


prison_pop_prop_plot <- ggplot(prison_pop_gen_prop, aes(x= YEAR, y= prisoner_per_100))+
  geom_line(linewidth=2, lineend="round", colour="#2274A5")+
  labs(x = "Date", 
       y = "", 
       title = "The proportion of the population in prison\n rose rapidly in the 1990s",
       subtitle = "Average number of prisoners held in England and Wales each year\nper 100,000 of the population",
       caption = "Source: Ministry of Justice\n ONS Population Estimates")+
  bbc_style()+
  scale_x_continuous(breaks = seq(1950, 2023, by =10))+
  scale_y_continuous(breaks= seq(0, 160,by=20))+
  ylim(0,160)

ggsave("population_per_100000_plot.jpeg",prison_pop_prop_plot,width = 30, height = 20,units="cm")  


## INTERACTIVE PLOT
interactive_prison_prop<- ggplotly(prison_pop_prop_plot)%>% 
  layout(title = list(text = paste0("The proportion of the population in prison\n rose rapidly in the 1990s",
                                    '<br>',
                                    '<sup>',
                                    'Average number of prisoners held in England and Wales each year per 100,000 of the population','</sup><br>')),pad=list(b = 500, l = 130, r = 50 ))

htmlwidgets::saveWidget(interactive_prison_prop, "overall_prison_pop_int.html")
  


