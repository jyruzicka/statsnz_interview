library("tidyverse")
library("readxl")
months <- c("Jan", "Feb", "Mar", "Apr", "May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
# Data source: MBIE monthly regional tourism estimates
tourism <- read_csv("tourism.csv") %>%
  rename(Region = REGION) %>%
  filter(Region == "Canterbury") %>%
  mutate(
    Date = as.Date(Date, "%d/%m/%Y"),
    Year = format(Date,"%Y"),
    Month= factor(format(Date,"%b"),levels=months) 
  ) %>%
  filter(Year > 2013 & (Month == "Jul" | Month == "Aug" | Month == "Sep" | Month == "Oct" |
                          Month == "Nov" | Month == "Dec")) %>%
  group_by(Month,Year) %>%
  summarise(Total.Spend = sum(Spend))

tourism.labels <- tourism[tourism$Month == "Sep", ]

ggplot(data=tourism) +
  geom_line(mapping=aes(Month,Total.Spend,group=Year,colour=Year)) +
  xlab("Month") +
  ylab("Total monthly spending [million NZD]") +
  labs(title="November 2016 tourism spending for Canterbury slumps") +
  annotate("text", x="Sep", y=162, label="2016") +
  annotate("text", x="Sep", y=152, label="2015") +
  annotate("text", x="Sep", y=141, label="2014") +
  theme(legend.position = "none")