library("tidyverse")

months <- c("Jan", "Feb", "Mar", "Apr", "May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")

# Data source: MBIE
tourism <- read_csv("tourism.csv") %>%
  rename(Region = REGION) %>%
  filter(Region == "Canterbury") %>%
  mutate(
    Date = as.Date(Date, "%d/%m/%Y"),
    Year = format(Date,"%Y"),
    Month= factor(format(Date,"%b"),levels=months) 
  ) %>%
  filter(
    Year > 2013 & (
      Month == "Jul" | Month == "Aug" |
      Month == "Sep" | Month == "Oct" |
      Month == "Nov" | Month == "Dec"
    )
  ) %>%
  group_by(Month,Year) %>%
  summarise(Total.Spend = sum(Spend))

ggplot(data=tourism) +
  geom_line(mapping=aes(Month,Total.Spend,group=Year,colour=Year)) +
  xlab("Month") +
  ylab("Total monthly spending [million NZD]") +
  labs(title="November 2016 tourism spending for Canterbury slumps")