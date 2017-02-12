library("tidyverse")

months <- c("Jan-16", "Feb-16", "Mar-16", "Apr-16", "May-16",
            "Jun-16", "Jul-16", "Aug-16", "Sep-16")

# Data source: Stats NZ
exports <- read_csv("exports.csv") %>%
  rename(Country=`Country of Destination`) %>%
  filter(grepl("16", Period)) %>%
  mutate(
    Period=factor(Period, months),
    Value = Value / 1000000
  ) %>%
  arrange(Period)

ggplot(data=exports) +
  geom_line(mapping = aes(Period,Value,group = Country)) +
  facet_wrap(~ Country, ncol=1, scales="free") +
  ylab("Free on board value of exports [million NZD]") +
  xlab("Month") +
  labs(title="New Zealand's exports to its main trading partners")