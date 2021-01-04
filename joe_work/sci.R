library(here)
library(dplyr)
library(readr)
library(stringr)
library(ggplot2)

#################### Read in data

countyCases <- read_csv("covid_confirmed_usafacts.csv") %>%
    mutate(countyFIPS = as.character(countyFIPS)) %>%
    filter(countyFIPS != "0", countyFIPS != "1") %>%
    mutate(countyFIPS = if_else(str_length(countyFIPS) == 4,
                                paste0(0, countyFIPS),
                                countyFIPS))

countyCasesPer10k <-
    countyCases %>%
    inner_join(select(countyPop, countyFIPS, population), by = "countyFIPS") %>%
    mutate(across(matches("[0-9]{1,2}/"), ~ 10000 * .x / population))

countyDeaths <- read_csv("covid_deaths_usafacts.csv")
countyPop <-
    read_csv("covid_county_population_usafacts.csv") %>%
    mutate(countyFIPS = as.character(countyFIPS)) %>%
    filter(countyFIPS != "0") %>%
    mutate(countyFIPS = if_else(str_length(countyFIPS) == 4,
                                paste0(0, countyFIPS),
                                countyFIPS))

sci <-
    read_tsv(here("county_county_aug2020.tsv")) %>%
    mutate(log_sci = log(scaled_sci, base = 2),
           myscale_sci = scaled_sci / 1000000000)

sciPrprtn <-
    sci %>%
    group_by(user_loc) %>%
    mutate(total_sci = sum(myscale_sci),
           prop_sci = myscale_sci / total_sci)

####### SCI data is symmetrical, unfortunately.
symmetry <-
    sci %>%
    filter(user_loc == "01003" & fr_loc == "01005" |
               user_loc == "01005" & fr_loc == "01003")

###### Find total number of connections (estimated using county pop.)
###### Not every county has FB connections which has recorded COVID cases
###### Not every county has recorded COVID cases which has FB connections
sciTotalConnection <-
    sci %>%
    filter(user_loc != fr_loc) %>%
    inner_join(countyPop, by = c("user_loc" = "countyFIPS")) %>%
    inner_join(select(countyPop, countyFIPS, population), by = c("fr_loc" = "countyFIPS")) %>%
    rename(user_pop = population.x, fr_pop = population.y) %>%
    mutate(totalConn = user_pop * fr_pop,
           fbConn = myscale_sci * totalConn)

mostConnectedCountiesCovid <-
    sciTotalConnection %>%
    group_by(user_loc) %>%
    summarise(medianConn = median(fbConn),
              meanConn = mean(fbConn)) %>%
    inner_join(select(countyCases, countyFIPS, `10/28/20`),
               by = c("user_loc" = "countyFIPS"))

ggplot(mostConnectedCountiesCovid, aes(x = meanConn, y = `10/28/20`)) +
    geom_point() + theme_bw() + geom_smooth(method = "lm", se = F, formula = y ~ x)
cor(mostConnectedCountiesCovid$`10/28/20`, mostConnectedCountiesCovid$meanConn)
summary(lm(`10/28/20` ~ meanConn, data = mostConnectedCountiesCovid))

ggplot(mostConnectedCountiesCovid, aes(x = medianConn, y = `10/28/20`)) +
    geom_point() + theme_bw() + geom_smooth(method = "lm", se = F, formula = y ~ x)
cor(mostConnectedCountiesCovid$`10/28/20`, mostConnectedCountiesCovid$medianConn)
summary(lm(`10/28/20` ~ medianConn, data = mostConnectedCountiesCovid))
