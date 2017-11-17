##CHG: load database once
##     IMPORTANT : IDEALLY ALL THESE DATABASE SHOULD BE PRE-CREATED AND LOADED AS-IT IN SHINY

  reportDF <- read.csv2(file = "./data/report2017.csv")
  reportDF$date <- as.Date(reportDF$date)
  

## group by LOB, network and date (ie aggregate regions and date)
  DFagg.LN <- as.data.table(
    reportDF %>%
      group_by(LOB, network) %>%  
      summarize(NB = sum(newBusiness),
                canc = sum(cancellation),
                portfolio = sum(portfolio)) %>%
      mutate(NB.rate = percent(round(NB/portfolio, 3)),
             canc.rate = percent(round(canc/portfolio, 3)),
             netInflow = comma(as.numeric(NB - canc)),
             portfolio = comma(portfolio)) %>%
      select(-c(NB, canc))
  )

## group by LOB, network and date (ie aggregate regions)
  DFagg.LND <- as.data.table(
    reportDF %>%
      group_by(LOB, network, date) %>%  
      summarize(NB = sum(newBusiness),
                canc = sum(cancellation),
                portfolio = sum(portfolio)) %>%
      mutate(NB.rate = NB/portfolio,
             canc.rate = canc/portfolio,
             netInflow = NB - canc,
             date = as.Date(date)) %>%
      select(-c(NB, canc))
  )

