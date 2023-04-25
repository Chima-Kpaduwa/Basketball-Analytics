library(BasketballAnalyzeR)

PbP <- PbPmanipulation(PbP.BDB)
subdata <- subset(PbP, player=="Kevin Durant")
subdata$xx <- subdata$original_x/10
subdata$yy <- subdata$original_y/10-41.75

shotchart(data=subdata, x="xx", y="yy", scatter = T,
          pt.col = 'red',
          bg.col = 'yellow')

shotchart(data=subdata, x="xx", y="yy", scatter = T,
          z = 'result')


shotchart(data=subdata, x="xx", y="yy", scatter = F,
          num.sect = 5,
          type = 'sectors',
          z = 'playlength',
          result = 'result')