---
title: "weather events which have huge impact on public health and economic problems"
output: 
      html_document: 
            keep_md: true
---

Download file from the Internet:

```r
link <- "http://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
download.file(url = link, destfile = "StormData")
```

Read a file in table format

```r
StormData <- read.csv(bzfile("StormData"),sep = ",",header=TRUE)
```


```r
table(StormData$PROPDMGEXP)
```

```
## 
##             -      ?      +      0      1      2      3      4      5 
## 465934      1      8      5    216     25     13      4      4     28 
##      6      7      8      B      h      H      K      m      M 
##      4      5      1     40      1      6 424665      7  11330
```

```r
StormData$PROPDMGEXP2 <- 1
StormData$PROPDMGEXP2[which(StormData$PROPDMGEXP == "K")] <- 1000
StormData$PROPDMGEXP2[which(StormData$PROPDMGEXP == "M" | StormData$PROPDMGEXP == "m")] <- 1000000
StormData$PROPDMGEXP2[which(StormData$PROPDMGEXP == "B")] <- 1000000000
```


```r
table(StormData$PROPDMGEXP2)
```

```
## 
##      1   1000  1e+06  1e+09 
## 466255 424665  11337     40
```




```r
StormData %>%
      select(FATALITIES, EVTYPE) %>%
      group_by(EVTYPE) %>%
      summarise(SumFATALITIES = sum(FATALITIES)) %>%
      top_n(n = 8, wt = SumFATALITIES) %>%
      ggplot(aes(y = SumFATALITIES, x = reorder(x = EVTYPE, X = SumFATALITIES), fill=EVTYPE))+
      geom_bar(stat = "identity", show.legend = FALSE) +
      #geom_text(aes(label=SumFATALITIES), size = 4, hjust = 0.5, vjust = -0.1) +
      xlab(label = "") +
      ylab(label = "Death toll") +
      coord_flip() +
      theme_light()
```


```r
StormData %>%
      select(INJURIES, EVTYPE) %>%
      group_by(EVTYPE) %>%
      summarise(SumINJURIES = sum(INJURIES)) %>%
      top_n(n = 8, wt = SumINJURIES) %>%
      ggplot(aes(y = SumINJURIES, x = reorder(x = EVTYPE, X = SumINJURIES), fill=EVTYPE))+
      geom_bar(stat = "identity", show.legend = FALSE) +
      #geom_text(aes(label=SumINJURIES), size = 4, hjust = 0.5, vjust = -0.1) +
      xlab(label = "") +
      ylab(label = "INJURIES") +
      coord_flip() +
      theme_light()
```


```r
StormData %>%
      select(PROPDMG, PROPDMGEXP2, EVTYPE) %>%
      group_by(EVTYPE) %>%
      mutate(SumPROPDMGEXP = (PROPDMG * PROPDMGEXP2)) %>%
      summarise(SumPROPDMGEXP2 = sum(SumPROPDMGEXP)) %>%
      top_n(n = 8, wt = SumPROPDMGEXP2) %>%
      ggplot(aes(y = SumPROPDMGEXP2, x = reorder(x = EVTYPE, X = SumPROPDMGEXP2), fill=EVTYPE))+
      geom_bar(stat = "identity", show.legend = FALSE) +
      #geom_text(aes(label=SumFATALITIES), size = 4, hjust = 0.5, vjust = -0.1) +
      xlab(label = "") +
      ylab(label = "Property damage estimates") +
      coord_flip() +
      theme_light()
```


