# Electricity Generation National Figure
# Andrew Zalesak, Sep 15, 2022
# Figures showing which power sources serve electricity demand

library(tidyverse)
library(ggpubr)

time <- proc.time()

### Set working directory
setwd("SET_BEFORE_USING")

### Constants and names
scenarios <- c("BAU", "M", "T", "M+T")
scenario_letters <- letters[1:length(scenarios)]
names(scenario_letters) <- scenarios

sources <- c("Renewables", "Nuclear", "Natural Gas", "Coal", "Other")

lowestvalue = c(0, -2750)
highestvalue = c(5500, 2750)
names(lowestvalue) = c('A', 'BCD')
names(highestvalue) = c('A', 'BCD')


### Inputs
data <- read.csv("Inputs/ElectricityGenerationData.csv", header=TRUE)

# colors from IBM palette at https://davidmathlogic.com/colorblind
colors <- c("#FFB000",
            "#FE6100",
            "#DC267F",
            "#785EF0",
            "#648FFF")
names(colors) <- sources


### Prepare data
data <- data %>% mutate(Source = factor(Source, levels=sources))
data <- aggregate(Pv ~ Period + Source + Scenario, data=data, sum)
BAU <- filter(data, Scenario == "BAU")
data <- left_join(data, BAU, by=c("Source", "Period"))
data <- mutate(data, versus.BAU=(Pv.x-Pv.y))
data <- arrange(data, Scenario.x, Source)


### Plot A (National Electricity Generation for BAU)
plotterA <- function(S) {
  
  print("plotterA")
  
  PX <- ggplot(data = filter(data, Scenario.x == S)) +
    geom_area(aes(y=Pv.x, x=Period, fill=Source)) +
    ylab("TWh") +
    theme_classic() +
    scale_fill_manual(values=colors) +
    scale_y_continuous(limits=c(lowestvalue['A'],highestvalue['A'])) +
    theme(axis.text=element_text(size=7),
          axis.title.x=element_blank()) +
    ggtitle(S)
  
  return(PX)
}


### Plots B-D (National Electricity Generation versus BAU)
plotterBCD <- function(S) {
  
  print("plotterBCD")
  
  PX <- ggplot(data=filter(data, Scenario.x == S),
               aes(y=versus.BAU, x=Period, fill=Source)) +
    geom_bar(position="stack", stat="identity") +
    ylab("TWh") +
    theme_classic() +
    scale_fill_manual(values=colors) +
    scale_y_continuous(limits=c(lowestvalue['BCD'],highestvalue['BCD'])) +
    theme(axis.text=element_text(size=7),
          axis.title.x=element_blank()) +
    ggtitle(sprintf("%s - BAU", S))
  
  return(PX)
}


### Export figure
figure <- ggarrange(plotterA(scenarios[1]),
                    plotterBCD(scenarios[2]),
                    plotterBCD(scenarios[3]),
                    plotterBCD(scenarios[4]),
                    ncol=2, nrow=2,
                    common.legend=TRUE, legend="right",
                    labels=scenario_letters)

ggsave(sprintf("Outputs/%s.png","ElectricityGenerationNationalFigure"),
       width=7, height=4.5)

### Export data table for figure
write.csv(write.csv(data, sprintf("Outputs/%s.csv", "ElectricityGenerationNationalFigure"),
                    row.names = FALSE))

print(proc.time() - time)
