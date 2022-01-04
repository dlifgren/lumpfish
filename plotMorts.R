library(tidyverse)
library(readxl)
library(reshape2)
library(lubridate)
library(openxlsx)

morts <- read_excel("mortalityLog.xlsx") %>%
  replace(is.na(.),0) %>%
  mutate(Tank = paste0("Tank", Tank))

mort <- function(x){
  output <- vector("integer", length(x))
  for(i in seq_along(x)){
    if(x[i]==16){
      output[i] <- x[i]
    }
    else{
      output[i] <- 16-sum(x[2:i])}
  }
  return(output)
}

y <- apply(morts[,-1], 1, mort)%>%
  t()%>%
  as_tibble()
y$Tank <- morts$Tank
y <- relocate(y, Tank)
y$Tank <- factor(y$Tank, levels = c("Tank4", "Tank5", "Tank6", "Tank10", "Tank11", "Tank12"))
names(y) <- names(morts)


t <- melt(y, id.vars = "Tank", variable.name = "Date")%>%
  mutate(Date = mdy(Date)) %>%
  ggplot(aes(x=Date, y=value, group = Tank, color = Tank))+
  geom_step(size = c(1))+
  scale_x_date(date_breaks = "2 days", date_labels = "%b-%d") +
  scale_y_continuous(breaks = seq(1,16))+
  theme_bw() +
  theme(legend.background = element_rect(fill = "white", size = 4, colour = "white"),
        axis.ticks = element_line(colour = "grey70", size = 0.2),
        panel.grid.major = element_line(colour = "grey70", size = 0.2),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = -45, vjust = 0.5, hjust = 0)
        )+
  labs(y="Lumpfish Count")
t 

date <- format(Sys.Date(), '%m.%d.%y')
write.xlsx(morts, paste0(date,"mortalityLog.xlsx" ))
