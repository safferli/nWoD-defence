rm(list = ls()); gc(); gc()

options(bitmapType='cairo')
options(scipen = 999)

library(tidyverse)


# Define your workspace: "X:/xxx/"
wd <- "C:/github/nWoD-defense/"
#wd <- path.expand("~/Documents/github/nWoD-defense")
setwd(wd)


f.get.successes <- function(x){
  sum(x >= 8)
}


f.get.rerolls <- function(x, rerolls = 0, eightagain = FALSE, nineagain = FALSE, rote = FALSE){
  # rote action: reroll all failures
  if (rote) rerolls <- rerolls + sum(x < 8)
  # 8-again: reroll all successes (8, 9, 10)
  if (eightagain) rerolls <- rerolls + sum(x >= 8)
  # 9-again: reroll all (9. 10); make sure that 8-again hasn't been called already
  if (nineagain & eightagain == FALSE) rerolls <- rerolls + sum(x >= 9)
  # 10-again (default): reroll all 10s; make sure 8/9-again haven't been called already
  if (!(eightagain | nineagain)) rerolls <- rerolls + sum(x == 10)
  
  return(rerolls)
}


f.roll.die <- function(n = 1, eightagain = FALSE, nineagain = FALSE, rote = FALSE){
  
  # initialise; initial "reroll" is 100% = n
  rerolls <- n
  successes <- 0
  
  while(rerolls > 0){
    rr <- sample(1:10, size = rerolls, replace = TRUE)
    successes <- successes + f.get.successes(rr)
    rerolls <- f.get.rerolls(rr, eightagain = eightagain, nineagain = nineagain, rote = rote)
  }
  
  return(successes)
}

# replicate(1000, f.roll.die(10)) %>% hist



f.roll.attack <- function(att, def, dodge = FALSE){
  # normal attack vs defense: roll attack-defense
  if (!dodge) {
    f.roll.die(att - def) %>% 
      return()
  # full dodge: roll attack vs roll defense x2
  } else {
    (f.roll.die(att) - f.roll.die(def * 2)) %>% 
      # can't have less than 0 damage
      if_else(. < 0, 0, .) %>% 
      return()
  }
}

# replicate(1000, f.roll.attack(6, 3, dodge = TRUE)) %>% hist()


f.simulate.attack <- function(repl = 10, att, def, dodge){
  replicate(repl, f.roll.attack(att = att, def = def, dodge = dodge))
}

# f.simulate.attack(10, 6, 3, FALSE)



dta <- tidyr::crossing(att = 6:12, def = 2:5, dodge = c(TRUE, FALSE)) %>% 
  arrange(att, def)

dta %<>% 
  mutate(
    repl = 1000
  ) %>% 
  mutate(
    sim = purrr::pmap(., f.simulate.attack)
  )



dta %>% 
  unnest() %>% 
  mutate(sim = parse_integer(sim)) %>% 
  ggplot()+
  geom_histogram(aes(x=sim, fill = dodge), position = "dodge", bins = max(dta %>% unnest %>% .$sim + 2L))+
  #geom_text(aes(x = 6, y = 600, label = paste0("average: ", mean(sim))))+
  #stat_summary(aes(x = 6, y = 600, label = ..y..), fun.y = mean, geom = "text")+
  facet_grid(att ~ def)+
  theme_bw()+
  theme(plot.title = element_text(lineheight=.8, face="bold"))+
  labs(
    title = "nWoD attack roll simulations", 
    x = "", 
    y = "damage dealt"
  )

# https://stackoverflow.com/questions/15720545/use-stat-summary-to-annotate-plot-with-number-of-observations

# n_fun <- function(x){
#   return(data.frame(y = median(x), label = paste0("n = ",length(x))))
# }



# https://stackoverflow.com/questions/20139978/ggplot2-label-values-of-barplot-that-uses-fun-y-mean-of-stat-summary
# ggplot(data= mtcars) +
#   aes(x = factor(gear),
#       y = mpg)+
#   #stat_summary(aes(fill = factor(gear)), fun.y=mean, geom="bar")
#   stat_summary(aes(label=round(..y..,2)), fun.y=mean, geom="text", size=6, vjust = -0.5)




