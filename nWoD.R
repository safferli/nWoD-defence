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
      if_else(. < 0, 0, .) %>% 
      return()
  }
}

# replicate(1000, f.roll.attack(6, 3, dodge = TRUE)) %>% hist()


f.simulate.attack <- function(repl = 10, att, def, dodge){
  replicate(repl, f.roll.attack(att = att, def = def, dodge = dodge))
}

# f.simulate.attack(10, 6, 3, FALSE)



dta <- tidyr::crossing(att = 3:4, def = 1:2, dodge = c(TRUE, FALSE)) %>% 
  arrange(att, def)

dta %<>% 
  mutate(
    repl = 10
  ) %>% 
  mutate(
    sim = purrr::pmap(., f.simulate.attack)
  )



dta %>% unnest() %>% 
  ggplot()+
  geom_histogram(aes(x=sim, fill = dodge), position = "dodge")







