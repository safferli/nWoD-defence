rm(list = ls()); gc(); gc()

options(bitmapType='cairo')
options(scipen = 999)

library(dplyr)


# Define your workspace: "X:/xxx/"
#wd <- "D:/github/nWoD-defense/"
wd <- path.expand("~/Documents/github/nWoD-defense")
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
  
  rolls <- sample(1:10, size = n, replace = TRUE)

  successes <- f.get.successes(rolls)
  rerolls <- f.get.rerolls(rolls, eightagain = eightagain, nineagain = nineagain, rote = rote)
  
  while(rerolls > 0){
    rr <- sample(1:10, size = rerolls, replace = TRUE)
    successes <- successes + f.get.successes(rr)
    rerolls <- f.get.rerolls(rr, eightagain = eightagain, nineagain = nineagain, rote = rote)
  }
  
  return(successes)
}


replicate(1000, f.roll.die(10)) %>% hist


## normal attack vs defense:
## roll attack-defense

f.roll.attack <- function(att, def, dodge = FALSE){
  if (!dodge) {
    f.roll.die(att - def) %>% 
      return()
  } else {
    (f.roll.die(att) - f.roll.die(def * 2)) %>% 
      if_else(. < 0, 0L, .) %>% 
      return()
  }
}


f.roll.attack(5, 5, dodge = FALSE)


## full dodge:
## roll attack vs roll defense x2






