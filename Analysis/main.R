# IRR-Analysis "Anatomical BAT Categories"
# Version 0.1 
# Date: 2016-09-01
# Author: Anton S. Becker, M.D.

source('Analysis/readin.R')
source('Presentation/printCI.R')
library(psych)
library(DescTools)
library(broom)
library(pROC)
library(agRee)
library(BlandAltmanLeh)

# Kappa -------------------------------------------------------------------

kappa.wrap <- function(x,y) {cbind(x,y) %>% psych::cohen.kappa(., alpha=0.05) %>% 
    tidy() %>% .[2,c(2:4)] %>% f.ci()}
PairApply(d.readout[,c('BAT','BAT.R1','BAT.R2','BAT.R3')], FUN=kappa.wrap)

psych::cohen.kappa(d.readout[,c('BAT','BAT.R1')], alpha=0.05) %>% tidy() %>% .[2,c(2:4)] %>% as.numeric()
psych::cohen.kappa(d.readout[,c('BAT','BAT.R2')], alpha=0.05) %>% tidy()
psych::cohen.kappa(d.readout[,c('BAT','BAT.R3')], alpha=0.05) %>% tidy()

# ICC ---------------------------------------------------------------------

DescTools::ICC(d.readout[,c('BAT.R1','BAT.R2','BAT.R3')], conf.level=0.95)
ICC.wrap <- function(x,y) {cbind(x,y) %>% DescTools::ICC(., type='ICC2k', conf.level=0.95) %>% f.ci()}
PairApply(d.readout[,c('BAT','BAT.R1','BAT.R2','BAT.R3')], FUN=ICC.wrap, symmetric=TRUE)

DescTools::ICC(d.readout[,c('BAT','BAT.R1')], conf.level=0.95)
DescTools::ICC(d.readout[,c('BAT','BAT.R2')], conf.level=0.95)
DescTools::ICC(d.readout[,c('BAT','BAT.R3')], conf.level=0.95)


# CCC ---------------------------------------------------------------------
# Concordance correlation coefficient proposed by Lin (1989)

agree.ccc(d.readout[,c('BAT.R1','BAT.R2','BAT.R3')] %>% as.matrix(), conf.level=0.95)
#ccc.wrap <- function(x,y) {cbind(x,y) %>% as.matrix() %>% agree.ccc(., conf.level=0.95) %>% unlist() %>% f.ci()}
#PairApply(d.readout[,c('BAT.R1','BAT.R2','BAT.R3')], FUN=ccc.wrap)

pair.ccc <- data.frame(
  'Reference'= c('1',
      agree.ccc(d.readout[,c('BAT','BAT.R1')] %>% as.matrix(), conf.level=0.95) %>% unlist() %>% f.ci(),
      agree.ccc(d.readout[,c('BAT','BAT.R2')] %>% as.matrix(), conf.level=0.95) %>% unlist() %>% f.ci(),
      agree.ccc(d.readout[,c('BAT','BAT.R3')] %>% as.matrix(), conf.level=0.95) %>% unlist() %>% f.ci()),
   'Reader 1'=c(
     agree.ccc(d.readout[,c('BAT','BAT.R1')] %>% as.matrix(), conf.level=0.95) %>% unlist() %>% f.ci(),
     '1',
     agree.ccc(d.readout[,c('BAT.R1','BAT.R2')] %>% as.matrix(), conf.level=0.95) %>% unlist() %>% f.ci(),
     agree.ccc(d.readout[,c('BAT.R1','BAT.R3')] %>% as.matrix(), conf.level=0.95) %>% unlist() %>% f.ci()),
  'Reader 2'=c(
    agree.ccc(d.readout[,c('BAT','BAT.R1')] %>% as.matrix(), conf.level=0.95) %>% unlist() %>% f.ci(),
    agree.ccc(d.readout[,c('BAT.R1','BAT.R2')] %>% as.matrix(), conf.level=0.95) %>% unlist() %>% f.ci(),    
    '1',
    agree.ccc(d.readout[,c('BAT.R2','BAT.R3')] %>% as.matrix(), conf.level=0.95) %>% unlist() %>% f.ci())
   )


# Bland-Altman Stats ------------------------------------------------------

bland.altman.stats(d.readout$BAT, d.readout$BAT.R1)
bland.altman.stats(d.readout$BAT, d.readout$BAT.R2)
bland.altman.stats(d.readout$BAT, d.readout$BAT.R3)


# ROC Tests ---------------------------------------------------------------

roc.test(roc(BAT~BAT.R1, d.readout %>% filter(BAT %in% c(0,1))), roc(BAT~BAT.R1, d.readout %>% filter(BAT %in% c(1,2))))
roc.test(roc(BAT~BAT.R1, d.readout %>% filter(BAT %in% c(1,2))), roc(BAT~BAT.R1, d.readout %>% filter(BAT %in% c(2,3))))

roc.test(roc(BAT~BAT.R2, d.readout %>% filter(BAT %in% c(0,1))), roc(BAT~BAT.R2, d.readout %>% filter(BAT %in% c(1,2))))
roc.test(roc(BAT~BAT.R2, d.readout %>% filter(BAT %in% c(1,2))), roc(BAT~BAT.R2, d.readout %>% filter(BAT %in% c(2,3))))

roc.test(roc(BAT~BAT.R3, d.readout %>% filter(BAT %in% c(0,1))), roc(BAT~BAT.R3, d.readout %>% filter(BAT %in% c(1,2))))
roc.test(roc(BAT~BAT.R3, d.readout %>% filter(BAT %in% c(1,2))), roc(BAT~BAT.R3, d.readout %>% filter(BAT %in% c(2,3))))
