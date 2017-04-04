source('Analysis/readin.R')
source('Analysis/multiplot.R')
library(pROC)
library(BlandAltmanLeh)
library(ggplot2)

# Bland-Altman Plots ------------------------------------------------------

bland.altman.plot(d.readout$BAT, d.readout$BAT.R1, 
                  graph.sys='ggplot2', geom_count=TRUE) +
                  theme_bw()
bland.altman.plot(d.readout$BAT, d.readout$BAT.R2,  
                  graph.sys='ggplot2', geom_count=TRUE) +
                  theme_bw()
bland.altman.plot(d.readout$BAT, d.readout$BAT.R3, 
                  graph.sys='ggplot2', geom_count=TRUE) +
                  theme_bw()

# ROC ---------------------------------------------------------------------

d.readout$BAT.ROC <- ifelse(d.readout$BAT>0, 1, 0)
#d.readout$BAT3 <- d.readout$BAT %/% 3

roc(BAT.ROC~BAT.R1, d.readout) %>% coords(1, ret=c("threshold", "sensitivity", "specificity", "npv", "ppv"))
roc(BAT.ROC~BAT.R2, d.readout) %>% coords(1, ret=c("threshold", "sensitivity", "specificity", "npv", "ppv"))
roc(BAT.ROC~BAT.R3, d.readout) %>% coords(1, ret=c("threshold", "sensitivity", "specificity", "npv", "ppv"))

par(mfrow=c(2,2))
plot.roc(BAT.ROC~BAT.R1, d.readout, lty=1, , print.auc=TRUE, 
         print.auc.y=0.4, print.auc.pattern="Az Reader 1: %.2f", main='Detection') 
plot.roc(BAT.ROC~BAT.R2, d.readout, add=TRUE, lty=2, print.auc=TRUE, 
         print.auc.y=0.3, print.auc.pattern="Az Reader 2: %.2f") 
plot.roc(BAT.ROC~BAT.R3, d.readout, add=TRUE, lty=3, print.auc=TRUE, 
         print.auc.y=0.2, print.auc.pattern="Az Reader 3: %.2f") 


roc(BAT~BAT.R1, d.readout %>% filter(BAT %in% c(0,1))) %>% plot(lty=1, print.auc=TRUE, 
                                                                print.auc.y=0.4, print.auc.pattern="Az Reader 1: %.2f", main='Cat. 0-1')
roc(BAT~BAT.R2, d.readout %>% filter(BAT %in% c(0,1))) %>% plot(add=TRUE, lty=2, print.auc=TRUE, 
                                                                print.auc.y=0.3, print.auc.pattern="Az Reader 2: %.2f")
roc(BAT~BAT.R3, d.readout %>% filter(BAT %in% c(0,1))) %>% plot(add=TRUE, lty=3, print.auc=TRUE, 
                                                                print.auc.y=0.2, print.auc.pattern="Az Reader 3: %.2f")

roc(BAT~BAT.R1, d.readout %>% filter(BAT %in% c(1,2))) %>% plot(print.auc=TRUE, 
                                                                print.auc.y=0.4, print.auc.pattern="Az Reader 1: %.2f", main='Cat. 1-2')
roc(BAT~BAT.R2, d.readout %>% filter(BAT %in% c(1,2))) %>% plot(add=TRUE, lty=2, print.auc=TRUE, 
                                                                print.auc.y=0.3, print.auc.pattern="Az Reader 2: %.2f")
roc(BAT~BAT.R3, d.readout %>% filter(BAT %in% c(1,2))) %>% plot(add=TRUE, lty=3, print.auc=TRUE, 
                                                                print.auc.y=0.2, print.auc.pattern="Az Reader 3: %.2f")

roc(BAT~BAT.R1, d.readout %>% filter(BAT %in% c(2,3))) %>% plot(print.auc=TRUE, 
                                                                print.auc.y=0.4, print.auc.pattern="Az Reader 1: %.2f", main='Cat. 2-3')
roc(BAT~BAT.R2, d.readout %>% filter(BAT %in% c(2,3))) %>% plot(add=TRUE, lty=2, print.auc=TRUE, 
                                                                print.auc.y=0.3, print.auc.pattern="Az Reader 2: %.2f")
roc(BAT~BAT.R3, d.readout %>% filter(BAT %in% c(2,3))) %>% plot(add=TRUE, lty=3, print.auc=TRUE, 
                                                                print.auc.y=0.2, print.auc.pattern="Az Reader 3: %.2f")
