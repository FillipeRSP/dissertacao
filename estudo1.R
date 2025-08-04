install.packages("lavaan")
install.packages("semPlot")

library(lavaan)
library(semPlot)

#Modelo 20 itens

modelo_esem <- '
efa("efa")*F1 +
efa("efa")*F2 =~
y1 + y2 + y4 + y5 + y7 + y8 + y10 + y11 + y13 + y14 + y16 + y17 + y19 + y20 + y3 + y6 + y9 + y12 + y15 + y18
Aq =~
1*y1 + 1*y2 + 1*y4 + 1*y5 + 1*y7 + 1*y8 + 1*y10 + 1*y11 + 1*y13 + 1*y14 + 1*y16 + 1*y17 + 1*y19 + 1*y20 + 1*y3 + 1*y6 + 1*y9 + 1*y12 + 1*y15 + 1*y18
Aq ~~ 0*F1 + 0*F2'

esem <- sem(modelo_esem, 
            data=dados, 
            ordered = names(estudo1))

summary(esem, 
        fit.measures=T)

#Modelo 13 itens

modelo_esem2 <- '
efa("efa")*F1 +
efa("efa")*F2 =~
y1 + y2 + y4 + y5 + y7 + y8 + y11 + y16 + y17 + y3 + y6 + y9 + y12
Aq =~
1*y1 + 1*y2 + 1*y4 + 1*y5 + 1*y7 + 1*y8 + 1*y11 + 1*y16 + 1*y17 + 1*y3 + 1*y6 + 1*y9 + 1*y12
Aq ~~ 0*F1 + 0*F2'

esem2 <- sem(modelo_esem2, 
             data=dados, 
             ordered = names(estudo1))

summary(esem2, 
        fit.measures=T)

#Modelo 12 itens

modelo_esem3 <- '
efa("efa")*F1 +
efa("efa")*F2 =~
y1 + y2 + y4 + y5 + y7 + y8 + y11 + y16 + y3 + y6 + y9 + y12
Aq =~
1*y1 + 1*y2 + 1*y4 + 1*y5 + 1*y7 + 1*y8 + 1*y11 + 1*y16 + 1*y3 + 1*y6 + 1*y9 + 1*y12
Aq ~~ 0*F1 + 0*F2'

esem3 <- sem(modelo_esem3, 
             data=dados, 
             ordered = names(estudo1))

summary(esem3, 
        fit.measures=T)

#Plotagem do modelo

semPlot::semPaths(object = esem3, 
                  what = "est", layout = "tree3", bifactor = "Aq", style = "lisrel",
                  intercepts = FALSE, thresholds = FALSE, residuals = FALSE,
                  edge.label.cex = 1,         
                  label.cex = 1.2,              
                  edge.label.position = 0.2,    
                  edge.label.font = 2,          
                  edge.color = "black",         
                  mar = c(5, 1, 4, 1), cut = 0.33,                    
                  sizeMan = 6, sizeLat = 10,                  
                  nCharNodes = 0, curvePivot = TRUE, optimizeLatRes = TRUE)        
