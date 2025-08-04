install.packages("psych")     
install.packages("mediation")
install.packages("DiagrammeR")

library(psych)
library(mediation)
library(DiagrammeR)

#Correlação
corr.test(estudo2[, c("qlq_t", "e3po_t", "qore_t", "tohe_o", "tohe_s")], 
          method = "spearman")

#Regressão

modelo <- lm(qlq_t ~ e3po_t + qore_t, 
             data = estudo2)

summary(modelo)

# Mediação

modelo.m <- lm(e3po_t ~ qore_t, 
               data = estudo2)
summary(modelo.m)

modelo.y <- lm(qlq_t ~ qore_t + e3po_t, 
               data = estudo2)
summary(modelo.y)

mediação <- mediate(modelo.m, modelo.y, 
                    treat = "qore_t", mediator = "e3po_t", 
                    boot = TRUE, sims = 5000)
summary(mediação)

#Plotagem do modelo

grViz("
digraph med_model {
  graph [layout = dot, rankdir = LR]
  
  QoRE [label = 'QoRE\n(Regulação Emocional)', shape = ellipse, style = filled, fillcolor = white]
  E3PO [label = 'E3PO\n(Estratégias de Enfrentamento)', shape = ellipse, style = filled, fillcolor = white]
  QLQC30 [label = 'QLQ-C30\n(Qualidade de Vida)', shape = ellipse, style = filled, fillcolor = white]

  QoRE -> E3PO [label = 'a = 0,37', fontsize = 12]
  E3PO -> QLQC30 [label = 'b = 1,29', fontsize = 12]
  QoRE -> QLQC30 [label = \"c* = 0,32\", fontsize = 12]
}
")
