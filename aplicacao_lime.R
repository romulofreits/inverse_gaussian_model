library(GLMsData)
library(ggplot2)
library(dplyr)
library(ggstatsplot)
library(statmod)
library(ggpubr)
library(e1071)

data(lime)
attach(lime)

media = round(mean(lime$Foliage), 4)
mediana = round(median(lime$Foliage), 4)
sk = round(e1071::skewness(lime$Foliage), 4)
kt = round(e1071::kurtosis(lime$Foliage), 4)
maximo = round(max(lime$Foliage), 4)
minimo = round(min(lime$Foliage), 4)

histograma = ggplot(lime, aes(x = Foliage)) +
  geom_histogram(aes(y = after_stat(density)),
                 color = 'black', fill = '#21908C', bins = 30) +
  labs(title = '',
       x = 'Biomassa Foliar (Kg)',
       y = 'Densidade') +
  annotate("text", x = Inf, y = Inf, hjust = 1.1, vjust = 2,
           label = paste('Média:', media,
                         '\nMediana:', mediana,
                         '\nAssimetria:', sk,
                         '\nCurtose:', kt,
                         '\nMínimo:', minimo,
                         '\nMáximo:', maximo),
           size = 5, color = 'black') +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),       # Remove linhas de grade
    panel.border = element_rect(color = "black", fill = NA, size = 1) # Caixa ao redor
  )

ggsave('C:/GRADUACAO/DISCIPLINAS 2025 1/MODELOS DE REGRESSAO II/SEMINARIOS/SEMINARIO I/histograma_lime.pdf', 
       plot = histograma,
       width = 10, height = 6, units = 'in')


# Observando o comportamento da variável resposta (Foliage) com a variavel DBH para cada origem de árvore
pdf('plot1.pdf', width = 10, height = 6, family = 'Helvetica', pointsize = 12)
par(mar = c(4, 4, 2, 1))
plot(Foliage ~ DBH, type = 'n', las = 1,
     xlab = 'DBH (cm)', ylab = 'Biomassa foliar (kg)',
     ylim = c(0, 15), xlim = c(0, 40), data = lime)

points(Foliage ~ DBH, data = subset(lime, Origin == 'Coppice'), pch = 1, col = '#440154')
points(Foliage ~ DBH, data = subset(lime, Origin == 'Natural'), pch = 2, col = '#21908C')
points(Foliage ~ DBH, data = subset(lime, Origin == 'Planted'), pch = 3, col = '#F39C12')
legend('topleft', col = c('#440154', '#21908C', '#F39C12'),
       pch = c(1, 2, 3), bty = 'n', 
       legend = c('Coppice', 'Natural','Planted'))
dev.off()


# Observando o comportamento do log variável resposta (Foliage) com o log da variavel DBH para cada origem de árvore
pdf('plot2.pdf', width = 10, height = 6, family = 'Helvetica', pointsize = 12)
par(mar = c(4, 4, 2, 1))
plot(log(Foliage) ~ log(DBH), type = 'n', las = 1, 
     xlab = 'log(DBH) em cm', ylab = 'log(Biomassa foliar) em kg',
     ylim = c(-5, 3), xlim = c(0, 4), data = lime) 
points(log(Foliage) ~ log(DBH), data = subset(lime, Origin == 'Coppice'), 
       pch = 1, col = '#440154')
points(log(Foliage) ~ log(DBH), data = subset(lime, Origin == 'Natural'),
       pch = 2, col = '#21908C')
points(log(Foliage) ~ log(DBH), data = subset(lime, Origin == 'Planted'),
       pch = 3, col = '#F39C12')
legend('topleft', col = c('#440154', '#21908C', '#F39C12'),
       pch = c(1, 2, 3), bty = 'n', 
       legend = c('Coppice', 'Natural','Planted'))
dev.off()


# Observando o comportamento da variável resposta (Foliage) com a variavel Age para cada origem de árvore
pdf('plot3.pdf', width = 10, height = 6, family = 'Helvetica', pointsize = 12)
par(mar = c(4, 4, 2, 1))
plot(Foliage ~ Age, type = 'n', las=1,
     xlab = 'Idade (anos)', ylab = 'Biomassa foliar (kg)',
     ylim = c(0, 15), xlim = c(0, 150), data = lime)
points(Foliage ~ Age, data = subset(lime, Origin == 'Coppice'), pch = 1, col = '#440154')
points(Foliage ~ Age, data = subset(lime, Origin == 'Natural'), pch = 2, col = '#21908C')
points(Foliage ~ Age, data = subset(lime, Origin == 'Planted'), pch = 3, col = '#F39C12')
legend('topleft', col = c('#440154', '#21908C', '#F39C12'),
       pch = c(1, 2, 3), bty = 'n', 
       legend = c('Coppice', 'Natural','Planted'))
dev.off()

# Gráfico de biomassa foliar por origem com borda
violino_lime = lime |>
  ggbetweenstats(
    x = Origin,
    y = Foliage,
    results.subtitle = FALSE,
    pairwise.comparisons = FALSE,
    bf.message = FALSE,
    messages = FALSE,
    plot.type = 'violin',
    violin.args = list(
      width = 0.8,
      alpha = 0.6,
      fill = 'gray90',
      trim = FALSE
    ),
    point.args = list(
      position = position_jitter(width = 0.15),
      size = 2,
      alpha = 0.6
    ),
    ggplot.component = list(
      scale_color_manual(
        values = c('Coppice' = '#440154', 
                   'Natural' = '#21908C', 
                   'Planted' = '#F39C12')
      )
    ),
    centrality.plotting = TRUE,
    centrality.point.args = list(
      size = 4,
      color = 'red4'
    ),
    xlab = 'Origem da Árvore',
    ylab = 'Biomassa Foliar (kg)'
  ) +
  scale_y_continuous(limits = c(0, 15), breaks = seq(0, 15, by = 2)) +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(), 
    axis.text.x = element_text(size = 11),
    legend.position = 'none',
    panel.border = element_rect(color = 'black', fill = NA, linewidth = 0.8)  
  )
ggsave('C:/GRADUACAO/DISCIPLINAS 2025 1/MODELOS DE REGRESSAO II/SEMINARIOS/SEMINARIO I/grafico_violino_lime.pdf', 
       plot = violino_lime,
       width = 10, height = 6, units = 'in')


# fit.model = modelo_analisado

X = model.matrix(fit.model)
n = nrow(X)
p = ncol(X)
w = fit.model$weights
W = diag(w)
H = solve(t(X)%*%W%*%X)
H = sqrt(W)%*%X%*%H%*%t(X)%*%sqrt(W)
h = diag(H)
soma = resid(fit.model, type="pearson")
soma = sum(soma^2)
fi = (n-p)/soma
ts = resid(fit.model,type="pearson")*sqrt(fi/(1-h))
td = resid(fit.model,type="deviance")*sqrt(fi/(1-h))
di = (h/(1-h))*(ts^2)
a = max(td)
b = min(td)


par(mfrow=c(2,2))


# Diagnostico (Gilberto)

#plot(fitted(fit.model),h,xlab="Valor Ajustado", ylab="Medida h", pch=16)
#identify(fitted(fit.model), h, n=2)
#
#plot(fitted(fit.model), di,xlab="Valor Ajustado", ylab="Distância de Cook", pch=16)
#identify(fitted(fit.model),di, n=1)
#

plot(fitted(fit.model),td,xlab="Valor Ajustado", ylab="Resíduo Componente do Desvio",
     ylim=c(b-1,a+1),pch=16)
abline(2,0,lty=2)
abline(-2,0,lty=2)
#identify(fitted(fit.model),td, n=1)
eta = predict(fit.model)
z = eta + resid(fit.model, type="pearson")/sqrt(w)
plot(predict(fit.model),z,xlab="Preditor Linear", 
     ylab="Variavel z", pch=16)
lines(smooth.spline(predict(fit.model), z, df=2))
plot(1:n, di, xlab="Índice", ylab="Distância de Cook", pch=16)


# criando uma funcao de ligacao
link_custom = make.link('identity')
link_custom$linkfun = function(mu) mu^(1/2)
link_custom$linkinv = function(eta) eta^2
link_custom$mu.eta = function(eta) 2*eta
link_custom$name = 'sqrt(mu)'

# Possível chute inicial
b0 = solve(t(X) %*% X) %*% t(X) %*% Foliage^1/2

# Modelo com todas as covariáveis
fit.model = glm(Foliage ~ Origin + DBH + Age, 
                family = inverse.gaussian(link = link_custom),
                data = lime,
                control = glm.control(maxit = 100))

summary(fit.model)
deviance(fit.model)
AIC(fit.model)

# Localizar pontos influentes
data.frame(Indice = 1:n, Distancia_Cook = di, Residuo_Desvio = td)[di > 150,]

# Modelo sem a observação 24
fit.model = glm(Foliage ~ Origin + DBH + Age, 
                family = inverse.gaussian(link = link_custom),
                data = lime[-24,])

summary(fit.model)
deviance(fit.model)
AIC(fit.model)


# Modelo sem a covariável Age
fit.model0 = glm(Foliage ~ Origin + DBH, 
                family = inverse.gaussian(link = 'sqrt'))


summary(fit.model)
deviance(fit.model)
AIC(fit.model)


# Modelo sem a covariável DBH
fit.model3 = glm(Foliage ~ Origin + Age, 
                 family = inverse.gaussian(link = link_custom),
                 data = lime,
                 control = glm.control(maxit = 100))


summary(fit.model3)
deviance(fit.model3)
AIC(fit.model3)


# Modelo sem a covariável Origin
fit.model4 = glm(Foliage ~ DBH + Age, 
                 family = inverse.gaussian(link = link_custom),
                 data = lime)
summary(fit.model4)
deviance(fit.model4)
AIC(fit.model4)

# Modelo sem a covariável Origin
fit.model5 = glm(Foliage ~ Origin*DBH + Age, 
                 family = inverse.gaussian(link = link_custom),
                 data = lime,
                 control = glm.control(maxit = 100))

summary(fit.model5)
deviance(fit.model5)
AIC(fit.model5)



fit.model6 = glm(Foliage ~ Origin, 
                 family = inverse.gaussian(link = link_custom),
                 data = lime)
summary(fit.model6)
deviance(fit.model6)
AIC(fit.model6)

fit.model.log = glm(Foliage ~ Origin + DBH + Age, 
                   family = inverse.gaussian(link = 'log'),
                   data = lime)
summary(fit.model.log)
deviance(fit.model.log)
AIC(fit.model.log)



fit.model.id = glm(Foliage ~ Origin + DBH + Age, 
                family = inverse.gaussian(link = 'identity'),
                data = lime)
summary(fit.model.id)
deviance(fit.model.id)
AIC(fit.model.id)

# Anova com os modelos finais
anova(fit.model, fit.model0, fit.model3, test = 'F')





