# descritivas -------------------------------------------------------------
library(tidyverse)
library(gapminder)
library(ggplot2)
library(GGally)
library(lme4)

#library(sjPlot)

summary(gapminder)

# criar decada
gapminder <- gapminder %>% mutate(decada = 10*(year%/%10)) %>% 
  mutate(decada = as.factor(decada))

# cada pais tem duas leituras por decada
n_distinct(gapminder$country)*2*n_distinct(gapminder$decada) == nrow(gapminder)


# summary
gapminder %>% select(lifeExp, gdpPercap, pop) %>% 
  summary() %>% xtable::xtable()
#summary lifeExp por continente
gapminder %>% group_by(continent) %>% 
  summarise(n_country = n_distinct(country),
            min = min(lifeExp),
            Q1 = quantile(lifeExp, 0.25),
            median = median(lifeExp),
            Q3 = quantile(lifeExp, 0.75), 
            max = max(lifeExp)) %>% xtable::xtable()

#plots facetados continente e década
gapminder %>% ggplot(aes(x = factor(decada), y = lifeExp, color = continent)) +
  geom_jitter(aes(alpha = 0.1), show.legend = FALSE) +
  geom_boxplot() + #scale_y_log10() +
  facet_wrap(~continent)


# scatterplot correlation matrix
ggplt <- gapminder %>% 
  select(continent, gdpPercap, pop, lifeExp, decada) %>%
  ggpairs(
    columns = 2:ncol(.),
    mapping = ggplot2::aes(color = continent),
    diag = list(continuous = 'barDiag'),
    legend = c(2, 2),
    upper = list(continuous = wrap("cor", size = 3, alignPercent = 1)),
    lower = list(mapping = ggplot2::aes(alpha = 0.9))
  )

ggplt[1,1] <- ggplt[1,1] + scale_x_log10()
ggplt[2,2] <- ggplt[2,2] + scale_x_log10()
ggplt[3,1] <- ggplt[3,1] + scale_x_log10()
ggplt[3,2] <- ggplt[3,2] + scale_x_log10()
ggplt[4,1] <- ggplt[4,1] + scale_x_log10()
ggplt[4,2] <- ggplt[4,2] + scale_x_log10()
ggplt[2,1] <- ggplt[2,1] + scale_x_log10() + scale_y_log10()

ggplt + 
  theme(panel.grid.major = element_blank()) + 
  ggtitle('Correlation Scatterplot matrix') +
  theme(plot.title = element_text(size = 10,hjust = 0.5),
        axis.text.x = element_text(size = 8, angle=45),
        axis.text.y = element_text(size = 8, angle=45)) 




# anovas ------------------------------------------------------------------

modelo_life2 <- lm(lifeExp ~ factor(decada) + continent + continent/country, data = gapminder)
anova(modelo_life2) %>% xtable::xtable()

par(mfrow=c(2,2))
plot(modelo_life2)


tuckey_modelo_life2 <- TukeyHSD(aov(modelo_life2), "continent", ordered = T)
tuckey_modelo_life2$continent %>% xtable::xtable()


broom::tidy(modelo_life2) %>% xtable::xtable()



# codigo nao usado --------------------------------------------------------



## com componente aleatorio
lmer(lifeExp ~ decada + continent + 1|continent/country, data = gapminder)



## nested anova
gapminder$CC <- factor(gapminder$continent:gapminder$country)
xtabs(lifeExp ~ continent + CC + decada, data = gapminder)

fm1 <- aov(lifeExp ~ decada + continent + continent/country, data = gapminder)
summary(fm1)

modelo_gdp <- lm(gdpPercap ~ decada + pop+ continent/country, data = gapminder)
summary(modelo_gdp)
anova(modelo_gdp)
drop1(modelo_gdp)
