install.packages('ggplot2')
install.packages('car')
library(ggplot2)
library(car)

#read the data
two_syl <- read.csv('https://raw.githubusercontent.com/safonovanastya/R_project/main/two_syll.csv', fileEncoding = "UTF-8")
three_syl <- read.csv('https://raw.githubusercontent.com/safonovanastya/R_project/main/three_syll.csv', fileEncoding = "UTF-8")

#add relative values (the percentage of the duration of the studied vowels from the stressed one within one word)
two_syl$post_percent <- two_syl$post * 100 / two_syl$stressed
three_syl$post1_percent <- three_syl$post * 100 / three_syl$stressed
three_syl$post2_percent <- three_syl$post2 * 100 / three_syl$stressed

#Chek whether the studied values have a normal distribution
hist(two_syl$post_percent, breaks = 30)
hist(three_syl$post1_percent, breaks = 30)
hist(three_syl$post2_percent, breaks = 30)

#ANOVA
summary(aov(post_percent ~ region + syl_type + right_cont, data = two_syl))
plot(aov(post_percent ~ region + syl_type + right_cont, data = two_syl))

summary(aov(post1_percent ~ region + syl_type + right_cont, data = three_syl))
plot(aov(post1_percent ~ region + syl_type + right_cont, data = three_syl))

summary(aov(post2_percent ~ region + syl_type + right_cont, data = three_syl))
plot(aov(post2_percent ~ region + syl_type + right_cont, data = three_syl))

##TukeyHSD

TukeyHSD(aov(post_percent ~ region + syl_type + right_cont, data = two_syl))

TukeyHSD(aov(post1_percent ~ region + syl_type + right_cont, data = three_syl))

TukeyHSD(aov(post2_percent ~ region + syl_type + right_cont, data = three_syl))

#Влияние типа слога и правого контекста на длительность гласного в двухсложном слове
two_syl %>%
  group_by(syl_type, right_cont) %>%
  summarise(meanloss = mean(post_percent),
                se = sem(post_percent)) %>%
  ggplot(aes(x = syl_type, 
                y = meanloss, 
                colour = right_cont)) +
  geom_line(aes(group = right_cont), position = pd) +
  geom_pointrange(aes(ymin = meanloss - se, 
                ymax = meanloss + se), position = pd) +
  theme_minimal()

#Влияние региона и правого контекста на длительность гласного в двухсложном слове
two_syl %>%
  group_by(region, right_cont) %>%
  summarise(meanloss = mean(post_percent),
            se = sem(post_percent)) %>%
  ggplot(aes(x = region, 
             y = meanloss, 
             colour = right_cont)) +
  geom_line(aes(group = right_cont), position = pd) +
  geom_pointrange(aes(ymin = meanloss - se, 
                      ymax = meanloss + se), position = pd) +
  theme_minimal()

#Влияние региона и типа слога на длительность гласного в двухсложном слове
two_syl %>%
  group_by(region, syl_type) %>%
  summarise(meanloss = mean(post_percent),
            se = sem(post_percent)) %>%
  ggplot(aes(x = region, 
             y = meanloss, 
             colour = syl_type)) +
  geom_line(aes(group = syl_type), position = pd) +
  geom_pointrange(aes(ymin = meanloss - se, 
                      ymax = meanloss + se), position = pd) +
  theme_minimal()


#Влияние типа слога и правого контекста на длительность первого заударного гласного в трехсложном слове

three_syl %>%
  group_by(syl_type, right_cont) %>%
  summarise(meanloss = mean(post1_percent),
            se = sem(post1_percent)) %>%
  ggplot(aes(x = syl_type, 
             y = meanloss, 
             colour = right_cont)) +
  geom_line(aes(group = right_cont), position = pd) +
  geom_pointrange(aes(ymin = meanloss - se, 
                      ymax = meanloss + se), position = pd) +
  theme_minimal()

#Влияние региона и правого контекста на длительность первого заударного гласного в трехсложном слове

three_syl %>%
  group_by(region, right_cont) %>%
  summarise(meanloss = mean(post1_percent),
                se = sem(post1_percent)) %>%
  ggplot(aes(x = region, 
                 y = meanloss, 
                 colour = right_cont)) +
  geom_line(aes(group = right_cont), position = pd) +
  geom_pointrange(aes(ymin = meanloss - se, 
                  ymax = meanloss + se), position = pd) +
  theme_minimal()

#Влияние региона и типа слога на длительность первого заударного гласного в трехсложном слове

three_syl %>%
  group_by(region, syl_type) %>%
  summarise(meanloss = mean(post1_percent),
            se = sem(post1_percent)) %>%
  ggplot(aes(x = region, 
             y = meanloss, 
             colour = syl_type)) +
  geom_line(aes(group = syl_type), position = pd) +
  geom_pointrange(aes(ymin = meanloss - se, 
                      ymax = meanloss + se), position = pd) +
  theme_minimal()

#Влияние типа слога и правого контекста на длительность второго заударного гласного в трехсложном слове

three_syl %>%
  group_by(syl_type, right_cont) %>%
  summarise(meanloss = mean(post2_percent),
            se = sem(post2_percent)) %>%
  ggplot(aes(x = syl_type, 
             y = meanloss, 
             colour = right_cont)) +
  geom_line(aes(group = right_cont), position = pd) +
  geom_pointrange(aes(ymin = meanloss - se, 
                      ymax = meanloss + se), position = pd) +
  theme_minimal()

#Влияние региона и правого контекста на длительность второго заударного гласного в трехсложном слове

three_syl %>%
  group_by(region, right_cont) %>%
  summarise(meanloss = mean(post2_percent),
            se = sem(post2_percent)) %>%
  ggplot(aes(x = region, 
             y = meanloss, 
             colour = right_cont)) +
  geom_line(aes(group = right_cont), position = pd) +
  geom_pointrange(aes(ymin = meanloss - se, 
                      ymax = meanloss + se), position = pd) +
  theme_minimal()

#Влияние региона и типа слога на длительность второго заударного гласного в трехсложном слове

three_syl %>%
  group_by(region, syl_type) %>%
  summarise(meanloss = mean(post2_percent),
            se = sem(post2_percent)) %>%
  ggplot(aes(x = region, 
             y = meanloss, 
             colour = syl_type)) +
  geom_line(aes(group = syl_type), position = pd) +
  geom_pointrange(aes(ymin = meanloss - se, 
                      ymax = meanloss + se), position = pd) +
  theme_minimal()

##Factorial ANOVA для двухсложных слов для московского региона
ezANOVA(data = two_syl[two_syl$region == 'moscow', ],
  dv= post_percent,
  wid = code, 
  between = .(age, syl_type, right_cont),
  detailed = T, 
  return_aov = T)

##Factorial ANOVA для первого заудраного в трехсложных словах для московского региона
ezANOVA(data = three_syl[three_syl$region == 'moscow', ],
        dv= post1_percent,
        wid = code, 
        between = .(age, syl_type, right_cont),
        detailed = T, 
        return_aov = T)

##Factorial ANOVA для второго заударного в трехсложных словах для московского региона
ezANOVA(data = three_syl[three_syl$region == 'moscow', ],
        dv= post2_percent,
        wid = code, 
        between = .(age, syl_type, right_cont),
        detailed = T, 
        return_aov = T)
