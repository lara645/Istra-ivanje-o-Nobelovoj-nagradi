library("RColorBrewer")
nobel <- read.csv2('nobel_final.csv', header = TRUE, sep = ",")
attach(nobel)
nobel <- nobel[c('year', 'gender', 'category', 'age_get_prize')]
green <- colorRampPalette(c('darkseagreen1','darkseagreen2','darkseagreen3'))


#GENDER
t=table(gender)
t
tab1 <- prop.table(t)
barplot(tab1,
        ylim = range(pretty(c(0,tab1))),
        col=c('mistyrose1','darkseagreen3'),
        names.arg = c('žene','muškarci'))

# theta - nepoznata proporcija žena među dobitnicima
#Hipoteze:
#  H0: theta = 0.1 
#  H1: theta < 0.1
length(gender)
binom.test(54, 923, p = 0.1, alternative = "less")
#p=4.473e-06 < 0.05 => Na razini značajnosti 0.05 odbacujemo H0, tj. možemo tvrditi da je proporcija
#žena među dobitnicima statistički značajno manja od 10%.


prije <- prop.table(table(gender[year < 1970]))
poslije <- prop.table(table(gender[year >= 1970]))

barplot(cbind(prije,poslije),
        names = c('[1901,1970)', '[1970,2019]'),
        col=c('mistyrose1','darkseagreen3'),
        legend = c('žene','muškarci'),
        args.legend = list(x = "topright", cex = 0.8, inset = c(0.045,0.1)))

do_1970 <- gender[year < 1970]
od_1970 <- gender[year >= 1970]
addmargins(table(do_1970))
addmargins(table(od_1970))
#p1 - proporcija žena među dobitnicima od 1970. do 2019. godine
#p2 - proporcija žena među dobitnicima do 1970. godine
#Hipoteze
# H0 : p1 = p2
# H1 : p1 > p2
prop.test(c(39, 15), c(534,389) , alternative = 'greater')
#p=0.01963 < 0.05 => Na razini značajnosti 0.05 odbacujemo H0 i prihvaćamo H1,
#tj. možemo tvrditi da je proporcija nagrada koje su osvojile žene od 1970 do 2019 veća.


#AGE_GET_PRIZE
summary(age_get_prize)
sd(age_get_prize)

#prosječna dob se povećava s godinama
prosj <- aggregate(age_get_prize, list(year), mean)
colnames(prosj) <- c('year', 'mean')

plot(prosj$year,prosj$mean, xlab = 'Godina', ylab = 'Prosječna dob')

# H0: rho_s = 0
# H1: rho_s > 0
cor(prosj$year,prosj$mean,method="spearman") 
cor.test(prosj$year,prosj$mean,method="spearman",alternative = "greater")
#p=3.242e-12 < 0.05 pa odbacujemo H0 i  možemo tvrditi da postoji rastuća monotona veza


#CATEGORY
factor(category)
table(category)
tab4 <- prop.table(table(category))
tab4
barplot(sort(tab4), 
        col = brewer.pal(n = 6, name = "Set3"),
        names = c('ekonomija', 'mir', 'književnost', 'kemija', 'fizika', 'medicina'),
        ylim = range(pretty(c(0,tab4))))

184+219+213 #broj dobitnika nagrade za kemiju, medicinu ili fiziku  
#Hipoteze:
#  H0: theta = 1/2
#  H1: theta > 1/2
binom.test(616, 923, p = 0.5, alternative = "greater")
#p< 2.2e-16 < 0.05 - Na razini značajnosti 0.05  odbacujemo H0,
#tj. na razini značajnosti 0.05  možemo tvrditi da je veća proporcija dobitnika iz znanstvenih disciplina.


#CATEGORY & GENDER
tab5 <- table(category,gender)
tab6 <- prop.table(tab5,2)
barplot(tab6, 
        beside = T,
        names.arg = c('žene','muškarci'),
        legend.text = c('kemija', 'ekonomija', 'književnost', 'medicina', 'mir', 'fizika'), 
        args.legend = list(x = "topright", cex = 0.7, inset = c(0,-0.21)),
        ylim = range(pretty(c(0,tab6))),
        col = brewer.pal(n = 6, name = "Set3"))

#Hipoteze:
# H0: kategorije su jednako zastupljene kod oba spola
# H1: kategorije nisu jednako zastupljene
tab7 <- table(gender,category)
tab7
chisq.test(tab7)
#p=3.82e-08 < 0.05 - na razini značajnosti 0.05 odbacujemo H0,
# tj. možemo tvrditi da kategorije nisu jednako zastupljene među nagradama koje su osvojile žene i nagradama
# koje su osvojili muškarci


#dob prema kategoriji
tapply(age_get_prize, category, sd)
#H0: ocekivana dob dobitnika je ista u svim kategorijama
#H1: iz barem jedne kategorije ocekivana dob dobitnika je razlicita od ostalih
rez <- aov(age_get_prize ~ category)
summary(rez)
#p = 4.56e-13 < 0.05, F=13.85
tuk <- TukeyHSD(rez)
tuk
#statisticki znacajne razlike: ekonomija-kemija (p=0.0000166), knjizevnost-kemija (p=0.0008231), 
#medicina-ekonomija (p=0.0000016), fizika-ekonomija(p=0), medicina-knjizevnost(p=0.0000955), 
#fizika-knjizevnost(p=0), fizika - mir (p=0.0156014)


#GODI�NJI BROJ DOBITNIKA
mean(table(year))
hist(table(year), 
     xlab = 'broj dobitnika godi�nje', 
     ylab = NULL,
     col = green(7),
     ylim = range(pretty(c(0,35))),
     main = NULL)

#Hipoteze :
# H0 : mu = 8
# H1 : mu != 8
t.test(table(year), mu = 8)
#p=0.8773>0.05