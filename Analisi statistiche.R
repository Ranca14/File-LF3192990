library(corrplot)
library(UBStats)
library(ggplot2)
library(dplyr)

annunci <- annunci_auto_completo
rm(annunci_auto_completo)

annunci_B_D <- subset(annunci, carburante %in% c("Benzina","Diesel"))
annunci_ibride <- subset(annunci, carburante == "Elettrica/Benzina")
annunci_EV <- subset(annunci, carburante == "Elettrica")

#Faccio un'analisi delle variabili che sono presenti nei tre dataset che ho creato dal principale
#B&D
summary(annunci_B_D)
boxplot(annunci_B_D$prezzo)
distr.plot.x(annunci_B_D$prezzo, plot.type = 'histogram', breaks = c(0,10000,20000,30000,40000,50000,60000), color = 'grey')
boxplot(annunci_B_D$km_mese)
distr.plot.x(annunci_B_D$km_mese, plot.type = 'histogram', breaks = c(0,500,1000,1500,2000,2500,3000), color = 'grey')
boxplot(annunci_B_D$età)
hist(x=(annunci$età/12), main = '', xlab = 'Età (anni)', ylab = 'Frequenza assoluta', labels = TRUE )
boxplot(annunci_B_D$consumi)
boxplot(annunci_B_D$MSRP)
distr.plot.x(annunci_B_D$MSRP, plot.type = 'histogram', breaks = c(20000,40000,60000,80000,100000,120000), color = 'grey')

#Ibride
summary(annunci_ibride)
boxplot(annunci_ibride$prezzo)
distr.plot.x(annunci_ibride$prezzo, plot.type = 'histogram', breaks = c(0,10000,20000,30000,40000,50000,60000), color = 'grey')
boxplot(annunci_ibride$km_mese)
distr.plot.x(annunci_ibride$km_mese, plot.type = 'histogram', breaks = c(0,500,1000,1500,2000,2500,3000), color = 'grey')
boxplot(annunci_ibride$età)
hist(x=(annunci_ibride$età/12), main = '', xlab = 'Età (anni)', ylab = 'Frequenza assoluta', labels = TRUE )
boxplot(annunci_ibride$consumi)
boxplot(annunci_ibride$MSRP)
distr.plot.x(annunci_ibride$MSRP, plot.type = 'histogram', breaks = c(20000,40000,60000,80000,100000,120000), color = 'grey')

#Elettriche
summary(annunci_EV)
boxplot(annunci_EV$prezzo)
distr.plot.x(annunci_EV$prezzo, plot.type = 'histogram', breaks = c(0,10000,20000,30000,40000,50000,60000), color = 'grey')
boxplot(annunci_EV$km_mese)
distr.plot.x(annunci_EV$km_mese, plot.type = 'histogram', breaks = c(0,500,1000,1500,2000,2500,3000), color = 'grey')
boxplot(annunci_EV$età)
hist(x=(annunci_EV$età/12), main = '', xlab = 'Età (anni)', ylab = 'Frequenza assoluta', labels = TRUE )
boxplot(annunci_EV$consumi_elettriche)
boxplot(annunci_EV$MSRP)
distr.plot.x(annunci_EV$MSRP, plot.type = 'histogram', breaks = c(20000,40000,60000,80000,100000,120000), color = 'grey')

#Creo le matrici delle correlazioni per i tre sotto-dataframe e ottengo la matrice ben formattata

corr_matrix_B_D <- cor(annunci_B_D[,c('km_mese','età','consumi', 'reddito','capillarità_rete_di_ricarica','MSRP')])
corr_matrix_ibride <- cor(annunci_ibride[,c('km_mese','età','consumi', 'reddito','capillarità_rete_di_ricarica','MSRP')])
corr_matrix_EV <- cor(annunci_EV[,c('km_mese','età','consumi_elettriche', 'reddito','capillarità_rete_di_ricarica','MSRP')])

corrplot(corr_matrix_B_D, method = 'color', addCoef.col = "black", tl.cex = 0.9, tl.col = 'black', tl.srt = 45)
corrplot(corr_matrix_ibride, method = 'color', addCoef.col = "black", tl.cex = 0.9, tl.col = 'black', tl.srt = 45)
corrplot(corr_matrix_EV, method = 'color', addCoef.col = "black", tl.cex = 0.9, tl.col = 'black', tl.srt = 45)

#Procedo a creare le variabili dummy
annunci_B_D$marchio <- factor(annunci_B_D$marchio, levels = c("volkswagen","mercedes-benz", 'audi', 'renault', 'bmw'))
annunci_ibride$marchio <- factor(annunci_ibride$marchio, levels = c('volkswagen', "mercedes-benz", 'audi', 'renault', 'bmw'))
annunci_EV$marchio <- factor(annunci_EV$marchio, levels = c("volkswagen","tesla", "bmw", "audi", "renault"))

#Verifico se ci sono delle trasformazioni da effettuare
#B_D
distr.plot.xy(annunci_B_D$km_mese, annunci_B_D$prezzo, plot.type = "scatter", fitline = TRUE)
distr.plot.xy(annunci_B_D$km_mese, log(annunci_B_D$prezzo), plot.type = "scatter", fitline = TRUE)

distr.plot.xy(annunci_B_D$età, annunci_B_D$prezzo, plot.type = "scatter", fitline = TRUE)
distr.plot.xy(annunci_B_D$età, log(annunci_B_D$prezzo), plot.type = "scatter", fitline = TRUE)

distr.plot.xy(annunci_B_D$consumi, annunci_B_D$prezzo, plot.type = "scatter", fitline = TRUE)
distr.plot.xy(annunci_B_D$consumi, log(annunci_B_D$prezzo), plot.type = "scatter", fitline = TRUE)

distr.plot.xy(annunci_B_D$MSRP, annunci_B_D$prezzo, plot.type = "scatter", fitline = TRUE)
distr.plot.xy(annunci_B_D$MSRP, log(annunci_B_D$prezzo), plot.type = "scatter", fitline = TRUE)
distr.plot.xy(log(annunci_B_D$MSRP), log(annunci_B_D$prezzo), plot.type = "scatter", fitline = TRUE)

#Ibride
distr.plot.xy(annunci_ibride$km_mese, annunci_ibride$prezzo, plot.type = "scatter", fitline = TRUE)
distr.plot.xy(annunci_ibride$km_mese, log(annunci_ibride$prezzo), plot.type = "scatter", fitline = TRUE)

distr.plot.xy(annunci_ibride$età, annunci_ibride$prezzo, plot.type = "scatter", fitline = TRUE)
distr.plot.xy(annunci_ibride$età, log(annunci_ibride$prezzo), plot.type = "scatter", fitline = TRUE)

distr.plot.xy(annunci_ibride$consumi, annunci_ibride$prezzo, plot.type = "scatter", fitline = TRUE)
distr.plot.xy(annunci_ibride$consumi, log(annunci_ibride$prezzo), plot.type = "scatter", fitline = TRUE)

distr.plot.xy(annunci_ibride$MSRP, annunci_ibride$prezzo, plot.type = "scatter", fitline = TRUE)
distr.plot.xy(annunci_ibride$MSRP, log(annunci_ibride$prezzo), plot.type = "scatter", fitline = TRUE)
distr.plot.xy(log(annunci_ibride$MSRP), log(annunci_ibride$prezzo), plot.type = "scatter", fitline = TRUE)

#EV
distr.plot.xy(annunci_EV$km_mese, annunci_EV$prezzo, plot.type = "scatter", fitline = TRUE)
distr.plot.xy(annunci_EV$km_mese, log(annunci_EV$prezzo), plot.type = "scatter", fitline = TRUE)

distr.plot.xy(annunci_EV$età, annunci_EV$prezzo, plot.type = "scatter", fitline = TRUE)
distr.plot.xy(annunci_EV$età, log(annunci_EV$prezzo), plot.type = "scatter", fitline = TRUE)

distr.plot.xy(annunci_EV$consumi, annunci_EV$prezzo, plot.type = "scatter", fitline = TRUE)
distr.plot.xy(annunci_EV$consumi, log(annunci_EV$prezzo), plot.type = "scatter", fitline = TRUE)

distr.plot.xy(annunci_EV$MSRP, annunci_EV$prezzo, plot.type = "scatter", fitline = TRUE)
distr.plot.xy(annunci_EV$MSRP, log(annunci_EV$prezzo), plot.type = "scatter", fitline = TRUE)
distr.plot.xy(log(annunci_EV$MSRP), log(annunci_EV$prezzo), plot.type = "scatter", fitline = TRUE)

#Creo i modelli di regressione lineare multipla
#B_D
m_B_D <- lm(log(prezzo) ~ marchio + km_mese + età + consumi + log(MSRP) + reddito + capillarità_rete_di_ricarica, data = annunci_B_D)
summary(m_B_D)
m_B_D_2 <- lm(log(prezzo) ~ marchio + km_mese + età + log(MSRP) + reddito + capillarità_rete_di_ricarica, data = annunci_B_D)
summary(m_B_D_2)
#Faccio l'analisi dei residui
plot(m_B_D)

#Ibride
m_ibride <- lm(log(prezzo) ~ marchio + km_mese + età + consumi + log(MSRP) + reddito + capillarità_rete_di_ricarica, data = annunci_ibride)
summary(m_ibride)
m_ibride_2 <- lm(log(prezzo) ~ marchio + km_mese + età + log(MSRP) + reddito + capillarità_rete_di_ricarica, data = annunci_ibride)
summary(m_ibride_2)
#Faccio l'analisi dei residui
plot(m_ibride)

#EV
m_EV <- lm(log(prezzo) ~ marchio + km_mese + età + consumi_elettriche + reddito + capillarità_rete_di_ricarica + log(MSRP), data = annunci_EV)
summary(m_EV)
m_EV_2 <- lm(log(prezzo) ~ marchio + km_mese + età + log(MSRP) + reddito + capillarità_rete_di_ricarica, data = annunci_EV)
summary(m_EV_2)
#Faccio l'analisi dei residui
plot(m_EV)


#analisi grafica
#Faccio un summary di annunci per determinare i valori medi di riferimento per la rappresentazione grafica
summary(annunci)
#Partiamo con analizzare l'impatto dell'età
eta_seq_BD <- seq(0,180,length.out = nrow(annunci_B_D))

df_BD <- data.frame(
  marchio = 'volkswagen',
  km_mese = 1200,
  età = eta_seq_BD,
  reddito = 49645,
  capillarità_rete_di_ricarica = 19.71,
  MSRP = 45000,
  carburante = 'benzina/diesel'
)

eta_seq_ibride <- seq(0,180,length.out = nrow(annunci_ibride))

df_ibride <- data.frame(
  marchio = 'volkswagen',
  km_mese = 1200,
  età = eta_seq_ibride,
  reddito = 49645,
  capillarità_rete_di_ricarica = 19.71,
  MSRP = 45000,
  carburante = 'ibrida'
)

eta_seq_EV = seq(0,180,length.out = nrow(annunci_EV))

df_EV <- data.frame(
  marchio = 'volkswagen',
  km_mese = 1200,
  età = eta_seq_EV,
  reddito = 49645,
  capillarità_rete_di_ricarica = 19.71,
  MSRP = 45000,
  carburante = 'elettrica'
)

df_BD$prezzo_pred <- exp(predict(m_B_D_2, newdata = df_BD))
df_BD$valore_residuo <- df_BD$prezzo_pred/df_BD$MSRP
valore0_BD <- df_BD$valore_residuo[df_BD$età == 0]
df_BD$valore_residuo_norm <- df_BD$valore_residuo/valore0_BD

df_ibride$prezzo_pred = exp(predict(m_ibride_2, newdata = df_ibride))
df_ibride$valore_residuo <- df_ibride$prezzo_pred/df_ibride$MSRP
valore0_ibride <- df_ibride$valore_residuo[df_ibride$età == 0]
df_ibride$valore_residuo_norm <- df_ibride$valore_residuo/valore0_ibride

df_EV$prezzo_pred = exp(predict(m_EV_2, newdata = df_EV))
df_EV$valore_residuo <- df_EV$prezzo_pred/df_EV$MSRP
valore0_EV <- df_EV$valore_residuo[df_EV$età == 0]
df_EV$valore_residuo_norm <- df_EV$valore_residuo/valore0_EV

df_final <- rbind(df_BD,df_ibride,df_EV)

ggplot(df_final, aes(x=età,y=valore_residuo_norm,color=carburante)) + geom_line(size = 1.2) + labs(x="Età (mesi)", y="Valore residuo previsto", color="Carburante") + theme_minimal()

#Analisi dell'impatto del prezzo di listino
MSRP_seq_BD <- seq(0,max(annunci$MSRP[annunci$marchio=="volkswagen"]),length.out = nrow(annunci_B_D))

df_BD_2 <- data.frame(
  marchio = 'volkswagen',
  km_mese = 1200,
  età = 61.17,
  reddito = 49645,
  capillarità_rete_di_ricarica = 19.71,
  MSRP = MSRP_seq_BD,
  carburante = 'benzina/diesel'
)

MSRP_seq_ibride <- seq(0,max(annunci$MSRP[annunci$marchio=="volkswagen"]),length.out = nrow(annunci_ibride))

df_ibride_2 <- data.frame(
  marchio = 'volkswagen',
  km_mese = 1200,
  età = 61.17,
  reddito = 49645,
  capillarità_rete_di_ricarica = 19.71,
  MSRP = MSRP_seq_ibride,
  carburante = 'ibrida'
)

MSRP_seq_EV = seq(0,max(annunci$MSRP[annunci$marchio=="volkswagen"]),length.out = nrow(annunci_EV))

df_EV_2 <- data.frame(
  marchio = 'volkswagen',
  km_mese = 1200,
  età = 61.17,
  reddito = 49645,
  capillarità_rete_di_ricarica = 19.71,
  MSRP = MSRP_seq_EV,
  carburante = 'elettrica'
)

df_BD_2$prezzo_pred <- exp(predict(m_B_D_2, newdata = df_BD_2))
df_ibride_2$prezzo_pred = exp(predict(m_ibride_2, newdata = df_ibride_2))
df_EV_2$prezzo_pred = exp(predict(m_EV_2, newdata = df_EV_2))

df_final_2 <- rbind(df_BD_2,df_ibride_2,df_EV_2)

ggplot(df_final_2, aes(x=MSRP,y=prezzo_pred,color=carburante)) + geom_line(size = 1.2) + labs(x="MSRP", y="Prezzo previsto", color="Carburante") + theme_minimal()

#Procedo con l'analisi grafica dell'impatto del chilometraggio mensile sul prezzo di rivendita
km_mese_seq_BD <- seq(0,max(annunci_B_D$km_mese),length.out = nrow(annunci_B_D))

df_BD_3 <- data.frame(
  marchio = 'volkswagen',
  km_mese = km_mese_seq_BD,
  età = 60,
  reddito = 49645,
  capillarità_rete_di_ricarica = 19.71,
  MSRP = 45000,
  carburante = 'benzina/diesel'
)

km_mese_ibride <- seq(0,max(annunci_ibride$km_mese),length.out = nrow(annunci_ibride))

df_ibride_3 <- data.frame(
  marchio = 'volkswagen',
  km_mese = km_mese_ibride,
  età = 60,
  reddito = 49645,
  capillarità_rete_di_ricarica = 19.71,
  MSRP = 45000,
  carburante = 'ibrida'
)

km_mese_seq_EV = seq(0,max(annunci_EV$km_mese),length.out = nrow(annunci_EV))

df_EV_3 <- data.frame(
  marchio = 'volkswagen',
  km_mese = km_mese_seq_EV,
  età = 60,
  reddito = 49645,
  capillarità_rete_di_ricarica = 19.71,
  MSRP = 45000,
  carburante = 'elettrica'
)

df_BD_3$prezzo_pred <- exp(predict(m_B_D_2, newdata = df_BD_3))
df_BD_3$valore_residuo <- df_BD_3$prezzo_pred/df_BD_3$MSRP

df_ibride_3$prezzo_pred = exp(predict(m_ibride_2, newdata = df_ibride_3))
df_ibride_3$valore_residuo <- df_ibride_3$prezzo_pred/df_ibride_3$MSRP

df_EV_3$prezzo_pred = exp(predict(m_EV_2, newdata = df_EV_3))
df_EV_3$valore_residuo <- df_EV_3$prezzo_pred/df_EV_3$MSRP

df_final_3 <- rbind(df_BD_3,df_ibride_3,df_EV_3)

ggplot(df_final_3, aes(x=km_mese*età,y=valore_residuo,color=carburante)) + geom_line(size = 1.2) + labs(x="km al mese 61.17", y="Valore residuo previsto", color="Carburante") + theme_minimal()

#Si può concludere che in ogni caso la variabile con maggiore impatto è l'età. Ora voglio procedere con un'ultima analisi, ossia una sensitivity analysis della stessa rispetto alle altre due variabili con maggior impatto: MSRP e chilometraggio mensile
eta_seq_BD_sens <- seq(0, max(annunci$età),length.out = nrow(annunci_B_D))
km_mese_BD_sens <- c(500,1189,1500,2000,2500)

df_sens_kmmese_BD <- expand.grid(
  età = eta_seq_BD_sens,
  km_mese = km_mese_BD_sens
) %>%
  mutate(
    marchio = "volkswagen",
    reddito = 49645,
    capillarità_rete_di_ricarica = 19.71,
    MSRP = 45000
  )

df_sens_kmmese_BD$prezzo_pred <- exp(predict(m_B_D_2,newdata = df_sens_kmmese_BD))
df_sens_kmmese_BD$valore_residuo <- df_sens_kmmese_BD$prezzo_pred/df_sens_kmmese_BD$MSRP

ggplot(df_sens_kmmese_BD, aes(x=età,y=valore_residuo,color = as.factor(km_mese))) + geom_line(size = 1.2) + labs(x="Età (mesi)", y="Valore residuo previsto", color = "km/mese", subtitle = "Benzina/Diesel") + theme_minimal()

eta_seq_ibride_sens <- seq(0, max(annunci$età),length.out = nrow(annunci_ibride))
km_mese_ibride_sens <- c(500,1189,1500,2000,2500)

df_sens_kmmese_ibride <- expand.grid(
  età = eta_seq_ibride_sens,
  km_mese = km_mese_ibride_sens
) %>% 
  mutate (
    marchio = "volkswagen",
    reddito = 49645,
    capillarità_rete_di_ricarica = 19.71,
    MSRP = 45000
  )

df_sens_kmmese_ibride$prezzo_pred <- exp(predict(m_ibride_2, newdata = df_sens_kmmese_ibride))
df_sens_kmmese_ibride$valore_residuo <- df_sens_kmmese_ibride$prezzo_pred/df_sens_kmmese_ibride$MSRP

ggplot(df_sens_kmmese_ibride, aes(x=età,y=valore_residuo,color = as.factor(km_mese))) + geom_line(size = 1.2) + labs(x="Età (mesi)", y="Valore residuo previsto", color = "km/mese", subtitle = "Ibrida") + theme_minimal()

eta_seq_EV_sens <- seq(0, max(annunci$età),length.out = nrow(annunci_EV))
km_mese_EV_sens <- c(500,1189,1500,2000,2500)

df_sens_kmmese_EV <- expand.grid(
  età = eta_seq_EV_sens,
  km_mese = km_mese_EV_sens
) %>% 
  mutate (
    marchio = "volkswagen",
    reddito = 49645,
    capillarità_rete_di_ricarica = 19.71,
    MSRP = 45000
  )

df_sens_kmmese_EV$prezzo_pred <- exp(predict(m_EV_2, newdata = df_sens_kmmese_EV))
df_sens_kmmese_EV$valore_residuo <- df_sens_kmmese_EV$prezzo_pred/df_sens_kmmese_EV$MSRP

ggplot(df_sens_kmmese_EV, aes(x=età,y=valore_residuo,color = as.factor(km_mese))) + geom_line(size = 1.2) + labs(x="Età (mesi)", y="Valore residuo previsto", color = "km/mese", subtitle = "Elettrica") + theme_minimal()

#Ora procedo con la sensitivity analysis rispetto a MSRP
MSRP_BD_sens <- c(30000, 45333, 60000, 80000)

df_sens_MSRP_BD <- expand.grid(
  età = eta_seq_BD_sens,
  MSRP = MSRP_BD_sens
) %>%
  mutate (
    marchio = "volkswagen",
    reddito = 49645,
    capillarità_rete_di_ricarica = 19.71,
    km_mese = 1189
  )

df_sens_MSRP_BD$prezzo_pred <- exp(predict(m_B_D_2, newdata = df_sens_MSRP_BD))
df_sens_MSRP_BD$valore_residuo <- df_sens_MSRP_BD$prezzo_pred/df_sens_MSRP_BD$MSRP

ggplot(df_sens_MSRP_BD, aes(x=età,y=valore_residuo,color = as.factor(MSRP))) + geom_line(size = 1.2) + labs(x="Età (mesi)", y="Valore residuo previsto", color = "MSRP", subtitle = "Benzina/Diesel") + theme_minimal()

MSRP_ibride_sens <- c(30000, 45333, 60000, 80000)

df_sens_MSRP_ibride <- expand.grid(
  età = eta_seq_ibride_sens,
  MSRP = MSRP_ibride_sens
) %>%
  mutate (
    marchio = "volkswagen",
    reddito = 49645,
    capillarità_rete_di_ricarica = 19.71,
    km_mese = 1189
  )

df_sens_MSRP_ibride$prezzo_pred <- exp(predict(m_ibride_2, newdata = df_sens_MSRP_ibride))
df_sens_MSRP_ibride$valore_residuo <- df_sens_MSRP_ibride$prezzo_pred/df_sens_MSRP_ibride$MSRP

ggplot(df_sens_MSRP_ibride, aes(x=età,y=valore_residuo,color = as.factor(MSRP))) + geom_line(size = 1.2) + labs(x="Età (mesi)", y="Valore residuo previsto", color = "MSRP", subtitle = "Ibrida") + theme_minimal()

MSRP_EV_sens <- c(30000, 45333, 60000, 80000)

df_sens_MSRP_EV <- expand.grid(
  età = eta_seq_EV_sens,
  MSRP = MSRP_EV_sens
) %>%
  mutate (
    marchio = "volkswagen",
    reddito = 49645,
    capillarità_rete_di_ricarica = 19.71,
    km_mese = 1189
  )

df_sens_MSRP_EV$prezzo_pred <- exp(predict(m_EV_2, newdata = df_sens_MSRP_EV))
df_sens_MSRP_EV$valore_residuo <- df_sens_MSRP_EV$prezzo_pred/df_sens_MSRP_EV$MSRP

ggplot(df_sens_MSRP_EV, aes(x=età,y=valore_residuo,color = as.factor(MSRP))) + geom_line(size = 1.2) + labs(x="Età (mesi)", y="Valore residuo previsto", color = "MSRP", subtitle = "Elettrica") + theme_minimal()


#Procedo ad analizzare l'impatto dell'età dividendola per nazione (Italia, Germania, Austria, Spagna, Belgio, Olanda, Francia)

annunci_B_D_i <- subset(annunci_B_D, stato == "i")
annunci_ibride_i <- subset(annunci_ibride, stato == "i")
annunci_EV_i <- subset(annunci_EV, stato == "i")

annunci_B_D_de <- subset(annunci_B_D, stato == "d")
annunci_ibride_de <- subset(annunci_ibride, stato == "d")
annunci_EV_de <- subset(annunci_EV, stato == "d")

annunci_B_D_a <- subset(annunci_B_D, stato == "a")
annunci_ibride_a <- subset(annunci_ibride, stato == "a")
annunci_EV_a <- subset(annunci_EV, stato == "a")

annunci_B_D_s <- subset(annunci_B_D, stato == "e")
annunci_ibride_s <- subset(annunci_ibride, stato == "e")
annunci_EV_s <- subset(annunci_EV, stato == "e")

annunci_B_D_b <- subset(annunci_B_D, stato == "b")
annunci_ibride_b <- subset(annunci_ibride, stato == "b")
annunci_EV_b <- subset(annunci_EV, stato == "b")

annunci_B_D_nl <- subset(annunci_B_D, stato == "nl")
annunci_ibride_nl <- subset(annunci_ibride, stato == "nl")
annunci_EV_nl <- subset(annunci_EV, stato == "nl")

annunci_B_D_f <- subset(annunci_B_D, stato == "f")
annunci_ibride_f <- subset(annunci_ibride, stato == "f")
annunci_EV_f <- subset(annunci_EV, stato == "f")

#B/D
m_BD_i <- lm(log(prezzo)~ marchio + km_mese + età + log(MSRP), data = annunci_B_D_i)
summary(m_BD_i)

m_BD_de <- lm(log(prezzo)~ marchio + km_mese + età + log(MSRP), data = annunci_B_D_de)
summary(m_BD_de)

m_BD_a <- lm(log(prezzo)~ marchio + km_mese + età + log(MSRP), data = annunci_B_D_a)
summary(m_BD_a)

m_BD_s <- lm(log(prezzo)~ marchio + km_mese + età + log(MSRP), data = annunci_B_D_s)
summary(m_BD_s)

m_BD_b <- lm(log(prezzo)~ marchio + km_mese + età + log(MSRP), data = annunci_B_D_b)
summary(m_BD_b)

m_BD_nl <- lm(log(prezzo)~ marchio + km_mese + età + log(MSRP), data = annunci_B_D_nl)
summary(m_BD_nl)

m_BD_f <- lm(log(prezzo)~ marchio + km_mese + età + log(MSRP), data = annunci_B_D_f)
summary(m_BD_f)

#Ibride
m_ibride_i <- lm(log(prezzo)~ marchio + km_mese + età + log(MSRP), data = annunci_ibride_i)
summary(m_ibride_i)

m_ibride_de <- lm(log(prezzo)~ marchio + km_mese + età + log(MSRP), data = annunci_ibride_de)
summary(m_ibride_de)

m_ibride_a <- lm(log(prezzo)~ marchio + km_mese + età + log(MSRP), data = annunci_ibride_a)
summary(m_ibride_a)

m_ibride_s <- lm(log(prezzo)~ marchio + km_mese + età + log(MSRP), data = annunci_ibride_s)
summary(m_ibride_s)

m_ibride_b <- lm(log(prezzo)~ marchio + km_mese + età + log(MSRP), data = annunci_ibride_b)
summary(m_ibride_b)

m_ibride_nl <- lm(log(prezzo)~ marchio + km_mese + età + log(MSRP), data = annunci_ibride_nl)
summary(m_ibride_nl)

m_ibride_f <- lm(log(prezzo)~ marchio + km_mese + età + log(MSRP), data = annunci_ibride_f)
summary(m_ibride_f)

#EV
m_EV_i <- lm(log(prezzo)~ marchio + km_mese + età + log(MSRP), data = annunci_EV_i)
summary(m_EV_i)

m_EV_de <- lm(log(prezzo)~ marchio + km_mese + età + log(MSRP), data = annunci_EV_de)
summary(m_EV_de)

m_EV_a <- lm(log(prezzo)~ marchio + km_mese + età + log(MSRP), data = annunci_EV_a)
summary(m_EV_a)

m_EV_s <- lm(log(prezzo)~ marchio + km_mese + età + log(MSRP), data = annunci_EV_s)
summary(m_EV_s)

m_EV_b <- lm(log(prezzo)~ marchio + km_mese + età + log(MSRP), data = annunci_EV_b)
summary(m_EV_b)

m_EV_nl <- lm(log(prezzo)~ marchio + km_mese + età + log(MSRP), data = annunci_EV_nl)
summary(m_EV_nl)

m_EV_f <- lm(log(prezzo)~ marchio + km_mese + età + log(MSRP), data = annunci_EV_f)
summary(m_EV_f)

#Creo i dataframe da unire per creare il grafico
#Italia

eta_seq_BD_i <- seq(0,180,length.out = nrow(annunci_B_D_i))

df_BD_i <- data.frame(
  marchio = "volkswagen",
  km_mese = 1189,
  età = eta_seq_BD_i,
  MSRP = 45333,
  carburante = "Benzina/Diesel",
  stato = "Italia"
)

df_BD_i$prezzo_pred <- exp(predict(m_BD_i, newdata = df_BD_i))
df_BD_i$valore_residuo <- df_BD_i$prezzo_pred/df_BD_i$MSRP
valore0_BD_i <- df_BD_i$valore_residuo[df_BD_i$età == 0]
df_BD_i$valore_residuo_norm <- df_BD_i$valore_residuo/valore0_BD_i

eta_seq_ibride_i <- seq(0,180, length.out = nrow(annunci_ibride_i))

df_ibride_i <- data.frame(
  marchio = "volkswagen",
  km_mese = 1189,
  età = eta_seq_ibride_i,
  MSRP = 45333,
  carburante = "Ibrida",
  stato = "Italia"
)

df_ibride_i$prezzo_pred <- exp(predict(m_ibride_i, newdata = df_ibride_i))
df_ibride_i$valore_residuo <- df_ibride_i$prezzo_pred/df_ibride_i$MSRP
valore0_ibride_i <- df_ibride_i$valore_residuo[df_ibride_i$età == 0]
df_ibride_i$valore_residuo_norm <- df_ibride_i$valore_residuo/valore0_ibride_i

eta_seq_EV_i <- seq(0,180, length.out = nrow(annunci_EV_i))

df_EV_i <- data.frame(
  marchio = "volkswagen",
  km_mese = 1189,
  età = eta_seq_EV_i,
  MSRP = 45333,
  carburante = "Elettrica",
  stato = "Italia"
)

df_EV_i$prezzo_pred <- exp(predict(m_EV_i, newdata = df_EV_i))
df_EV_i$valore_residuo <- df_EV_i$prezzo_pred/df_EV_i$MSRP
valore0_EV_i <- df_EV_i$valore_residuo[df_EV_i$età == 0]
df_EV_i$valore_residuo_norm <- df_EV_i$valore_residuo/valore0_EV_i

#Germania

eta_seq_BD_d <- seq(0,180,length.out = nrow(annunci_B_D_de))

df_BD_d <- data.frame(
  marchio = "volkswagen",
  km_mese = 1189,
  età = eta_seq_BD_d,
  MSRP = 45333,
  carburante = "Benzina/Diesel",
  stato = "Germania"
)

df_BD_d$prezzo_pred <- exp(predict(m_BD_de, newdata = df_BD_d))
df_BD_d$valore_residuo <- df_BD_d$prezzo_pred/df_BD_d$MSRP
valore0_BD_d <- df_BD_d$valore_residuo[df_BD_d$età == 0]
df_BD_d$valore_residuo_norm <- df_BD_d$valore_residuo/valore0_BD_d

eta_seq_ibride_d <- seq(0,180, length.out = nrow(annunci_ibride_de))

df_ibride_d <- data.frame(
  marchio = "volkswagen",
  km_mese = 1189,
  età = eta_seq_ibride_d,
  MSRP = 45333,
  carburante = "Ibrida",
  stato = "Germania"
)

df_ibride_d$prezzo_pred <- exp(predict(m_ibride_de, newdata = df_ibride_d))
df_ibride_d$valore_residuo <- df_ibride_d$prezzo_pred/df_ibride_d$MSRP
valore0_ibride_d <- df_ibride_d$valore_residuo[df_ibride_d$età == 0]
df_ibride_d$valore_residuo_norm <- df_ibride_d$valore_residuo/valore0_ibride_d

eta_seq_EV_d <- seq(0,180, length.out = nrow(annunci_EV_de))

df_EV_d <- data.frame(
  marchio = "volkswagen",
  km_mese = 1189,
  età = eta_seq_EV_d,
  MSRP = 45333,
  carburante = "Elettrica",
  stato = "Germania"
)

df_EV_d$prezzo_pred <- exp(predict(m_EV_de, newdata = df_EV_d))
df_EV_d$valore_residuo <- df_EV_d$prezzo_pred/df_EV_d$MSRP
valore0_EV_d <- df_EV_d$valore_residuo[df_EV_d$età == 0]
df_EV_d$valore_residuo_norm <- df_EV_d$valore_residuo/valore0_EV_d

#Austria

eta_seq_BD_a <- seq(0,180,length.out = nrow(annunci_B_D_a))

df_BD_a <- data.frame(
  marchio = "volkswagen",
  km_mese = 1189,
  età = eta_seq_BD_a,
  MSRP = 45333,
  carburante = "Benzina/Diesel",
  stato = "Austria"
)

df_BD_a$prezzo_pred <- exp(predict(m_BD_a, newdata = df_BD_a))
df_BD_a$valore_residuo <- df_BD_a$prezzo_pred/df_BD_a$MSRP
valore0_BD_a <- df_BD_a$valore_residuo[df_BD_a$età == 0]
df_BD_a$valore_residuo_norm <- df_BD_a$valore_residuo/valore0_BD_a

eta_seq_ibride_a <- seq(0,180, length.out = nrow(annunci_ibride_a))

df_ibride_a <- data.frame(
  marchio = "volkswagen",
  km_mese = 1189,
  età = eta_seq_ibride_a,
  MSRP = 45333,
  carburante = "Ibrida",
  stato = "Austria"
)

df_ibride_a$prezzo_pred <- exp(predict(m_ibride_a, newdata = df_ibride_a))
df_ibride_a$valore_residuo <- df_ibride_a$prezzo_pred/df_ibride_a$MSRP
valore0_ibride_a <- df_ibride_a$valore_residuo[df_ibride_a$età == 0]
df_ibride_a$valore_residuo_norm <- df_ibride_a$valore_residuo/valore0_ibride_a

eta_seq_EV_a <- seq(0,180, length.out = nrow(annunci_EV_a))

df_EV_a <- data.frame(
  marchio = "volkswagen",
  km_mese = 1189,
  età = eta_seq_EV_a,
  MSRP = 45333,
  carburante = "Elettrica",
  stato = "Austria"
)

df_EV_a$prezzo_pred <- exp(predict(m_EV_a, newdata = df_EV_a))
df_EV_a$valore_residuo <- df_EV_a$prezzo_pred/df_EV_a$MSRP
valore0_EV_a <- df_EV_a$valore_residuo[df_EV_a$età == 0]
df_EV_a$valore_residuo_norm <- df_EV_a$valore_residuo/valore0_EV_a

#Spagna

eta_seq_BD_s <- seq(0,180,length.out = nrow(annunci_B_D_s))

df_BD_s <- data.frame(
  marchio = "volkswagen",
  km_mese = 1189,
  età = eta_seq_BD_s,
  MSRP = 45333,
  carburante = "Benzina/Diesel",
  stato = "Spagna"
)

df_BD_s$prezzo_pred <- exp(predict(m_BD_s, newdata = df_BD_s))
df_BD_s$valore_residuo <- df_BD_s$prezzo_pred/df_BD_s$MSRP
valore0_BD_s <- df_BD_s$valore_residuo[df_BD_s$età == 0]
df_BD_s$valore_residuo_norm <- df_BD_s$valore_residuo/valore0_BD_s

eta_seq_ibride_s <- seq(0,180, length.out = nrow(annunci_ibride_s))

df_ibride_s <- data.frame(
  marchio = "volkswagen",
  km_mese = 1189,
  età = eta_seq_ibride_s,
  MSRP = 45333,
  carburante = "Ibrida",
  stato = "Spagna"
)

df_ibride_s$prezzo_pred <- exp(predict(m_ibride_s, newdata = df_ibride_s))
df_ibride_s$valore_residuo <- df_ibride_s$prezzo_pred/df_ibride_s$MSRP
valore0_ibride_s <- df_ibride_s$valore_residuo[df_ibride_s$età == 0]
df_ibride_s$valore_residuo_norm <- df_ibride_s$valore_residuo/valore0_ibride_s

eta_seq_EV_s <- seq(0,180, length.out = nrow(annunci_EV_s))

df_EV_s <- data.frame(
  marchio = "volkswagen",
  km_mese = 1189,
  età = eta_seq_EV_s,
  MSRP = 45333,
  carburante = "Elettrica",
  stato = "Spagna"
)

df_EV_s$prezzo_pred <- exp(predict(m_EV_s, newdata = df_EV_s))
df_EV_s$valore_residuo <- df_EV_s$prezzo_pred/df_EV_s$MSRP
valore0_EV_s <- df_EV_s$valore_residuo[df_EV_s$età == 0]
df_EV_s$valore_residuo_norm <- df_EV_s$valore_residuo/valore0_EV_s

#Belgio

eta_seq_BD_b <- seq(0,180,length.out = nrow(annunci_B_D_b))

df_BD_b <- data.frame(
  marchio = "volkswagen",
  km_mese = 1189,
  età = eta_seq_BD_b,
  MSRP = 45333,
  carburante = "Benzina/Diesel",
  stato = "Belgio"
)

df_BD_b$prezzo_pred <- exp(predict(m_BD_b, newdata = df_BD_b))
df_BD_b$valore_residuo <- df_BD_b$prezzo_pred/df_BD_b$MSRP
valore0_BD_b <- df_BD_b$valore_residuo[df_BD_b$età == 0]
df_BD_b$valore_residuo_norm <- df_BD_b$valore_residuo/valore0_BD_b

eta_seq_ibride_b <- seq(0,180, length.out = nrow(annunci_ibride_b))

df_ibride_b <- data.frame(
  marchio = "volkswagen",
  km_mese = 1189,
  età = eta_seq_ibride_b,
  MSRP = 45333,
  carburante = "Ibrida",
  stato = "Belgio"
)

df_ibride_b$prezzo_pred <- exp(predict(m_ibride_b, newdata = df_ibride_b))
df_ibride_b$valore_residuo <- df_ibride_b$prezzo_pred/df_ibride_b$MSRP
valore0_ibride_b <- df_ibride_b$valore_residuo[df_ibride_b$età == 0]
df_ibride_b$valore_residuo_norm <- df_ibride_b$valore_residuo/valore0_ibride_b

eta_seq_EV_b <- seq(0,180, length.out = nrow(annunci_EV_b))

df_EV_b <- data.frame(
  marchio = "volkswagen",
  km_mese = 1189,
  età = eta_seq_EV_b,
  MSRP = 45333,
  carburante = "Elettrica",
  stato = "Belgio"
)

df_EV_b$prezzo_pred <- exp(predict(m_EV_b, newdata = df_EV_b))
df_EV_b$valore_residuo <- df_EV_b$prezzo_pred/df_EV_b$MSRP
valore0_EV_b <- df_EV_b$valore_residuo[df_EV_b$età == 0]
df_EV_b$valore_residuo_norm <- df_EV_b$valore_residuo/valore0_EV_b

#Olanda

eta_seq_BD_nl <- seq(0,180,length.out = nrow(annunci_B_D_nl))

df_BD_nl <- data.frame(
  marchio = "volkswagen",
  km_mese = 1189,
  età = eta_seq_BD_nl,
  MSRP = 45333,
  carburante = "Benzina/Diesel",
  stato = "Olanda"
)

df_BD_nl$prezzo_pred <- exp(predict(m_BD_nl, newdata = df_BD_nl))
df_BD_nl$valore_residuo <- df_BD_nl$prezzo_pred/df_BD_nl$MSRP
valore0_BD_nl <- df_BD_nl$valore_residuo[df_BD_nl$età == 0]
df_BD_nl$valore_residuo_norm <- df_BD_nl$valore_residuo/valore0_BD_nl

eta_seq_ibride_nl <- seq(0,180, length.out = nrow(annunci_ibride_nl))

df_ibride_nl <- data.frame(
  marchio = "volkswagen",
  km_mese = 1189,
  età = eta_seq_ibride_nl,
  MSRP = 45333,
  carburante = "Ibrida",
  stato = "Olanda"
)

df_ibride_nl$prezzo_pred <- exp(predict(m_ibride_nl, newdata = df_ibride_nl))
df_ibride_nl$valore_residuo <- df_ibride_nl$prezzo_pred/df_ibride_nl$MSRP
valore0_ibride_nl <- df_ibride_nl$valore_residuo[df_ibride_nl$età == 0]
df_ibride_nl$valore_residuo_norm <- df_ibride_nl$valore_residuo/valore0_ibride_nl

eta_seq_EV_nl <- seq(0,180, length.out = nrow(annunci_EV_nl))

df_EV_nl <- data.frame(
  marchio = "volkswagen",
  km_mese = 1189,
  età = eta_seq_EV_nl,
  MSRP = 45333,
  carburante = "Elettrica",
  stato = "Olanda"
)

df_EV_nl$prezzo_pred <- exp(predict(m_EV_nl, newdata = df_EV_nl))
df_EV_nl$valore_residuo <- df_EV_nl$prezzo_pred/df_EV_nl$MSRP
valore0_EV_nl <- df_EV_nl$valore_residuo[df_EV_nl$età == 0]
df_EV_nl$valore_residuo_norm <- df_EV_nl$valore_residuo/valore0_EV_nl

#Francia

eta_seq_BD_f <- seq(0,180,length.out = nrow(annunci_B_D_f))

df_BD_f <- data.frame(
  marchio = "volkswagen",
  km_mese = 1189,
  età = eta_seq_BD_f,
  MSRP = 45333,
  carburante = "Benzina/Diesel",
  stato = "Francia"
)

df_BD_f$prezzo_pred <- exp(predict(m_BD_f, newdata = df_BD_f))
df_BD_f$valore_residuo <- df_BD_f$prezzo_pred/df_BD_f$MSRP
valore0_BD_f <- df_BD_f$valore_residuo[df_BD_f$età == 0]
df_BD_f$valore_residuo_norm <- df_BD_f$valore_residuo/valore0_BD_f

eta_seq_ibride_f <- seq(0,180, length.out = nrow(annunci_ibride_f))

df_ibride_f <- data.frame(
  marchio = "volkswagen",
  km_mese = 1189,
  età = eta_seq_ibride_f,
  MSRP = 45333,
  carburante = "Ibrida",
  stato = "Francia"
)

df_ibride_f$prezzo_pred <- exp(predict(m_ibride_f, newdata = df_ibride_f))
df_ibride_f$valore_residuo <- df_ibride_f$prezzo_pred/df_ibride_f$MSRP
valore0_ibride_f <- df_ibride_f$valore_residuo[df_ibride_f$età == 0]
df_ibride_f$valore_residuo_norm <- df_ibride_f$valore_residuo/valore0_ibride_f

eta_seq_EV_f <- seq(0,180, length.out = nrow(annunci_EV_f))

df_EV_f <- data.frame(
  marchio = "volkswagen",
  km_mese = 1189,
  età = eta_seq_EV_f,
  MSRP = 45333,
  carburante = "Elettrica",
  stato = "Francia"
)

df_EV_f$prezzo_pred <- exp(predict(m_EV_f, newdata = df_EV_f))
df_EV_f$valore_residuo <- df_EV_f$prezzo_pred/df_EV_f$MSRP
valore0_EV_f <- df_EV_f$valore_residuo[df_EV_f$età == 0]
df_EV_f$valore_residuo_norm <- df_EV_f$valore_residuo/valore0_EV_f

#Unisco tutto e creo il grafico

df_final_stati_carburanti <- rbind(df_BD_i, df_ibride_i, df_EV_i, df_BD_d, df_ibride_d, df_EV_d, df_BD_a, df_ibride_a, df_EV_a, df_BD_s, df_ibride_s, df_EV_s, df_BD_b, df_ibride_b, df_EV_b, df_BD_nl, df_ibride_nl, df_EV_nl, df_BD_f, df_ibride_f, df_EV_f)

ggplot(df_final_stati_carburanti, aes(x=età, y=valore_residuo_norm, color = stato, linetype = carburante)) + geom_line(size=0.8) + labs(x="Età (mesi)", y="Valore residuo previsto") + theme_minimal() + theme(legend.position =  "bottom") + scale_linetype_manual(values = c("Benzina/Diesel" = "solid", "Elettrica" = "twodash", "Ibrida" = "dotted")) + scale_color_manual(values = c("Austria" = "red", "Francia"= "goldenrod", "Italia"= "cyan", "Spagna" = "grey", "Belgio"= "black", "Germania" = "pink", "Olanda" = "purple"))
