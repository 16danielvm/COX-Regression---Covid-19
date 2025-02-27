# Cargar el paquete
library(survival)
library(survminer)


# Convertir comorbilidades a factores (opcional pero recomendado)
comorbilidades <- c('NEUMONIA', 'DIABETES', 'EPOC', 'ASMA', 'INMUSUPR',
                    'HIPERTENSION', 'OTRA_COM', 'CARDIOVASCULAR', 
                    'OBESIDAD', 'RENAL_CRONICA', 'TABAQUISMO')

# Convierte las comorbilidades a factores
df_cox_completo[comorbilidades] <- lapply(df_cox_completo[comorbilidades], factor)

# Definir la fórmula del modelo de Cox
formula_cox <- as.formula("Surv(DIAS_SINTOMAS_DEF, DEFUNCION) ~ NEUMONIA + DIABETES + EPOC + 
                          ASMA + INMUSUPR + HIPERTENSION + OTRA_COM + 
                          CARDIOVASCULAR + OBESIDAD + RENAL_CRONICA + TABAQUISMO")

# Ajustar el modelo de Cox
modelo_cox <- coxph(formula_cox, data = df_cox_completo)

# Ver el resumen del modelo
summary(modelo_cox)


# Test de proporcionalidad de riesgos
test_ph <- cox.zph(modelo_cox)
print(test_ph)

# Gráfica para verificar visualmente
ggcoxzph(test_ph)







# Ajuste del modelo Kaplan-Meier
km_model <- survfit(Surv(DIAS_SINTOMAS_DEF, DEFUNCION) ~ 1, data = df_cox_completo)

# Visualización general de la curva de supervivencia
ggsurvplot(km_model, 
           conf.int = TRUE,          # Intervalos de confianza
           pval = TRUE,              # Valor p de log-rank test
           risk.table = TRUE,        # Tabla de riesgo debajo del gráfico
           xlab = "Días", 
           ylab = "Probabilidad de Supervivencia",
           ggtheme = theme_minimal())


# Curvas de Kaplan-Meier estratificadas por DIABETES
km_diabetes <- survfit(Surv(DIAS_SINTOMAS_DEF, DEFUNCION) ~ DIABETES, data = df_cox_completo)

ggsurvplot(km_diabetes, 
           conf.int = TRUE, 
           pval = TRUE, 
           risk.table = TRUE,
           legend.labs = c("Sin Diabetes", "Con Diabetes"),
           legend.title = "Diabetes",
           xlab = "Días", 
           ylab = "Probabilidad de Supervivencia",
           ggtheme = theme_minimal())

km_comorbilidad <- survfit(Surv(DIAS_SINTOMAS_DEF, DEFUNCION) ~ DIABETES, data = df_cox_completo)
ggsurvplot(km_comorbilidad, 
           conf.int = TRUE, 
           pval = TRUE, 
           risk.table = TRUE,
           legend.title = "Comorbilidad",
           xlab = "Días", 
           ylab = "Probabilidad de Supervivencia",
           ggtheme = theme_minimal())


