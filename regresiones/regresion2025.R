# Cargar el paquete
library(survival)
library(survminer)


# Convertir comorbilidades a factores (opcional pero recomendado)
#comorbilidades <- c('NEUMONIA', 'DIABETES', 'EPOC', 'ASMA', 'INMUSUPR',
#                    'HIPERTENSION', 'CARDIOVASCULAR', 
#                    'OBESIDAD', 'RENAL_CRONICA', 'TABAQUISMO')

# Convierte las comorbilidades a factores
#df_cox2025[comorbilidades] <- lapply(df_cox2025[comorbilidades], factor)

# Definir la fórmula del modelo de Cox
formula_cox <- as.formula("Surv(DIAS_SINTOMAS_DEF, DEFUNCION) ~ NEUMONIA + DIABETES + EPOC + 
                          ASMA + INMUSUPR + HIPERTENSION + OBESIDAD + 
                          CARDIOVASCULAR + RENAL_CRONICA + TABAQUISMO")


# Ajustar el modelo de Cox
modelo_cox <- coxph(formula_cox, data = df_cox2025)

# Ver el resumen del modelo
summary(modelo_cox)


### Test de Schoenfeld
test_schoenfeld <- cox.zph(modelo_cox)
print(test_schoenfeld)

# Gráfico de residuos de Schoenfeld
ggcoxzph(cox.zph(modelo_cox))

# Crear la interacción NEUMONIA * log(tiempo)
df_cox2025$NEUMONIA_TIEMPO <- df_cox2025$NEUMONIA * log(df_cox2025$DIAS_SINTOMAS_DEF + 1)

# Nueva fórmula con estratificación
formula_cox_corr <- as.formula("Surv(DIAS_SINTOMAS_DEF, DEFUNCION) ~ NEUMONIA + NEUMONIA_TIEMPO +
                               DIABETES + EPOC + ASMA + INMUSUPR + HIPERTENSION + 
                               OBESIDAD + RENAL_CRONICA + TABAQUISMO + strata(CARDIOVASCULAR)")

# Ajustar el modelo con estratificación
modelo_cox_corr <- coxph(formula_cox_corr, data = df_cox2025)

# Ver el resumen del modelo corregido
summary(modelo_cox_corr)

# Curva de supervivencia para NEUMONIA
ggsurvplot(survfit(Surv(DIAS_SINTOMAS_DEF, DEFUNCION) ~ NEUMONIA, data = df_cox2025))
             
test_schoenfeld <- cox.zph(modelo_cox)
print(test_schoenfeld)

### Multicolinealidad
library(car)
vif(modelo_cox)
