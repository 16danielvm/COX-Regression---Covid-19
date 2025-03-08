# Cargar el paquete
library(survival)
library(survminer)


# Convertir comorbilidades a factores (opcional pero recomendado)
#comorbilidades <- c('DIABETES', 'HIPERTENSION','CARDIOVASCULAR', 
#                    'OBESIDAD', 'TABAQUISMO')

# Convertir comorbilidades a factores (opcional pero recomendado)
comorbilidades <- c('NEUMONIA', 'DIABETES', 'EPOC', 'ASMA', 'INMUSUPR',
                    'HIPERTENSION', 'OTRA_COM', 'CARDIOVASCULAR', 
                    'OBESIDAD', 'RENAL_CRONICA', 'TABAQUISMO')

# Convierte las comorbilidades a factores
df_cox2025[comorbilidades] <- lapply(df_cox2025[comorbilidades], factor)

# Definir la fórmula del modelo de Cox
#formula_cox <- as.formula("Surv(DIAS_SINTOMAS_DEF, DEFUNCION) ~ DIABETES + HIPERTENSION + CARDIOVASCULAR + OBESIDAD + TABAQUISMO")

# Definir la fórmula del modelo de Cox
formula_cox <- as.formula("Surv(DIAS_SINTOMAS_DEF, DEFUNCION) ~ NEUMONIA + DIABETES + EPOC + 
                          ASMA + INMUSUPR + HIPERTENSION + OTRA_COM + 
                          CARDIOVASCULAR + OBESIDAD + RENAL_CRONICA + TABAQUISMO")


# Ajustar el modelo de Cox
modelo_cox <- coxph(formula_cox, data = df_cox2025)

# Ver el resumen del modelo
summary(modelo_cox)


### Test de Schoenfeld
test_schoenfeld <- cox.zph(modelo_cox)
print(test_schoenfeld)
plot(test_schoenfeld)

### Multicolinealidad
library(car)
vif(modelo_cox)


### Estratificación
modelo_cox_estrat <- coxph(Surv(DIAS_SINTOMAS_DEF, DEFUNCION) ~ 
                             strata(NEUMONIA) + DIABETES + EPOC + ASMA + 
                             INMUSUPR + HIPERTENSION + 
                             strata(CARDIOVASCULAR) + OBESIDAD + RENAL_CRONICA + TABAQUISMO,
                           data = df_cox2025)
# Ver el resumen del modelo
summary(modelo_cox_estrat)


### Test de Schoenfeld
test_schoenfeld_estrat <- cox.zph(modelo_cox_estrat)
print(test_schoenfeld_estrat)
plot(test_schoenfeld_estrat)


### visualización

library(survminer)

# Curvas de supervivencia para el modelo estratificado
curvas_estrat <- survfit(modelo_cox_estrat)

# Visualización específica para variables significativas
ggsurvplot(survfit(Surv(DIAS_SINTOMAS_DEF, DEFUNCION) ~ DIABETES, data = df_cox2025),
           data = df_cox2025,
           risk.table = TRUE,
           pval = TRUE,
           conf.int = TRUE,
           legend.title = "Diabetes",
           legend.labs = c("Sin Diabetes", "Con Diabetes"),
           break.time.by = 30,
           xlab = "Días desde el inicio de síntomas",
           ylab = "Probabilidad de supervivencia")

ggsurvplot(survfit(Surv(DIAS_SINTOMAS_DEF, DEFUNCION) ~ HIPERTENSION, data = df_cox2025),
           data = df_cox2025,
           risk.table = TRUE,
           pval = TRUE,
           conf.int = TRUE,
           legend.title = "Hipertensión",
           legend.labs = c("Sin Hipertensión", "Con Hipertensión"),
           break.time.by = 30,
           xlab = "Días desde el inicio de síntomas",
           ylab = "Probabilidad de supervivencia")

# Evaluación del potencial efecto protector de la obesidad
## Análisis bivariado: Curvas de supervivencia para obesidad

ggsurvplot(survfit(Surv(DIAS_SINTOMAS_DEF, DEFUNCION) ~ OBESIDAD, data = df_cox2025),
           data = df_cox2025,
           risk.table = TRUE,
           pval = TRUE,
           conf.int = TRUE,
           legend.title = "Obesidad",
           legend.labs = c("Sin Obesidad", "Con Obesidad"),
           break.time.by = 30,
           xlab = "Días desde el inicio de síntomas",
           ylab = "Probabilidad de supervivencia")

## Análisis de interacción en el modelo de Cox

modelo_cox_interaccion <- coxph(Surv(DIAS_SINTOMAS_DEF, DEFUNCION) ~ 
                                  OBESIDAD * (DIABETES + HIPERTENSION + TABAQUISMO + RENAL_CRONICA), 
                                data = df_cox2025)

summary(modelo_cox_interaccion)

## Comparación de características basales

library(tableone)
CreateTableOne(vars = c("DIABETES", "HIPERTENSION", "TABAQUISMO", "RENAL_CRONICA"),
               strata = "OBESIDAD", data = df_cox2025, test = TRUE)

