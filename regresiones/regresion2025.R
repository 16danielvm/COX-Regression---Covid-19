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

