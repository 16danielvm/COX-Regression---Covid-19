# Cargar el paquete
library(survival)
library(survminer)


# Convertir comorbilidades a factores (opcional pero recomendado)
comorbilidades <- c('DIABETES', 'HIPERTENSION','CARDIOVASCULAR', 
                    'OBESIDAD', 'TABAQUISMO')

# Convierte las comorbilidades a factores
df_cox2023[comorbilidades] <- lapply(df_cox2023[comorbilidades], factor)

# Definir la fórmula del modelo de Cox
formula_cox <- as.formula("Surv(DIAS_SINTOMAS_DEF, DEFUNCION) ~ DIABETES + HIPERTENSION + CARDIOVASCULAR + OBESIDAD + TABAQUISMO")

# Ajustar el modelo de Cox
modelo_cox <- coxph(formula_cox, data = df_cox2023)

# Ver el resumen del modelo
summary(modelo_cox)

