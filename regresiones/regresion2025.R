# Cargar el paquete
library(survival)
library(survminer)


# Definir la fórmula del modelo de Cox
formula_cox <- as.formula("Surv(DIAS_SINTOMAS_DEF, DEFUNCION) ~ NEUMONIA + DIABETES + EPOC + 
                          ASMA + INMUSUPR + HIPERTENSION + OBESIDAD + 
                          CARDIOVASCULAR + RENAL_CRONICA + TABAQUISMO")


# Ajustar el modelo de Cox
modelo_cox <- coxph(formula_cox, data = df_cox2025)

# Ver el resumen del modelo
summary(modelo_cox)

### Test de Schoenfeld
test_proporcionalidad <- cox.zph(modelo_cox)
print(test_proporcionalidad)
plot(test_proporcionalidad)

# SUPUESTO DE COLINEALIDAD:
library(car)
vif(modelo_cox)

### CORRECCION MODELO
# Corrección del modelo usando tt() para las variables que no cumplieron el supuesto
modelo_cox_corr <- coxph(Surv(DIAS_SINTOMAS_DEF, DEFUNCION) ~ 
                           DIABETES + EPOC + ASMA + INMUSUPR + 
                           HIPERTENSION + OBESIDAD + RENAL_CRONICA +
                           TABAQUISMO + tt(NEUMONIA) + tt(CARDIOVASCULAR), 
                         data = df_cox2025,
                         tt = function(x, t, ...) x * log1p(t))

# Resumen del modelo corregido
summary(modelo_cox_corr)


modelo_sin_tt <- coxph(Surv(DIAS_SINTOMAS_DEF, DEFUNCION) ~ 
                         DIABETES + EPOC + ASMA + INMUSUPR + 
                         HIPERTENSION + OBESIDAD + RENAL_CRONICA +
                         TABAQUISMO, 
                       data = df_cox2025)

# Evaluación del supuesto de riesgos proporcionales para las variables restantes
cox.zph(modelo_sin_tt)

library(survival)
library(survminer)

# Curva de supervivencia para NEUMONIA
surv_neumonia <- survfit(Surv(DIAS_SINTOMAS_DEF, DEFUNCION) ~ NEUMONIA, data = df_cox2025)
ggsurvplot(surv_neumonia, 
           data = df_cox2025,
           risk.table = TRUE, 
           pval = TRUE, 
           conf.int = TRUE,
           title = "Curva de Supervivencia - NEUMONIA",
           legend.title = "Neumonía",
           legend.labs = c("No", "Sí"),
           xlab = "Tiempo (días)",
           ylab = "Probabilidad de Supervivencia")

# Curva de supervivencia para CARDIOVASCULAR
surv_cardio <- survfit(Surv(DIAS_SINTOMAS_DEF, DEFUNCION) ~ CARDIOVASCULAR, data = df_cox2025)
ggsurvplot(surv_cardio, 
           data = df_cox2025,
           risk.table = TRUE, 
           pval = TRUE, 
           conf.int = TRUE,
           title = "Curva de Supervivencia - ENF. CARDIOVASCULAR",
           legend.title = "Enf. Cardiovascular",
           legend.labs = c("No", "Sí"),
           xlab = "Tiempo (días)",
           ylab = "Probabilidad de Supervivencia")

# Curvas de supervivencia para las variables que SI cumplieron el supuesto
surv_diabetes <- survfit(Surv(DIAS_SINTOMAS_DEF, DEFUNCION) ~ DIABETES, data = df_cox2025)
ggsurvplot(surv_diabetes, 
           data = df_cox2025,
           risk.table = TRUE, 
           pval = TRUE, 
           conf.int = TRUE,
           title = "Curva de Supervivencia - DIABETES",
           legend.title = "Diabetes",
           legend.labs = c("No", "Sí"),
           xlab = "Tiempo (días)",
           ylab = "Probabilidad de Supervivencia")
summary(surv_diabetes, times = c(30, 60, 90, 180, 365))
summary(survfit(Surv(DIAS_SINTOMAS_DEF, DEFUNCION) ~ DIABETES, data = df_cox2025),
        times = c(30, 60, 90, 180, 365, 400))  # Evaluar hasta 3 años


# Curvas de supervivencia para las variables que SI cumplieron el supuesto
surv_neumonia <- survfit(Surv(DIAS_SINTOMAS_DEF, DEFUNCION) ~ NEUMONIA, data = df_cox2025)
ggsurvplot(surv_neumonia, 
           data = df_cox2025,
           risk.table = TRUE, 
           pval = TRUE, 
           conf.int = TRUE,
           title = "Curva de Supervivencia - NEUMONIA",
           legend.title = "Diabetes",
           legend.labs = c("No", "Sí"),
           xlab = "Tiempo (días)",
           ylab = "Probabilidad de Supervivencia")
summary(surv_neumonia, times = c(30, 60, 90, 180, 365))
