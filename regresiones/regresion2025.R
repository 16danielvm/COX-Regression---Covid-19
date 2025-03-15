# Cargar el paquete
library(survival)
library(survminer)


hist(df_cox2025$DIAS_SINTOMAS_DEF)

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

test_proporcionalidad_sin_tt <- cox.zph(modelo_cox_sin_tt)
print(test_proporcionalidad_sin_tt)


library(survival)
library(survminer)

# Curvas de supervivencia para las variables que SI cumplieron el supuesto
surv_NEUMONIA <- survfit(Surv(DIAS_SINTOMAS_DEF, DEFUNCION) ~ NEUMONIA, data = df_cox2025)
ggsurvplot(surv_NEUMONIA, 
           data = df_cox2025,
           #risk.table = TRUE, 
           pval = TRUE, 
           conf.int = TRUE,
           title = "Curva de Supervivencia - NEUMONIA",
           legend.title = "Diabetes",
           legend.labs = c("No", "Sí"),
           xlab = "Tiempo (días)",
           ylab = "Probabilidad de Supervivencia")
summary(surv_NEUMONIA, times = c(30, 60, 90, 180, 365))
summary(survfit(Surv(DIAS_SINTOMAS_DEF, DEFUNCION) ~ NEUMONIA, data = df_cox2025),
        times = c(30, 60, 90, 180, 365, 400))

survdiff(Surv(DIAS_SINTOMAS_DEF, DEFUNCION) ~ NEUMONIA, data = df_cox2025)



surv_DIABETES <- survfit(Surv(DIAS_SINTOMAS_DEF, DEFUNCION) ~ DIABETES, data = df_cox2025)
ggsurvplot(surv_DIABETES, 
           data = df_cox2025,
           #risk.table = TRUE, 
           pval = TRUE, 
           conf.int = TRUE,
           title = "Curva de Supervivencia - DIABETES",
           legend.title = "Diabetes",
           legend.labs = c("No", "Sí"),
           xlab = "Tiempo (días)",
           ylab = "Probabilidad de Supervivencia")
summary(surv_DIABETES, times = c(30, 60, 90, 180, 365))
summary(survfit(Surv(DIAS_SINTOMAS_DEF, DEFUNCION) ~ DIABETES, data = df_cox2025),
        times = c(30, 60, 90, 180, 365, 400))
survdiff(Surv(DIAS_SINTOMAS_DEF, DEFUNCION) ~ DIABETES, data = df_cox2025)


surv_EPOC <- survfit(Surv(DIAS_SINTOMAS_DEF, DEFUNCION) ~ EPOC, data = df_cox2025)
ggsurvplot(surv_EPOC, 
           data = df_cox2025,
           #risk.table = TRUE, 
           pval = TRUE, 
           conf.int = TRUE,
           title = "Curva de Supervivencia - EPOC",
           legend.title = "Diabetes",
           legend.labs = c("No", "Sí"),
           xlab = "Tiempo (días)",
           ylab = "Probabilidad de Supervivencia")
summary(surv_EPOC, times = c(30, 60, 90, 180, 365))
summary(survfit(Surv(DIAS_SINTOMAS_DEF, DEFUNCION) ~ EPOC, data = df_cox2025),
        times = c(30, 60, 90, 180, 365, 400))
survdiff(Surv(DIAS_SINTOMAS_DEF, DEFUNCION) ~ EPOC, data = df_cox2025)

surv_HIPERTENSION <- survfit(Surv(DIAS_SINTOMAS_DEF, DEFUNCION) ~ HIPERTENSION, data = df_cox2025)
ggsurvplot(surv_HIPERTENSION, 
           data = df_cox2025,
           #risk.table = TRUE, 
           pval = TRUE, 
           conf.int = TRUE,
           title = "Curva de Supervivencia - HIPERTENSION",
           legend.title = "Diabetes",
           legend.labs = c("No", "Sí"),
           xlab = "Tiempo (días)",
           ylab = "Probabilidad de Supervivencia")
summary(surv_HIPERTENSION, times = c(30, 60, 90, 180, 365))
summary(survfit(Surv(DIAS_SINTOMAS_DEF, DEFUNCION) ~ HIPERTENSION, data = df_cox2025),
        times = c(30, 60, 90, 180, 365, 400))
survdiff(Surv(DIAS_SINTOMAS_DEF, DEFUNCION) ~ HIPERTENSION, data = df_cox2025)

surv_RENAL_CRONICA <- survfit(Surv(DIAS_SINTOMAS_DEF, DEFUNCION) ~ RENAL_CRONICA, data = df_cox2025)
ggsurvplot(surv_RENAL_CRONICA, 
           data = df_cox2025,
           #risk.table = TRUE, 
           pval = TRUE, 
           conf.int = TRUE,
           title = "Curva de Supervivencia - RENAL_CRONICA",
           legend.title = "Diabetes",
           legend.labs = c("No", "Sí"),
           xlab = "Tiempo (días)",
           ylab = "Probabilidad de Supervivencia")
summary(surv_RENAL_CRONICA, times = c(30, 60, 90, 180, 365))
summary(survfit(Surv(DIAS_SINTOMAS_DEF, DEFUNCION) ~ RENAL_CRONICA, data = df_cox2025),
        times = c(30, 60, 90, 180, 365, 400))
survdiff(Surv(DIAS_SINTOMAS_DEF, DEFUNCION) ~ RENAL_CRONICA, data = df_cox2025)


surv_TABAQUISMO <- survfit(Surv(DIAS_SINTOMAS_DEF, DEFUNCION) ~ TABAQUISMO, data = df_cox2025)
ggsurvplot(surv_TABAQUISMO, 
           data = df_cox2025,
           #risk.table = TRUE, 
           pval = TRUE, 
           conf.int = TRUE,
           title = "Curva de Supervivencia - TABAQUISMO",
           legend.title = "Diabetes",
           legend.labs = c("No", "Sí"),
           xlab = "Tiempo (días)",
           ylab = "Probabilidad de Supervivencia")
summary(surv_TABAQUISMO, times = c(30, 60, 90, 180, 365))
summary(survfit(Surv(DIAS_SINTOMAS_DEF, DEFUNCION) ~ TABAQUISMO, data = df_cox2025),
        times = c(30, 60, 90, 180, 365, 400))
survdiff(Surv(DIAS_SINTOMAS_DEF, DEFUNCION) ~ TABAQUISMO, data = df_cox2025)
