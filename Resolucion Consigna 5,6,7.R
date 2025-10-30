# --- 0. CONFIGURACIÓN INICIAL Y CARGA DE LIBRERÍAS ---

# (Opcional) Instalar 'readr' si no lo tienes.
# install.packages("readr")
library(readr)

print("Cargando los datos...")

# --- ¡Paso Clave! ---
# Ejecutar la línea de abajo. Cuando se abra la ventana:
# Elegir el archivo llamado: "TUPAD-2025-EST-TPI-planilla2.xlsx - planilla.csv"
datos_poblacion <- read_csv(file.choose())

# Verificamos que los datos se cargaron bien
print("--- Datos Cargados (revisa que sea la tabla correcta) ---")
print(head(datos_poblacion))


# =========================================================================
# === 📊 EJERCICIO 5: DISTRIBUCIÓN BINOMIAL ===============================
# =========================================================================

print("--- Iniciando Ejercicio 5 (Binomial) ---")

# 1. Calcular las probabilidades de la población (nuestros 'p')
total_estudiantes <- nrow(datos_poblacion)
tabla_satisfaccion <- table(datos_poblacion$`SATISFACCIÓN CON LA CARRERA`)

# prop.table() nos da las proporciones (probabilidades)
prop_poblacional <- prop.table(tabla_satisfaccion)
print("Proporciones de Satisfacción (Población):")
print(prop_poblacional)

# 2. Asignar parámetros del problema
p_muy_satisfecho <- prop_poblacional["1"]
p_satisfecho <- prop_poblacional["2"]
p_insatisfecho <- prop_poblacional["3"]
p_muy_insatisfecho <- prop_poblacional["4"]

n <- 16 # Tamaño de la muestra (n fijo)

print("--- Resultados Pregunta 5 ---")

# 5a. Más de 9 "muy satisfechos" (P(X > 9))
# Usamos lower.tail = FALSE para obtener la cola derecha (P(X > k))
prob_5a <- pbinom(9, size = n, prob = p_muy_satisfecho, lower.tail = FALSE)
print(paste("5a. Prob. (Más de 9 muy satisfechos):", round(prob_5a, 6)))

# 5b. Entre 4 y 8 "satisfechos" (P(4 <= X <= 8))
# Esto es P(X <= 8) - P(X <= 3)
prob_5b <- pbinom(8, size = n, prob = p_satisfecho) - pbinom(3, size = n, prob = p_satisfecho)
print(paste("5b. Prob. (Entre 4 y 8 satisfechos):", round(prob_5b, 6)))

# 5c. Menos de 5 "insatisfechos" (P(X < 5))
# Esto es P(X <= 4)
prob_5c <- pbinom(4, size = n, prob = p_insatisfecho)
print(paste("5c. Prob. (Menos de 5 insatisfechos):", round(prob_5c, 6)))

# 5d. Exactamente 10 "muy insatisfechos" (P(X = 10))
# Usamos dbinom para probabilidad exacta
prob_5d <- dbinom(10, size = n, prob = p_muy_insatisfecho)
print(paste("5d. Prob. (Exactamente 10 muy insatisfechos):", round(prob_5d, 6)))

# --- Gráfico para 5b (Binomial) ---
# Guardamos el gráfico en un archivo PNG
png("grafico_binomial_5b.png")

# 1. Creamos todos los resultados posibles (de 0 a 16)
k_valores <- 0:n
# 2. Calculamos la probabilidad para CADA resultado
probabilidades_bino <- dbinom(k_valores, size = n, prob = p_satisfecho)
# 3. Creamos un vector de colores
colores_bino <- rep("lightblue", n + 1) # Color por defecto
# 4. Coloreamos las barras que nos interesan (de 4 a 8)
# (Sumamos 1 a los índices porque R empieza en 1, pero nuestros valores en 0)
indices_interes <- (k_valores >= 4) & (k_valores <= 8)
colores_bino[indices_interes] <- "blue"

# 5. Creamos el gráfico de barras
barplot(probabilidades_bino,
        names.arg = k_valores,
        xlab = "Nro. de Estudiantes 'Satisfechos' (k)",
        ylab = "Probabilidad P(X=k)",
        main = "Ejercicio 5b: P(4 <= X <= 8) Satisfechos",
        col = colores_bino,
        border = "darkgray")
# 6. Cerramos el archivo PNG
dev.off()
print("Gráfico 'grafico_binomial_5b.png' guardado.")


# =========================================================================
# === ⏰ EJERCICIO 6: DISTRIBUCIÓN DE POISSON ==============================
# =========================================================================

print("--- Iniciando Ejercicio 6 (Poisson) ---")

# 1. Definir la tasa base
tasa_por_minuto <- 15 / 30 # 0.5 consultas/minuto

print("--- Resultados Pregunta 6 ---")

# 6a. Por lo menos 6 consultas en 20 minutos (P(X >= 6))
lambda_a <- tasa_por_minuto * 20 # lambda = 10
# P(X >= 6) es lo mismo que P(X > 5)
prob_6a <- ppois(5, lambda = lambda_a, lower.tail = FALSE)
print(paste("6a. Prob. (Al menos 6 en 20 min, λ=10):", round(prob_6a, 6)))

# 6b. A lo sumo 12 consultas en 40 minutos (P(X <= 12))
lambda_b <- tasa_por_minuto * 40 # lambda = 20
prob_6b <- ppois(12, lambda = lambda_b)
print(paste("6b. Prob. (A lo sumo 12 en 40 min, λ=20):", round(prob_6b, 6)))

# 6c. Más de 7 y menos de 10 consultas en 30 minutos (P(X=8) + P(X=9))
lambda_c <- 15 # lambda = 15 (tasa original)
# Sumamos las probabilidades exactas P(X=8) y P(X=9)
prob_6c <- dpois(8, lambda = lambda_c) + dpois(9, lambda = lambda_c)
print(paste("6c. Prob. (Entre 7 y 10 en 30 min, λ=15):", round(prob_6c, 6)))

# --- Gráfico para 6a (Poisson) ---
png("grafico_poisson_6a.png")

# 1. Creamos un rango de valores (ej: de 0 a 25)
k_poisson <- 0:25
# 2. Calculamos las probabilidades para lambda = 10
probabilidades_pois <- dpois(k_poisson, lambda = lambda_a)
# 3. Creamos colores
colores_pois <- rep("lightgreen", length(k_poisson))
indices_interes_pois <- (k_poisson >= 6)
colores_pois[indices_interes_pois] <- "darkgreen"

# 4. Creamos el gráfico
barplot(probabilidades_pois,
        names.arg = k_poisson,
        xlab = "Nro. de Consultas (k)",
        ylab = "Probabilidad P(X=k)",
        main = "Ejercicio 6a: P(X >= 6) con Lambda = 10",
        col = colores_pois,
        border = "darkgray")
# 5. Cerramos el archivo
dev.off()
print("Gráfico 'grafico_poisson_6a.png' guardado.")


# =========================================================================
# === 📏 EJERCICIO 7: DISTRIBUCIÓN NORMAL =================================
# =========================================================================

print("--- Iniciando Ejercicio 7 (Normal) ---")

# 1. Calcular la media (μ) y desviación estándar (σ) de la población
# Usamos na.rm = TRUE por si hay algún valor faltante (NA)
columna_estatura <- datos_poblacion$`ESTATURA CM.`
mu_estatura <- mean(columna_estatura, na.rm = TRUE)
sigma_estatura <- sd(columna_estatura, na.rm = TRUE)

print("Parámetros de Estatura (Población):")
print(paste("Media (μ):", round(mu_estatura, 2)))
print(paste("Desv. Estándar (σ):", round(sigma_estatura, 2)))

print("--- Resultados Pregunta 7 ---")

# 7a. Estatura mayor o igual que 179 cm (P(X >= 179))
prob_7a <- pnorm(179, mean = mu_estatura, sd = sigma_estatura, lower.tail = FALSE)
print(paste("7a. Prob. (Estatura >= 179 cm):", round(prob_7a, 6)))

# 7b. Estatura comprendida entre 147 cm y 172 cm (P(147 <= X <= 172))
# P(X <= 172) - P(X <= 147)
prob_7b <- pnorm(172, mean = mu_estatura, sd = sigma_estatura) - pnorm(147, mean = mu_estatura, sd = sigma_estatura)
print(paste("7b. Prob. (Estatura entre 147 y 172 cm):", round(prob_7b, 6)))

# 7c. Hallar el valor que excede al 97,5% de los datos (Percentil 97.5)
# Buscamos x tal que P(X < x) = 0.975
valor_7c <- qnorm(0.975, mean = mu_estatura, sd = sigma_estatura)
print(paste("7c. Valor (Percentil 97.5):", round(valor_7c, 2), "cm"))

# --- Gráfico para 7b (Normal) ---
# (Versión corregida)

print("--- Generando Gráfico 7b (Normal)... ---")

# (Calculamos la media y sigma de nuevo, por si acaso)
mu_estatura <- mean(datos_poblacion$`ESTATURA CM.`, na.rm = TRUE)
sigma_estatura <- sd(datos_poblacion$`ESTATURA CM.`, na.rm = TRUE)

# 1. Abrimos el archivo PNG
png("grafico_normal_7b.png")

# 2. Creamos el eje X y la curva Y
x_curva <- seq(mu_estatura - 3.5 * sigma_estatura,
               mu_estatura + 3.5 * sigma_estatura,
               length.out = 500)
y_curva <- dnorm(x_curva, mean = mu_estatura, sd = sigma_estatura)

# 3. Dibujamos la curva
plot(x_curva, y_curva, type = "l",
     lwd = 2,
     xlab = "Estatura (cm)",
     ylab = "Densidad de Probabilidad",
     main = "Ejercicio 7b: P(147 <= X <= 172)",
     col = "darkblue")

# 4. Definimos el área a sombrear
x_sombra <- seq(147, 172, length.out = 100)
y_sombra <- dnorm(x_sombra, mean = mu_estatura, sd = sigma_estatura)

# 5. Dibujamos el polígono sombreado --- ¡AQUÍ ESTÁ LA CORRECCIÓN! ---
polygon(c(147, x_sombra, 172), 
        c(0, y_sombra, 0),    
        col = "#00008B4D",  # <--- Este es el color corregido (hex con alfa)
        border = NA) 

# 6. Añadir líneas verticales
abline(v = c(147, 172), col = "red", lty = 2)

# 7. Cerramos el archivo
dev.off()

print("Gráfico 'grafico_normal_7b.png' guardado CORRECTAMENTE.")