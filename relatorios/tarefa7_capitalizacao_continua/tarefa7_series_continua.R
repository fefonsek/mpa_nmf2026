
# QUESTÃO 1 ---------------------------------------------------------------

# taxas efetivas
i1 <- 0.20
i2 <- 0.12

# conversão para taxa contínua
delta1 <- log(1 + i1)
delta2 <- log(1 + i2)

# formatação
fmt <- function(x) formatC(x * 100, format="f", digits=4, decimal.mark=",")

cat("QUESTÃO 1\n\n")

cat("Taxa contínua equivalente:\n")
cat("A) 20% a.a.:", fmt(delta1), "% a.a.\n")
cat("B) 12% a.a.:", fmt(delta2), "% a.a.\n")


# QUESTÃO 2 ---------------------------------------------------------------

# dados
C <- 1000
delta <- 0.15
t <- 2

# cálculo do montante
M <- C * exp(delta * t)

# formatação
M_fmt <- formatC(M, format="f", digits=4, decimal.mark=",", big.mark=".")

cat("QUESTÃO 2\n\n")

cat("Montante final: R$", M_fmt, "\n")


# QUESTÃO 3 ---------------------------------------------------------------

# preços
P <- c(10.00, 10.50, 10.00)

# cálculo dos retornos mensais
retornos <- log(P[-1] / P[-length(P)]) * 100

# formatação
retornos_fmt <- formatC(retornos, format="f", digits=3, decimal.mark=",")

cat("QUESTÃO 3\n\n")

cat("Jan -> Fev:", retornos_fmt[1], "\n")
cat("Fev -> Mar:", retornos_fmt[2], "\n")


# QUESTÃO 4 ---------------------------------------------------------------

# dados
total <- 500000
T <- 2
i <- 0.10

# fluxo anual uniforme
c <- total / T

# converter taxa para contínua
delta <- log(1 + i)

# cálculo do valor presente
VP <- c * (1 - exp(-delta * T)) / delta

# formatação
VP_fmt <- formatC(VP, format="f", digits=2, decimal.mark=",", big.mark=".")

cat("QUESTÃO 4\n\n")

cat("Valor presente do investimento: R$", VP_fmt, "\n")


# QUESTÃO 5 ---------------------------------------------------------------

# dados
c <- 120000        # fluxo anual
T <- 10            # anos
i <- 0.20          # taxa discreta
I0 <- 1000000      # investimento inicial

# converter para taxa contínua
delta <- log(1 + i)

# valor presente dos fluxos
VP <- c * (1 - exp(-delta * T)) / delta

# VPL
VPL <- VP - I0

# formatação
fmt <- function(x) formatC(x, format="f", digits=2, decimal.mark=",", big.mark=".")

cat("Valor Presente dos fluxos: R$", fmt(VP), "\n")
cat("VPL do projeto: R$", fmt(VPL), "\n")
