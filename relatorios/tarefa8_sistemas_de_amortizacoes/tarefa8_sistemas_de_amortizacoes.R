# QUESTÃO 2 ---------------------------------------------------------------

# dados
PV <- 40000
i <- 0.033
n <- 36
k <- 16

# prestação
PMT <- PV * (i / (1 - (1 + i)^(-n)))

# saldo devedor após 16 parcelas
SD <- PMT * (1 - (1 + i)^-(n - k)) / i

# formatação
PMT_fmt <- formatC(PMT, format="f", digits=2, decimal.mark=",", big.mark=".")
SD_fmt  <- formatC(SD, format="f", digits=2, decimal.mark=",", big.mark=".")

cat("QUESTÃO 2\n")
cat("Prestação: R$", PMT_fmt, "\n")
cat("Saldo devedor após 16 parcelas: R$", SD_fmt, "\n")


# QUESTÃO 3 ---------------------------------------------------------------

# dados
PV <- 97200
i_a <- 0.42576
n <- 120
k <- 35

# converter taxa para mensal
i <- (1 + i_a)^(1/12) - 1

# prestação
PMT <- PV * (i / (1 - (1 + i)^(-n)))

# saldo devedor após 34 parcelas
SD_34 <- PMT * (1 - (1 + i)^-(n - (k - 1))) / i

# juros da 35ª parcela
juros_35 <- SD_34 * i

# amortização da 35ª parcela
amort_35 <- PMT - juros_35

# formatação
fmt <- function(x) formatC(x, format="f", digits=2, decimal.mark=",", big.mark=".")

cat("Prestação: R$", fmt(PMT), "\n")
cat("Amortização na 35ª parcela: R$", fmt(amort_35), "\n")


# QUESTÃO 4 ---------------------------------------------------------------

PV <- 22500
A <- 375
P <- 480
i <- 0.02

# juros da parcela
J <- P - A

# saldo anterior
SD <- J / i

# cálculo do período
k <- (PV - SD) / A + 1

cat("Mês correspondente:", k, "\n")


# QUESTÃO 5 ---------------------------------------------------------------

# dados
PV <- 12000
i <- 0.02
n <- 5

# custos iniciais
TAC <- 0.004 * PV
IOF <- 0.01 * PV

# valor líquido recebido
VL <- PV - TAC - IOF

# juros na carência
juros_carencia <- PV * i

# prestação (Price)
PMT <- PV * (i / (1 - (1 + i)^(-n)))

# fluxo de caixa
fluxo <- c(
  VL,
  -juros_carencia,
  -juros_carencia,
  rep(-PMT, n)
)

# cálculo da TIR
tir <- uniroot(function(x) sum(fluxo / (1 + x)^(0:(length(fluxo)-1))), c(0, 1))$root

# anualizar (opcional)
tir_anual <- (1 + tir)^12 - 1

# formatação
fmt <- function(x) formatC(x * 100, format="f", digits=4, decimal.mark=",")

cat("Custo efetivo mensal:", fmt(tir), "% a.m.\n")
cat("Custo efetivo anual:", fmt(tir_anual), "% a.a.\n")
