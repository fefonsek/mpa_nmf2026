############################
# QUESTÃO 1
############################

# dados
PMT <- 102.59   # prestação
i <- 0.06       # taxa mensal
n <- 36         # número de parcelas

# valor presente (preço à vista)
VP <- PMT * (1 - (1 + i)^(-n)) / i

# formatar
VP_fmt <- formatC(VP, format="f", digits=4, decimal.mark=",")

cat("QUESTÃO 1\n")
cat("Valor à vista: R$", VP_fmt, "\n")


############################
# QUESTÃO 2
############################

# dados
PV_total <- 50000
entrada <- 0.20 * PV_total
VP <- PV_total - entrada

i <- 0.03   # taxa mensal
n <- 18     # número de parcelas

# cálculo da prestação
PMT <- VP * (i / (1 - (1 + i)^(-n)))

# formatar
PMT_fmt <- formatC(PMT, format="f", digits=4, decimal.mark=",")

cat("QUESTÃO 2\n")
cat("Valor das prestações: R$", PMT_fmt, "\n")


############################
# QUESTÃO 3
############################

# dados
FV <- 100000   # valor futuro desejado
i <- 0.01      # taxa mensal
n <- 48        # número de períodos

# cálculo do valor da aplicação mensal
PMT <- FV * (i / ((1 + i)^n - 1))

# formatar
PMT_fmt <- formatC(PMT, format="f", digits=4, decimal.mark=",")

cat("QUESTÃO 3\n")
cat("Valor da aplicação mensal: R$", PMT_fmt, "\n")


############################
# QUESTÃO 4
############################

# fluxo de caixa
VP <- 20000

f <- function(i) {
  6000 / (1 + i)^12 +
    8000 / (1 + i)^24 +
    10000 / (1 + i)^36 - VP
}

# encontrar a taxa mensal (i)
taxa_mensal <- uniroot(f, interval = c(0, 0.1))$root

# converter para taxa anual efetiva
taxa_anual <- (1 + taxa_mensal)^12 - 1

# formatar
tm_fmt <- formatC(taxa_mensal*100, format="f", digits=4, decimal.mark=",")
ta_fmt <- formatC(taxa_anual*100, format="f", digits=4, decimal.mark=",")

cat("QUESTÃO 4\n")
cat("Taxa mensal:", tm_fmt, "% a.m.\n")
cat("Taxa anual efetiva:", ta_fmt, "% a.a.\n")


############################
# QUESTÃO 5
############################

# dados
VP <- 25000
i <- 0.0075   # 0,75% a.m.
n <- 5        # parcelas
carencia <- 24

# levar o valor para o tempo 24
FV24 <- VP * (1 + i)^carencia

# calcular as parcelas
PMT <- FV24 * (i / (1 - (1 + i)^(-n)))

# formatar
PMT_fmt <- formatC(PMT, format="f", digits=4, decimal.mark=",")

cat("QUESTÃO 5\n")
cat("Valor das parcelas: R$", PMT_fmt, "\n")