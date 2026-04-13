
# QUESTÃO 1 ---------------------------------------------------------------

# dados
i_nom <- 0.07
infl <- 0.03

# cálculo
i_real <- (1 + i_nom) / (1 + infl) - 1

# formatação
i_real_fmt <- formatC(i_real * 100, format="f", digits=4, decimal.mark=",")

cat("Taxa real:", i_real_fmt, "% a.a.\n")


# QUESTÃO 2 ---------------------------------------------------------------

# dados
P0 <- 140000
Pf <- 220000
infl <- 0.40

# rentabilidade aparente
i_ap <- Pf / P0 - 1

# rentabilidade real
i_real <- (1 + i_ap) / (1 + infl) - 1

# formatação
fmt <- function(x) formatC(x * 100, format="f", digits=3, decimal.mark=",")

cat("Rentabilidade aparente:", fmt(i_ap), "% a.a.\n")
cat("Rentabilidade real:", fmt(i_real), "% a.a.\n")


# QUESTÃO 3 ---------------------------------------------------------------

# dados
C <- 130000
i_real <- 0.05
n <- 15
infl <- 0.943

# cálculo
M <- C * (1 + i_real)^n * (1 + infl)

# formatação
M_fmt <- formatC(M, format="f", digits=2, decimal.mark=",", big.mark=".")

cat("Valor de resgate: R$", M_fmt, "\n")


# QUESTÃO 4 ---------------------------------------------------------------

# dados
C <- 200000
M <- 230000
infl <- c(0.05, 0.023, -0.025, 0.015, -0.01)

# inflação acumulada
infl_total <- prod(1 + infl) - 1

# inflação média mensal
infl_media <- (1 + infl_total)^(1/length(infl)) - 1

# rentabilidade nominal (período)
i_nom <- M / C - 1

# rentabilidade real (período)
i_real_total <- (1 + i_nom) / (1 + infl_total) - 1

# rentabilidade real mensal
i_real_mensal <- (1 + i_real_total)^(1/length(infl)) - 1

# formatação
fmt <- function(x) formatC(x * 100, format="f", digits=3, decimal.mark=",")

cat("Inflação média mensal:", fmt(infl_media), "% a.m.\n")
cat("Rentabilidade real mensal:", fmt(i_real_mensal), "% a.m.\n")


# QUESTÃO 5 ---------------------------------------------------------------

# dados
PV <- 81600
i <- 0.05
n <- 4
infl <- 0.10
imposto <- 0.02 * PV

# valor líquido recebido
VL <- PV - imposto

# prestação (Price)
PMT <- PV * (i / (1 - (1 + i)^(-n)))

# criar estrutura
saldo <- PV
tabela <- data.frame(
  Mes = 1:n,
  Saldo_Anterior = NA,
  Juros = NA,
  Amortizacao = NA,
  Prestacao = PMT,
  Prestacao_Atualizada = NA,
  Saldo_Final = NA
)

# loop da tabela
for (k in 1:n) {
  
  juros <- saldo * i
  amort <- PMT - juros
  saldo_final <- saldo - amort
  
  # atualização pela inflação
  prest_atual <- PMT * (1 + infl)^k
  
  tabela[k, 2] <- saldo
  tabela[k, 3] <- juros
  tabela[k, 4] <- amort
  tabela[k, 6] <- prest_atual
  tabela[k, 7] <- saldo_final
  
  saldo <- saldo_final
}

# -------------------------
# CUSTO EFETIVO
# -------------------------

# fluxo nominal
fluxo_nom <- c(VL, rep(-PMT, n))

# TIR nominal
tir_nom <- uniroot(function(x) sum(fluxo_nom / (1 + x)^(0:n)), c(0, 1))$root

# fluxo real (deflacionado)
fluxo_real <- c(
  VL,
  -PMT / (1 + infl)^1,
  -PMT / (1 + infl)^2,
  -PMT / (1 + infl)^3,
  -PMT / (1 + infl)^4
)

# TIR real
tir_real <- uniroot(function(x) sum(fluxo_real / (1 + x)^(0:n)), c(-0.5, 1))$root

# -------------------------
# FORMATAÇÃO
# -------------------------
fmt <- function(x) formatC(x, format="f", digits=2, decimal.mark=",", big.mark=".")
fmt_pct <- function(x) formatC(x * 100, format="f", digits=3, decimal.mark=",")

# exibir tabela
print(tabela)

cat("\nCusto efetivo nominal:", fmt_pct(tir_nom), "% a.m.\n")
cat("Custo efetivo real:", fmt_pct(tir_real), "% a.m.\n")

a <- (1 + 0.05875) / (1 + 0.10) - 1
a
