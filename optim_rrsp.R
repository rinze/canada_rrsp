library(tidyverse)
library(ggplot2)
theme_set(theme_bw(12))

#### FILL IN DATA HERE ####
TAXABLE_INCOME <- 67000
MAX_RRSP_ROOM <- 27000

# Change these accordingly (year, province, etc).
# prov_brackets uses QC values in this example.
# Current year data: 2020
federal_brackets <- data.frame(threshold = c(0, 48535, 97069, 150473, 214638),
                               rate = c(0.15, 0.205, 0.26, 0.29, 0.33))

prov_brackets <- data.frame(threshold = c(0, 43790, 87575, 106555),
                            rate = c(0.15, 0.20, 0.24, 0.2575))
#### END FILL DATA ####

#### FUNCTION DEFINITIONS ####
compute_tax_paid <- function(amount, brackets) {
    nrows <- nrow(brackets)
    brackets$next_bracket <- c(brackets$threshold[2:nrows], Inf)
    brackets$applicable <- ifelse(amount > brackets$next_bracket,
                                  brackets$next_bracket - brackets$threshold,
                                  amount - brackets$threshold)
    brackets$tax <- brackets$applicable * brackets$rate
    return(sum(brackets$tax[brackets$tax > 0]))
}

compute_effective_tax_rate <- function(amount, federal_brackets, prov_brackets) {
    total_tax <- compute_tax_paid(amount, federal_brackets) + 
        compute_tax_paid(amount, prov_brackets)
    return(100 * total_tax / amount)
}

#### END FUNCTION DEFINITIONS ####

#### MAIN ####
total_tax_paid <- compute_tax_paid(TAXABLE_INCOME, federal_brackets) + 
    compute_tax_paid(TAXABLE_INCOME, prov_brackets)
effective_tax_rate_paid <- compute_effective_tax_rate(TAXABLE_INCOME, 
                                                      federal_brackets, 
                                                      prov_brackets)
print(sprintf("Total tax paid: %.2f", total_tax_paid))
print(sprintf("Effective tax rate: %.2f %%", effective_tax_rate_paid))

# What's the optimal RRSP contribution?
rrsp_vector <- seq(500, MAX_RRSP_ROOM, 500)
res <- data.frame(rrsp_contribution = rrsp_vector,
                  taxable_income = TAXABLE_INCOME - rrsp_vector,
                  effective_tax_rate = sapply(rrsp_vector,
                                              function(x) compute_effective_tax_rate(
                                                  TAXABLE_INCOME - x,
                                                  federal_brackets,
                                                  prov_brackets)
                                            )
                )
                
res$tax_savings <- total_tax_paid - (res$taxable_income * res$effective_tax_rate / 100)
res$rrsp_efficiency <- 100 * res$tax_savings / res$rrsp_contribution

# Plot
plt1 <- ggplot(res %>% 
                   select(-taxable_income) %>% 
                   gather(key, value, -rrsp_contribution),
       aes(x = rrsp_contribution, y = value)) +
    geom_line() + geom_point() +
    facet_wrap(~ key, scale = "free_y") +
    xlab("\nRRSP contribution [CAD]") + ylab("Value [check facet legend]\n")
plot(plt1)
