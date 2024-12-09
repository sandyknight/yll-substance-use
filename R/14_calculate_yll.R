calculate_yll <-
  function(number.deaths,
           average.age.death,
           model.life.expectancy,
           discount.rate = 0.03,
           beta.constant = 0.04,
           modulation.constant = 0,
           adjustment.constant = 0.1658) {
    ## abbreviate inputs
    N <- number.deaths
    a <- average.age.death
    L <- model.life.expectancy
    r <- discount.rate
    b <- beta.constant
    K <- modulation.constant
    CC <- adjustment.constant
    ## do calculations
    if (discount.rate == 0) {
      N * (K * CC * ((exp(-b * a)) / b^2) * ((exp(-b * L)) * (-b * (L + a) - 1) - (-b * a - 1)) + ((1 - K) * L))
    } else {
      N * (K * ((CC * exp(r * a)) / (-(r + b)^2)) * ((exp(-(r + b) * (L + a)) * (-(r + b) * (L + a) - 1)) - (exp(-(r + b) * a) * (-(r + b) * a - 1))) + ((1 - K) / r) * ((1 - exp(-r * L))))
    }
  }
