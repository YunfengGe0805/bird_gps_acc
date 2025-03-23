calculate_moon_illumination <- function(moon_illumination, TCC) {
  # Calculate k_diffusion
  k_diffusion <- 0.9 * TCC + 0.1
  
  # Calculate k_transmission
  k_transmission <- (1 - TCC) * 0.95 + 0.05
  
  # Calculate moon illumination
  moon_illumination_calculated <- (moon_illumination * k_diffusion * k_transmission) +
    (moon_illumination * (1 - k_diffusion) * k_transmission)
  
  return(moon_illumination_calculated)
}

# Example usage:
moon_illumination <- 1  # Example value for moon illumination
TCC <- 0.5  # Example value for total cloud cover (0 to 1)

calculated_illumination <- calculate_moon_illumination(moon_illumination, TCC)
print(calculated_illumination)
