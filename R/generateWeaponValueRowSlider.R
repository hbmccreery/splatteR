#' Function to generate sliders for weapon values on weapon value table
#'
#'
generateWeaponValueRowSlider <- function(val, label, min_val, max_val, session) {
  column(
    2,
    sliderInput(
      inputId = session$ns(val),
      label = label,
      min = min_val,
      max = max_val,
      value = c(min_val, max_val)
    )
  )
}
