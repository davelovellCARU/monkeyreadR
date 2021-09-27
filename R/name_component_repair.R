### name.component.repair

## start_low is a potential argument for name.component.repair
### It makes sure every component starts with a lowercase character
sm_repair_start_low <- function(x){
  stringr::str_trim(
    paste0(
      stringr::str_to_lower(stringr::str_sub(x,1,1)),
      stringr::str_sub(x,2,-1)
    ))}
