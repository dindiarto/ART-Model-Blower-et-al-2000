behaviours <- function(t, cells, params) {
  with(
    as.list(c(individuals, params)),
    {
      
      dX_dt = pi - (c_p * lambda * X) - (mu * X)
      
      dYu_dt =  (c_p * lambda * X) + (g * Yt)  - (sigma * Yu) - (mu * Yu) - (vu * Yu)
      
      dYt_dt =  (sigma * Yu) -  (g * Yt) - (mu * Yt) - (vt * Yt)
      
      dN_dt = dX_dt + dYu_dt + dYt_dt
      
      lambda = (beta_t * Yt/N) + (beta_u*Yu/N)
      
      return(list(c(dX_dt, dYu_dt, dYt_dt, dN_dt, lambda)))
      
    })
}