message(paste(rep("-", 100), collapse = ""))

message("\tCargando datos para la reserva matmática")
load(paste0(parametros$RData, "IESS_beneficiarios.RData"))
load(paste0( parametros$RData, 'IESS_actualizacion_pensiones.RData'))
load(paste0( parametros$RData, 'IESS_tabla_mortalidad.RData'))

message("\tCalculando reserva matemática")

#Pensionistas con derecho a IVM al corte------------------------------------------------------------
derecho_ivm <- actualizacion_pensiones %>%
  filter( edad_derecho_ivm < edad ) %>%

  filter( anio == 2022) %>%
  dplyr::select( cedula,
                 renta_concedida,
                 fecha_derecho_ivm,
                 edad_derecho_ivm,
                 coef) %>%
  mutate( renta_ivm = coef * renta_concedida,
          renta_ce = (1 - coef) * renta_concedida )

reserva_matematica_1 <- beneficiarios %>%
  dplyr::select( cedula,
                 edad,
                 g ) %>%
  left_join(., tabla_mortalidad, by=c('edad','g')) %>%
  left_join(derecho_ivm,., by = 'cedula') %>%
  mutate( reserva_matematica = 13 * a_x * renta_ce )


#Pensionistas sin derecho a IVM al corte------------------------------------------------------------

#Renta anticipada y temporal------------------------------------------------------------------------



#Renta anticipada, diferida y vitalicia-------------------------------------------------------------




#Guardar en un RData--------------------------------------------------------------------------------
message( '\tGuardando intereses para IVM' )

save( interes,
      file = paste0( parametros$RData, 'IESS_reserva_matematica.RData' ) )

# Borrar elementos restantes -----------------------------------------------------------------------
message(paste(rep("-", 100), collapse = ""))
rm(list = ls()[!(ls() %in% c("parametros"))])
gc()