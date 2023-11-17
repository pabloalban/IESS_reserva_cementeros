message( paste( rep('-', 100 ), collapse = '' ) )

message( '\tLectura de nomina y concesiones' )

#Cargando información financiera--------------------------------------------------------------------
file <- paste0( parametros$Data, 'IESS_nomina_concesiones.xlsx' )
file_c <- paste0( parametros$Data, 'IESS_beneficiarios_2.xlsx' )
#Cargar función tíldes a latex----------------------------------------------------------------------
source( 'R/503_tildes_a_latex.R', encoding = 'UTF-8', echo = FALSE )

#Liquidación de beneficiarios-----------------------------------------------------------------------
liquidacion <- read_excel(file,
                  sheet = 'liquidacion',
                  col_names = TRUE,
                  col_types = NULL,
                  na = "",
                  skip = 0) %>% clean_names() %>%
  mutate(fecha_del_derecho = as.Date( fecha_del_derecho, "%Y-%m-%d"),
         fecha_del_acuerdo = as.Date( fecha_del_acuerdo, "%Y-%m-%d"),
         periodo_inicial_de_pago = as.Date(periodo_inicial_de_pago, "%Y-%m-%d"),
         fecha_pago  = as.Date( fecha_pago , "%d/%m/%Y")) %>% 
  dplyr::select(-nombre)


#Nomina desde 2021----------------------------------------------------------------------------------

nomina <- read_excel(file,
                          sheet = 'nomina',
                          col_names = TRUE,
                          col_types = NULL,
                          na = "",
                          skip = 0) %>% clean_names() %>%
  mutate(periodo = as.Date( periodo, "%d/%m/%Y")) %>% 
  dplyr::select(-nombre)


#Nomina del segundo grupo---------------------------------------------------------------------------
nomina_cem_v2 <- read_excel( file_c,
                             sheet = 'nomina_cementeros',
                             col_names = TRUE,
                             col_types = NULL,
                             na = "",
                             skip = 0) %>% clean_names() %>%
  mutate( periodo = paste0( periodo, '-01') ) %>% 
  mutate( periodo = as.Date( periodo, "%Y-%m-%d") ) %>% 
  dplyr::select( -apellidos_y_nombres )


nomina_ivm_v2 <- read_excel( file_c,
                             sheet = 'nomina_vejez',
                             col_names = TRUE,
                             col_types = NULL,
                             na = "",
                             skip = 0) %>% clean_names() %>%
  mutate( periodo = paste0( periodo, '-01') ) %>% 
  mutate( periodo = as.Date( periodo, "%Y-%m-%d") ) %>% 
  dplyr::select( -apellidos_y_nombres )

#Guardando en un Rdata------------------------------------------------------------------------------
message( '\tGuardando' )

save( liquidacion, 
      nomina,
      nomina_cem_v2,
      nomina_ivm_v2,
      file = paste0( parametros$RData, 'IESS_nomina_concesiones.RData' ) )

#Borrando data.frames-------------------------------------------------------------------------------
message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% 'parametros' ) ]  )
gc()