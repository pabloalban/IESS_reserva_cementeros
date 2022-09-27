message( paste( rep('-', 100 ), collapse = '' ) )

message( '\tLectura de nomina y concesiones' )

#Cargando información financiera--------------------------------------------------------------------
file<-paste0(parametros$Data, 'IESS_nomina_concesiones.xlsx' )

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


#Guardando en un Rdata------------------------------------------------------------------------------
message( '\tGuardando SBU' )

save( liquidacion, nomina,
      file = paste0( parametros$RData, 'IESS_nomina_concesiones.RData' ) )

#Borrando data.frames-------------------------------------------------------------------------------
message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% 'parametros' ) ]  )
gc()