message( paste( rep('-', 100 ), collapse = '' ) )

message( '\tLectura de lista de beneficarios' )

#Cargando información financiera--------------------------------------------------------------------
file<-paste0(parametros$Data, 'IESS_beneficiarios.xlsx' )
file_b<-paste0(parametros$Data, 'BaseCementera.txt' )

#Cargar función tíldes a latex----------------------------------------------------------------------
source( 'R/503_tildes_a_latex.R', encoding = 'UTF-8', echo = FALSE )

#Listado de beneficiarios---------------------------------------------------------------------------
beneficiarios <- read_excel(file,
                  #sheet = 'CBF',
                  col_names = TRUE,
                  col_types = NULL,
                  na = "NA",
                  skip = 0) %>% clean_names() %>%
  mutate( fecha_de_nacimiento = as.Date(fecha_de_nacimiento, "%Y-%m-%d")) %>%
  mutate( f1_renta = as.Date(f1_renta, "%Y-%m-%d")) %>%  
  mutate( fecha_derecho_ivm = as.Date(fecha_derecho_ivm, "%Y-%m-%d")) %>%  
  mutate( fecha_liquidacion_1 = as.Date(fecha_liquidacion_1, "%Y-%m-%d")) %>%
  mutate( fecha_liquidacion_2 = as.Date(fecha_liquidacion_2, "%Y-%m-%d"))
  
base <- read.table(file_b,   
           header = TRUE,
           fill = TRUE,
           sep = "|",      
           dec = ".",
           #nrows=1000,
           colClasses = c(
             "integer",
             "integer",
             "character",
             "character",
             "character",
             "character",
             "character",
             "character",
             "numeric",
             "character",
             "character",
             "character",
             "character",
             "character",
             "character",
             "character",
             "character",
             "character",
             "character",
             "character",
             "numeric",
             "character",
             "character",
             "integer",
             "character",
             "numeric",
             "numeric",
             "character",
             "character",
             "character",
             "character",
             "integer",
             "numeric",
             "character",
             "character",
             "character",
             "integer",
             "numeric",
             "numeric",
             "numeric",
             "character")
                        )
aa<-as.data.frame(sapply(base, class))

cargo <- base %>%
  dplyr::select( anio_planilla,
                 mes_planilaa,
                 cedula, 
                 aporte_cemento,
                 ocupacion_plani_normal,
                 CargoHomologado) %>%
  filter( aporte_cemento=='Si')

aux <- beneficiarios %>%
  left_join(cargo, by='cedula') %>%
  filter(CargoHomologado=='Administrativo') %>%
  distinct(cedula, ocupacion_plani_normal,.keep_all = TRUE)


beneficiarios <- beneficiarios %>%
  mutate( edad_derecho_ivm = round(age_calc(fecha_de_nacimiento,
                                            enddate = fecha_derecho_ivm,
                                            units = "years"),0) ) %>%
  mutate(edad =round(age_calc(fecha_de_nacimiento,
                              enddate = as.Date("31/08/2022","%d/%m/%Y"),
                              units = "years"),0)) %>%
  mutate( n = edad_derecho_ivm ) %>%
  mutate( k =  n - edad ) %>%
  mutate( k = ifelse( k <= 0, NA, k ) )

#Guardando en un Rdata------------------------------------------------------------------------------
message( '\tGuardando beneficiarios CE' )

save( beneficiarios,
      file = paste0( parametros$RData, 'IESS_beneficiarios.RData' ) )

#Borrando data.frames-------------------------------------------------------------------------------
message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% 'parametros' ) ]  )
gc()