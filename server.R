
function(input, output) {
  
  final_name <- reactive({
    file <- input$archivo_base[1]
    if (is.null(file)) return(NULL) else {
      last <- file %>% substr(nchar(.)-2,nchar(.))
      if (last=="lsx") {
        base <- file %>% substr(1,nchar(.)-5)
      } else base <- file %>% substr(1,nchar(.)-4)
      base%+%"_SA_"%+%format(Sys.Date(),"%d-%b-%y")%+%".xlsx"
    }
  })
  
  based <- eventReactive(input$archivo_base, {
    filep <- input$archivo_base
    if (is.null(filep)) return(NULL) else {
      read_excel(filep$datapath,col_names=T)
    }
  })
  
  tipo <- reactive({
    input$tipo
  })
  
  tfreq <- reactive({
    input$tfreq
  })
  
  output$nombreSA <- renderUI({
    textInput("nombreSA","Save SA file as:",value=final_name())
  })
  
  SA_data <- reactive({
    base <- based()
    tipo_1 <- tipo()
    freq <- tfreq() %>% as.numeric
    
    # # borrar----
    # 
    # # Cantidad de grupos de series:
    # N <- 1
    # # NULL si N=1:
    # series_2 <- NULL # vector de series (índices) a las que aplicarle distinto SA
    # series_3 <- NULL # vector de series (índices) a las que aplicarle distinto SA
    # series_4 <- NULL # vector de series (índices) a las que aplicarle distinto SA
    # # series_1 se define por diferencia
    # 
    # # Ruta del excel con regresores propios (NULL si nada):
    # file_reg <- NULL
    # 
    # # otrs parámetros series_1:
    # aictest_1 <- c("td","easter")
    # regvars_1 <- list(NULL)
    # mixedmod_1 <- "no" 
    # regusr_1 <- F # TRUE si tiene regresor propio
    # regnames_1 <- c("census","eurostat") # nombre del reg. en el excel
    # 
    # # Parámetros series_2:
    # tipo_2 <- "flow"
    # aictest_2 <- c("td","easter")
    # regvars_2 <- list(NULL)
    # mixedmod_2 <- "no" 
    # regusr_2 <- F
    # regnames_2 <- c("census")
    # # Parámetros series_3:
    # tipo_3 <- "flow"
    # aictest_3 <- c("td","easter")
    # regvars_3 <- list(NULL)
    # mixedmod_3 <- "no" 
    # regusr_3 <- T
    # regnames_3 <- c("census","eurostat")
    # # Parámetros series_4:
    # tipo_4 <- "flow"
    # aictest_4 <- c("td","easter")
    # regvars_4 <- list(NULL)
    # mixedmod_4 <- "no" 
    # regusr_4 <- T
    # regnames_4 <- c("census")
    # 
    # 
    # 
    # 
    # ## Lectura datos
    # fechas <- base[[1]]
    # mes_i <- format(fechas,"%m")[1] %>% as.numeric
    # ano_i <- format(fechas,"%Y")[1] %>% as.numeric
    # series_so <- lapply(base[-1],ts,
    #                     start=c(ano_i,mes_i),frequency=freq)
    # 
    # ## Listas con las especificaciones
    # regnames <- rep(NULL,N)
    # aictest <- rep(NULL,N)
    # tipo <- rep(NULL,N)
    # mixedmod <- rep(NULL,N)
    # regvars <- rep(NULL,N)
    # regusr <- rep(NULL,N)
    # for (i in 1:N) {
    #   regnames[[i]] <- eval(parse(text="regnames_"%+%i))
    #   aictest[[i]] <- eval(parse(text="aictest_"%+%i))
    #   tipo[[i]] <- eval(parse(text="tipo_"%+%i))
    #   mixedmod[[i]] <- eval(parse(text="mixedmod_"%+%i))
    #   regvars[[i]] <- eval(parse(text="regvars_"%+%i))
    #   regusr[[i]] <- eval(parse(text="regusr_"%+%i))
    # }
    # 
    # ## Lista con regresores del usuario
    # if (!is.null(file_reg)) {
    #   regres <- read_excel(file_reg,col_names = T)
    # }
    # usr <- rep(list(NULL),N)
    # usrtype <- rep(list(NULL),N)
    # for (i in 1:N) {
    #   if (regusr[[i]]) {
    #     usr[[i]] <- lapply(
    #       regres[regnames[[i]]],
    #       ts, start=c(ano_i,mes_i),frequency=freq) %>%
    #       do.call(cbind,.)
    #     usrtype[[i]] <- "seasonal"
    #     aictest[[i]] <- c("lpyear")
    #   }
    # }
    # 
    # ## Lista de las series
    # otras_l <- rep(list(NA),N-1)
    # if (N>1) {
    #   for (i in 1:(N-1)) otras_l[[i]] <- eval(parse(text="series_"%+%(i+1)))  
    # }
    # otras <- Reduce(c,otras_l)
    # series_1 <- setdiff(1:length(series_so),otras)
    # 
    # series_list <- rep(list(NA),N)
    # for (i in 1:N) {
    #   series_list[[i]] <- series_so[eval(parse(text="series_"%+%i))]  
    # }
    # 
    # 
    # 
    # SA <- list(rep(NA),N)
    # for (i in 1:N) {
    #   SA[[i]] <- lapply(
    #     series_list[[i]],
    #     function(x) try(seas(
    #       x,
    #       
    #       series.period = freq,
    #       series.type = tipo[[i]],
    #       regression.variables = regvars[[i]][[1]],
    #       regression.aictest = aictest[[i]],
    #       automdl.mixed = mixedmod[[i]], #ATENTO QUE ESTA SENTENCIA GENERA CAMBIOS C.R.A. MÉTODO BCRA
    #       
    #       x11.appendfcst = "yes",
    #       # x11.seasonalma = c("s3x5","s3x5","s3x5","s3x5","s3x5","s3x5","s3x3","s3x5","s3x5","s3x5","s3x5","s3x5")
    #       # "3x3" si SI<2,5 ; "3x9" si SI>6,5
    #       
    #       regression.chi2test = "yes",
    #       outlier.types = c("ao", "ls","tc"),
    #       
    #       xreg = usr[[i]],
    #       regression.usertype = usrtype[[i]],
    #       
    #       x11.final = "user",
    #       x11.seasonalma = "msr",
    #       x11.calendarsigma = "all",
    #       spectrum = "",
    #       estimate.outofsample = "yes", 
    #       transform.save = "a3",
    #       check.maxlag = 36
    #       # na.action = na.fail,
    #       
    #       # x11.savelog = "all",
    #       # slidingspans.print = c("default", "fmi"),
    #       # slidingspans.save = "sfspans",
    #       # slidingspans.savelog = "percent"   , 
    #       # slidingspans.additivesa = "percent",
    #       # history.estimates = c("fcst","aic","sadj","sadjchng","trend","trendchng","seasonal"),
    #       # history.sadjlags = c("1,2,","3,12,24"),
    #       # history.trendlags = "1,2,3,12,24",
    #       # history.save = c("sar","trr","fce"),
    #       # history.savelog = c("asa","ach","atr","atc") 
    #     )
    #     )
    #   )
    # }
    # 
    # 
    # 
    # 
    # # Ordena nombres
    # SA %<>% Reduce(c,.) %>% "["(match( names(series_so) , names(.) ))
    # 
    # # Función para pasar de lista a data frame con distinto nros de fila
    # list_order <- function(lista) {
    #   for (i in 1:length(lista)) {
    #     lista[[i]] %<>% as.data.frame
    #     names(lista[[i]]) <- names(lista[i])
    #     lista[[i]]$index <- seq(1:nrow(lista[[i]]))
    #   }
    #   lista_d <- Reduce(function(a,b) merge(a,b,by="index",all=T), lista) %>% 
    #     select(-index)
    #   return(lista_d)
    # }
    # 
    # # Index series con errores
    # err <- sapply(SA,class)=="try-error"
    # errores <- ifelse(any(err==T)==T,which(err==T),"NO HAY ERRORES")
    # 
    # out_a <- list(
    #   # Seasonally Adj
    #   series_SA = lapply(SA[!err],final) %>% cbind,
    #   # Ciclo Tendencia
    #   series_CT = lapply(SA[!err],trend) %>% cbind,
    #   # Irregular
    #   series_I = lapply(SA[!err],irregular) %>% cbind,
    #   # Preajustadas (¿"a3"o "b1"?)
    #   series_PA = lapply(SA[!err],function(x) series(x,"b1")) %>% cbind,
    #   # Seasonal factors
    #   series_SF = lapply(SA[!err],function(x) series(x,"d10")) %>% cbind
    # )
    # 
    # # Advertencias ("warns"):
    # warns <- rep(list(NA),length(SA)) %>% setNames(names(SA))
    # for (i in 1:length(SA)){
    #   if (length(SA[[i]]$err$warning)==0) next
    #   warns[[i]] <- SA[[i]]$err$warning
    # }
    # warns[is.na(warns)] <- NULL
    # # Convierte en dataframe
    # if (length(warns)==0) warns_d <- "NO WARNINGS" else {
    #   warns_d <- list_order(warns) %>% t %>% as.data.frame %>% 
    #     mutate(serie=rownames(.),
    #            nro=match(rownames(.),names(series_so))) %>% 
    #     select(serie,nro,everything())
    # }
    # 
    # # Regresores ("regs"):
    # regs <- rep(list(NA),length(SA)) %>% setNames(names(SA))
    # for (i in 1:length(SA)) {
    #   if (is.null(summary(SA[[i]])$model$regression$variables)) next
    #   regs[[i]] <- summary(SA[[i]])$model$regression$variables
    # }
    # # Convierte en dataframe
    # if (length(regs)==0) regs_d <- "NO REGRESSION VARIABLES" else {
    #   regs_d <- list_order(regs) %>% t %>% as.data.frame %>% 
    #     mutate(serie=rownames(.)) %>% select(serie,everything())
    # }
    # 
    # # Parámetros ("parameters")
    # param_nom <- c("series","tipo","aictest","regvars","mixedmod","regnames")
    # param <- matrix(NA,nr=N,nc=length(param_nom),
    #                 dimnames=list(NULL,param_nom))
    # for (i in param_nom) {
    #   for (j in 1:N) {
    #     param[j,i] <- get(i%+%"_"%+%j) %>% paste0(collapse=",")
    #   }
    # }
    # # regnames=NULL cuando regusr=F
    # for (j in 1:N) {
    #   if (get("regusr_"%+%j)==F) param[j,"regnames"] <- "NULL"
    # }
    # 
    # # Convierte en dataframe (si no se quiere este formato, pegar "param" en
    # #   la lista, en lugar de "param_d")
    # param_d <- param %>% as.data.frame %>% 
    #   separate_rows(series,sep=",",convert=T) %>% 
    #   select(series,everything()) %>% arrange(series)
    # 
    # # Lista "out_vf" con todas las salidas
    # out_b <- rep(list(NA),length(out_a)) %>% setNames(names(out_a))
    # # reemplazo las so por los outputs para mantener los NA y poder cargar series
    # # de distinta longitud
    # for (i in 1:(length(out_b)-2)) {
    #   out_b[[i]] <- as.data.frame(base[-1])
    #   for (j in 1:length(out_b[[i]])) {
    #     out_b[[i]][,j][!is.na(out_b[[i]][,j])] <- as.numeric(out_a[[i]][[j]])
    #   }
    # }
    # # carga PA y SF por separado porque tienen más datos (forecast)
    # # (repitiendo los últimos 12 datos y después pegando)   
    # for (i in c("series_PA","series_SF")) {
    #   out_b[[i]] <- as.data.frame(base[-1]) %>%
    #     bind_rows(tail(.,freq)) %>% as.matrix
    #   for (j in 1:ncol(out_b[[i]])) {
    #     out_b[[i]][,j][1:length(out_a[[i]][[j]])] <- 
    #       as.numeric(out_a[[i]][[j]])
    #   }
    # }
    # # Agrega s.originales
    # out_b$series_so <- as.data.frame(base[-1])
    # # Agrega fechas en la primera columna
    # out_vf <- lapply(out_b, function(x) 
    #   cbind("fechas"=seq(from=fechas[1],by="month",length.out=nrow(x)) %>%
    #           format("%b-%y"),
    #         x))
    # # Agrega warns_d,regs_d y ordena
    # out_vf$Warns <- warns_d
    # out_vf$Regs <- regs_d
    # out_vf$Param <- param # poner 'param_d' para cambiar formato
    # 
    # 
    # ----
    source("script_SA.R",local=T)
    
    list(out_vf$series_so,out_vf$series_SA,out_vf$series_CT,
         out_vf$series_I,out_vf$series_PA,out_vf$series_SF,
         out_vf$Warns,out_vf$Regs,out_vf$Param) %>% 
      setNames(c("Original","SA","CT","Irregular","PriorAdjusted","SF",
                 "Warnings","Reg.Variables","Parameters"))
  })  
  
  output$down <- downloadHandler(
    filename = function() final_name(),
    content = function(file) {
      fname <- file%+%".xlsx"
      SA_dfs <- SA_data()
      wb <- loadWorkbook(fname, create=T)
      hojas <- names(SA_dfs)
      for (i in hojas) {
        createSheet(wb,name=i)
        clearSheet(wb,sheet=i)
        createName(wb,name=i,formula=i%+%"!$A$1",overwrite=T)
        writeNamedRegion(wb,data=SA_dfs[[i]],name=i)
      }
      saveWorkbook(wb)
      file.rename(fname,file)
    }
  )
  
  
  # output$mensaje <- renderText( {
  #   if (length(input$nombreSA)>0) paste("Series desestacionalizadas con éxito")
  # } )
  
  # LOGRAR EXTRAER RUTA DEL ARCHIVO DE ORIGEN PARA PONER COMO TEXTINPUT
  #   (o ver https://github.com/wleepang/shiny-directory-input)
  # output$folderSA <- renderUI({
  #   textInput("folderSA","In:",value=input$archivo_base$datapath)
  # })
  
  # output$submensaje <- renderText( {
  #   if (input$Run>0) paste("Click Download to save SA file")
  # } )
  
  
  # detalles no utiles:
  
  imagen <- reactive( {
    if (is.null(final_name)) return(NULL) else {
      imglist <- list.files("www")
      imgname <- imglist[sample(1:length(imglist),1)]
      list(src=normalizePath("www/"%+%imgname),alt="img")
    }
  } )
  
  output$imagen <- renderImage( {
    if (is.null(final_name)) return(NULL) else {
      imagen()
    }
  },
  deleteFile=FALSE)
  
  output$calor <- renderText( {
    if (length(input$nombreSA)) paste("Cool")
  } )
  
}
 