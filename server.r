library(shiny)
library(plotly)
library(shinythemes)
library(dplyr)

chunk2 <- function(x,n) split(x, cut(seq_along(x), n, labels = FALSE)) 

c_bounce <- "rgba(221,81,58,1)"
c_default <- "rgba(252,255,164,1)"
c_right = c_default
c_left = c_default
c_top = c_default
c_bottom = c_default
c_front = c_default
c_back = c_default

shinyServer( function(input,output,session){
  
  #General Parameters
  #----------------------------------------------------------
  observeEvent(input$instruPanel1, {
    showModal(modalDialog(
      title = "Instructions",
      "debug level>0: write dispersivities arrays", br(),
      "debug level>2: write darcy velocities in gslib format", br(),
      "", br(),
      "Give a unique name for each aqueous/mineral species",
      easyClose = TRUE
    ))
  })
  
  
  #---------------------------
  output$Inputfile <- renderUI({
    if (input$ImportInp == TRUE) {
      list(
        div(style="display:inline-block; width: 250px",textInput("inpname","Name of input file","Coming soon!",width = "90%")),
        div(style="display:inline-block; width: 100px",actionButton("inpbutton", "Import"))
      )
    }
  })
  
  #---------------------------
  output$HnameAq <- renderUI({
    numAq <- as.integer(input$naq)
    if (numAq>0) {
    helpText("Given a name to each aqueous species:")
    }
  })
  
  #---------------------------
  output$nameAq <- renderUI({
    numAq <- as.integer(input$naq)
    if (numAq > 0) {
      lapply(1:numAq, function(i) {
       txt <- c("Name of aqueous species #",i)
       textInput(paste0("nameaq",i),paste(txt, collapse = ''),"",width = "65%")
    })}
  })
  
  #---------------------------
  output$HnameMn <- renderUI({
    numMn <- as.integer(input$nmn)
    if (numMn>0) {
      helpText("Given a name to each mineral species:")
    }
  })
  
  #---------------------------
  output$nameMn <- renderUI({
    numMn <- as.integer(input$nmn)
    if (numMn>0) {
      lapply(1:numMn, function(i) {
        txt <- c("Name of mineral species #",i)
        textInput(paste0("namemn",i),paste(txt, collapse = ''),"",width = "65%")
      }) }
  })
  
  #---------------------------
  Prob <- reactive({
  
    inplist <- as.list(input)
    
    namAq <- vector(mode="character", length=input$naq)
    if (input$naq > 0 ){
      for(i in 1:input$naq){
        namAq[i] <- get(paste("nameaq", i, sep = ""),inplist)
      }
    }
    
    namMn  <- vector(mode="character", length=input$nmn)
    if (input$nmn > 0 ) {
      for(i in 1:input$nmn) {
        namMn[i] <- get(paste("namemn", i, sep = ""),inplist)
      }
    }
    
    data.frame(
      Parameter = c("Number of aqueous species",
                    "Name(s) of aqueous species",
                    "Number of mineral species",
                    "Name(s) of mineral species",
                    "Debug level",
                    "End simulation at time"),
      
      Values = c(input$naq,
                if (input$naq == 0){"-"},
                if (input$naq > 0) { paste(namAq, collapse = " ") },
                input$nmn,
                if (input$nmn == 0){"-"},
                if (input$nmn > 0) { paste(namMn, collapse = " ") },
                input$dbg,
                input$tend))
  })
  
  output$problem <- renderTable({
    Prob()
  })
  
  
  #Geometry
  #----------------------------------------------------------
  #PLot x-grid
  output$grid <- renderPlotly({
    nx <- as.double(input$nx)
    ny <- as.double(input$ny)
    nz <- as.double(input$nz)

    dx <- as.double(input$dx)
    dy <- as.double(input$dy)
    dz <- as.double(input$dz)
    

    # #Plot grid 
    
    # xv <- seq(from = 0, to = nx*dx, by = dx)
    # yv <- rep(0, times = nx+1)
    # zv <- rep(0, times = nx+1)
    # data <- data.frame(xv, yv, zv)
    # p <- plot_ly(data, x = ~xv, y = ~yv, z = ~zv, type = 'scatter3d', mode = 'lines+markers')
    # p
    # for (y in 1:ny) {
    #   yv <- rep(y, times = nx+1)
    #   p <- add_trace(p, y = ~yv,
    #                  evaluate = TRUE)
    # }
    
    c_right = ifelse(input$ibx1 == TRUE, c_bounce, c_default)
    c_left = ifelse(input$ibx2 == TRUE, c_bounce, c_default)
    c_front = ifelse(input$iby1 == TRUE, c_bounce, c_default)
    c_back = ifelse(input$iby2 == TRUE, c_bounce, c_default)
    c_top = ifelse(input$ibz1 == TRUE, c_bounce, c_default)
    c_bottom = ifelse(input$ibz2 == TRUE, c_bounce, c_default)
    
    plot_ly(type = 'mesh3d',
            opacity = 1.0,
      x = c(0, 0,     nx*dx, nx*dx, 0,     0,     nx*dx, nx*dx),
      y = c(0, ny*dy, ny*dy, 0,     0,     ny*dy, ny*dy, 0),
      z = c(0, 0,     0,     0,     nz*dz, nz*dz, nz*dz, nz*dz),
      i = c(7, 0, 0, 0, 4, 4, 2, 6, 4, 0, 3, 7),
      j = c(3, 4, 1, 2, 5, 6, 5, 5, 0, 1, 2, 2),
      k = c(0, 7, 2, 3, 6, 7, 1, 2, 5, 5, 7, 6),
      facecolor = c(c_left,c_left,c_bottom,c_bottom,c_top,c_top,c_right,c_right,c_front,c_front,c_back,c_back)
      ) %>% 
    layout(
      scene = list(
        aspectratio = list(x = 1, y = (ny*dy)/(nx*dx), z = (nz*dz)/(nx*dx)),
        camera = list(eye = list(x=-1.5,y=-1.5,z=.9)) #, up = list(x=0,y=0,z=-1))
      ))
    
  })
  
  output$DiscX <- renderUI({
    if (input$filedx == "TRUE") {
      list(
      textInput("filedx","Name of file","",width = "65%"),
      numericInput("Multdx","Multiplier","1.0",min = 0, max = NA,step = 1,width = "65%")
      )
    }
  })
  
  output$DiscY <- renderUI({
    if (input$filedy == "TRUE") {
      list(
      textInput("filedy","Name of file","",width = "65%"),
      numericInput("Multdy","Multiplier","1.0",min = 0, max = NA,step = 1,width = "65%")
      )
    }
  })
  
  output$DiscZ <- renderUI({
    if (input$filedz == "TRUE") {
      list(
      textInput("filedz","Name of file","",width = "65%"),
      numericInput("Multdz","Multiplier","1.0",min = 0, max = NA,step = 1,width = "65%")
      )
    }
  })
  
  
  #Time discretization
  #----------------------------------------------------------
  output$TimeDisc <- renderUI({
    if (input$meth_dt == 'optimum_dt') {
      list(
        numericInput("Cu","Courant Number","0.1",min = 0, max = 1, step = 0.01, width = "65%"),
        numericInput("Pe","Peclet Number","0.1",min = 0, max = 1, step = 0.01, width = "65%"),
        numericInput("DaMMT","Damkohler Number Multirate Mass Transfer","0.1",min = 0, max = 1, step = 0.01, width = "65%"),
        numericInput("DaK","Damkohler Number First-Order Decay","0.1",min = 0, max = 1, step = 0.01, width = "65%")
      )
    } else if (input$meth_dt == 'constant_dt') {
      numericInput("dt","Time step","1.0",min = 0, max = NA, step = 0.1, width = "65%")
    } else if (input$meth_dt == 'constant_cu') {
      numericInput("Cu","Courant Number","0.1",min = 0, max = 1, step = 0.01, width = "65%")
    } else if (input$meth_dt == 'constant_pe') {
      numericInput("Pe","Peclet Number","0.1",min = 0, max = 1, step = 0.01, width = "65%")
    } else if (input$meth_dt == 'constant_DaMT') {
      numericInput("DaMMT","Damkohler Number Multirate Mass Transfer","0.1",min = 0, max = 1, step = 0.01, width = "65%")
    } else if (input$meth_dt == 'constant_DaDecay') {
      numericInput("DaK","Damkohler Number First-Order Decay","0.1",min = 0, max = 1, step = 0.01, width = "65%")
    } 
    
  })
  
  output$TimeDiscOut <- renderUI({
    if (input$meth_dt == 'optimum_dt') {
      str1 <- paste("<h4>Method: Optimum</h4> <br/>")
      str2 <- paste("Estimates the time step from grid Curant number, Peclet number and Damkohler numbers based on reaction; and pick the more restrictive one, i.e.:")
      HTML(paste(str1,str2,sep='<br/>'))
    } 
    else if (input$meth_dt == 'constant_dt') {
      str1 <- paste("<h4>Method: Constant time step</h4> <br/>")
      str2 <- paste("The time step is fixed: standard random walk.")
      HTML(paste(str1,str2,sep='<br/>'))
    } 
    else if (input$meth_dt == 'constant_cu') {
      str1 <- paste("<h4>Method: Constant Courant number</h4> <br/>")
      str2 <- paste("The time step is estimated from:")
      HTML(paste(str1,str2,sep='<br/>'))
    } 
    else if (input$meth_dt == 'constant_pe') {
      str1 <- paste("<h4>Method: Constant Peclet number</h4> <br/>")
      str2 <- paste("The time step is estimated from:")
      HTML(paste(str1,str2,sep='<br/>'))
    } 
    else if (input$meth_dt == 'constant_DaMT') {
      str1 <- paste("<h4>Method: Constant MMT Damkohler number</h4> <br/>")
      str2 <- paste("The time step is estimated from:")
      HTML(paste(str1,str2,sep='<br/>'))
    } 
    else if (input$meth_dt == 'constant_DaDecay') {
      str1 <- paste("<h4>Method: Constant Decay Rate Damkohler number</h4> <br/>")
      str2 <- paste("The time step is estimated from:")
      HTML(paste(str1,str2,sep='<br/>'))
    } 
  })
  
  output$ex1 <- renderUI({
    if (input$meth_dt == 'optimum_dt') {
      withMathJax(
        helpText('$$ \\Delta t = \\min\\{Cu \\times tc_{adv}, Pe \\times tc_{disp}, Da_{MT} \\times tc_{MT}, Da_{K} \\times tc_{K}, Da_{Kinetic} \\times tc_{Kinetic}\\} $$')
      )
    }
    else if (input$meth_dt == 'constant_cu') {
      withMathJax(
        helpText('$$ \\Delta t = Cu \\times tc_{ADV} $$')
      )
    } 
    else if (input$meth_dt == 'constant_pe') {
      withMathJax(
        helpText('$$ \\Delta t = Pe \\times tc_{DISP} $$')
      )
    } 
    else if (input$meth_dt == 'constant_DaMT') {
      withMathJax(
        helpText('$$ \\Delta t = Da_{MT} \\times tc_{MT} $$')
      )
    } 
    else if (input$meth_dt == 'constant_DaDecay') {
      withMathJax(
        helpText('$$ \\Delta t = Da_{K} \\times tc_{K} $$')
      )
    } 
  })
  
  output$TimeDiscOut2 <- renderUI({
    if (input$meth_dt == 'optimum_dt') {
      str1 <- paste("where Cu is the (user defined) grid-Curant number, and tc<sub>adv</sub> is the advective characteristic time;")
      str2 <- paste("Pe is the (user defined) grid-Peclet number, and tc<sub>disp</sub> is the dispersion characteristic time;")
      str3 <- paste("Da<sub>MT</sub> is the (user defined) grid-Damkholer number based on Mass Transfer process, and tc<sub>MT</sub> is the characteristic time for mass transfer;")
      str4 <- paste("Da<sub>K</sub> is the (user defined) grid-Damkholer number based on First-order decay process, and tc<sub>K</sub> is the characteristic time for first-order decay; and")
      str5 <- paste("Da<sub>Kinetic</sub> is the (user defined) grid-Damkholer number based on kinetic reaction process, and tc<sub>Kinetic</sub> is the characteristic time for kinetic reaction.")
      str6 <- paste("<br/> The different characteristic times are characterized as:")
      HTML(paste(str1,str2,str3,str4,str5,str6,sep='<br/>'))
    } 
    else if (input$meth_dt == 'constant_dt') {
      HTML("")
    } 
    else if (input$meth_dt == 'constant_cu') {
      HTML("where Cu is the (user defined) grid-Curant number, and tc<sub>adv</sub> is the advective characteristic time, i.e.:")
    } 
    else if (input$meth_dt == 'constant_pe') {
      HTML("where Pe is the (user defined) grid-Peclet number, and tc<sub>disp</sub> is the dispersion characteristic time, i.e.:")
    } 
    else if (input$meth_dt == 'constant_DaMT') {
      HTML("where Da<sub>MT</sub> is the (user defined) grid-Damkholer number based on Mass Transfer process, and tc<sub>MT</sub> is the characteristic time for mass transfer, i.e.:")
    } 
    else if (input$meth_dt == 'constant_DaDecay') {
      HTML("where Da<sub>K</sub> is the (user defined) grid-Damkholer number based on First-order decay process, and tc<sub>K</sub> is the characteristic time for first-order decay, i.e.:")
    } 
  })
  
  output$ex2 <- renderUI({
    if (input$meth_dt == 'optimum_dt') {
      withMathJax(
        helpText('$$ tc_{adv} = \\frac{\\Delta_{s}}{\\min_{v1,v2,v3} \\{v_i/R\\}} $$')
      )
    }
    else if (input$meth_dt == 'constant_cu') {
      withMathJax(
        helpText('$$ tc_{adv} = \\frac{\\Delta_{s}}{\\min_{v1,v2,v3} \\{v_i/R\\}} $$')
      )
    } 
    else if (input$meth_dt == 'constant_pe') {
      withMathJax(
        helpText('$$  tc_{disp} =  $$')
      )
    } 
    else if (input$meth_dt == 'constant_DaMT') {
      withMathJax(
        helpText('$$ tc_{MT} =  $$')
      )
    } 
    else if (input$meth_dt == 'constant_DaDecay') {
      withMathJax(
        helpText('$$ tc_{K} =  $$')
      )
    } 
  })
  
  
  #Advection
  #----------------------------------------------------------
  
  output$AdvMeth <- renderUI({
    if (input$advFlag) {
      selectInput("advmeth", "Method for computation of advective displacement:",
                  c("Eulerian" = "veul",
                    "Exponential" = "vexp"
                    ))
      }
  })
  
  output$AdvVal <- renderUI({
    if (input$advFlag) {
      selectInput("advval", "Method to read Darcy velocities:",
                  c("Constant" = "vcons",
                    "Gslib Array" = "vgslib",
                    "Modflow cell-by-cell" = "vmf"
                  ))
    }
  })
  
  output$AdvCons <- renderUI({
    if (input$advFlag) {
      if (input$advval == 'vcons') {
        list(
        numericInput("qx","Darcy velocity in x",0,min = 0, max = NA,width = "65%"),
        numericInput("qy","Darcy velocity in y",0,min = 0, max = NA,width = "65%"),
        numericInput("qz","Darcy velocity in z",0,min = 0, max = NA,width = "65%")
        )
      }
      else if (input$advval == 'vgslib') {
        list(
          div(style="display:inline-block; width: 250px",textInput("fileqx","Gslib file with qx","",width = "90%")),
          div(style="display:inline-block; width: 100px",numericInput("Multqx","Multiplier","1.0",min = 0, max = NA,step = 1,width = "100%")),
          div(style="display:inline-block; width: 250px",textInput("fileqy","Gslib file with qy","",width = "90%")),
          div(style="display:inline-block; width: 100px",numericInput("Multqy","Multiplier","1.0",min = 0, max = NA,step = 1,width = "100%")),
          div(style="display:inline-block; width: 250px",textInput("fileqz","Gslib file with qz","",width = "90%")),
          div(style="display:inline-block; width: 100px",numericInput("Multqz","Multiplier","1.0",min = 0, max = NA,step = 1,width = "100%"))
        )
      }
      else if (input$advval == 'vmf') {
        list(
          div(style="display:inline-block; width: 250px",textInput("fileqx","CBB file with qx","",width = "90%")),
          div(style="display:inline-block; width: 100px",numericInput("Multqx","Multiplier","1.0",min = 0, max = NA,step = 1,width = "100%")),
          div(style="display:inline-block; width: 250px",textInput("fileqy","CBB file with qy","",width = "90%")),
          div(style="display:inline-block; width: 100px",numericInput("Multqy","Multiplier","1.0",min = 0, max = NA,step = 1,width = "100%")),
          div(style="display:inline-block; width: 250px",textInput("fileqz","CBB file with qz","",width = "90%")),
          div(style="display:inline-block; width: 100px",numericInput("Multqz","Multiplier","1.0",min = 0, max = NA,step = 1,width = "100%")),
          checkboxInput("cbbtransFlag", "Check if transient flow", value = FALSE, width = "100%")
        )
      }
    }
  })
  
  output$CBBtransnSTPR <- renderUI({
    if (input$advFlag) {
      if (input$advval == 'vmf') {
        if (input$cbbtransFlag == "TRUE") {
          list(
            div(style="display:inline-block; width: 200px",numericInput("nSTPR","Number of Stress Period",1,min = 1, max = NA,width = "90%")),
            div(style="display:inline-block; width: 100px",checkboxInput("loopSTPR", "Loop STPR", value = FALSE, width = "100%"))
          )
    }}}
  })
  
  output$CBBtransSTPR <- renderUI({
    if (input$advFlag) {
      if (input$advval == 'vmf') {
        if (input$cbbtransFlag == "TRUE") {
          numSTPR <- as.integer(input$nSTPR)
          if (numSTPR > 0) {
            lapply(1:numSTPR, function(i) {
              txt1 <- c("Length of STPR #",i)
              txt2 <- c("Type of STPR #",i)
              txt3 <- c("Number of DT for STPR #",i)
              txt4 <- c("Multiplier for STPR #",i)
              list(
                div(style="display:inline-block; width: 150px", numericInput(paste0("lSTPR",i),paste(txt1, collapse = ''),"",width = "90%")),
                div(style="display:inline-block; width: 135px", selectInput(paste0("tySTPR",i),paste(txt2, collapse = ''),c("SS" = "SS","TR" = "TR"))),
                div(style="display:inline-block; width: 150px", numericInput(paste0("dtSTPR",i),paste(txt3, collapse = ''),"",width = "90%")),
                div(style="display:inline-block; width: 150px", numericInput(paste0("ldtSTPR",i),paste(txt4, collapse = ''),"",width = "90%"))
                )
            })
          }
    }}}
  })
  
  
  output$poro_gslib <- renderUI({
    if (input$fileporo == "TRUE") {
      list(
        div(style="display:inline-block; width: 250px",textInput("file_poro","Name of file","",width = "100%")),
        div(style="display:inline-block; width: 100px",numericInput("Multporo","Multiplier","1.0",min = 0, max = NA,step = 1,width = "100%"))
      )
    }
  })
  
  #Dispersion
  #----------------------------------------------------------
  
  output$aLval <- renderUI({
    if (input$dispFlag == "TRUE") {
      list(
      numericInput("aL","Longitudinal dispersivity",0.0,min = 0, max = NA,step = 0.01,width = "80%"),
      checkboxInput("fileaL", "Check if use gslib file to read aL", value = FALSE, width = "100%")
      )
    }
  })
  output$aL_gslib <- renderUI({
    if (input$dispFlag == "TRUE") {
    if (input$fileaL == "TRUE") {
      list(
        div(style="display:inline-block; width: 250px",textInput("file_aL","Name of file","",width = "100%")),
        div(style="display:inline-block; width: 100px",numericInput("MultaL","Multiplier","1.0",min = 0, max = NA,step = 1,width = "100%"))
      )
    }}
  })
  
  output$aTHval <- renderUI({
    if (input$dispFlag == "TRUE") {
      list(
        numericInput("aTH","Horizontal transverse dispersivity",0.0,min = 0, max = NA,step = 0.01,width = "80%"),
        checkboxInput("fileaTH", "Check if use gslib file to read aTH", value = FALSE, width = "100%")
      )
    }
  })
  output$aTH_gslib <- renderUI({
    if (input$dispFlag == "TRUE") {
      if (input$fileaTH == "TRUE") {
        list(
          div(style="display:inline-block; width: 250px",textInput("file_aTH","Name of file","",width = "100%")),
          div(style="display:inline-block; width: 100px",numericInput("MultaTH","Multiplier","1.0",min = 0, max = NA,step = 1,width = "100%"))
        )
      }}
  })
  
  output$aTVval <- renderUI({
    if (input$dispFlag == "TRUE") {
      list(
        numericInput("aTV","Vertical transverse dispersivity",0.0,min = 0, max = NA,step = 0.01,width = "80%"),
        checkboxInput("fileaTV", "Check if use gslib file to read aTV", value = FALSE, width = "100%")
      )
    }
  })
  output$aTV_gslib <- renderUI({
    if (input$dispFlag == "TRUE") {
      if (input$fileaTV == "TRUE") {
        list(
          div(style="display:inline-block; width: 250px",textInput("file_aTV","Name of file","",width = "100%")),
          div(style="display:inline-block; width: 100px",numericInput("MultaTV","Multiplier","1.0",min = 0, max = NA,step = 1,width = "100%"))
        )
      }}
  })

  
  output$multDisp <- renderUI({
    if (input$dispFlag == "TRUE") {
      #get all species names
      numAq <- as.integer(input$naq)
      numMn <- as.integer(input$nmn)
      numSp <- numAq+numMn
      if (numSp>1) {
        inplist <- as.list(input)
        namSp <- vector(mode="character", length=numSp)
        if (numAq > 0 ){
          for(i in 1:numAq){
            namSp[i] <- get(paste("nameaq", i, sep = ""),inplist)
          }
        }
        if (numMn > 0 ){
          for(i in 1:numMn){
            namSp[i+numAq] <- get(paste("namemn", i, sep = ""),inplist)
          }
        }
        
        lapply(1:numSp, function(i) {
          txt <- c("Mult. species ",namSp[i])
          div(style="display:inline-block; width: 150px",numericInput(paste0("multDispspe",i),paste(txt, collapse = ''),"1.0",width = "90%"))
        })
      }
    }
  })
  
  
  #Diffusion
  #----------------------------------------------------------
  output$DmLval <- renderUI({
    if (input$diffFlag == "TRUE") {
      list(
        numericInput("DmL","Longitudinal diffusion",0.0,min = 0, max = NA,step = 0.01,width = "80%"),
        checkboxInput("fileDmL", "Check if use gslib file to read Dm_L", value = FALSE, width = "100%")
      )
    }
  })
  output$DmL_gslib <- renderUI({
    if (input$diffFlag == "TRUE") {
      if (input$fileDmL == "TRUE") {
        list(
          div(style="display:inline-block; width: 250px",textInput("file_DmL","Name of file","",width = "100%")),
          div(style="display:inline-block; width: 100px",numericInput("MultDmL","Multiplier","1.0",min = 0, max = NA,step = 1,width = "100%"))
        )
      }}
  })
  
  output$DmTHval <- renderUI({
    if (input$diffFlag == "TRUE") {
      list(
        numericInput("DmTH","Horizontal transverse diffusion",0.0,min = 0, max = NA,step = 0.01,width = "80%"),
        checkboxInput("fileDmTH", "Check if use gslib file to read Dm_TH", value = FALSE, width = "100%")
      )
    }
  })
  output$DmTH_gslib <- renderUI({
    if (input$diffFlag == "TRUE") {
      if (input$fileDmTH == "TRUE") {
        list(
          div(style="display:inline-block; width: 250px",textInput("file_DmTH","Name of file","",width = "100%")),
          div(style="display:inline-block; width: 100px",numericInput("MultDmTH","Multiplier","1.0",min = 0, max = NA,step = 1,width = "100%"))
        )
      }}
  })
  
  output$DmTVval <- renderUI({
    if (input$diffFlag == "TRUE") {
      list(
        numericInput("DmTV","Vertical transverse diffusion",0.0,min = 0, max = NA,step = 0.01,width = "80%"),
        checkboxInput("fileDmTV", "Check if use gslib file to read Dm_TV", value = FALSE, width = "100%")
      )
    }
  })
  output$DmTV_gslib <- renderUI({
    if (input$diffFlag == "TRUE") {
      if (input$fileDmTV == "TRUE") {
        list(
          div(style="display:inline-block; width: 250px",textInput("file_DmTV","Name of file","",width = "100%")),
          div(style="display:inline-block; width: 100px",numericInput("MultDmTV","Multiplier","1.0",min = 0, max = NA,step = 1,width = "100%"))
        )
      }}
  })
  
  output$multDiff <- renderUI({
    if (input$diffFlag == "TRUE") {
      #get all species names
      numAq <- as.integer(input$naq)
      numMn <- as.integer(input$nmn)
      numSp <- numAq+numMn
      if (numSp>1) {
        inplist <- as.list(input)
        namSp <- vector(mode="character", length=numSp)
        if (numAq > 0 ){
          for(i in 1:numAq){
            namSp[i] <- get(paste("nameaq", i, sep = ""),inplist)
          }
        }
        if (numMn > 0 ){
          for(i in 1:numMn){
            namSp[i+numAq] <- get(paste("namemn", i, sep = ""),inplist)
          }
        }
        
        lapply(1:numSp, function(i) {
          txt <- c("Mult. species ",namSp[i])
          div(style="display:inline-block; width: 150px",numericInput(paste0("multDiffspe",i),paste(txt, collapse = ''),"1.0",width = "90%"))
        })
      }
    }
  })
  
  
  #Multirate Mass Transfer
  #----------------------------------------------------------
  output$MMTType <- renderUI({
    if (input$mmtFlag) {
      selectInput("mmttype", "Mass transfer type / geometry:",
                  c("Multirate" = "mmt_multir",
                    "Spherical Diffusion" = "mmt_sphdif",
                    "Layered Diffusion" = "mmt_laydif",
                    "Cylindral Diffusion" = "mmt_cyldif",
                    "Power Law" = "mmt_power",
                    "Lognormal Law" = "mmt_lognor",
                    "Composite Media" = "mmt_comp"
                  ))
    }
  })
  
  output$MMTNim <- renderUI({
    mmtFlag <- as.logical(input$mmtFlag)
    if (mmtFlag) {
      if (input$mmttype == "mmt_multir" | input$mmttype == "mmt_sphdif" | input$mmttype == "mmt_laydif" | input$mmttype == "mmt_cyldif" | input$mmttype == "mmt_power" | input$mmttype == "mmt_lognor") {
        numericInput("nim","Number of immobile zone",0,min = 0, max = NA,width = "100%")
      }
    }
  })
  
  
  output$MMTPara <- renderUI({
    mmtFlag <- as.logical(input$mmtFlag)
    if (mmtFlag) {
      nim <- as.integer(input$nim)
      if (nim > 0) {
        list(
          lapply(1:nim, function(i) {
            txt <- c("Porosity of immobile zone #",i)
            textInput(paste0("poroim",i),paste(txt, collapse = ''),"",width = "75%")
            }),
          lapply(1:nim, function(i) {
            txt <- c("Mass transfer rate of immobile zone #",i)
            textInput(paste0("alpha",i),paste(txt, collapse = ''),"",width = "75%")
          })
        )
      }
    }
  })
  
  
  
}) #function #SinyServer