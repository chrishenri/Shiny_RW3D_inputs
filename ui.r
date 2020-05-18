#-----------------------------------------
#Genereate input form for RW3D using Shiny
#-----------------------------------------

library(shiny)
library(plotly)
# library(shinythemes)

# Source the switch button function
source("./Rsource/SwitchButton.R")

#shinyUI(navbarPage(theme = shinytheme("flatly"),
#shinyUI(navbarPage(theme = "button.css",
shinyUI(navbarPage(#theme = "bootstrap.css",             
  "RW3D_Inputs",
  theme = "flatly_button.css",#shinythemes::shinytheme("flatly"),

  #--------------------------
  #Panel#1: General parameters
  #--------------------------
  tabPanel(title = "General Parameters",
  sidebarLayout(
    sidebarPanel(switchButton(inputId = "ImportInp",
                              label = h4("Import Inputs?"),
                              value = FALSE, col = "GB", type = "TF"),
                 uiOutput("Inputfile"),
                 
                 br(),
                 h4("Debug / Species / Time"),
                 sliderInput("dbg", "Debug level", min=0, max=3, value = 0),

                 numericInput("tend","End simulation at time [T]:","0",min = 0, max = NA,step = 10,width = "100%"),
                 
                 br(),
                 numericInput("naq","Number of aqueous species","0",min = 0, max = NA,step = 1,width = "100%"),
                 uiOutput("HnameAq"),
                 uiOutput("nameAq"),
                 
                 br(),
                 numericInput("nmn","Number of mineral species","0",min = 0, max = NA,step = 1,width = "100%"),
                 uiOutput("HnameMn"),
                 uiOutput("nameMn"),
                 
                 br(),
                 actionButton("instruPanel1", "?")
                 #downloadButton("download_button", label = "Generate Input File")
                 ),
    
    mainPanel(img(src='RW3D_logov2.png', width = "120px", align = "right"),
              h3(""),
              br(), br(), br(),
              tableOutput("problem")
              )
    
  )),
  
  
  #--------------------------
  #Panel#2: Geometry
  #--------------------------
  tabPanel(title = "Geometry",
           sidebarLayout(
             sidebarPanel(
               #Disc. x
               h4("Domain discretization in x-direction"),
               
               div(style="display:inline-block; width: 160px",numericInput("nx","Number of cells in x",1,min = 1, max = NA,step = 1,width = "90%")),
               div(style="display:inline-block; width: 160px",numericInput("dx","Cell size in x",1,min = 0, max = NA,step = 1,width = "90%")),
               checkboxInput("filedx", "Check if use gslib  file for discretization", value = FALSE, width = "100%"),
               uiOutput("DiscX"),

               #Disc. y
               br(),h4("Domain discretization in y-direction"),
               
               div(style="display:inline-block; width: 160px",numericInput("ny","Number of cells in y",1,min = 1, max = NA,step = 1,width = "90%")),
               div(style="display:inline-block; width: 160px",numericInput("dy","Cell size in y",1,min = 0, max = NA,step = 1,width = "90%")),
               checkboxInput("filedy", "Check if use gslib file for discretization", value = FALSE, width = "100%"),
               uiOutput("DiscY"),

               #Disc. z
               br(),h4("Domain discretization in z-direction"),
               
               div(style="display:inline-block; width: 160px",numericInput("nz","Number of cells in z",1,min = 1, max = NA,step = 1,width = "90%")),
               div(style="display:inline-block; width: 160px",numericInput("dz","Cell size in z",1,min = 0, max = NA,step = 1,width = "90%")),
               checkboxInput("filedz", "Check if use gslib file for discretization", value = FALSE, width = "100%"),
               uiOutput("DiscZ"), br(),
               
               #Boundary conditions
               h4("Check for particles to bounce on:"), 
               
               div(style="display:inline-block; width: 100px",checkboxInput("ibx1", "left side", value = FALSE, width = "200%")),
               div(style="display:inline-block; width: 100px",checkboxInput("ibx2", "right side", value = FALSE, width = "200%")), br(),
               
               div(style="display:inline-block; width: 100px",checkboxInput("iby1", "front side", value = FALSE, width = "100%")),
               div(style="display:inline-block; width: 100px",checkboxInput("iby2", "back side", value = FALSE, width = "100%")), br(),
               
               div(style="display:inline-block; width: 100px",checkboxInput("ibz1", "top", value = FALSE, width = "100%")),
               div(style="display:inline-block; width: 100px",checkboxInput("ibz2", "bottom", value = FALSE, width = "100%"))

             ),
    
             mainPanel(img(src='RW3D_logov2.png', width = "120px", align = "right"),
                       h3("Grid visualization"), 
                       plotlyOutput("grid", height = "700px")
             )
           )
  ),
  
  
  #--------------------------
  #Panel#3: Time discretization
  #--------------------------
  tabPanel(title = "Time Disc.",
           sidebarLayout(
             sidebarPanel(
               h4("Time Discretization"),
               selectInput("meth_dt", "Method used to calculate particles time step",
                           c("Optimum" = "optimum_dt",
                             "Constant" = "constant_dt",
                             "Courant" = "constant_cu",
                             "Peclet" = "constant_pe",
                             "Damkohler Mass Transfer" = "constant_DaMT",
                             "Damkohler Decay" = "constant_DaDecay"
                           )),
               uiOutput("TimeDisc")
             ),
             
             mainPanel(img(src='RW3D_logov2.png', width = "120px", align = "right"),
                       withMathJax(),
                       br(),
                       htmlOutput("TimeDiscOut"),
                       uiOutput('ex1'),
                       htmlOutput("TimeDiscOut2"),
                       uiOutput('ex2')
             )
             
           )
  ),
  
  
  #--------------------------
  #Panel#4: Advection
  #--------------------------
  tabPanel(title = "Advection",
           sidebarLayout(
             sidebarPanel(
               
               #Advection
               #h4("Advection"),
               #checkboxInput("advFlag", "Activate"),
               
               numericInput("poro","Porosity",0.3,min = 0, max = NA,step = 0.01,width = "100%"),
               checkboxInput("fileporo", "Check if use gslib file to read porosity", value = FALSE, width = "100%"),
               uiOutput("poro_gslib"),
               br(),
               
               switchButton(inputId = "advFlag",
                            label = h4("Activate Advection Package"),
                            value = FALSE, col = "GB", type = "TF"),
               
               uiOutput("AdvMeth"),
               uiOutput("AdvVal"),
               uiOutput("AdvCons"),
               uiOutput("CBBtransnSTPR"),
               uiOutput("CBBtransSTPR")
               
             ),
             
             mainPanel(img(src='RW3D_logov2.png', width = "120px", align = "right")
             )
             
           )
  ),
  
  
  #--------------------------
  #Panel#5: Dispersion
  #--------------------------
  tabPanel(title = "Dispersion",
           sidebarLayout(
             sidebarPanel(
               
               #h4("Dispersion"),
               #checkboxInput("dispFlag", "Activate"),
               switchButton(inputId = "dispFlag",
                            label = h4("Activate Dispersion Package"),
                            value = FALSE, col = "GB", type = "TF"),
               uiOutput("aLval"),
               uiOutput("aL_gslib"),
               uiOutput("aTHval"),
               uiOutput("aTH_gslib"),
               uiOutput("aTVval"),
               uiOutput("aTV_gslib"),
               uiOutput("multDisp"),
               
               br(),
               switchButton(inputId = "diffFlag",
                            label = h4("Activate Diffusion Package"),
                            value = FALSE, col = "GB", type = "TF"),
               uiOutput("DmLval"),
               uiOutput("DmL_gslib"),
               uiOutput("DmTHval"),
               uiOutput("DmTH_gslib"),
               uiOutput("DmTVval"),
               uiOutput("DmTV_gslib"),
               uiOutput("multDiff")
               
             ),
             
             mainPanel(img(src='RW3D_logov2.png', width = "120px", align = "right")
             )
             
           )
  ),
  
  #--------------------------
  #Panel#6: Multirate MT
  #--------------------------
  tabPanel(title = "MRMT",
           sidebarLayout(
             sidebarPanel(
               #h4("Multirate Mass Transfer"),
               #checkboxInput("mmtFlag", "Activate"),
               switchButton(inputId = "mmtFlag",
                            label = h4("Activate Multirate Mass Transfer Package"),
                            value = FALSE, col = "GB", type = "TF"),
               uiOutput("MMTType"),
               uiOutput("MMTNim"),
               uiOutput("MMTPara")
             ),
             
             mainPanel(img(src='RW3D_logov2.png', width = "120px", align = "right")
             )
             
           )
  ),
  
  
  #--------------------------
  #Panel#7: Reaction
  #--------------------------
  tabPanel(title = "Reaction",
           sidebarLayout(
             sidebarPanel(
               #Sorption
               switchButton(inputId = "sorptionFlag",
                            label = h4("Activate Sorption Package"),
                            value = FALSE, col = "GB", type = "TF"),
               #uiOutput("SorpType"),
               uiOutput("SorpPara"),
               uiOutput("SorpMRMT"),
               uiOutput("SorpParaMRMT"),
               
               br(),
               #First order decay
               switchButton(inputId = "decayFlag",
                            label = h4("Activate 1st-order Decay Package"),
                            value = FALSE, col = "GB", type = "TF"),
               
               br(),
               #Bimolecular reaction 
               switchButton(inputId = "molrxFlag",
                            label = h4("Activate Bimolecular Reaction Package"),
                            value = FALSE, col = "GB", type = "TF"),
             ),
             
             mainPanel(img(src='RW3D_logov2.png', width = "120px", align = "right")
             )
             
           )
  ),
  
  #--------------------------
  #Panel#8: Injection
  #--------------------------
  tabPanel(title = "Injection",
           sidebarLayout(
             sidebarPanel(
             ),

             mainPanel(img(src='RW3D_logov2.png', width = "120px", align = "right")
             )
             
           )
  ),
  
  #--------------------------
  #Panel#9: Recirculation
  #--------------------------
  tabPanel(title = "Recirculation",
           sidebarLayout(
             sidebarPanel(
               switchButton(inputId = "recirculFlag",
                            label = h4("Activate Recirculation Package"),
                            value = FALSE, col = "GB", type = "TF"),
               
             ),
             
             mainPanel(img(src='RW3D_logov2.png', width = "120px", align = "right")
             )
             
           )
  ),
  
  #--------------------------
  #Panel#10: Outputs
  #--------------------------
  tabPanel(title = "Outputs",
           sidebarLayout(
             sidebarPanel(

             ),
             
             mainPanel(img(src='RW3D_logov2.png', width = "120px", align = "right")
             )
             
           )
  )
  
  
#--------------------------  
) #navbar
) #shinyUI