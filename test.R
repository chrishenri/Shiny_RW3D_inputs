input <- list()

input$nspe <- 3

input$nameaq1 <- 'A'
input$nameaq2 <- 'B'
input$nameaq3 <- 'C'

nam  <- vector(mode="character", length=input$nspe)
for(i in 1:input$nspe) { 
  nam[i] <- get(paste("nameaq", i, sep = ""), input)
}

nameall <- paste(nam, collapse = " ")
inp <- as.environment(input)