library(shiny)

# ==========================================
# 1. USER INTERFACE (from your ui.R)
# ==========================================

#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
parameter_tabs <- tabsetPanel(
  id = "params",
  type = "hidden",
  tabPanel("less",
           numericInput("mul", 
                        "Population Mean, (\u03BC)", 
                        value = 0),
           numericInput("sigl", 
                        "Population Standard Deviation, (\u3C3)", 
                        value = 1),
           numericInput("xnl", "X Value", value = 0),
  ),
  tabPanel("greater", 
           numericInput("mug", 
                        "Population Mean, (\u03BC)", 
                        value = 0),
           numericInput("sigg", 
                        "Population Standard Deviation, (\u3C3)", 
                        value = 1),
           numericInput("xng", "X Value", value = 0),
  ),
  tabPanel("less_great", 
           numericInput("mulg", 
                        "Population Mean, (\u03BC)", 
                        value = 0),
           numericInput("siglg", 
                        "Population Standard Deviation, (\u3C3)", 
                        value = 1),
           numericInput("xlg1", "X1 Value", value = 0),
           numericInput("xlg2", "X2 Value", value = 1),
  ),
  tabPanel("between", 
           numericInput("mub", 
                        "Population Mean, (\u03BC)", 
                        value = 0),
           numericInput("sigb", 
                        "Population Standard Deviation, (\u3C3)", 
                        value = 1),
           numericInput("xb1", "X1 Value", value = 0),
           numericInput("xb2", "X2 Value", value = 1),
  ),
)



# Define UI for application that draws a histogram
ui <- fluidPage(navbarPage("Normal and Standard Normal Distributions",
                           tabPanel("Find Probability",
                                    sidebarPanel(
                                      #numericInput("mu", 
                                      #             "Population Mean, (\u03BC)", 
                                      #             value = 0),
                                      #numericInput("sig", 
                                      #             "Population Standard Deviation, (\u3C3)", 
                                      #             value = 1),
                                      selectInput("type", "Probability", 
                                                  choices = c("Less than X" = "less", 
                                                              "Greater than X" = "greater", 
                                                              "Less than X1 or Greater than X2" = "less_great", 
                                                              "Between X1 and X2" = "between")),
                                      parameter_tabs,
                                      actionButton("ref", "Refresh"),
                                    ),
                                    
                                    # Show a plot of the generated distribution
                                    mainPanel(
                                      h4("Directions"),
                                      p("Use this tab if you know a value (or values) of X for a given normal or standard normal distribution and want to find probability."),
                                      p("First, choose the probability that you want to solve for. Next, input the Population Mean (\u03BC) and the Population Standard Deviation (\u3C3). Then, based on your choice of Probability to solve for, enter the value or values of X. Finally, click 'Refresh' to update the plot and obtain the probability."),
                                      p(tags$b("Note:"), "If Standard Normal (Z) values provided, the population mean (\u03BC) is equal to 0 and the population standard deviation (\u3C3) is equal to 1."),
                                      hr(),
                                      fluidRow(
                                        column(8, plotOutput("distPlot"))
                                      )
                                    )
                           ),
                           tabPanel("Find X",
                                    sidebarPanel(
                                      numericInput("mup", 
                                                   "Population Mean, (\u03BC)", 
                                                   value = 0),
                                      numericInput("sigp", 
                                                   "Population Standard Deviation, (\u3C3)", 
                                                   value = 1),
                                      numericInput("probab", 
                                                   "Probability (%)", 
                                                   value = 20,
                                                   min = 0,
                                                   max = 100),
                                      selectInput("typep", "Probability", 
                                                  choices = c("Less than" = "lessp", 
                                                              "Greater than" = "greaterp", 
                                                              "Symmetric around the Mean" = "symm")),
                                    ),
                                    
                                    # Show a plot of the generated distribution
                                    mainPanel(
                                      h4("Directions"),
                                      p("Use this tab if you know the probability associated with a range for a given normal or standard normal distribution and want to find the X or X values."),
                                      p("First, input the Population Mean (\u03BC) and the Population Standard Deviation (\u3C3). Then enter the known probability value. Then, identify the type of probability that is known: probability less than, probability greater than, or probability symmetric around the mean."),
                                      p(tags$b("Note:"), "If Standard Normal (Z) values provided, the population mean (\u03BC) is equal to 0 and the population standard deviation (\u3C3) is equal to 1."),
                                      hr(),
                                      fluidRow(
                                        column(8, plotOutput("distPlotp"))
                                      )
                                    ))
))

# ==========================================
# 2. SERVER LOGIC (from your server.R)
# ==========================================
# Define server logic required to draw a histogram
server <- function(input, output) {
  
  observeEvent(input$type, {
    updateTabsetPanel(inputId = "params", selected = input$type)
  }) 
  
  plot_info <- eventReactive(input$ref, {
    if(input$type == "less"){
      prob <- round(pnorm(input$xnl, input$mul, input$sigl, lower.tail = TRUE), 4)
      seqy <- seq(min(seq(-4,4,length = 100)*input$sigl + input$mul),input$xnl,length = 100)
      dnormy <- dnorm(seqy, input$mul, input$sigl)
      #shady <- polygon(c(min(seqy), seqy, input$xnl),c(0,dnormy,0),col="steelblue")
      poly1 <- c(min(seqy), seqy, input$xnl)
      poly2 <- c(0,dnormy,0)
      xl <- seq(-4, 4,length = 100) * input$sigl + input$mul
      hxl <- dnorm(xl,input$mul,input$sigl)
      #titley <- title(main = paste0("P(X < ", input$xnl, ") = ", prob))
      titley <- paste0("P(X < ", input$xnl, ") = ", prob)
    } else if(input$type == "greater"){
      prob <- round(pnorm(input$xng, input$mug, input$sigg, lower.tail = FALSE),4)
      seqy <- seq(input$xng, max(seq(-4,4,length = 100)*input$sigg + input$mug), length=100)
      dnormy <- dnorm(seqy, input$mug, input$sigg)
      poly1 <- c(input$xng, seqy, max(seqy))
      poly2 <- c(0,dnormy,0)
      #shady <- polygon(c(input$xng, seqy, max(seqy)),c(0,dnormy,0),col="steelblue")
      xl <- seq(-4, 4,length = 100) * input$sigg + input$mug
      hxl <- dnorm(xl,input$mug,input$sigg)
      #titley <- title(main = paste0("P(X > ", input$xng, ") = ", prob))
      titley <- paste0("P(X > ", input$xng, ") = ", prob)
    } else if(input$type == "less_great"){
      prob <- round(pnorm(input$xlg1, input$mulg, input$siglg, lower.tail = TRUE) + pnorm(input$xlg2, input$mulg, input$siglg, lower.tail = FALSE),4)
      seqy <- list(seq(min(seq(-4,4,length = 100)*input$siglg + input$mulg),input$xlg1,length = 100), seq(input$xlg2, max(seq(-4,4,length = 100)*input$siglg + input$mulg), length=100))
      dnormy <- list(dnorm(seqy[[1]], input$mulg, input$siglg),dnorm(seqy[[2]], input$mulg, input$siglg))
      poly1 <- c(c(min(seqy[[1]]),seqy[[1]],input$xlg1), c(input$xlg2, seqy[[2]], max(seqy[[2]])))
      poly2 <- c(c(0,dnormy[[1]],0), c(0,dnormy[[2]],0))
      #shady <- c(polygon(c(min(seqy[[1]]),seqy[[1]],input$xlg1),c(0,dnormy[[1]],0),col="steelblue"),polygon(c(input$xlg2, seqy[[2]], max(seqy[[2]])),c(0,dnormy[[2]],0),col="steelblue"))
      xl <- seq(-4, 4,length = 100) * input$siglg + input$mulg
      hxl <- dnorm(xl,input$mulg,input$siglg)
      #titley <- title(main = paste0("P(X < ", input$xlg1, " or X > ", input$xlg2, ") = ", prob))
      titley <- paste0("P(X < ", input$xlg1, " or X > ", input$xlg2, ") = ", prob)
    } else {
      prob <- round(pnorm(input$xb2, input$mub, input$sigb, lower.tail = TRUE) - pnorm(input$xb1, input$mub, input$sigb, lower.tail = TRUE),4)
      seqy <- seq(input$xb1,input$xb2,length = 100)
      dnormy <- dnorm(seqy, input$mub, input$sigb)
      poly1 <- c(input$xb1,seqy,input$xb2)
      poly2 <- c(0,dnormy,0)
      #shady <- polygon(c(input$xb1,seqy,input$xb2),c(0,dnormy,0),col="steelblue")
      xl <- seq(-4, 4,length = 100) * input$sigb + input$mub
      hxl <- dnorm(xl,input$mub,input$sigb)
      #titley <- title(main = paste0("P(", input$xb1, " < X < ",input$xb2, ") = ", prob))
      titley <- paste0("P(", input$xb1, " < X < ",input$xb2, ") = ", prob)
    }
    p_info <- list(poly1, poly2, xl, hxl, titley)
  })
  
  
  output$distPlot <- renderPlot({
    if(input$type == "less"){
      validate(need(input$mul != "", "Please input a (\u03BC) value."),
               need(input$sigl != "", "Please input a (\u3C3) value."),
               need(input$xnl != "", "Please input an X value."))
    } else if (input$type == "greater"){
      validate(need(input$mug != "", "Please input a (\u03BC) value."),
               need(input$sigg != "", "Please input a (\u3C3) value."),
               need(input$xng != "", "Please input an X value."))
    } else if (input$type == "less_great"){
      validate(need(input$mulg != "", "Please input a (\u03BC) value."),
               need(input$siglg != "", "Please input a (\u3C3) value."),
               need(input$xlg1 != "", "Please input an X1 value."),
               need(input$xlg2 != "", "Please input an X2 value."),
               need(input$xlg1 < input$xlg2, "Value of X1 must be less than value of X2."))
    } else {
      validate(need(input$mub != "", "Please input a (\u03BC) value."),
               need(input$sigb != "", "Please input a (\u3C3) value."),
               need(input$xb1 != "", "Please input an X1 value."),
               need(input$xb2 != "", "Please input an X2 value."),
               need(input$xb1 < input$xb2, "Value of X1 must be less than value of X2."))
    }
    
    plot(plot_info()[[3]], plot_info()[[4]], type="n", yaxt="n", bty="n", xlab="X Values", ylab="", main="")
    lines(plot_info()[[3]], plot_info()[[4]])
    polygon(plot_info()[[1]], plot_info()[[2]], col="steelblue")
    title(plot_info()[[5]])
    
    #if(input$type == "less"){
    #  xl <- seq(-4, 4,length = 100) * input$sigl + input$mul
    #  hxl <- dnorm(xl,input$mul,input$sigl)
    #  plot(xl, hxl, type="n", yaxt="n", bty="n", xlab="X Values", ylab="",
    #       main="")
    #  lines(xl, hxl)
    #  plot_info()[[1]]
    #  title(main = paste0("P(X < ", input$xnl, ") = ", plot_info()[[2]]))
    #} else if(input$type == "greater"){
    #  xl <- seq(-4, 4,length = 100) * input$sigg + input$mug
    #  hxl <- dnorm(xl,input$mug,input$sigg)
    #  plot(xl, hxl, type="n", yaxt="n", bty="n", xlab="X Values", ylab="",
    #       main="")
    #  lines(xl, hxl)
    #  plot_info()[[1]]
    #  title(main = paste0("P(X > ", input$xng, ") = ", plot_info()[[2]]))
    #} else if(input$type == "less_great"){
    #  xl <- seq(-4, 4,length = 100) * input$siglg + input$mulg
    #  hxl <- dnorm(xl,input$mulg,input$siglg)
    #  plot(xl, hxl, type="n", yaxt="n", bty="n", xlab="X Values", ylab="",
    #       main="")
    #  lines(xl, hxl)
    #  plot_info()[[1]]
    #  title(main = paste0("P(X < ", input$xlg1, " or X > ", input$xlg2, ") = ", plot_info()[[2]]))
    #} else {
    #  xl <- seq(-4, 4,length = 100) * input$sigb + input$mub
    #  hxl <- dnorm(xl,input$mub,input$sigb)
    #  plot(xl, hxl, type="n", yaxt="n", bty="n", xlab="X Values", ylab="",
    #       main="")
    #  lines(xl, hxl)
    #  plot_info()[[1]]
    #  title(main = paste0("P(", input$xb1, " < X < ",input$xb2, ") = ", plot_info()[[2]]))
    #}
  })
  
  
  
  #  seqy <- reactive({
  #    switch(input$type,
  #           less = seq(min(seq(-4,4,length = 100)*input$sig + input$mu),input$xnl,length = 100),
  #           greater = seq(input$xng, max(seq(-4,4,length = 100)*input$sig + input$mu), length=100),
  #           less_great = list(seq(min(seq(-4,4,length = 100)*input$sig + input$mu),input$xlg1,length = 100), seq(input$xlg2, max(seq(-4,4,length = 100)*input$sig + input$mu), length=100)),
  #           between = seq(input$xb1,input$xb2,length = 100)
  #    )
  #  })
  
  #  dnormy <- reactive({
  #    switch(input$type,
  #           less = dnorm(seqy(), input$mu, input$sig),
  #           greater = dnorm(seqy(), input$mu, input$sig),
  #           less_great = list(dnorm(seqy()[[1]], input$mu, input$sig),dnorm(seqy()[[2]], input$mu, input$sig)),
  #           between = dnorm(seqy(), input$mu, input$sig)
  #    )
  #  })
  
  #  shad <- reactive({
  #    switch(input$type,
  #           less = polygon(c(min(seqy()),seqy(),input$xnl),c(0,dnormy(),0),col="steelblue"),
  #           greater = polygon(c(input$xng, seqy(), max(seqy())),c(0,dnormy(),0),col="steelblue"),
  #           less_great = c(polygon(c(min(seqy()[[1]]),seqy()[[1]],input$xlg1),c(0,dnormy()[[1]],0),col="steelblue"),polygon(c(input$xlg2, seqy()[[2]], max(seqy()[[2]])),c(0,dnormy()[[2]],0),col="steelblue")),
  #           between = polygon(c(input$xb1,seqy(),input$xb2),c(0,dnormy(),0),col="steelblue")
  #    )
  #  })
  
  #  titl <- reactive({
  #    switch(input$type,
  #           less = paste0("P(X < ", input$xnl, ") = ", prob()),
  #           greater = paste0("P(X > ", input$xng, ") = ", prob()),
  #           less_great = paste0("P(X < ", input$xlg1, " or X > ", input$xlg2, ") = ", prob()),
  #           between = paste0("P(", input$xb1, " < X < ",input$xb2, ") = ", prob())
  #    )
  #  })
  
  #  suby <- reactive({
  #    switch(input$type,
  #           less = paste0("P(X < ", input$xnl, ") = ", prob()),
  #           greater = paste0("P(X > ", input$xng, ") = ", prob()),
  #           less_great = paste0("P(X < ", input$xlg1, " or X > ", input$xlg2, ") = ", prob()),
  #           between = paste0("P(", input$xb1, " < X < ",input$xb2, ") = ", prob())
  #    )
  #  })
  
  
  #  output$distPlot <- renderPlot({
  #      if(input$type == "less"){
  #        validate(need(input$mu != "", "Please input a (\u03BC) value."),
  #                 need(input$sig != "", "Please input a (\u3C3) value."),
  #          need(input$xnl != "", "Please input an X value."))
  #      } else if (input$type == "greater"){
  #          validate(need(input$mu != "", "Please input a (\u03BC) value."),
  #                   need(input$sig != "", "Please input a (\u3C3) value."),
  #                   need(input$xng != "", "Please input an X value."))
  #      } else if (input$type == "less_great"){
  #          validate(need(input$mu != "", "Please input a (\u03BC) value."),
  #                   need(input$sig != "", "Please input a (\u3C3) value."),
  #                   need(input$xlg1 != "", "Please input an X1 value."),
  #        need(input$xlg2 != "", "Please input an X2 value."))
  #      } else {
  #        validate(need(input$mu != "", "Please input a (\u03BC) value."),
  #                 need(input$sig != "", "Please input a (\u3C3) value."),
  #                 need(input$xb1 != "", "Please input an X1 value."),
  #        need(input$xb2 != "", "Please input an X2 value."))
  #      }
  #    xl <- seq(-4, 4,length = 100) * input$sig + input$mu
  #    hxl <- dnorm(xl,input$mu,input$sig)
  #    plot(xl, hxl, type="n", yaxt="n", bty="n", xlab="X Values", ylab="",
  #         main=titl())
  #    lines(xl, hxl)
  #    shad()})
  
  
  
  zsc  <- reactive({
    switch(input$typep,
           lessp = qnorm(p = input$probab/100, mean = input$mup, sd = input$sigp, lower.tail = TRUE),
           greaterp = qnorm(p = input$probab/100, mean = input$mup, sd = input$sigp, lower.tail = FALSE),
           symm = list(qnorm(p = (1-input$probab/100)/2, mean = input$mup, sd = input$sigp, lower.tail = TRUE), qnorm(p = (1-input$probab/100)/2, mean = input$mup, sd = input$sigp, lower.tail = FALSE))
    )
  })
  
  seqyp <- reactive({
    switch(input$typep,
           lessp = seq(min(seq(-4,4,length = 100)*input$sigp + input$mup),zsc(),length = 100),
           greaterp = seq(zsc(), max(seq(-4,4,length = 100)*input$sigp + input$mup), length=100),
           symm = seq(zsc()[[1]], zsc()[[2]],length = 100)
    )
  })
  
  dnormyp <- reactive({
    switch(input$typep,
           lessp = dnorm(seqyp(), input$mup, input$sigp),
           greaterp = dnorm(seqyp(), input$mup, input$sigp),
           symm = dnorm(seqyp(), input$mup, input$sigp)
    )
  })
  
  shadp <- reactive({
    switch(input$typep,
           lessp = polygon(c(min(seqyp()),seqyp(),zsc()),c(0,dnormyp(),0),col="steelblue"),
           greaterp = polygon(c(zsc(), seqyp(), max(seqyp())),c(0,dnormyp(),0),col="steelblue"),
           symm = polygon(c(zsc()[[1]],seqyp(),zsc()[[2]]),c(0,dnormyp(),0),col="steelblue")
    )
  })
  
  titlp <- reactive({
    switch(input$typep,
           lessp = paste0("P(X < ", round(zsc(), 4), ") = ", input$probab, "%"),
           greaterp = paste0("P(X > ", round(zsc(), 4), ") = ", input$probab, "%"),
           symm = paste0("P(", round(zsc()[[1]], 4), " < X < ", round(zsc()[[2]], 4), ") = ", input$probab, "%")
    )
  })
  
  
  output$distPlotp <- renderPlot({
    validate(need(input$mup != "", "Please input a (\u03BC) value."),
             need(input$sigp != "", "Please input a (\u3C3) value."),
             need(input$probab != "", "Please input a probability (%) value between 0 and 100."))
    
    xlp <- seq(-4, 4,length = 100) * input$sigp + input$mup
    hxlp <- dnorm(xlp,input$mup,input$sigp)
    plot(xlp, hxlp, type="n", yaxt="n", bty="n", xlab="X Values", ylab="",
         main=titlp())
    lines(xlp, hxlp)
    shadp()
  })
  
}

# ==========================================
# 3. RUN THE APPLICATION
# ==========================================
shinyApp(ui = ui, server = server)
