library(shiny)
library(ggplot2)
library(pwr)
library(radiant.basics)
library(binGroup)


ui <- navbarPage("Sample Size Calculation Tool",
                 theme = shinythemes::shinytheme("superhero"),
                 
                 ############################
                 #########TAB 3##############
                 ############################
                 tabPanel("Hypothesis Testing",
                          sidebarLayout(
                            
                            # Define the sidebar with one input
                            sidebarPanel(
                              sliderInput("target3", "Target of Agreement %", value=90,max=100,step=0.1,min=50 ),
                              helpText('Clinical criteria set to be reached.'),
                              
                              sliderInput("exp3", "Expectation of Agreement %", value=95,max=100,step=0.1,min=51),
                              helpText("estimation/guess about the true proportion of agrrement. It should be higher than Target of Agreement"),
                              sliderInput("ss3", "Number of People Tested", value=500,max=1000,step=1,min=50),
                              helpText('Either positively or negatively tested people with a comparator test'),
                              radioButtons("null3", "Null Hypothesis type:",
                                           c("Two Sided" = "two.sided",
                                             "Greater" = "greater")),
                              sliderInput("alpha3",
                                           "Confidence Level(%):",
                                           value = 95,
                                           min = 80,
                                           max = 100,
                                          step=0.3),
                              selectInput("select_method3", label = "Confidence Interval",
                                          choices = list("Wilson" = 'Score', "Wald" = 'Wald',"Clopper-Pearson"='CP'),
                                          selected = 'Score')
                              
                            ),
                            
                            
                            # Create a spot for the barplot
                            mainPanel(
                              
                              # Output: Tabset w/ plot, summary, and table ----
                              tabsetPanel(type = "tabs",
                                          tabPanel("Plot", plotOutput("plot3"),textOutput('interp8')),
                                          tabPanel('Confidence Interval',verbatimTextOutput("CI"),textOutput('interp3'))
                                          
                              )
                              
                            )
                          )
                 ),
                 
                 
                 
                 
                 tabPanel(
                   ########################
                   ########TAB 1###########
                   ########################
                   "Power Calculation",
                   sidebarLayout(
                     
                     # Sidebar panel for inputs ----
                     sidebarPanel(
                       selectInput("select_method", label = "Calculation Method", 
                                   choices = list("Exact" = 1, "Approximate" = 2), 
                                   selected = 1),
                       helpText("Select the method to calculate the power"),
                       sliderInput("target", "Target of Agreement %", value =80,max=100,step=0.1,min=50 ),
                       sliderInput("estimation", "Estimation on Agreement %", value =85,max=100,step=0.1,min=51 ),
                       conditionalPanel(
                         "input.target >=input.estimation",
                         "Estimation of Agreement must be higher than the Target!!!"
                       ),
                       
                       sliderInput("slider2", label = ("Sample Size Range"), min =10, 
                                   max = 10000, value = c(100, 800),step=100),
                       # Input: Select the random distribution type ----
                       numericInput('sample',label=('Actual Sample Size'),value=300,min=10,max=2000),
                       conditionalPanel(
                         "input.sample >input.slider2[1] ",
                         "Actual Sample Size must be a value between the range!!!"
                       ),
                       conditionalPanel(
                         "input.sample <input.slider2[2] ",
                         "Actual Sample Size must be a value between the range!!!"
                       ),
                       sliderInput("alpha",
                                   "Confidence Level(%):",
                                   value = 95,
                                   min = 80,
                                   max = 100),
                       radioButtons("null", "Null Hypothesis type:",
                                    c("Greater" = "greater","Two Sided" = "two.sided"
                                    ))
                     ),
                     
                     # Main panel for displaying outputs ----
                     mainPanel(
                       
                       # Output: Tabset w/ plot, summary, and table ----
                       tabsetPanel(type = "tabs",
                                   tabPanel("Plot", plotOutput("plot",click="plot_click"),verbatimTextOutput("info"),textOutput('summary')),
                       )
                     )
                     
                   )),
                 
                 
                 ############################
                 #############TAB 2###########
                 #############################
                 
                 tabPanel("Sample Size Calculation",
                          
                          sidebarLayout(
                            
                            # Sidebar panel for inputs ----
                            sidebarPanel(
                              #   selectInput("select_method", label = "Calculation Method",
                              # choices = list("Arcsine Transformation" = 1, "Exact Calculation" = 2),
                              # selected = 1),
                              # br(),
                              numericInput("target2", "Target of Agreement %", value =85,max=100,step=0.5,min=60 ),
                              numericInput('est2','Estimation of Agreement %',value=90,max=99,min=60),
                              conditionalPanel(
                                "input.target2 >=input.est2",
                                "Estimation of Agreement must be higher than the Target!!!"
                              ),
                              # Input: Select the random distribution type ----
                              # br() element to introduce extra vertical spacing ----
                              # Input: Slider for the number of observations to generate ----
                              numericInput("power2",
                                           "Power (%):",
                                           value = 80,
                                           min = 60,
                                           max = 100),
                              helpText("Probability of correctly rejecting the null"),
                              # br() element to introduce extra vertical spacing ----
                              numericInput("alpha2",
                                           "Confidence Level(%):",
                                           value = 95,
                                           min = 80,
                                           max = 100),
                              
                              radioButtons("null2", "Null Hypothesis type:",
                                           c("Greater" = "greater",
                                             "Two Sided" = "two.sided")),
                              numericInput('ylim','Maximum sample size',min=1000,max=100000,step=500,value=10000),
                              helpText("The maximum value that will be displayed in the plot")
                            ),
                            
                            
                            # Main panel for displaying outputs ----
                            mainPanel(
                              
                              # Output: Tabset w/ plot, summary, and table ----
                              tabsetPanel(type = "tabs",
                                          tabPanel("Plot", plotOutput("plot2")),
                                          tabPanel("Explanation", verbatimTextOutput("summary2"),
                                                   textOutput('interp2'))
                              )
                              
                            )
                          )
                 )
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 ,
                
                 
                 
                 
                 ########################
                 ######TAB 4 ############
                 ########################
                 
                 tabPanel("Final Sample Size",
                          
                          # Sidebar layout with input and output definitions ----
                          sidebarLayout(
                            
                            # Sidebar panel for inputs ----
                            sidebarPanel(
                              # Input: Select the random distribution type ----
                              
                              radioButtons("measure", "Select Meausere %", choices=c('Sensitivity'='Sensitivity','Specificity'='Specificity'), selected='Sensitivity'),
                              helpText('Select the measure that will drive the final sample size'),
                              numericInput("number", "Sample Size",value=300,max=100000,step=1,min=0),
                              helpText('Provide the sample size calculated  in the previous tab based on the desired meauserment'),
                              
                              numericInput("pre", "Estimated Prevalence %", value =5,max=90,step=1,min=1) 
                            ),
                            
                            # Main panel for displaying outputs ----
                            mainPanel(
                              
                              # Output: Tabset w/ plot, summary, and table ----
                              tabsetPanel(type = "tabs",
                                          tabPanel("Plot", plotOutput("plot4"),textOutput('interp4'))
                                          
                              )
                              
                            )
                          )
                 )
                 
)

server <- function(input, output) {
  
  ###################################
  ## TAB 1###########################
  ##################################
  
  dataa<-reactive({
    ss<-input$slider2[1]:input$slider2[2]
    if (input$null=='greater'){
      critical = qbinom(input$alpha/100, ss, input$target/100)
      beta<-pbinom(critical, ss,input$estimation/100)
      power=1-beta
    }
    else {
      critical = qbinom(1-(1-input$alpha/100)/2, ss, input$target/100)
      beta =pbinom(critical-1,ss,input$estimation/100)-pbinom(ss-critical,ss,input$estimation/100) 
      power=1-beta
    }
    
    data.frame(ss,power)
  }
  )
  
  
  result<-reactive({
    pwr.p.test(h =ES.h(input$estimation/100,input$target/100), sig.level=1-input$alpha/100, power =0.8, alternative=input$null)
    
  })
  
  
  result2<-reactive({
    ss<-input$slider2[1]:input$slider2[2]
    if (input$null=='greater'){
      critical = qbinom(input$alpha/100, ss, input$target/100)
      beta<-pbinom(critical, ss,input$estimation/100)
      power=1-beta
    }
    else {
      critical = qbinom(1-(1-input$alpha/100)/2, ss, input$target/100)
      beta =pbinom(critical-1,ss,input$estimation/100)-pbinom(ss-critical,ss,input$estimation/100) 
      power=1-beta
    }
    power[which(ss==input$sample)]
  })
  
  result3<-reactive({
    ss10<-10:200000
    if (input$null=='greater'){
      critical10 = qbinom(input$alpha/100, ss10, input$target/100)
      beta10<-pbinom(critical10, ss10,input$estimation/100)
      power10=1-beta10
    }
    else {
      critical10 = qbinom(1-(1-input$alpha/100)/2, ss10, input$target/100)
      beta10 =pbinom(critical10-1,ss10,input$estimation/100)-pbinom(ss10-critical10,ss10,input$estimation/100) 
      power10=1-beta10
      
    }
    
    ##find optimal n
    #first find all values that are higher than 0.8
    a<-which(power10>0.8)
    j=1
    m=length(power10)
    ## make sure that all values after that are higher
    while(all(power10[a[j]:m]>0.8)=='FALSE'){
      j=j+1
    }
    
    
    optimal_n<-ss10[a[j]]
    return(optimal_n)
    
    
  })
  
  output$plot <- renderPlot({if (input$target<input$estimation){
    
    if(input$select_method==2){
      plot(result())
    }
    else{
      ggplot(dataa(),aes(x=ss,y=power))+geom_line(col='blue')+geom_hline(yintercept = 0.8,linetype='dotted',col='red')+geom_vline(xintercept=input$sample,linetype='dotted',col='purple') +labs(x= "Sample Size",
                                                                                                                                                                                                y= "Power",
                                                                                                                                                                                                title=paste0("Exact Power Calculation"), subtitle=paste0('tails: ',input$null))
    }
  }
    else{
      ggplot() +
        annotate("text", x = 10,  y = 10,
                 size = 5,
                 label = "Ouups! Make sure expectation is higher than target please ;)",col='red') + theme_void()
    }
    
    
  },res=100)
  
  
  
  output$summary<-renderText(
    { if(input$sample>=input$slider2[1] &input$sample<=input$slider2[2]){
      if (input$target<input$estimation){
    if(input$select_method==2){
      paste0("Interpretation: Given a sample size of ",input$sample, " we will correctly reject the hypothesis that the agreement is ",input$target, "% with a power of ",round(pwr.p.test(h =ES.h(input$estimation/100,input$target/100), sig.level=1-input$alpha/100, n=input$sample, alternative=input$null)$power,3)," assuming that the true agreement is ",input$estimation, " %")
    }
    else{
      paste0("Interpretation: Given a sample size of ",input$sample, " we will correctly reject the hypothesis that the agreement is ",input$target, "% with a power of ",round(result2(),3)," assuming that the true agreement is ",input$estimation, " %. The minimum number of individuals needed to ensure that the power does not fall the 80% catuff is ",result3() )
      
    }
      }
    }
    
  })
  
  
  output$info <- renderPrint({
    req(input$plot_click)
    x <- ceiling(input$plot_click$x)
    y <- round(input$plot_click$y, 3)*100
    cat("For a sample size ",x," we have a power of ",y, "%",sep="")
  })
  
  
  ############################
  ######TAB 2###################
  #############################
  
  dataaa <- reactive({ 
    p_hat<-seq(input$target2+0.5,99.9,by=0.5)
    p_0<-input$target2/100
    ss<-c()
    effect_size<- abs(2*asin(sqrt(p_hat/100))-2*asin(sqrt(p_0)))
    for (i in 1:length(effect_size)){
      x<-pwr.p.test(h =effect_size[i] , sig.level =1-input$alpha2/100, power =input$power2/100, alternative=input$null2)
      ss[i]<-x$n
    }
    
    data.frame(p_hat,ss,input$est2)
    
    
  })
  
  
  output$plot2 <- renderPlot( if(input$target2<input$est2){
    ggplot(dataaa(),aes(x=p_hat,y=ss))+geom_point(colour='red',size=1.5)+geom_line(col='blue')+geom_vline(xintercept=input$est2,col='purple',linetype='dotted',size=1.2)+geom_hline(yintercept=pwr.p.test(h=ES.h(input$est2/100,input$target2/100), sig.level =1-input$alpha2/100, power=input$power2/100, alternative=input$null2)$n,col='green',linetype='dotted',size=1.2)+labs(x= "Expected Agreement (%)",
                                                                                                                                                                                                                                                                                                                                                                                   y= "Required Sample Size",
                                                                                                                                                                                                                                                                                                                                                                                   title="Sample Size Calculator")+ylim(0,input$ylim)
  }
  
  else{
    ggplot() +
      annotate("text", x = 10,  y = 10,
               size = 5,
               label = "Ouups! Make sure expectation is higher than target please ;)",col='red') + theme_void()
  }
  
  ,width =600,res =100
  
  )
  
  output$summary2<-renderPrint({if(input$target2>=input$est2){
    paste("Come on! You thought that would work? Change the variables and try again")}
    
    
    else{ pwr.p.test(h=ES.h(input$est2/100,input$target2/100), sig.level =1-input$alpha2/100, power =input$power2/100, alternative=input$null2)
    }
  })
  
  output$interp2<-renderText({ if(input$target2<input$est2){
    paste0('Interpretation:
We will need ',  ceiling(pwr.p.test(h=ES.h(input$est2/100,input$target2/100), sig.level =1-input$alpha2/100, power =input$power2/100, alternative=input$null2)$n), ' individuals to have a ',input$power2, '% chance of correctly rejecting the null (that the agreement is ',input$target2,') % with ', input$alpha2, '% confidence assuming that the true agreement is ',input$est2,'%')}
    
  })
  
  
  
  ###########################
  #####TAB 3##############
  ########################
  
  decision<-reactive({
    succ=floor(input$exp3/100*input$ss3)
    x=binCI(n=input$ss3, y=succ, conf.level =input$alpha3/100, alternative = input$null3,
            method = input$select_method3)
    if (input$target3/100<x$conf.int[2] & input$target3/100>x$conf.int[1]){
      paste('do not reject the null')
    }
    else{
      paste('reject the null')
    }
    
  })
  
  critical<-reactive({
    
    list(qbinom(input$alpha3/100,input$ss3,input$target3/100))
  })
  
  # results<-reactive({
  #   if (input$null3=='greater'){
  #   prob_binom(n=input$ss3,p=input$target3/100,pub=input$alpha3/100)
  #   }
  #   else{
  #     prob_binom(n=input$ss3,p=input$target3/100,plb=(1-input$alpha3/100)/2,
  #                pub=1-(1-input$alpha/100)/2)
  #   }
  #   
  # }
  #   )
  
  data_binom<-reactive({
    x<-0:input$ss3
    p<-input$target3/100
    y<-100 * dbinom(x, input$ss3, p)
    if (input$null3=='greater'){
      z<-qbinom(input$alpha3/100,input$ss3,p)
      Decision<-ifelse(x<=z,'Accept','Reject')
    }
    else{
      z1<-qbinom(1-(1-(input$alpha3/100))/2,input$ss3,p)
      z2<-qbinom((1-input$alpha3/100)/2,input$ss3,p)
      Decision<-ifelse(x<=z1 &x>=z2,'Accept','Reject')
    }
    data.frame(x,y,Decision) %>% filter(y>0.1)
  })
  
  
  reject_region<-reactive({
    x<-0:input$ss3
    p<-input$target3/100
    y<-100 * dbinom(x, input$ss3, p)
    if (input$null3=='greater'){
      z<-qbinom(input$alpha3/100,input$ss3,p)
     paste0('higher than ',z)
    }
    else{
      z1<-qbinom(1-(1-(input$alpha3/100))/2,input$ss3,p)
      z2<-qbinom((1-input$alpha3/100)/2,input$ss3,p)
      paste0('lower than ',z2,' or higher than ', z1)
     
    }
    
  })
  
  
  # Reactive expression to generate the requested distribution ----
  # This is called whenever the inputs change. The output functions
  # defined below then use the value computed from this expression
  output$plot3 <- renderPlot({
    
    ggplot(data=data_binom(),aes(fill=Decision,x=x,y=y))+
      geom_col(width=0.25)+
      labs(x= "Number of agreed results",
           y= "Probability of event ocurring (%)",
           title=paste0("Binomial Distribution: p=",input$target3/100, " n=",input$ss3))+scale_fill_manual(values=c('green','red'))
    
  })
  output$interp8<-renderText({
    paste0('We will fail to reject the hypothesis stating that the true proportion of aggrement is ',input$target3, '% if the number of agreed results lies in the green region.
           Therefore, we will reject that hypothesis if the number of agreed (negative/positive) results is ', reject_region())
  })
  
  
  output$CI<-renderPrint({
    binCI(n=input$ss3, y=as.integer(floor(input$exp3/100*input$ss3)), conf.level =input$alpha3/100, alternative = input$null3,
          method = input$select_method3)
    
  })
  
  
  output$interp3<-renderText({
    
    paste0('Interpretation: We will fail to reject the null hypothesis when the target of agreement is included in the confidence interval. Here, we can say with  ', input$alpha3, "% confidece that we ",decision(),' since the target of agreement is : ',input$target3/100)
    
  })
  
  
  
  ############
  #####TAB 4######
  #################
  data4 <- reactive({
    
    pr<-seq(0.05,0.7,by=0.01)
    
    if(input$measure=='Sensitivity'){
      N<-input$number/pr
    }
    else{
      N<-input$number/(1-pr)
    }
    data.frame(pr,N)
    
  })
  
  
  
  SS<-reactive({
    pr<-input$pre/100 
    if(input$measure=='Sensitivity'){
      N<-input$number/pr
    }
    else{
      N<-input$number/(1-pr)
    }
    ceiling(N)
  }
  )
  
  output$plot4 <- renderPlot({
    ggplot(data4(),aes(x=pr,y=N))+geom_point(colour='red',size=1.5)+geom_line(col='blue')+labs(x= "Expected Prevalence",
                                                                                               y= "Required Sample Size",
                                                                                               title=paste0("Sample Size Calculator driven by ",input$measure))+geom_vline(xintercept=input$pre/100,col='purple',linetype='dotted')
    
  },res=100)
  
  output$interp4<-renderText({
    paste0('Interpretation:
We will need minimum ', SS(), ' individuals to be tested if we assume that the condition of interest in the target population is estimated to be around ',input$pre,'%')
    
  })
  
  
  
}



shinyApp(ui,server)