library(shiny)
library(rsconnect)
#i: interest rate
#d: discount rate
#df: discount factor
#IBP: insurance benefit payment
#BP: benefit payment
#PP: premium payment

Lxs = c(10000.0000, 9994.0000, 9988.0636, 9982.2006, 9976.3909, 9970.6346, 9964.9313, 9959.2613, 9953.6144, 9947.9807, 
        9942.3402, 9936.6730, 9930.9694, 9925.2094, 9919.3535, 9913.3821, 9907.2655, 9900.9645, 9894.4299, 9887.6126,
        9880.4540, 9872.8954, 9864.8688, 9856.2863, 9847.0510, 9837.0661, 9826.2060, 9814.3359, 9801.3123, 9786.9534, 
        9771.0789, 9753.4714, 9733.8865, 9712.0728, 9687.7149, 9660.5021, 9630.0522, 9595.9715, 9557.8179, 9515.1040,
        9467.2906, 9413.8004, 9354.0040, 9287.2164, 9212.7143, 9129.7170, 9037.3973, 8934.8771, 8821.2612, 8695.6199, 
        8557.0118, 8404.4916, 8237.1329, 8054.0544, 7854.4508, 7637.6208, 7403.0084, 7150.2401, 6879.1673, 6589.9258, 
        6282.9803, 5959.1680, 5619.7577, 5266.4064, 4901.4789, 4527.4960, 4147.6708, 3765.5998, 3385.2479, 3010.8395, 
        2646.7416, 2297.2976, 1966.6499, 1658.5545, 1376.1906, 1121.9889, 897.5052, 703.3242, 539.0643, 403.4023, 294.2061,
        208.7060, 143.7120, 95.8476, 61.7733, 38.3796, 22.9284, 13.1359, 7.1968, 3.7596, 1.8669, 0.8784, 0.3903, 0.1632, 
        0.0640, 0.0234, 0.0080, 0.0025, 0.0007, 0.0002, 0.0000, 0.0000, 0.0000, 0.000)

group_y <- "y"
group_x <- "x"
IBP_END_DEATH <- "end_year_death"
IBP_IMMEDIATE_DEATH <- "immediately_death"
BP_PURE_ENDOWNMENT <- "pure_endownment"
BP_ENDOWNMENT_ASSURANCE <- "endownment_assurance"
BP_TERM_ASSURANCE <- "term_assurance"
PP_SINGLE <- "single"
PP_LEVEL <- "level"

#WHOLE LIFE ANNUITY
whole_life_annuity= function(age, i, group) {
  if (group == group_y) {
    survival_probabilities = (Lxs[-(1:(age - 19))] / Lxs[age - 19])
  } else if (group == group_x) {
    survival_probabilities = (Lxs[-(1:(age - 15))] / Lxs[age - 15])
  }
  df = (1 / (1 + i)) ^ (1:length(survival_probabilities))
  output = sum(c(1, (df * survival_probabilities)))
  return(output)
}

#TEMPORARY ANNUITY
tem_annuity= function(age, i, term, group) {
  if (group == group_y) {
    df = Lxs[age - 19 + term] / Lxs[age - 19] * ((1 / (1 + i)) ^ term)
  } else if (group == group_x) {
    df = Lxs[age - 15 + term] / Lxs[age - 15] * ((1 / (1 + i)) ^ term)
  }
  output = whole_life_annuity(age, i, group) - df * whole_life_annuity(age + term, i, group)
  return(output)
}

#WHOLE LIFE ASSURANCE
whole_life_assurance = function(S, age, i, group) {
  d = i / (1 + i)
  output = S*(1 - d * (whole_life_annuity(age, i, group)))
  return(output)
}

#TERM ASSURANCE
term_assurance = function(S, age, i, term, ibp, group) {
  if (group == group_y) {
    df = Lxs[age - 19 + term] / Lxs[age - 19] * ((1 / (1 + i)) ^ term)
  } else if (group == group_x) {
    df = Lxs[age - 15 + term] / Lxs[age - 15] * ((1 / (1 + i)) ^ term)
  }
  if (ibp == IBP_END_DEATH) {
    output = S*(whole_life_assurance(1, age, i, group) - df * whole_life_assurance(1, age + term, i, group))
    return(output)
  } else if (ibp == IBP_IMMEDIATE_DEATH) {
    output = S*(((1 + i) ^ 0.5) * (whole_life_assurance(1, age, i, group) - df * whole_life_assurance(1, age + term, i, group)))
    return(output)
  }
}

#PURE ENDOWNMENT
pure_endownment = function(S, age, i, term, group) {
  df = (1 / (1 + i)) ^ term
  if (group == group_y) {
    survival_probabilities = (Lxs[age - 19 + term] / Lxs[age - 19])
  } else if (group == group_x) {
    survival_probabilities = (Lxs[age - 15 + term] / Lxs[age - 15])
  }
  output = S*df*survival_probabilities
  return(output)
}

#ENDOWNMENT ASSURANCE
endownment_assurance = function(S, age, i, term, ibp, group) {
  if (ibp == IBP_END_DEATH) {
    output = S*(term_assurance(1, age, i, term, IBP_END_DEATH, group) + pure_endownment(1, age, i, term, group))
    return(output)
  } else if (ibp == IBP_IMMEDIATE_DEATH) {
    output = S*(term_assurance(1, age, i, term, IBP_IMMEDIATE_DEATH, group) + pure_endownment(1, age, i, term, group))
    return(output)
  }
}

#PREMIUM
premium = function(S, option, IB, age, i , term, ibp, group) {
  if (option == PP_SINGLE) {
    if (IB == BP_PURE_ENDOWNMENT) {
      output = pure_endownment(S, age, i, term, group)
      return(output)
    } else if (IB == BP_TERM_ASSURANCE) {
      output = term_assurance(S, age, i, term, ibp, group)
      return(output)
    } else if (IB == BP_ENDOWNMENT_ASSURANCE) {
      output = endownment_assurance(S, age, i, term, ibp, group)
      return(output)
    }
  } else if (option == PP_LEVEL) {
    if (IB == BP_PURE_ENDOWNMENT) {
      output = (pure_endownment(S, age, i, term, group)) / tem_annuity(age, i, term, group)
      return(output)
    } else if (IB == BP_TERM_ASSURANCE) {
      output = (term_assurance(S, age, i, term, ibp, group)) / tem_annuity(age, i, term, group)
      return(output)
    } else if (IB == BP_ENDOWNMENT_ASSURANCE) {
      output = (endownment_assurance(S, age, i, term, ibp, group)) / tem_annuity(age, i, term, group)
      return(output)
    }
  }
}


#PREMIUM INCLUDE EXPENSE
premium_include_expense = function(S, option, IB, age, i , term, ibp, group, initial_expense, claim_expense, premium_expense) {
  if (option == PP_SINGLE) {
    if (IB == BP_PURE_ENDOWNMENT) {
      output = ((1 + claim_expense/100)*pure_endownment(S, age, i, term, group))/(1 - initial_expense/100)
      return(output)
    } else if (IB == BP_TERM_ASSURANCE) {
      output = ((1 + claim_expense/100)*term_assurance(S, age, i, term, ibp, group))/(1 - initial_expense/100)
      return(output)
    } else if (IB == BP_ENDOWNMENT_ASSURANCE) {
      output = ((1 + claim_expense/100)*endownment_assurance(S, age, i, term, ibp, group))/(1 - initial_expense/100)
      return(output)
    }
  } else if (option == PP_LEVEL) {
    if (IB == BP_PURE_ENDOWNMENT) {
      output = ((1 + claim_expense/100)*(pure_endownment(S, age, i, term, group))) / 
        (tem_annuity(age, i, term, group) - (initial_expense/100) - (premium_expense/100)*(tem_annuity(age, i, term, group) - 1))
      return(output)
    } else if (IB == BP_TERM_ASSURANCE) {
      output = ((1 + claim_expense/100)*(term_assurance(S, age, i, term, ibp, group))) / 
        (tem_annuity(age, i, term, group) - (initial_expense/100) - (premium_expense/100)*(tem_annuity(age, i, term, group) - 1))
      return(output)
    } else if (IB == BP_ENDOWNMENT_ASSURANCE) {
      output = ((1 + claim_expense/100)*(endownment_assurance(S, age, i, term, ibp, group))) / 
        (tem_annuity(age, i, term, group) - (initial_expense/100) - (premium_expense/100)*(tem_annuity(age, i, term, group) - 1))
      return(output)
    }
  }
}  


#FEATURE PRODUCT
#ASSURANCE - FUNERAL FOR THE BELOVED (y)
joint_life_assurance = function(S, agex, agey, ir) {
  death_prob_y = c() 
  if (agey == 20) {
    L2 <- Lxs[-(1:(agey - 19))]
    for(i in 1 : length(L2)) {
      death_prob_y[i] = (Lxs[i] - Lxs[i + 1])/Lxs[1]
    }
  } else {
    L1 <- Lxs[-(1:(agey - 20))]
    L2 <- Lxs[-(1:(agey - 19))]
    for (i in 1:length(L2)) {
      death_prob_y[i] = (L1[i] - L1[i + 1]) / L1[1]
    }
  }
  survival_prob_x = (Lxs[-(1:(agex - 15))] / Lxs[agex - 15])
  if (length(death_prob_y) > length(survival_prob_x)) {
    number_of_0 = length(death_prob_y) - length(survival_prob_x)
    survival_prob_x_new = c(survival_prob_x, rep(0, number_of_0))
    prob_pay_benefit = death_prob_y*survival_prob_x_new
  } else if (length(death_prob_y) < length(survival_prob_x)) {
    number_of_0 = length(survival_prob_x) - length(death_prob_y)
    death_prob_y_new = c(death_prob_y, rep(0, number_of_0))
    prob_pay_benefit = death_prob_y_new*survival_prob_x
  }
  df = 1/(1+ir)
  all_df = df^(1:length(prob_pay_benefit))
  output = S*sum(c(all_df*prob_pay_benefit))
  return(output)
}

joint_life_assurance_expense = function(S, agex, agey, ir, initial_expense, claim_expense) {
  output = ((1 + claim_expense)*joint_life_assurance(S, agex, agey, ir))/(1 - initial_expense)
  return(output)
}

#ui PART
single_product_ui <- fluidPage(
  titlePanel("Insurance Benefit Valuation Tool"),
  sidebarLayout(
    sidebarPanel(
      sliderInput(
        inputId = "Age",
        label = h4("Age"),
        min = 20,
        max = 90,
        value = 50
      ),
      radioButtons(
        inputId = "group",
        label = h4("Group"),
        choices = c("Group X" = group_x,
                    "Group Y" = group_y),
        selected = group_x
      ),
      
      numericInput(
        inputId = "IR",
        label = h4("Interest Rate (in %)"),
        min = 0,
        max = 15,
        value = 4
      ),
      
      radioButtons(
        inputId = "PP",
        label = h4("Premium Payment"),
        choices = c("Single" = PP_SINGLE,
                    "Level" = PP_LEVEL),
        selected = PP_SINGLE
      ),
      
      radioButtons(
        inputId = "IB",
        label = h4("Insurance Benefit"),
        choices = c(
          "Pure Endownment" = BP_PURE_ENDOWNMENT,
          "Term Assurance" = BP_TERM_ASSURANCE,
          "Endownment Assurance" = BP_ENDOWNMENT_ASSURANCE
        ),
        selected = BP_PURE_ENDOWNMENT
      ),
      
      radioButtons(
        inputId = "IBP",
        label = h4("Insurance Benefit Payment"),
        choices = c(
          "End of Year of Death" = IBP_END_DEATH,
          "Immediately on Death" = IBP_IMMEDIATE_DEATH
        ),
        selected = IBP_END_DEATH
      ),
      
      numericInput(
        inputId = "AS",
        h4("Assured Sum"),
        min = 0,
        max = 1000000,
        value = 1
      ),
      
      numericInput(
        "BT",
        h4("Benefit Term"),
        min = 0,
        max = 100,
        value = 10
      ),
    
      numericInput(
        inputId = "IE",
        label = h4("Initial Expense (in % of Gross Premium)"),
        min = 0,
        max = 100,
        value = 1
      ),
      
      numericInput(
        inputId = "CE",
        label = h4("Claim Expense (in %)"),
        min = 0,
        max = 100,
        value = 1
      ),
      
      numericInput(
        inputId = "PE",
        label = h4("Premium Expense (in %)"),
        min = 0,
        max = 100,
        value = 1
      ),
    ),
    mainPanel(
      plotOutput(outputId = "distPlot_1"),
      plotOutput(outputId = "distPlot_2")
    )
  )
)

feature_product_ui <- fluidPage(
  titlePanel("Insurance Benefit Valuation Tool"),
  sidebarLayout(
    sidebarPanel(
      sliderInput(
        inputId = "age_x",
        label = h4("Age x"),
        min = 20,
        max = 90,
        value = 50
      ),
      sliderInput(
        inputId = "age_y",
        label = h4("Age y"),
        min = 20,
        max = 90,
        value = 50
      ),
      
      numericInput(
        inputId = "ASJ",
        h4("Assured Sum"),
        min = 0,
        max = 1000000,
        value = 1
      ),
      
      numericInput(
        inputId = "IRJ",
        label = h4("Interest Rate (in %)"),
        min = 0,
        max = 15,
        value = 4
      ),
      
      numericInput(
        inputId = "IEJ",
        label = h4("Initial Expense (in % of Gross Premium)"),
        min = 0,
        max = 100,
        value = 1
      ),
      
      numericInput(
        inputId = "CEJ",
        label = h4("Claim Expense (in %)"),
        min = 0,
        max = 100,
        value = 1
      ),
    ),
    mainPanel(plotOutput(outputId = "distPlot_3"))
  )
)
ui <- tabsetPanel(
  tabPanel("Single Life", single_product_ui),
  tabPanel("Funeral for the beloved (y)", feature_product_ui)
)
#server PART
server <- function(input, output) {
  obsB <- observe({
    Vector_1 = c()
    IRs = seq(0, 0.15, length.out = 100)
    for (i in 1:length(IRs)) {
      Vector_1[i] = premium(input$AS, input$PP, input$IB, input$Age, IRs[i], input$BT, input$IBP, input$group)
    }
    vector_premium = premium(input$AS, input$PP, input$IB, input$Age, (input$IR)/100, input$BT, input$IBP, input$group)
    output$distPlot_1 <- renderPlot({
      plot(IRs, Vector_1, type = "l", lwd = 3, xlab = "Effective interest rate", ylab = "Premium",
           main = paste("Premium: ", round(vector_premium, digits = 5))) + 
      abline(h = vector_premium, col = "red", lwd = 3, lty = 2) +
      grid(nx = NULL, ny = NULL, col = "grey", lwd = 2)
    })
    Vector_2 = c()
    for (i in 1:length(IRs)) {
      Vector_2[i] = premium_include_expense(input$AS, input$PP, input$IB, input$Age, IRs[i], input$BT, input$IBP, input$group,
                                            input$IE, input$CE, input$PE)
    }
    vector_premium_include_expense = premium_include_expense(input$AS, input$PP, input$IB, input$Age, (input$IR)/100, 
                                                             input$BT, input$IBP, input$group, input$IE, input$CE, input$PE)
    output$distPlot_2 <- renderPlot({
      plot(IRs, Vector_2, type = "l", lwd = 3, xlab = "Effective interest rate", ylab = "Premium include expense",
           main = paste("Premium include expense: ", round(vector_premium_include_expense, digits = 5))) + 
      abline(h = vector_premium_include_expense, col = "red", lwd = 3, lty = 2) + 
      grid(nx = NULL, ny = NULL, col = "grey", lwd = 2)
    }) 
    Vector_3 = c()
    for (i in 1:length(IRs)) {
      Vector_3[i] = joint_life_assurance_expense(input$ASJ, input$age_x, input$age_y, IRs[i], input$IEJ/100, input$CEJ/100)
    }
    vector_joint = joint_life_assurance_expense(input$ASJ, input$age_x, input$age_y, input$IRJ/100, input$IEJ/100, input$CEJ/100)
    output$distPlot_3 <- renderPlot({
      plot(IRs, Vector_3, type = "l", lwd = 3, xlab = "Effective interest rate", ylab = "Premium include expense",
           main = paste("Premium include expense: ", round(vector_joint, digits = 5))) + 
        abline(h = vector_joint, col = "red", lwd = 3, lty = 2) +
        grid(nx = NULL, ny = NULL, col = "grey", lwd = 2)
    })
  })
}


#COMBINATION PART
shinyApp(ui, server)






