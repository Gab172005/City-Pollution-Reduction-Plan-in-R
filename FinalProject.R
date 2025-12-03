#Kalugdan Luis Gabriel S. 202407527 
#Final Project
library(shiny)
library(bslib)


project_data <- data.frame(
  No = 1:30,
  Name = c(
    "Large Solar Park", "Small Solar Installations", "Wind Farm", "Gas-to-renewables conversion",
    "Boiler Retrofit", "Catalytic Converters for Buses", "Diesel Bus Replacement", "Traffic Signal/Flow Upgrade",
    "Low-Emission Stove Program", "Residential Insulation/Efficiency", "Industrial Scrubbers",
    "Waste Methane Capture System", "Landfill Gas-to-energy", "Reforestation (acre-package)",
    "Urban Tree Canopy Program (street trees)", "Industrial Energy Efficiency Retrofit",
    "Natural Gas Leak Repair", "Agricultural Methane Reduction", "Clean Cookstove & Fuel Switching",
    "Rail Electrification", "EV Charging Infrastructure", "Biochar for soils (per project unit)",
    "Industrial VOC", "Heavy-Duty Truck Retrofit", "Port/Harbor Electrification",
    "Black Carbon reduction", "Wetlands restoration", "Household LPG conversion program",
    "Industrial process change", "Behavioral demand-reduction program"
  ),
  Cost = c(
    4000, 1200, 3800, 3200, 1400, 2600, 5000, 1000, 180, 900, 4200,
    3600, 3400, 220, 300, 1600, 1800, 2800, 450, 6000, 2200, 1400,
    2600, 4200, 4800, 600, 1800, 700, 5000, 400
  ),
  CO2 = c(
    60, 18, 55, 25, 20, 30, 48, 12, 2, 15, 6, 28, 24, 3.5, 4.2, 22,
    10, 8, 3.2, 80, 20, 6, 2, 36, 28, 1.8, 10, 2.5, 3, 9
  ),
  NOx = c(
    0, 0, 0, 1, 0.9, 2.8, 3.2, 0.6, 0.02, 0.1, 0.4, 0.2, 0.15, 0.04,
    0.06, 0.5, 0.05, 0.02, 0.04, 2, 0.3, 0.01, 0.01, 2.2, 1.9, 0.02,
    0.03, 0.03, 0.02, 0.4
  ),
  SO2 = c(
    0, 0, 0, 0.2, 0.4, 0.6, 0.9, 0.1, 0.01, 0.05, 6, 0.1, 0.05, 0.02,
    0.01, 0.3, 0.01, 0.01, 0.02, 0.4, 0.05, 0, 0, 0.6, 0.8, 0.01,
    0.02, 0.01, 0.01, 0.05
  ),
  PM2.5 = c(
    0, 0, 0, 0.1, 0.2, 0.8, 1, 0.4, 0.7, 0.05, 0.4, 0.05, 0.03, 0.01,
    0.03, 0.15, 0.01, 0.02, 0.9, 1.2, 0.1, 0.01, 0, 0.6, 0.7, 0.6,
    0.02, 0.4, 0, 0.05
  ),
  CH4 = c(
    0, 0, 0, 1.5, 0.1, 0, 0, 0.05, 0, 0.02, 0, 8, 6.5, 0.8, 0.6, 0.2,
    4, 7.2, 0.1, 0, 0, 2.5, 0, 0, 0, 0.05, 3.2, 0.05, 0, 0.01
  ),
  VOC = c(
    0, 0, 0, 0.5, 0.05, 0.5, 0.7, 0.2, 0.01, 0.02, 0.1, 0.2, 0.1, 0.03,
    0.02, 0.1, 0.02, 0.05, 0.02, 0.6, 0.05, 0.01, 6.5, 0.3, 0.2, 0.01,
    0.01, 0.02, 0, 0.3
  ),
  CO = c(
    0, 0, 0, 2, 1.2, 5, 6, 3, 1.5, 0.5, 0.6, 0.1, 0.05, 0.1, 0.15, 1,
    0.02, 0.02, 2, 10, 0.5, 0.01, 0.1, 4.2, 3.6, 1, 0.05, 1.2, 0, 2.5
  ),
  NH3 = c(
    0, 0, 0, 0.05, 0.02, 0.01, 0.02, 0.02, 0.03, 0, 0.01, 0, 0, 0.01,
    0.005, 0.01, 0, 0.1, 0.05, 0.02, 0.01, 0.2, 0, 0.01, 0.01, 0.02,
    0.15, 0.03, 0, 0.01
  ),
  BC = c(
    0, 0, 0, 0.01, 0.01, 0.05, 0.08, 0.02, 0.2, 0, 0.01, 0, 0, 0.005,
    0.02, 0.01, 0, 0, 0.25, 0.1, 0.01, 0, 0, 0.04, 0.03, 0.9, 0.02,
    0.1, 0, 0.01
  ),
  N2O = c(
    0, 0, 0, 0.3, 0.05, 0.02, 0.03, 0.01, 0, 0.01, 0, 0.05, 0.03, 0.005,
    0.002, 0.03, 0.01, 0.05, 0, 0.05, 0.01, 0.02, 0, 0.02, 0.02, 0,
    0.04, 0, 1.5, 0.01
  )
)

# pollutant minimum reduction targets
pollutant_targets <- c(
  CO2 = 1000,
  NOx = 35,
  SO2 = 25,
  PM2.5 = 20,
  CH4 = 60,
  VOC = 45,
  CO = 80,
  NH3 = 12,
  BC = 6,
  N2O = 10
)


#function using simplex
Simplex <- function(tableau){
  n = nrow(tableau);
  m = ncol(tableau);
  
  cat(paste("=========================================INITIAL TABLEAU=========================================\n"))
  print(tableau)
  cat(paste("==================================================================================================\n"))
  #traverse through column
  for (i in 1:m){
    #find the pivot column(Most negative)
    PIVOTCOL = which.min(tableau[n, 1:(m-1)])
    if (tableau[n, PIVOTCOL] >= 0) {
      break
    }
    #test ratio
    ratios = matrix()
    for (r in 1:(n-1)) {
      if (tableau[r, PIVOTCOL] > 0) {
        ratios[r] = tableau[r, m] / tableau[r, PIVOTCOL]
      }
    }
    #find the pivot row from the test ratio
    PIVOTROW = which.min(ratios)
    #find the PIvot element
    PE = tableau[PIVOTROW, PIVOTCOL]
    #normalize pivot row
    tableau[PIVOTROW,] = tableau[PIVOTROW,] /PE
    for (j in 1 : n){
      #skip the row if the row is the pivot row
      if (j == PIVOTROW){
        next
      }
      # subtract a multiple of the pivot row
      tableau[j,] = tableau[j,] - tableau[PIVOTROW,] * tableau[j,PIVOTCOL]
    }
    
    #print each iteration
    cat(paste("=========================================ITERATION ",i,"=========================================\n"))
    print(tableau)
    cat(paste("=========================================BASIC SOLUTION ",i,"=========================================\n"))
    
    basicSolution = matrix()
    #Traverse through column
    for(i in 1:(m-1)){
      #Traverse through row
      for(j in 1:n){
        #Get the solution
        if(tableau[j,i] == 1){
          basicSolution[i] = tableau[j,m]
        }
        else if(tableau[j,i] == 0){
          next
        }
        #If there are other values other than 1 and 0 
        else if (tableau[j,i] != 0 || tableau[j,i] != 1)
          basicSolution[i] = 0
      }
    }
    if(i == (m-1)){
      basicSolution = tableau[n, (1:m)]
    }
    print(basicSolution)
  }
  
  #create list
  newList = list(finalTableau = tableau, finalSolution = tableau[n, (1:m)], Z = tableau[n, m], basicSolution = basicSolution)
  return(newList)
}


convertTableau <- function(tableau) {
  
  #transpose the matrix
  transposedMatrix = t(tableau)

  # get dimensions of the transposed matrix
  nrt <- nrow(transposedMatrix)
  nct <- ncol(transposedMatrix)
  
  #create an identity matrix
  identityMatrix = diag(nrt)
  
  #convert the transposed matrix last row to negative
  transposedMatrix[nrt,1:(nct-1)] = transposedMatrix[nrt,1:(nct-1)] * -1
  
  #get the last column 
  rhs = transposedMatrix[,nct]
  
  #copy transposed tableau excluding last column
  finalTableau = transposedMatrix[1:nrt, 1:(nct-1)]
  
  #combine the transposed tableau, identity matrix and last column to get the initial tableau
  finalTableau = cbind(finalTableau,identityMatrix)
  finalTableau = cbind(finalTableau,rhs)
  return(finalTableau)
}



# -----------------------------------------------------------------
# SHINY UI
# -----------------------------------------------------------------
ui <- page_sidebar(
  #use minty theme
  theme = bs_theme(bootswatch = "minty", version = 5),
  
  # css design
  tags$head(
    tags$style(HTML("
      
      #project_list_container {
        max-height: 50vh;
        overflow-y: auto;
        border: 1px solid #ddd;
        padding: 10px;
        border-radius: 5px;
        background-color: white;
      }
      .card-header { font-weight: bold; font-size: 1.2rem; }
      .btn-primary { background-color: #2c3e50; border-color: #2c3e50; }
      .btn-primary:hover { background-color: #1a252f; }
      
      #Iterations, #finalTableau, #Solution {
        max-height: 100%;
        overflow-y: auto;
        border: 1px solid #ddd;
        padding: 10px;
        border-radius: 5px;
        background-color: white;
      }
    "))
  ),
  
  title = "City Pollution Reduction Plan",
  
  sidebar = sidebar(
    width = 300,
    h4("Configuration"),
    p("Select available mitigation projects to optimize cost against pollution targets."),
    
    div(class = "d-flex gap-2 mb-3",
        actionButton("check_all", "Check All", class = "btn-sm btn-outline-secondary"),
        actionButton("reset", "Clear All", class = "btn-sm btn-outline-danger")
    ),
    
    # Scrollable container for the long list
    div(id = "project_list_container",
        checkboxGroupInput("projects", 
                           label = NULL,
                           choices = setNames(project_data$No, project_data$Name),
                           selected = project_data$No 
        )
    ),
    hr(),
    actionButton("solve", "CALCULATE", class = "btn-primary w-100 btn-lg", icon = icon("calculator"))
  ),
  
  #Main panel
  navset_card_underline(
    title = "Optimization Results",
    
    # Tab 1: solution
    nav_panel("Optimal Solution", 
              card_body(
                h5("Cost Breakdown by Mitigation Project"),
                verbatimTextOutput("Solution")
              )
    ),
    
    # Tab 2: Final Tableau
    nav_panel("Final Tableau", 
              card_body(
                h5("Final Simplex Tableau"),
                verbatimTextOutput("finalTableau")
              )
    ),
    
    # Tab 3: Iterations
    nav_panel("Simplex Algorithm Iterations", 
              card_body(
                h5("Simplex Algorithm Step-by-Step"),
                verbatimTextOutput("Iterations")
              )
    )
  )
)

# -----------------------------------------------------------------
# SHINY SERVER
# -----------------------------------------------------------------
server <- function(input, output, session) {
  

  # check All button
  observeEvent(input$check_all, {
    updateCheckboxGroupInput(session, "projects",
                             choices = setNames(project_data$No, project_data$Name),
                             selected = project_data$No)
  })
  
  # Reset (Uncheck All) button
  observeEvent(input$reset, {
    updateCheckboxGroupInput(session, "projects",
                             choices = setNames(project_data$No, project_data$Name),
                             selected = character(0))
  })
  
  

  # Use eventReactive to run code when the button is pressed
  results <- eventReactive(input$solve, {
    
    # get selected project indices
    selected_indices = as.integer(input$projects)
    #get selected project names
    selectedNames = project_data$Name[selected_indices]
    
    #check if the user selected anything
    if (length(selected_indices) == 0) {
      return(list(status = "Error", message = "No projects selected. Please select at least one project."))
    }
    
    # filter the main data based on selected projects
    selected_projects_data = project_data[selected_indices, ]
    
    #set project names
    rownames(selected_projects_data) = selectedNames
    num_vars = nrow(selected_projects_data)
    
   #create matrix for conversion

    # Objective Function
    objfunc = selected_projects_data$Cost
    
    # Constraint Coefficients
    
      # Pollutant constraints
      pollutant_matrix = t(selected_projects_data[, names(pollutant_targets)])
    
      # limit matrix
      limit_matrix = -diag(num_vars) 
    
    # combine them
    A_matrix <- rbind(pollutant_matrix, limit_matrix)
    
    # RHS 
    rhs_pollutants <- pollutant_targets
    rhs_limits <- rep(-20, num_vars) 
    ObjectiveFunction <- c(rhs_pollutants, rhs_limits)
    
    # Construct the matrix for conversion
    constraint_part <- cbind(A_matrix, ObjectiveFunction)
    objective_part <- c(objfunc, 0) #0 is for the bottom-right corner
    tableau = rbind(constraint_part, objective_part)
    
    #convert
    initialTableau = convertTableau(tableau)
    
    #plug in the Initial Tableau to the Simplex Function
    finalList = NULL
    
    options(width = 10000, max.print = 100000)
    
    #capture each log
    log_text = capture.output({
      #enclose finalList in trycatch for error catching
      finalList = tryCatch({
        Simplex(initialTableau)
      }, error = function(e) {
        return(NULL)
      })
    })
    
    #if there is no error
    if(!is.null(finalList$Z)){
    
    #construct final solution
    totalcol= length(finalList$finalSolution) -1  #get the column size of the basic solution
    start = totalcol - num_vars #the starting index of the values
    end   = totalcol - 1 #the end index of the values
    
    Solution = matrix( , nrow = nrow(selected_projects_data), ncol = 2)
    rownames(Solution) = selectedNames
    colnames(Solution) = c("Number of Project Units", "  Cost($)")
    
    #insert Values
    Solution[,1] = finalList$finalSolution[start:end]
    #compute Cost
    for(i in 1: nrow(selected_projects_data)){
        Solution[i,2] = Solution[i,1] * initialTableau[i, ncol(initialTableau)]
    }
    
    return(list(status = "success", 
                log = log_text, 
                finalTableau = finalList$finalTableau, 
                Solution = Solution,
                finalSolution = finalList$finalSolution,
                Z = finalList$Z))
    }
    #infeasible project (error in simplex)
    else {
      return(list(status = "error", message = "The problem is INFEASIBLE or the solver failed.", log = log_text))
    }
  })
  
  
  #Output Renderers
  
  #for iterations
  output$Iterations <- renderPrint({
    res <- results()
    if(res$status == "error"){
      cat(res$message)
      if(!is.null(res$log)) cat("\n\nLog leading to failure:\n", paste(res$log, collapse="\n"))
    } else {
      # print the captured log lines
      cat(paste(res$log, collapse="\n"))
    }
  })
  
  #for final Tableau
  output$finalTableau <- renderPrint({
    res <- results()
    if(res$status == "success"){
      options(max.print = 3000, width = 3000, scipen = 1200)    
      print(res$finalTableau)
      cat(paste("=========================================BASIC SOLUTION=========================================\n"))
      print(res$finalSolution)
    }
    else{
      cat(paste(res$message))
    }
  })
  
  #for cost breakdown
  output$Solution <- renderPrint({
    res <- results()
    if(res$status == "success"){
      options(max.print = 3000, width = 3000, scipen = 1200)
      print(res$Solution)
      cat("-------------------------------------------------------------------------------------\n")
      cat(" TOTAL COST OF OPTIMAL MITIGATION PROJECT : $", sprintf("%.2f", res$Z), "\n")
      cat("-------------------------------------------------------------------------------------\n")
    }
    else{
      cat(paste(res$message))
    }
  })
  
}

# -----------------------------------------------------------------
# Run the application
# -----------------------------------------------------------------
shinyApp(ui = ui, server = server)