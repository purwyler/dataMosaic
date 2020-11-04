#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
if (!requireNamespace("shiny", quietly = TRUE)){install.packages("shiny")}
if (!requireNamespace("reshape2", quietly = TRUE)){install.packages("shiny")}
if (!requireNamespace("igraph", quietly = TRUE)){install.packages("shiny")}

library(shiny)
library(reshape2)
library(igraph)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Observations"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            h4("Analyzed data"),
            uiOutput("analyzed_features"),
            actionButton("analyze_data", label = "Analyze data"),
            hr(),
            fileInput("upload_file", label = h5("Upload data")),
            hr(),
            
        ),

        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("grph_plot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    data <- reactiveValues()
    
    observeEvent(input$analyze_data, {
        data$data <- read.table("./data.txt", header = TRUE, sep = "\t")
        if(!is.null(input$upload_file)){data$data <- input$upload_file}
        
        #### correlations
        get_corr_p_one_tbl <- function(tbl, method = "spearman"){
            corr_martix <- matrix(0, ncol = ncol(tbl), nrow = ncol(tbl))
            colnames(corr_martix) <- colnames(tbl)
            rownames(corr_martix) <- colnames(tbl)
            p_val_martix <- matrix(0, ncol = ncol(tbl), nrow = ncol(tbl))
            colnames(p_val_martix) <- colnames(tbl)
            rownames(p_val_martix) <- colnames(tbl)
            for (i in colnames(tbl)){
                for (j in colnames(tbl)){
                    tmp <- cor.test(tbl[,i], tbl[,j], method = method)
                    corr_martix[i,j] <- tmp$estimate
                    p_val_martix[i,j] <- tmp$p.value
                }
            }
            return(list(corr_martix, p_val_martix))
        }
        
        get_tidy_corr_p_table <- function(tbls){
            corr_spreadsheet <- melt(tbls[[1]])
            colnames(corr_spreadsheet) <- c("cluster1", "Cluster2", "corr_coef")
            rownames(corr_spreadsheet) <- paste(corr_spreadsheet$cluster1, corr_spreadsheet$Cluster2, sep = "_vs_")
            
            corr_spreadsheet_pVal <- melt(tbls[[2]])
            colnames(corr_spreadsheet_pVal) <- c("cluster1", "Cluster2", "pValue")
            rownames(corr_spreadsheet_pVal) <- paste(corr_spreadsheet_pVal$cluster1, corr_spreadsheet_pVal$Cluster2, sep = "_vs_")
            
            corr_spreadsheet <- cbind(corr_spreadsheet, 
                                      corr_spreadsheet_pVal[rownames(corr_spreadsheet), "pValue"])
            colnames(corr_spreadsheet) <- c("feature1", "feature2", "corr_coef", "pValue")
            return(corr_spreadsheet)
        }

        data$corr_inf <- get_tidy_corr_p_table(get_corr_p_one_tbl(data$data))
        data$corr_inf <- data$corr_inf[data$corr_inf$pValue <= 0.05,]
        data$corr_inf <- data$corr_inf[data$corr_inf$feature1 != data$corr_inf$feature2,]
        data$vertices <- data.frame(id = colnames(data$data))
        data$edges <- data.frame(from = data$corr_inf$feature1, to = data$corr_inf$feature2, coeff = data$corr_inf$corr_coef)
        print("-----")
        print(data$corr_inf)
        print("-----")
        print(data$vertices)
        print("==")
        print(data$edges)

        
        
    })
    output$analyzed_features <- renderUI({
        if(is.null(data$data)){return(NULL)}
        checkboxGroupInput("features", label = h5("Choose features"), 
                           choices = colnames(data$data),
                           selected = colnames(data$data))
    })
    
    
    output$grph_plot <- renderPlot({
        if(is.null(data$vertices)){return(NULL)}
        if(is.null(data$edges)){return(NULL)}
        edg_col <- rep("red", nrow(data$edges))
        edg_col[data$edges$coeff < 0] <- "blue"
        net <- graph_from_data_frame(d=data$edges, vertices=data$vertices, directed=F)
        plot(net, edge.color=edg_col)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
