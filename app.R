#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(config)
library(data.table)
library(DT)
library(jsonlite)
library(lubridate)
library(plotly)
library(RODBC)
library(shiny)

config <- config::get()


options(shiny.port = 1234)

# UI
ui <- fluidPage(
    tags$head(
        tags$script(
	    HTML(
	        '$(document).keyup(function(event) {
    		     if ($("#command").is(":focus") && (event.key == "Enter")) {
        	         $("#submitbutton").click();
    		     }
		 });'
	    )
	)
    ),
    tabsetPanel(
    	tabPanel("Overview", fluid = TRUE,
	    sidebarLayout(
	        sidebarPanel(
    		    sliderInput("overviewthreshold", "Anomaly Threshold:",
                        min = 0, max = 1,
                        value = 0.5, step = 0.01),
		    br(),
		    htmlOutput("header"),
		    uiOutput("asearch"),
		    actionButton("subbtn", "Submit"),
		    actionButton("addbtn", "Add Option"),
		    actionButton("rmbtn", "Remove Option"),
		),
		mainPanel(
		    br(),
		    DT::dataTableOutput("overviewtable")
		)
  	    )
        ),
	tabPanel("Search", fluid = TRUE,
            sidebarLayout(
                sidebarPanel(
		    checkboxInput("clearbox", "Clear on submit", FALSE),
                    textInput("command", "", ""),
		    actionButton("submitbutton", "Submit"),
		    br(),
		    br(),
		    tableOutput("history")
                ),
                mainPanel(
                   br(),
		   htmlOutput("searchquery"),
		   br(),
		   DT::dataTableOutput("searchtable")
                )
            )   
        ),
	tabPanel("Stats", fluid = TRUE,
            sidebarLayout(
                sidebarPanel(
		    sliderInput("statsthreshold", "Anomaly Threshold:",
                        min = 0, max = 1,
                        value = 0.5, step = 0.01),
		    fluidRow(column(width = 3, selectInput("statsunit", "Time Unit:", choices = c("Day", "Hour", "Minute"), selected = "Day")))
                ),
                mainPanel(
		    br(),
                    plotlyOutput("statsgraph")
                )
            )   
        )
    )
)

# Server logic
server <- function(input, output, session) {

    ### Overview page ###
    overviewdf <- reactive({
	if (input$subbtn == 0) {
	    return()
	}
	isolate({
	    if (counter$n == 0) {
                querystring <- paste("SELECT", config$idcol, ",", config$datacol, ",", config$predcol, ",", config$truthcol, "FROM", config$tablename, "WHERE", config$predcol, ">", input$overviewthreshold, "ORDER BY", config$predcol, ";")
            } else {
                start <- paste("SELECT", config$idcol, ",", config$datacol, ",", config$predcol, ",", config$truthcol, "FROM", config$tablename, "WHERE", config$predcol, ">", input$overviewthreshold, "AND ")
                content <- sapply(seq_len(counter$n), function(i) {
                                      if (i == counter$n) {
                                              paste0(config$datacol, " LIKE ", '\'%"', input[[paste0("searchselect", i)]], '" : "', input[[paste0("searchtext", i)]], '"%\' ORDER BY ', config$predcol, ';')
                                      } else {
                                              paste0(config$datacol, " LIKE ", '\'%"', input[[paste0("searchselect", i)]], '" : "', input[[paste0("searchtext", i)]], '"%\' ', input[[paste0("searchoption", i + 1)]], ' ')
                                      }
                })
                querystring <- paste(c(start, content), collapse = '')
            }
	    query <- sqlQuery(vertica, querystring)
	    qdata <- as.data.table(query)
	})
    })

    output$overviewtable <- DT::renderDataTable({
        if (input$subbtn == 0) {
	    return()
	}
	isolate({
	    rawtable <- overviewdf()
	    cols <- c(config$idcol, config$predcol, config$truthcol)
	    table <- rawtable[, ..cols]
	    setnames(table, cols, c("inputId", "Predicted Label", "Truth Label"))
	    table$inputId <- as.character(table$inputId)
	    table[, EntryID := paste0('<a id="', inputId, '" href="#" class="action-button" onclick="Shiny.setInputValue(id = &#39;linkclick&#39;, value = ', inputId, ', {priority: \'event\'});">', inputId, '</a>')][, inputId := NULL]
	    table[,c(3, 1, 2)]
	})
    }, escape = FALSE)

    observeEvent(input$linkclick, {
        showModal(modalDialog(
	    title = input$linkclick,
	    tableOutput("rawrequest"),
	    size = "xl"
	))
    })

    output$rawrequest <- renderTable({
        req(input$linkclick)
	table <- overviewdf()
	jsonlist <- fromJSON(paste(table[table[[config$idcol]] == input$linkclick][[config$datacol]]))
	jsonlist[sapply(jsonlist, is.null)] <- NA
	data <- data.table()
	data[, Field := names(jsonlist)][, Value := unlist(jsonlist, use.names = F)]
    })

    # Advanced search options #
    counter <- reactiveValues(n = 0)

    observeEvent(input$addbtn, {
	counter$select <- c(lapply(seq_len(counter$n), function(i) {input[[paste0("searchselect", i)]]}), config$datafields[1])
	counter$text <- c(lapply(seq_len(counter$n), function(i) {input[[paste0("searchtext", i)]]}), '')
	counter$option <- c(lapply(seq_len(counter$n), function(i) {if (i > 1) {input[[paste0("searchoption", i)]]}}), 'AND')
	counter$n <- counter$n + 1
    })
    observeEvent(input$rmbtn, {
	if (counter$n > 0) {
	    counter$select <- lapply(seq_len(counter$n), function(i) {input[[paste0("searchselect", i)]]})
	    counter$text <- lapply(seq_len(counter$n), function(i) {input[[paste0("searchtext", i)]]})
	    counter$option <- lapply(seq_len(counter$n), function(i) {if (i > 1) {input[[paste0("searchoption", i)]]}})
	    counter$n <- counter$n - 1
	}
    })

    options <- reactive({
    	n <- counter$n
	if (n > 0) {
	    isolate({
	    	lapply(seq_len(n), function(i) {
		    div(
		    if (i > 1) {fluidRow(column(width = 4, offset = 4, selectInput(inputId = paste0("searchoption", i), "Operator", choices = c('AND', 'OR'), selected = counter$option[[i]])))},
    	    	    fluidRow(
                    	column(
                      	    width = 6,
                            selectInput(inputId = paste0("searchselect", i), "Field", choices = config$datafields, selected = counter$select[[i]])
                            ),
                    	column(
                            width = 6,
                            textInput(inputId = paste0("searchtext", i), "Value", value = counter$text[[i]])
                    	),
            	    )
		    )
    	    	})
	    })
	}
    })

    output$asearch <- renderUI({
    	options()
    })

    output$header <- renderText({
    	"<h4><b>Advanced search options:<b><h4>"
    })

    ### Search page ###
    searchdf <- reactive({
	query <- sqlQuery(vertica, input$command)
	qdata <- as.data.table(query)
    })

    output$searchquery <- renderText({
	if (input$submitbutton == 0) {
	    return()
	}
    	isolate({paste0("Displaying: <b>", input$command, "<b>")})
    })

    output$searchtable <- DT::renderDataTable({
	if (input$submitbutton == 0) {
	    return()
	}
        isolate({
	    table <- searchdf()
	    if (input$clearbox == TRUE) {
	        updateTextInput(session, "command", value = "")
	    }
	    table
	})
    })

    # Command history #
    commands <- reactiveValues()

    observeEvent(input$submitbutton, {
	commandlink <- paste0('<a href="#" class="action-button" onclick="Shiny.setInputValue(\'command\', \'', gsub("'", "\\\\\\\'", gsub('"', '&quot;', input$command)), '\', {priority: \'event\'});document.getElementById(\'submitbutton\').click();">', input$command, '</a>')
	commands$hist <- setdiff(commands$hist, commandlink)
	commands$hist <- c(commandlink, commands$hist)
	commands$hist <- head(commands$hist, n = 10)
    })

    output$history <- renderTable({
	table <- data.table("History" = commands$hist)
    }, sanitize.text.function = function(x) x)

    ### Stats page ###
    statsdf <- reactive({
	query <- sqlQuery(vertica, paste("SELECT", config$idcol, ",", config$predcol, "FROM", config$tablename, "WHERE", config$predcol, ">", input$statsthreshold))
        qdata <- as.data.table(query)
    })

    output$statsgraph <- renderPlotly({
    	df <- statsdf()
	df$date <- as_datetime(as.numeric(substr(df$LOG_TIMESTAMP, 1, 10)))
	df[, LOG_TIMESTAMP := NULL][, MODEL_LABEL := NULL]
	if (input$statsunit == "Day") {
	    df$date <- floor_date(df$date, unit = "day")
	} else if (input$statsunit == "Hour") {
	    df$date <- floor_date(df$date, unit = "hour")
	} else {
	    df$date <- floor_date(df$date, unit = "min")
	}
	df$count <- 1
	df <- df[,sum(count),by = date]
    	p <- plot_ly(df, x = ~date, y = ~V1, type = "scatter", mode = "lines") %>%
		layout(title = "Detected anomalies", yaxis = list(title = "Number"), xaxis = list(title = "Date"))
	p
    })
}

# Run app
vertica <- odbcDriverConnect(config$vertsettings)
shinyApp(ui = ui, server = server)
odbcClose(vertica)
