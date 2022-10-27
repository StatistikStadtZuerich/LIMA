### Libraries
library(shiny)
library(reactable)
library(ggplot2)
library(dplyr)
library(shinyjs)
library(dqshiny)
library(gtools)

### Data
source("DataLoad.R")

### Source Export Excel
source("exportExcel.R", encoding = "UTF-8")


### TryCatch for Error in Data Load
# if data load didn't work show message
if(is.null(data)) {
  
  # Define UI
  ui <- fluidPage(
    
    # Include CSS
    includeCSS("sszTheme.css"),
    
    h1("Fehler"),
    p("Aufgrund momentaner Wartungsarbeiten ist die Applikation zur Zeit nicht verfügbar.")
  )
  
  # Server function
  server <- function(input, output) {}
  
  # Run the application 
  shinyApp(ui = ui, server = server)
  
}else{
  
  
  ###GUI
  ui <- fluidPage(
  	
  	#CSS
  	includeCSS("sszTheme.css"),
  
  	#App Selection
  	tags$div(
  	  class = "radioDiv",
  	  h1("Wählen Sie eine Abfrage"),
  	  hr(),
  	  radioButtons(inputId="query", 
  	               label = NULL,
  	               choices=c("Abfrage 1: Zeitreihen nach Bauzonen für ganze Stadt und Teilgebiete",
  	                         "Abfrage 2: Zeitreihen für Quartiere und Bauzonen über Adresseingabe"),
  	               selected = character(0))
  	),
  
  	
  	#App1: 
  	conditionalPanel(
  		condition = 'input.query == "Abfrage 1: Zeitreihen nach Bauzonen für ganze Stadt und Teilgebiete"',
  		sidebarLayout(
  			
  			#Sidebar (Query)
  			sidebarPanel(
  				
  				#Area
  				selectInput("area", 
  										"Gebietsauswahl",
  										choices = c(unique(zones$GebietLang))
  				),
  				
  				#Price
  				radioButtons("price", 
  										 "Preise",
  										 choices = c(unique(zones$PreisreiheLang))
  				),
  				
  				#Group (conditional to price)
  				conditionalPanel(
  					condition = 'input.price != "Stockwerkeigentum pro m2 Wohnungsfläche"',
  					radioButtons("group", 
  											 "Art",
  											 choices = c("Ganze Liegenschaften",
  											             "Stockwerkeigentum",
  											             "Alle Verkäufe")
  					),
  				),
  				
  				#Action Button
  				actionButton(inputId = "buttonStart", 
  										 label = "Abfrage starten"),
  				br(),
  				
  				#Downloads
  				conditionalPanel(
  					condition = 'input.buttonStart',
  					h2("Daten herunterladen"),
  					tags$div(
  					  id = "downloadWrapperId",
  					  class = "downloadWrapperDiv",
  					  sszDownload("csvDownload",
  					              label = "csv"
  					  ),
  					  sszDownload("excelDownload",
  					              label = "xlsx"
  					  ),
  					  actionButton(inputId = "ogdDown",
  					               label = "OGD",
  					               onclick ="window.open('https://data.stadt-zuerich.ch/dataset?tags=lima', '_blank')"
  					  )
  					)
  				)
  			),
  			
  			#Main Panel (Results)
  			mainPanel(
  				
  				#Table Title (prices)
  				tags$div(
  					id = "title_id",
  					class = "title_div",
  					textOutput("title")
  				),
  				
  				conditionalPanel(
  				  condition = 'input.buttonStart',
  					hr()
  				),
  				
  				#Table Subtitle (prices)
  				tags$div(
  					id = "subtitle_id",
  					class = "subtitle_div",
  					textOutput("subtitle")
  				),
  				
  				#Table Subsubtitle (prices)
  				tags$div(
  					id = "subSubtitle_id",
  					class = "subSubtitle_div",
  					textOutput("subSubtitle")
  				),
  				
  				#Title for BZO16 (prices)
  				tags$div(
  					id = "tableTitle16_id",
  					class = "tableTitle_div",
  					textOutput("tableTitle16")
  				),
  				
  				#Table for BZO 16 (prices)
  				reactableOutput("resultsPrice16"),
  				
  				#title for BZO99 (prices)
  				tags$div(
  					id = "tableTitle99_id",
  					class = "tableTitle_div",
  					textOutput("tableTitle99")
  				),
  				
  				#Table for BZO 99 (prices)
  				reactableOutput("resultsPrice99"),
  				
  				#Action Link for Hand Changes (counts)
  				useShinyjs(),
  				conditionalPanel(
  					condition = 'input.buttonStart',
  					tags$div(
  					  class = "linkCount",
  					  actionLink("linkCount",
  					             "Anzahl Handänderungen einblenden", 
  					             icon = icon("angle-down")
  					  )
  					)
  				),
  				
  
  				#Hidden Titles and Tables for Hand Changes
  				shinyjs::hidden(
  					div(id='countDiv',
  							
  							#Title for BZO16 (counts)
  							tags$div(
  								id = "tableTitleTwo16_id",
  								class = "tableTitle_div",
  								textOutput("tableTitleTwo16")
  							),
  							
  							#Table for BZO16 (counts)
  							reactableOutput("resultsCount16"),
  							
  							#Title for BZO99 (counts)
  							tags$div(
  								id = "tableTitleTwo99_id",
  								class = "tableTitle_div",
  								textOutput("tableTitleTwo99")
  							),
  							
  							#Table for BZO99 (counts)
  							reactableOutput("resultsCount99")
  					)
  				),
  				
  				conditionalPanel(
  				  condition = 'input.buttonStart',
      				tags$div(
      				  class = "radioDiv",
      				  h3("Erklärung Zonenarten"),
      				  hr(),
      				  p("Z = Zentrumszone"),
      				  p("K = Kernzone"),
      				  p("Q = Quartiererhaltungszone"),
      				  p("W2 = Wohnzone 2"),
      				  p("W3 = Wohnzone 3"),
      				  p("W4 = Wohnzone 4"),
      				  p("W5 = Wohnzone 5")
      				)
  				  )
  				)
        )
  	  ),
  	
  	#App 2
  	conditionalPanel(
  		condition = 'input.query == "Abfrage 2: Zeitreihen für Quartiere und Bauzonen über Adresseingabe" ',
  		sidebarLayout(
  			
  			#Sidebar (Query)
  			sidebarPanel(
  				
  				#Street input
  			 autocomplete_input("street",
  			                    "Geben Sie eine Strasse ein",
  			                    unique(addresses$StrasseLang)),
  				
  				#Number input
  			  selectInput("number", 
  											 "Wählen Sie eine Hausnummer aus", 
  											 choices = c("",sort(unique(addresses$Hnr))),
  											 selected = NULL),
  				
  				#Action Button
  				actionButton("buttonStartTwo", 
  										 "Abfrage starten"),
  				br(),
  				
  				#Conditional Data Download
  				conditionalPanel(
  					condition = 'input.street && input.number && input.buttonStartTwo',
  					uiOutput("dataTwo"),
  					tags$div(
  					  id = "downloadWrapperId",
  					  class = "downloadWrapperDiv",
  					  uiOutput("tagCSV"),
  					  uiOutput("tagEXCEL"),
  					  uiOutput("tagOGD")
  					)
  				)
  			),
  			
  			#Main Panel (results)
  			mainPanel(
  				br(),
  				
  				#Info Table
  				htmlOutput("resultsInfos"),
  				tags$div(
  					id = "info_id",
  					class = "info_div",
  					textOutput("info")
  				),
  				br(),
  				
  				#Table for prices
  				reactableOutput("resultsPriceSeries"),
  				
  				#Action Link for Hand Changes (counts)
  				useShinyjs(),
  				conditionalPanel(
  					condition = 'input.buttonStartTwo',
  					tags$div(
  						id = "linkCountTwoId",
  						class = "linkCountTwoDiv",
  						uiOutput("linken")
  					)
  				),
  				
  				#Hidden Table for Hand Changes
  				shinyjs::hidden(
  					div(id='countDivTwo',
  					    reactableOutput("resultsCountSeries")
  					)
  				),
  				conditionalPanel(
  				  condition = 'input.street && input.number && input.buttonStartTwo',
  				  tags$div(
  				    id = "defs",
  				    class = "radioDiv",
  				    h3("Begriffserklärung"),
  				    hr(),
  				    p("StwE = Stockwerkeigentum"),
  				    p("VersW = Versicherungswert des Gebäudes")
  				  )
  				)
  			)
  		)
  	)
  )
  
  ###Server
  server <- function(input, output, session) {
  	
  	###Get Data for Download
  	#App 1
  	dataDownload <- eventReactive(input$buttonStart, {
  		if(input$price == "Stockwerkeigentum pro m2 Wohnungsfläche") {
  			filtered <- zones %>%
  				filter(GebietLang == input$area,
  							 PreisreiheLang == input$price) %>% 
  				select(Typ, GebietLang, PreisreiheLang, ArtLang, BZO, Jahr, ALLE, ZE, KE, QU, W2, W23, W34, W45, W56)
  			filtered
  		} else {
  			filtered <- zones %>%
  				filter(GebietLang == input$area,
  							 PreisreiheLang == input$price,
  							 ArtLang == input$group) %>% 
  				select(Typ, GebietLang, PreisreiheLang, ArtLang, BZO, Jahr, ALLE, ZE, KE, QU, W2, W23, W34, W45, W56)
  			filtered
  		}
  	})
  	
  	#App 2
  	dataDownloadTwo <- eventReactive(input$buttonStartTwo, {
  		
  		#Pull district
  		district <- addresses %>%
  			filter(StrasseLang == input$street & Hnr == input$number) %>% 
  			pull(QuarLang)
  		
  		#Pull zone BZO16
  		zoneBZO16 <- addresses %>%
  			filter(StrasseLang == input$street & Hnr == input$number) %>% 
  			pull(ZoneBZO16Lang)
  		
  		#Pull zone BZO99
  		zoneBZO99 <- addresses %>%
  			filter(StrasseLang == input$street & Hnr == input$number) %>% 
  			pull(ZoneBZO99Lang)
  		
  		#Serie BZO16
  		serieBZO16 <- series %>% 
  			filter(QuarLang == district & ZoneLang == zoneBZO16,
  						 Jahr >= 2019) 
  		
  		#Serie BZO99
  		serieBZO99 <- series %>% 
  			filter(QuarLang == district & ZoneLang == zoneBZO99,
  						 Jahr < 2019) 
  		
  		#Total series
  		seriesPriceCount <- bind_rows(serieBZO16, serieBZO99) %>% 
  			select(-QuarCd, -ZoneSort, -ZoneLang) %>% 
  			arrange(factor(Typ, levels = c("Preis",
  																		 "Zahl")), 
  							desc(Jahr))
  		seriesPriceCount
  	})
  	
  	###Tables Output
  	##App 1
  	#Get Data for Output Prices
  	#BZO16
  	priceOutput16 <- eventReactive(input$buttonStart, {
  		if(input$price == "Stockwerkeigentum pro m2 Wohnungsfläche") {
  			filtered <- zonesBZO16 %>%
  				filter(Typ == "Preis",
  							 GebietLang == input$area,
  							 PreisreiheLang == input$price) %>% 
  				select(Jahr, Total, Z, K, Q, W2, W3, W4, W5, W6) %>% 
  				mutate_all(., ~replace(., is.na(.), " "))  
  			filtered
  		} else {
  			filtered <- zonesBZO16 %>%
  				filter(Typ == "Preis",
  							 GebietLang == input$area,
  							 PreisreiheLang == input$price,
  							 ArtLang == input$group) %>% 
  				select(Jahr, Total, Z, K, Q, W2, W3, W4, W5, W6) %>% 
  				mutate_all(., ~replace(., is.na(.), " "))  
  			filtered
  		}
  	})
  	
  	#BZO99
  	priceOutput99 <- eventReactive(input$buttonStart, {
  		if(input$price == "Stockwerkeigentum pro m2 Wohnungsfläche") {
  			filtered <- zonesBZO99 %>%
  				filter(Typ == "Preis",
  							 GebietLang == input$area,
  							 PreisreiheLang == input$price) %>% 
  				select(Jahr, Total, Z, K, Q, ` `,W2, W3, W4, W5) %>% 
  				mutate_all(., ~replace(., is.na(.), " "))  
  			filtered 
  		} else {
  			filtered <- zonesBZO99 %>%
  				filter(Typ == "Preis",
  							 GebietLang == input$area,
  							 PreisreiheLang == input$price,
  							 ArtLang == input$group) %>% 
  				select(Jahr, Total, Z, K, Q, ` `,W2, W3, W4, W5) %>% 
  				mutate_all(., ~replace(., is.na(.), " ")) 
  			filtered
  		}
  	})
  	
  	#Get Data for Output Counts
  	#BZO16
  	countOutput16 <- eventReactive(input$buttonStart, {
  		if(input$price == "Stockwerkeigentum pro m2 Wohnungsfläche") {
  			filtered <- zonesBZO16 %>%
  				filter(Typ == "Zahl",
  							 GebietLang == input$area,
  							 PreisreiheLang == input$price) %>% 
  				select(Jahr, Total, Z, K, Q, W2, W3, W4, W5, W6) %>% 
  				mutate_all(., ~replace(., is.na(.), " "))
  			filtered
  		} else {
  			filtered <- zonesBZO16 %>%
  				filter(Typ == "Zahl",
  							 GebietLang == input$area,
  							 PreisreiheLang == input$price,
  							 ArtLang == input$group) %>% 
  				select(Jahr, Total, Z, K, Q, W2, W3, W4, W5, W6) %>% 
  				mutate_all(., ~replace(., is.na(.), " "))
  			filtered
  		}
  	})
  	
  	#BZO99
  	countOutput99 <- eventReactive(input$buttonStart, {
  		if(input$price == "Stockwerkeigentum pro m2 Wohnungsfläche") {
  			filtered <- zonesBZO99 %>%
  				filter(Typ == "Zahl",
  							 GebietLang == input$area,
  							 PreisreiheLang == input$price) %>% 
  				select(Jahr, Total, Z, K, Q, ` `,W2, W3, W4, W5) %>% 
  				mutate_all(., ~replace(., is.na(.), " ")) 
  			filtered
  		} else {
  			filtered <- zonesBZO99 %>%
  				filter(Typ == "Zahl",
  							 GebietLang == input$area,
  							 PreisreiheLang == input$price,
  							 ArtLang == input$group) %>% 
  				select(Jahr, Total, Z, K, Q, ` `,W2, W3, W4, W5) %>% 
  				mutate_all(., ~replace(., is.na(.), " ")) 
  			filtered
  		}
  	})
  	
  	#Show Output Prices (App 1)
  	observeEvent(input$buttonStart, {
  	  output$resultsPrice16 <- renderReactable({
  	    out16 <- reactable(priceOutput16(),
  	                             theme = reactableTheme(
  	                               borderColor = "#DEDEDE"
  	                             ),
  	                             defaultColDef = colDef(
  	                               align = "left",
  	                               minWidth = 50
  	                             ),
  	                             outlined = TRUE,
  	                             highlight = TRUE,
  	                             defaultPageSize = 5,
  	                             rowClass = JS("function(rowInfo) {return rowInfo.selected ? 'selected' : ''}"),
  	                             rowStyle = JS("function(rowInfo) {if (rowInfo.selected) { return { backgroundColor: '#F2F2F2'}}}")
  	    )
  	    out16
  	  })
  	  output$resultsPrice99 <- renderReactable({
  	    out99 <- reactable(priceOutput99(),
  	                       theme = reactableTheme(
  	                         borderColor = "#DEDEDE"
  	                       ),
  	                       defaultColDef = colDef(
  	                         align = "left",
  	                         minWidth = 50
  	                       ),
  	                       outlined = TRUE,
  	                       highlight = TRUE,
  	                       defaultPageSize = 15,
  	                       rowClass = JS("function(rowInfo) {return rowInfo.selected ? 'selected' : ''}"),
  	                       rowStyle = JS("function(rowInfo) {if (rowInfo.selected) { return { backgroundColor: '#F2F2F2'}}}")
  	    )
  	    out99
  	  })
  
  	})
  	
  	#Show Output Counts (App 1)
  	observeEvent(input$linkCount, {
  		shinyjs::toggle('countDiv')
  	  
  	  output$resultsCount16 <- renderReactable({
  	    out16Count <- reactable(countOutput16(),
  	                       theme = reactableTheme(
  	                         borderColor = "#DEDEDE"
  	                       ),
  	                       defaultColDef = colDef(
  	                         align = "left",
  	                         minWidth = 50
  	                       ),
  	                       outlined = TRUE,
  	                       highlight = TRUE,
  	                       defaultPageSize = 5,
  	                       rowClass = JS("function(rowInfo) {return rowInfo.selected ? 'selected' : ''}"),
  	                       rowStyle = JS("function(rowInfo) {if (rowInfo.selected) { return { backgroundColor: '#F2F2F2'}}}")
  	    )
  	    out16Count
  	  })
  	  output$resultsCount99 <- renderReactable({
  	    out99Count <- reactable(countOutput99(),
  	                            theme = reactableTheme(
  	                              borderColor = "#DEDEDE"
  	                            ),
  	                            defaultColDef = colDef(
  	                              align = "left",
  	                              minWidth = 50
  	                            ),
  	                            outlined = TRUE,
  	                            highlight = TRUE,
  	                            defaultPageSize = 15,
  	                            rowClass = JS("function(rowInfo) {return rowInfo.selected ? 'selected' : ''}"),
  	                            rowStyle = JS("function(rowInfo) {if (rowInfo.selected) { return { backgroundColor: '#F2F2F2'}}}")
  	    )
  	    out99Count
  	  })
  
  		if(input$linkCount %% 2 == 1) {
  			txt <- "Anzahl Handänderungen verbergen"
  			updateActionLink(session, "linkCount", label = txt, icon = icon("angle-up"))
  			shinyjs::addClass("linkCount", "visitedLink")
  		} else {
  			txt <- "Anzahl Handänderungen einblenden"
  			updateActionLink(session, "linkCount", label = txt, icon = icon("angle-down"))
  			shinyjs::removeClass("linkCount", "visitedLink")
  		}
  	})
  	
  	#Captions
  	#Reactive Title
  	titleReactive <- eventReactive(input$buttonStart, {
  		input$price
  	})
  	output$title <- renderText({
  		titleReactive()
  	})
  	
  	#Reactive Subtitle
  	subtitleReactive <- eventReactive(input$buttonStart, {
  		if(input$price == "Stockwerkeigentum pro m2 Wohnungsfläche"){
  			title <- NULL
  		} else {
  			title <- input$group
  		}
  	})
  	output$subtitle <- renderText({
  		subtitleReactive()
  	})
  	
  	#Reactive Sub-Subtitle
  	subSubtitleReactive <- eventReactive(input$buttonStart, {
  		subSubtitle <- paste0(input$area, ", Medianpreise in CHF")
  	})
  	output$subSubtitle <- renderText({
  		subSubtitleReactive()
  	})
  	
  	#Reactive Table Title BZO 16
  	tableTitle16Reactive <- eventReactive(input$buttonStart, {
  		tableTitle16 <- paste0("Nach Zonenart gemäss BZO 2016")
  	})
  	output$tableTitle16 <- renderText({
  		tableTitle16Reactive()
  	})
  	
  	tableTitleTwo16Reactive <- eventReactive(input$buttonStart, {
  		tableTitleTwo16 <- paste0("Nach Zonenart gemäss BZO 2016")
  	})
  	output$tableTitleTwo16 <- renderText({
  		tableTitleTwo16Reactive()
  	})
  	
  	#Reactive Table Title BZO 16
  	tableTitle99Reactive <- eventReactive(input$buttonStart, {
  		tableTitle99 <- paste0("Nach Zonenart gemäss BZO 1999")
  	})
  	output$tableTitle99 <- renderText({
  		tableTitle99Reactive()
  	})
  	
  	tableTitleTwo99Reactive <- eventReactive(input$buttonStart, {
  		tableTitleTwo99 <- paste0("Nach Zonenart gemäss BZO 1999")
  	})
  	output$tableTitleTwo99 <- renderText({
  		tableTitleTwo99Reactive()
  	})
  	
  	##App 2
  	#Show Output Info (App 2)
  	#Sort for House Number in Drop Down
  	observe({
  		updateSelectInput(
  			session, "number",
  			choices = addresses %>% 
  			  filter(StrasseLang == input$street) %>% 
  			  pull(Hnr) %>% 
  			  mixedsort
  		)
  	})
  	
  	#Get Information of Address
  	infosReactive <- eventReactive(input$buttonStartTwo, {
  		req(input$street)
  		req(input$number)
  		infosFiltered <- addresses %>%
  			filter(StrasseLang == input$street & Hnr == input$number) %>% 
  			mutate(Adresse = paste0(StrasseLang, " ", Hnr)) %>% 
  			select(Adresse, QuarLang, Zones) %>% 
  			mutate(pivot = 1) %>% 
  			pivot_longer(!pivot) %>% 
  			mutate(name = case_when(name == "Adresse" ~ "Die Adresse",
  															name == "QuarLang" ~ "liegt im Quartier",
  															name == "Zones" ~ "in folgender Zone")) %>% 
  			select(-pivot) %>% 
  			knitr::kable("html",
  						align = "lr",
  						col.names = NULL) %>% 
  			kableExtra::kable_styling(bootstrap_options = c("condensed"))
  		infosFiltered
  	})
  	
  	#Show Output Information Address
  	observeEvent(input$buttonStartTwo, {
  		output$resultsInfos <- renderText({
  			outInfos <- infosReactive() 
  			outInfos
  		})
  	})
  	
  	#Get Information if Data Frame is empty
  	dataAvailable <- eventReactive(input$buttonStartTwo, {
  		req(input$street)
  		req(input$number)
  		#Pull district
  		district <- addresses %>%
  			filter(StrasseLang == input$street & Hnr == input$number) %>% 
  			pull(QuarLang)
  		
  		#Pull zone BZO16
  		zoneBZO16 <- addresses %>%
  			filter(StrasseLang == input$street & Hnr == input$number) %>% 
  			pull(ZoneBZO16Lang)
  		
  		#Pull zone BZO99
  		zoneBZO99 <- addresses %>%
  			filter(StrasseLang == input$street & Hnr == input$number) %>% 
  			pull(ZoneBZO99Lang)
  		
  		#Price serie BZO16
  		priceSerieBZO16 <- series %>% 
  			filter(QuarLang == district & ZoneLang == zoneBZO16,
  						 Typ == "Preis",
  						 Jahr >= 2019) 
  		
  		#Price serie BZO99
  		priceSerieBZO99 <- series %>% 
  			filter(QuarLang == district & ZoneLang == zoneBZO99,
  						 Typ == "Preis",
  						 Jahr < 2019) 
  		
  		#Total series
  		priceSerieTotal <- bind_rows(priceSerieBZO16, priceSerieBZO99) %>% 
  			select(-Typ, -QuarCd, -QuarLang, -ZoneSort, -ZoneLang)
  		
  		if(nrow(priceSerieTotal)>0) {
  			available <- 1
  		} else {
  			avaiable <- 0
  		}
  	})
  	
  	#Reactive Info
  	infoReactive <- eventReactive(input$buttonStartTwo, {
  		req(input$street)
  		req(input$number)
  		
  		availability <- dataAvailable()
  		if(availability>0) {
  			req(input$street)
  			req(input$number)
  			infoTitle <- paste0("In dieser Zone dieses Quartiers wurden folgende Medianpreise und Handänderungen festgestellt:")
  		} else {
  			req(input$street)
  			req(input$number)
  			infoTitle <- paste0("Die gewünschte Adresse liegt nicht in einer Wohnzone oder Mischzone (Kernzone, Zentrumszone, Quartiererhaltungszone).\nWählen Sie eine andere Adresse und machen Sie eine erneute Abfrage.")
  		}
  	})
  	
  	#Show Info (App 2)
  	output$info <- renderText({
  		infoReactive()
  	})
  	
  	#Show Output (App 2)  
  	#Get Data for Output Prices (District-Zone-Combination)
  	distReactivePrice <- eventReactive(input$buttonStartTwo, {
  		req(input$street)
  		req(input$number)
  		
  		#Pull district
  		district <- addresses %>%
  			filter(StrasseLang == input$street & Hnr == input$number) %>% 
  			pull(QuarLang)
  		
  		#Pull zone BZO16
  		zoneBZO16 <- addresses %>%
  			filter(StrasseLang == input$street & Hnr == input$number) %>% 
  			pull(ZoneBZO16Lang)
  		
  		#Pull zone BZO99
  		zoneBZO99 <- addresses %>%
  			filter(StrasseLang == input$street & Hnr == input$number) %>% 
  			pull(ZoneBZO99Lang)
  		
  		#Price serie BZO16
  		priceSerieBZO16 <- series %>% 
  			filter(QuarLang == district & ZoneLang == zoneBZO16,
  						 Typ == "Preis",
  						 Jahr >= 2019) 
  		
  		#Price serie BZO99
  		priceSerieBZO99 <- series %>% 
  			filter(QuarLang == district & ZoneLang == zoneBZO99,
  						 Typ == "Preis",
  						 Jahr < 2019) 
  		
  		#Total series
  		priceSerieTotal <- bind_rows(priceSerieBZO16, priceSerieBZO99) %>% 
  			select(-Typ, -QuarCd, -QuarLang, -ZoneSort, -ZoneLang)
  		
  		if(nrow(priceSerieTotal)>0) {
  			priceDistZone <- priceSerieTotal 
  			priceDistZone
  		} else {
  			priceDistZone <- NULL
  		}
  	})
  	
  	#Get Data for Output Counts (District-Price-Combination)
  	distReactiveCount <- eventReactive(input$buttonStartTwo, {
  		req(input$street)
  		req(input$number)
  		
  		#Pull district
  		district <- addresses %>%
  			filter(StrasseLang == input$street & Hnr == input$number) %>% 
  			pull(QuarLang)
  		
  		#Pull zone BZO16
  		zoneBZO16 <- addresses %>%
  			filter(StrasseLang == input$street & Hnr == input$number) %>% 
  			pull(ZoneBZO16Lang)
  		
  		#Pull zone BZO99
  		zoneBZO99 <- addresses %>%
  			filter(StrasseLang == input$street & Hnr == input$number) %>% 
  			pull(ZoneBZO99Lang)
  		
  		#Price serie BZO16
  		countSerieBZO16 <- series %>% 
  			filter(QuarLang == district & ZoneLang == zoneBZO16,
  						 Typ == "Zahl",
  						 Jahr >= 2019) 
  		
  		#Price serie BZO99
  		countSerieBZO99 <- series %>% 
  			filter(QuarLang == district & ZoneLang == zoneBZO99,
  						 Typ == "Zahl",
  						 Jahr < 2019) 
  		
  		#Total series
  		countSerieTotal <- bind_rows(countSerieBZO16, countSerieBZO99) %>% 
  			select(-Typ, -QuarCd, -QuarLang, -ZoneSort, -ZoneLang)
  		
  		if(nrow(countSerieTotal)>0) {
  			countDistZone <- countSerieTotal 
  			countDistZone
  		} else {
  			countDistZone <- NULL
  		}
  	})
  
  	#Show Output Prices
  	observeEvent(input$buttonStartTwo, {
  		req(input$street)
  		req(input$number)
  		req(input$buttonStartTwo)
  		
  		#Table if data is available for zone
  		availability <- dataAvailable()
  		if(availability>0) {
  		  output$resultsPriceSeries <- renderReactable({
  		    outPriceSeries <- reactable(distReactivePrice(),
  		                                theme = reactableTheme(
  		                                  borderColor = "#DEDEDE"
  		                                ),
  		                                columns = list(
  		                                  Jahr = colDef(name = "Jahr"),
  		                                  FrQmBodenGanzeLieg = colDef(name = "Ganze Liegen-\nschaften"),
  		                                  FrQmBodenStwE = colDef(name = "StwE"),
  		                                  FrQmBodenAlleHA = colDef(name = "Alle Verkäufe"),
  		                                  FrQmBodenNettoGanzeLieg = colDef(name = "Ganze Liegen-\nschaften"),
  		                                  FrQmBodenNettoStwE = colDef(name = "StwE"),
  		                                  FrQmBodenNettoAlleHA = colDef(name = "Alle Verkäufe"),
  		                                  FrQmWohnflStwE = colDef(name = "")
  		                                ),
  		                                columnGroups = list(
  		                                  colGroup(name = "Preise pro m2 Boden", 
  		                                           columns = c("FrQmBodenGanzeLieg", "FrQmBodenStwE", "FrQmBodenAlleHA"),
  		                                           align = "left", headerVAlign = "bottom"),
  		                                  colGroup(name = "Preise pro m2 Boden abzgl. VersW", 
  		                                           columns = c("FrQmBodenNettoGanzeLieg", "FrQmBodenNettoStwE", "FrQmBodenNettoAlleHA"),
  		                                           align = "left", headerVAlign = "bottom"),
  		                                  colGroup(name = "StwE pro m2 Wohnungsfläche (alle Zonen)", 
  		                                           columns = "FrQmWohnflStwE",
  		                                           align = "left", headerVAlign = "bottom")
  		                                ),
  		                                defaultColDef = colDef(
  		                                  align = "left",
  		                                  headerVAlign = "bottom",
  		                                  minWidth = 50
  		                                ),
  		                                outlined = TRUE,
  		                                highlight = TRUE,
  		                                defaultPageSize = 15,
  		                                rowClass = JS("function(rowInfo) {return rowInfo.selected ? 'selected' : ''}"),
  		                                rowStyle = JS("function(rowInfo) {if (rowInfo.selected) { return { backgroundColor: '#F2F2F2'}}}")
  		    )
  		    outPriceSeries
  		  })
  		  output$resultsCountSeries <- renderReactable({
  		    outCountSeries <- reactable(distReactiveCount(),
  		                                theme = reactableTheme(
  		                                  borderColor = "#DEDEDE"
  		                                ),
  		                                columns = list(
  		                                  Jahr = colDef(name = "Jahr"),
  		                                  FrQmBodenGanzeLieg = colDef(name = "Ganze Liegen-\nschaften"),
  		                                  FrQmBodenStwE = colDef(name = "StwE"),
  		                                  FrQmBodenAlleHA = colDef(name = "Alle Verkäufe"),
  		                                  FrQmBodenNettoGanzeLieg = colDef(name = ""),
  		                                  FrQmBodenNettoStwE = colDef(name = ""),
  		                                  FrQmBodenNettoAlleHA = colDef(name = ""),
  		                                  FrQmWohnflStwE = colDef(name = "" )
  		                                ),
  		                                columnGroups = list(
  		                                  colGroup(name = "Anzahl Handänderungen", 
  		                                           columns = c("FrQmBodenGanzeLieg", "FrQmBodenStwE", "FrQmBodenAlleHA"),
  		                                           align = "left", headerVAlign = "bottom"),
  		                                  colGroup(name = "StwE pro m2 Wohnungsfläche (alle Zonen)", 
  		                                           columns = c("FrQmBodenNettoGanzeLieg", "FrQmBodenNettoStwE", "FrQmBodenNettoAlleHA", "FrQmWohnflStwE"),
  		                                           align = "right", headerVAlign = "bottom")
  		                                ),
  		                                defaultColDef = colDef(
  		                                  align = "left",
  		                                  headerVAlign = "bottom",
  		                                  minWidth = 50
  		                                ),
  		                                outlined = TRUE,
  		                                highlight = TRUE,
  		                                defaultPageSize = 15,
  		                                rowClass = JS("function(rowInfo) {return rowInfo.selected ? 'selected' : ''}"),
  		                                rowStyle = JS("function(rowInfo) {if (rowInfo.selected) { return { backgroundColor: '#F2F2F2'}}}")
  		    )
  		    outCountSeries
  		  })
  		  
  		} else {
  		 output$resultsPriceSeries <- NULL
  		 output$resultsCountSeries <- NULL
  		}
  	})
  
  	
  	#Show Output Counts
  	output$linken <- renderUI({
  		availability <- dataAvailable()
  		if(availability>0) {
  			actionLink("linkCountTwoTest",
  								 "Anzahl Handänderungen einblenden", 
  								 icon = icon("angle-down"),
  								 style='font-size:12px')
  		} else {
  			txt <- NULL
  		}
  	})
  	
  	#Js for the drop down link
  	observeEvent(input$linkCountTwoTest, {
  	    shinyjs::toggle('countDivTwo')
  	    if(input$linkCountTwoTest %% 2 == 1) {
  	      txt <- "Anzahl Handänderungen verbergen"
  	      updateActionLink(session, "linkCountTwoTest", label = txt, icon = icon("angle-up"))
  	      shinyjs::addClass("linkCountTwoTest", "visitedLink")
  	    } else {
  	      txt <- "Anzahl Handänderungen einblenden"
  	      updateActionLink(session, "linkCountTwoTest", label = txt, icon = icon("angle-down"))
  	      shinyjs::removeClass("linkCountTwoTest", "visitedLink")
  	    }
  	})
  	
  	
  	#Disable Container with Tables and definition, if data is not available
  	observe({
  	  availability <- dataAvailable()
  	  if (availability>0) {
  	    shinyjs::show("resultsPriceSeries")
  	    shinyjs::show("resultsCountSeries")
  	    shinyjs::show("defs")
  	    shinyjs::show("linkCountTwoId")
  	  } else {
  	    shinyjs::hide("resultsPriceSeries")
  	    shinyjs::hide("resultsCountSeries")
  	    shinyjs::hide("defs")
  	    shinyjs::hide("linkCountTwoId")
  	  }
  	})
  	
  	###Write Download Table
  	##App 1
  	#CSV
  	output$csvDownload <- downloadHandler(
  		filename = function(price) {
  			price <- input$price
  			if(price == "Stockwerkeigentum pro m2 Wohnungsfläche") {
  				price <- gsub(" ", "-", input$price, fixed = TRUE)
  				area <- gsub(" ", "-", input$area, fixed = TRUE)
  				paste0("Liegenschaftenhandel_nach_Bauzonenordnung_und_Zonenart_", price, "_", area, ".csv")
  			} else {
  				price <- gsub(" ", "-", input$price, fixed = TRUE)
  				group <- gsub(" ", "-", input$group, fixed = TRUE)
  				area <- gsub(" ", "-", input$area, fixed = TRUE)
  				paste0("Liegenschaftenhandel_nach_Bauzonenordnung_und_Zonenart_", price, "_", group, "_", area, ".csv")
  			}
  		},
  		content = function(file) {
  			write.csv(dataDownload(), file, row.names = FALSE, na = " ")
  		}
  	)
  	
  	#Excel
  	output$excelDownload <- downloadHandler(
  		filename = function(price) {
  			price <- input$price
  			if(price == "Stockwerkeigentum pro m2 Wohnungsfläche") {
  				price <- gsub(" ", "-", input$price, fixed = TRUE)
  				area <- gsub(" ", "-", input$area, fixed = TRUE)
  				paste0("Liegenschaftenhandel_nach_Bauzonenordnung_und_Zonenart_", price, "_", area, ".xlsx")
  			} else {
  				price <- gsub(" ", "-", input$price, fixed = TRUE)
  				group <- gsub(" ", "-", input$group, fixed = TRUE)
  				area <- gsub(" ", "-", input$area, fixed = TRUE)
  				paste0("Liegenschaftenhandel_nach_Bauzonenordnung_und_Zonenart_", price, "_", group, "_", area, ".xlsx")
  			}
  		},
  		content = function(file) {
  		  sszDownloadExcel(dataDownload(), file, input$query, input$area, input$price, input$group)
  		}
  	)
  	
  	#App 2
  	#CSV
  	output$downloadDataCSVTwo <- downloadHandler(
  		filename = function() {
  			district <- addresses %>%
  				filter(StrasseLang == input$street & Hnr == input$number) %>% 
  				pull(QuarLang)
  			paste0("Liegenschaftenhandel_nach_Bauzonenordnung_und_Quartier_", district, ".csv")
  		},
  		content = function(file) {
  			write.csv(dataDownloadTwo(), file, row.names = FALSE, na = " ")
  		}
  	)
  	
  	#Excel
  	output$downloadDataEXCELTwo <- downloadHandler(
  		filename = function() {
  			district <- addresses %>%
  				filter(StrasseLang == input$street & Hnr == input$number) %>% 
  				pull(QuarLang)
  			paste0("Liegenschaftenhandel_nach_Bauzonenordnung_und_Quartier_", district, ".xlsx")
  		},
  		content = function(file) {
  		  sszDownloadExcel(dataDownloadTwo(), file, input$query, input$street, input$number)
  		}
  	)
  	
  	#Conditional Data Download (title)
  	observeEvent(input$buttonStartTwo, {
  		output$dataTwo <- renderUI({
  			availability <- dataAvailable()
  			if(availability>0) {
  				req(input$street)
  				req(input$number)
  				req(input$buttonStartTwo)
  				h2("Daten herunterladen")
  			} else {
  				txt <- NULL
  			}
  		})
  	})
  	
  	#Conditional Data Download (CSV link)
  	output$tagCSV <- renderUI({
  		availability <- dataAvailable()
  		if(availability>0) {
  			req(input$street)
  			req(input$number)
  			req(input$buttonStartTwo)
  			  sszDownload("downloadDataCSVTwo",
  			              label = "csv"
  			  )
  		} else {
  			txt <- NULL
  		}
  	})
  	
  	#Conditional Data Download (Excel link)
  	output$tagEXCEL <- renderUI({
  		availability <- dataAvailable()
  		if(availability>0) {
  			req(input$street)
  			req(input$number)
  			req(input$buttonStartTwo)
  			  sszDownload("downloadDataEXCELTwo",
  			              label = "xlsx"
  			  )
  		} else {
  			txt <- NULL
  		}
  	})
  	
  	#Conditional Data Download (OGD link)
  	output$tagOGD <- renderUI({
  		availability <- dataAvailable()
  		if(availability>0) {
  			req(input$street)
  			req(input$number)
  			req(input$buttonStartTwo)
  			  actionButton(inputId = "linkOGD",
  			               label = "OGD",
  			               onclick ="window.open('https://data.stadt-zuerich.ch/dataset?tags=lima', '_blank')"
  			  )
  		} else {
  			txt <- NULL
  		}
  	})
  	
  	
  	###Change Action Query Button when first selected
  	##App 1
  	observe({
  		req(input$buttonStart)
  		updateActionButton(session, "buttonStart",
  											 label = "Erneute Abfrage")
  	})
  	
  	##App 2
  	observe({
  		req(input$buttonStartTwo)
  		updateActionButton(session, "buttonStartTwo",
  											 label = "Erneute Abfrage")
  	})
  }
  
  ###Run the App
  shinyApp(ui = ui, server = server)
}