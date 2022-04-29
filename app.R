### Required packages
packages <- c("shiny",
	      "knitr",
	      "kableExtra",
	      "ggplot2",
	      "dplyr",
	      "shinyjs",
	      "dqshiny",
	      "gtools",
	      "xlsx")

### Load packages
packageCheck <- lapply(
  packages,
  FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
      library(x, character.only = TRUE)
    }
  }
)

### Data
source("DataLoad.R", local = TRUE)

### GUI
ui <- fluidPage(
	
	# CSS
	includeCSS("sszTheme.css"),
	
	br(),
	
	# App Selection
	radioButtons(inputId="query",
	             label="Wählen Sie eine Abfrage",
	             choices=c("Abfrage 1: Zeitreihen nach Bauzonen für ganze Stadt und Teilgebiete",
	                       "Abfrage 2: Zeitreihen für Quartiere und Bauzonen über Adresseingabe"),
	             character(0)),
	br(),
	
	# App 1: 
	conditionalPanel(
		condition = 'input.query == "Abfrage 1: Zeitreihen nach Bauzonen für ganze Stadt und Teilgebiete"',
		sidebarLayout(
			
			# Sidebar (Query)
			sidebarPanel(
				
				# Area
				selectInput("area",
				            "Gebietsauswahl",
				            choices = c(unique(zones$GebietLang))
				),
				
				# Price
				radioButtons("price",
				             "Preise",
					     choices = c(unique(zones$PreisreiheLang))
				),
				
				# Group (conditional to price)
				conditionalPanel(
					condition = 'input.price != "Stockwerkeigentum pro m2 Wohnungsfläche"',
					radioButtons("group",
					             "Art",
						     choices = c("Nur ganze Liegenschaften",
								 "Nur Stockwerkeigentum",
								 "Alle Verkäufe")
					),
				),
				
				# Action Button
				actionButton("buttonStart",
				             "Abfrage starten", 
					     icon = icon("database")),
				br(),
				
				# Downloads
				conditionalPanel(
					condition = 'input.buttonStart',
					h5("Daten herunterladen"),
					tags$div(
						id = "downloadLinkCSV",
						class = "downloadLink",
						icon("file-csv"),
						tags$div(
							id = "downloadLinkCSVText",
							class = "downloadLinkIcon",
							downloadLink("downloadDataCSV", ".csv herunterladen")
						)
					),
					tags$div(
						id = "downloadLinkEXCEL",
						class = "downloadLink",
						icon("file-excel"),
						tags$div(
							id = "downloadLinkEXCELText",
							class = "downloadLinkIcon",
							downloadLink("downloadDataEXCEL", ".xlsx herunterladen")
						)
					),
					tags$div(
						id = "linkOGD",
						class = "downloadLink",
						icon("database"),
						tags$div(
							id = "downloadLinkOGDText",
							class = "downloadLinkIcon",
							tags$a(
								class = "downloadLinkOGD",
								href = "https://data.stadt-zuerich.ch/dataset?tags=lima",
								target="_blank",
								"im OGD-Portal herunterladen"
							)
						)
					),
					
					# Zones Definition
					h5("Erklärung Wohnzonen"),
					h6("Z = Zentrumszone"),
					h6("K = Kernzone"),
					h6("Q = Quartiererhaltungszone"),
					h6("W2 = Wohnzone 2"),
					h6("W3 = Wohnzone 3"),
					h6("W4 = Wohnzone 4"),
					h6("W5 = Wohnzone 5")
				)
			),
			
			# Main Panel (Results)
			mainPanel(
				
				#T able Title (prices)
				tags$div(
					id = "title_id",
					class = "title_div",
					textOutput("title")
				),
				
				# Table Subtitle (prices)
				tags$div(
					id = "subtitle_id",
					class = "subtitle_div",
					textOutput("subtitle")
				),
				
				# Table Subsubtitle (prices)
				tags$div(
					id = "subSubtitle_id",
					class = "subSubtitle_div",
					textOutput("subSubtitle")
				),
				
				# Title for BZO16 (prices)
				tags$div(
					id = "tableTitle16_id",
					class = "tableTitle_div",
					textOutput("tableTitle16")
				),
				
				# Table for BZO 16 (prices)
				htmlOutput("resultsPrice16"),
				
				# title for BZO99 (prices)
				tags$div(
					id = "tableTitle99_id",
					class = "tableTitle_div",
					textOutput("tableTitle99")
				),
				
				# Table for BZO 99 (prices)
				htmlOutput("resultsPrice99"),
				
				# Action Link for Sales (Sales)
				useShinyjs(),
				conditionalPanel(
					condition = 'input.buttonStart',
					actionLink("linkCount",
					           "Anzahl Handänderungen einblenden", 
						   icon = icon("angle-down"),
						   style='font-size:12px')
				),
				
				# Hidden Titles and Tables for Sales
				shinyjs::hidden(
					div(id='countDiv',
							
							# Title for BZO16 (Sales)
							tags$div(
								id = "tableTitleTwo16_id",
								class = "tableTitle_div",
								textOutput("tableTitleTwo16")
							),
							
							# Table for BZO16 (Sales)
							htmlOutput("resultsCount16"),
							
							# Title for BZO99 (Sales)
							tags$div(
								id = "tableTitleTwo99_id",
								class = "tableTitle_div",
								textOutput("tableTitleTwo99")
							),
							
							# Table for BZO99 (Sales)
							htmlOutput("resultsCount99")
					)
				)
			)
		)
	),
	
	# App 2
	conditionalPanel(
		condition = 'input.query == "Abfrage 2: Zeitreihen für Quartiere und Bauzonen über Adresseingabe" ',
		sidebarLayout(
			
			# Sidebar (Query)
			sidebarPanel(
				
				#Street input
				autocomplete_input("street",
				                   "Geben Sie eine Strasse ein", 
						   unique(addresses$StrasseLang)),
				
				# Number input
				selectizeInput("number",
				               "Wählen Sie eine Hausnummer aus",
				               choices = c("", sort(unique(addresses$Hnr))),
				               selected = NULL),
				
				# Action Button
				actionButton("buttonStartTwo",
				             "Abfrage starten", 
					     icon = icon("database")),
				br(),
				
				# Conditional Data Download
				conditionalPanel(
					condition = 'input.street && input.number && input.buttonStartTwo',
					uiOutput("dataTwo"),
					uiOutput("tagCSV"),
					uiOutput("tagEXCEL"),
					uiOutput("tagOGD")
				)
			),
			
			# Main Panel (results)
			mainPanel(
				br(),
				
				# Info Table
				htmlOutput("resultsInfos"),
				tags$div(
					id = "info_id",
					class = "info_div",
					textOutput("info")
				),
				br(),
				
				# Table for prices
				htmlOutput("resultsPriceSeries"),
				
				# Action Link for Sales (Sales)
				useShinyjs(),
				conditionalPanel(
					condition = 'input.buttonStartTwo',
					tags$div(
						id = "linkCountTwoId",
						class = "linkCountTwoDiv",
						uiOutput("linken")
					)
				),
				
				# Hidden Table for Sales
				shinyjs::hidden(
					div(id='countDivTwo',
					    htmlOutput("resultsCountSeries")
					)
				)
			)
		)
	)
)

### Server
server <- function(input, output, session) {
	
	### Get Data for Download
	## App 1
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
	
	## App 2
	dataDownloadTwo <- eventReactive(input$buttonStartTwo, {
		
		# Pull district
		district <- addresses %>%
			filter(StrasseLang == input$street & Hnr == input$number) %>% 
			pull(QuarLang)
		
		# Pull zone BZO16
		zoneBZO16 <- addresses %>%
			filter(StrasseLang == input$street & Hnr == input$number) %>% 
			pull(ZoneBZO16Lang)
		
		# Pull zone BZO99
		zoneBZO99 <- addresses %>%
			filter(StrasseLang == input$street & Hnr == input$number) %>% 
			pull(ZoneBZO99Lang)
		
		# Serie BZO16
		serieBZO16 <- series %>% 
			filter(QuarLang == district & ZoneLang == zoneBZO16,
			       Jahr >= 2019) 
		
		# Serie BZO99
		serieBZO99 <- series %>% 
			filter(QuarLang == district & ZoneLang == zoneBZO99,
			       Jahr < 2019) 
		
		# Total series
		seriesPriceCount <- bind_rows(serieBZO16, serieBZO99) %>% 
			select(-QuarCd, -ZoneSort, -ZoneLang) %>% 
			arrange(factor(Typ, levels = c("Preis",
			                               "Zahl")), 
							desc(Jahr))
		seriesPriceCount
	})
	
	### Tables Output
	## App 1
	# Get Data for Output Prices
	# BZO16
	priceOutput16 <- eventReactive(input$buttonStart, {
		if(input$price == "Stockwerkeigentum pro m2 Wohnungsfläche") {
			filtered <- zonesBZO16 %>%
				filter(Typ == "Preis",
				       GebietLang == input$area,
				       PreisreiheLang == input$price) %>% 
				select(Jahr, Total, Z, K, Q, W2, W3, W4, W5, W6) %>% 
				mutate_all(., ~replace(., is.na(.), " ")) %>% 
				kable("html", align = c("c")) %>% 
				kable_styling(bootstrap_options = c("hover", "condensed")) %>% 
				row_spec(0, font_size = 10) %>% 
				column_spec(1:10, width = "10%")
			filtered
		} else {
			filtered <- zonesBZO16 %>%
				filter(Typ == "Preis",
				       GebietLang == input$area,
				       PreisreiheLang == input$price,
				       ArtLang == input$group) %>% 
				select(Jahr, Total, Z, K, Q, W2, W3, W4, W5, W6) %>% 
				mutate_all(., ~replace(., is.na(.), " ")) %>% 
				kable("html", align = c("c")) %>% 
				kable_styling(bootstrap_options = c("hover", "condensed")) %>% 
				row_spec(0, font_size = 10) %>% 
				column_spec(1:10, width = "10%")
			filtered
		}
	})
	
	# BZO99
	priceOutput99 <- eventReactive(input$buttonStart, {
		if(input$price == "Stockwerkeigentum pro m2 Wohnungsfläche") {
			filtered <- zonesBZO99 %>%
				filter(Typ == "Preis",
				       GebietLang == input$area,
				       PreisreiheLang == input$price) %>% 
				select(Jahr, Total, Z, K, Q, ` `,W2, W3, W4, W5) %>% 
				mutate_all(., ~replace(., is.na(.), " ")) %>% 
				kable("html", align = c("c")) %>% 
				kable_styling(bootstrap_options = c("hover", "condensed")) %>% 
				row_spec(0, font_size = 10) %>% 
				column_spec(1:10, width = "10%")
			filtered 
		} else {
			filtered <- zonesBZO99 %>%
				filter(Typ == "Preis",
				       GebietLang == input$area,
				       PreisreiheLang == input$price,
				       ArtLang == input$group) %>% 
				select(Jahr, Total, Z, K, Q, ` `,W2, W3, W4, W5) %>% 
				mutate_all(., ~replace(., is.na(.), " ")) %>% 
				kable("html", align = c("c")) %>% 
				kable_styling(bootstrap_options = c("hover", "condensed")) %>% 
				row_spec(0, font_size = 10) %>% 
				column_spec(1:10, width = "10%")
			filtered
		}
	})
	
	# Get Data for Output Sales
	# BZO16
	countOutput16 <- eventReactive(input$buttonStart, {
		if(input$price == "Stockwerkeigentum pro m2 Wohnungsfläche") {
			filtered <- zonesBZO16 %>%
				filter(Typ == "Zahl",
				       GebietLang == input$area,
				       PreisreiheLang == input$price) %>% 
				select(Jahr, Total, Z, K, Q, W2, W3, W4, W5, W6) %>% 
				mutate_all(., ~replace(., is.na(.), " ")) %>% 
				kable("html", align = c("c")) %>% 
				kable_styling(bootstrap_options = c("hover", "condensed")) %>% 
				row_spec(0, font_size = 10) %>% 
				column_spec(1:10, width = "10%")
			filtered
		} else {
			filtered <- zonesBZO16 %>%
				filter(Typ == "Zahl",
				       GebietLang == input$area,
				       PreisreiheLang == input$price,
				       ArtLang == input$group) %>% 
				select(Jahr, Total, Z, K, Q, W2, W3, W4, W5, W6) %>% 
				mutate_all(., ~replace(., is.na(.), " ")) %>% 
				kable("html", align = c("c")) %>% 
				kable_styling(bootstrap_options = c("hover", "condensed")) %>% 
				row_spec(0, font_size = 10) %>% 
				column_spec(1:10, width = "10%")
			filtered
		}
	})
	
	# BZO99
	countOutput99 <- eventReactive(input$buttonStart, {
		if(input$price == "Stockwerkeigentum pro m2 Wohnungsfläche") {
			filtered <- zonesBZO99 %>%
				filter(Typ == "Zahl",
				       GebietLang == input$area,
				       PreisreiheLang == input$price) %>% 
				select(Jahr, Total, Z, K, Q, ` `,W2, W3, W4, W5) %>% 
				mutate_all(., ~replace(., is.na(.), " ")) %>% 
				kable("html", align = c("c")) %>% 
				kable_styling(bootstrap_options = c("hover", "condensed")) %>% 
				row_spec(0, font_size = 10) %>% 
				column_spec(1:10, width = "10%")
			filtered
		} else {
			filtered <- zonesBZO99 %>%
				filter(Typ == "Zahl",
				       GebietLang == input$area,
				       PreisreiheLang == input$price,
				       ArtLang == input$group) %>% 
				select(Jahr, Total, Z, K, Q, ` `,W2, W3, W4, W5) %>% 
				mutate_all(., ~replace(., is.na(.), " ")) %>% 
				kable("html", align = c("c")) %>% 
				kable_styling(bootstrap_options = c("hover", "condensed")) %>% 
				row_spec(0, font_size = 10) %>% 
				column_spec(1:10, width = "10%")
			filtered
		}
	})
	
	# Show Output Prices (App 1)
	observeEvent(input$buttonStart, {
		output$resultsPrice16 <- renderText({
			out16 <- priceOutput16()
			out16
		})
		output$resultsPrice99 <- renderText({
			out99 <- priceOutput99()
			out99
		})
	})
	
	# Show Output Sales (App 1)
	observeEvent(input$linkCount, {
		shinyjs::toggle('countDiv')
		output$resultsCount16 <- renderText({
			out16Count <- countOutput16() 
			out16Count
		})
		output$resultsCount99 <- renderText({
			out99Count <- countOutput99() 
			out99Count
		})
		if(input$linkCount %% 2 == 1) {
			txt <- "Anzahl Handänderungen verbergen"
			updateActionLink(session, "linkCount", label = txt, icon = icon("angle-up"))
		} else {
			txt <- "Anzahl Handänderungen einblenden"
			updateActionLink(session, "linkCount", label = txt, icon = icon("angle-down"))
		}
	})
	
	# Captions
	# Reactive Title
	titleReactive <- eventReactive(input$buttonStart, {
		input$price
	})
	output$title <- renderText({
		titleReactive()
	})
	
	# Reactive Subtitle
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
	
	# Reactive Sub-Subtitle
	subSubtitleReactive <- eventReactive(input$buttonStart, {
		subSubtitle <- paste0(input$area, ", Medianpreise in CHF")
	})
	output$subSubtitle <- renderText({
		subSubtitleReactive()
	})
	
	# Reactive Table Title BZO 16
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
	
	# Reactive Table Title BZO 16
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
	
	## App 2
	# Show Output Info (App 2)
	# Sort for House Number in Drop Down
	observe({
		updateSelectInput(
			session, "number",
			choices = addresses %>% 
			  filter(StrasseLang == input$street) %>% 
			  pull(Hnr) %>% 
			  mixedsort
		)
	})
	
	# Get Information of Address
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
			kable("html", align = "lr", col.names = NULL) %>% 
			kable_styling(bootstrap_options = c("condensed")) %>% 
			column_spec(2, bold = TRUE)
		infosFiltered
	})
	
	# Show Output Information Address
	observeEvent(input$buttonStartTwo, {
		output$resultsInfos <- renderText({
			outInfos <- infosReactive() 
			outInfos
		})
	})
	
	# Get Information if Data Frame is empty
	dataAvailable <- eventReactive(input$buttonStartTwo, {
		req(input$street)
		req(input$number)
		
		# Pull district
		district <- addresses %>%
			filter(StrasseLang == input$street & Hnr == input$number) %>% 
			pull(QuarLang)
		
		# Pull zone BZO16
		zoneBZO16 <- addresses %>%
			filter(StrasseLang == input$street & Hnr == input$number) %>% 
			pull(ZoneBZO16Lang)
		
		# Pull zone BZO99
		zoneBZO99 <- addresses %>%
			filter(StrasseLang == input$street & Hnr == input$number) %>% 
			pull(ZoneBZO99Lang)
		
		# Price serie BZO16
		priceSerieBZO16 <- series %>% 
			filter(QuarLang == district & ZoneLang == zoneBZO16,
						 Typ == "Preis",
						 Jahr >= 2019) 
		
		# Price serie BZO99
		priceSerieBZO99 <- series %>% 
			filter(QuarLang == district & ZoneLang == zoneBZO99,
						 Typ == "Preis",
						 Jahr < 2019) 
		
		# Total series
		priceSerieTotal <- bind_rows(priceSerieBZO16, priceSerieBZO99) %>% 
			select(-Typ, -QuarCd, -QuarLang, -ZoneSort, -ZoneLang)
		
		if(nrow(priceSerieTotal)>0) {
			available <- 1
		} else {
			avaiable <- 0
		}
	})
	
	# Reactive Info
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
	
	# Show Info (App 2)
	output$info <- renderText({
		infoReactive()
	})
	
	# Show Output (App 2)  
	# Get Data for Output Prices (District-Zone-Combination)
	distReactivePrice <- eventReactive(input$buttonStartTwo, {
		req(input$street)
		req(input$number)
		
		# Pull district
		district <- addresses %>%
			filter(StrasseLang == input$street & Hnr == input$number) %>% 
			pull(QuarLang)
		
		# Pull zone BZO16
		zoneBZO16 <- addresses %>%
			filter(StrasseLang == input$street & Hnr == input$number) %>% 
			pull(ZoneBZO16Lang)
		
		# Pull zone BZO99
		zoneBZO99 <- addresses %>%
			filter(StrasseLang == input$street & Hnr == input$number) %>% 
			pull(ZoneBZO99Lang)
		
		# Price serie BZO16
		priceSerieBZO16 <- series %>% 
			filter(QuarLang == district & ZoneLang == zoneBZO16,
						 Typ == "Preis",
						 Jahr >= 2019) 
		
		# Price serie BZO99
		priceSerieBZO99 <- series %>% 
			filter(QuarLang == district & ZoneLang == zoneBZO99,
						 Typ == "Preis",
						 Jahr < 2019) 
		
		# Total series
		priceSerieTotal <- bind_rows(priceSerieBZO16, priceSerieBZO99) %>% 
			select(-Typ, -QuarCd, -QuarLang, -ZoneSort, -ZoneLang)
		
		if(nrow(priceSerieTotal)>0) {
			priceDistZone <- priceSerieTotal %>% 
				kable("html", col.names = c("Jahr",
				                            "Ganze\nLiegenschaft",
				                            "Stockwerk-\nEigentum",
				                            "Total",
				                            "Ganze\nLiegenschaft",
				                            "Stockwerk-\nEigentum",
				                            "Total", " "),
				      align = c("c")) %>% 
				add_header_above(c(" ",
				                   "Gesamtpreise pro m2 Boden" = 3,
						   "Gesamtpreise pro m2 Boden abzgl. Gebäudeversicherungswert" = 3,
						   "Stockwerk-\nEigentum pro m2 Wohnungsfläche (alle Zonen)"),
						 font_size = 10,
						 color = "#0F05A0",
						 align = c("l")) %>% 
				kable_styling(bootstrap_options = c("hover", "condensed")) %>% 
				row_spec(0, font_size = 10) %>% 
			  column_spec(1:8, width = "10%")
			priceDistZone
		} else {
			priceDistZone <- NULL
		}
	})
	
	# Get Data for Output Counts (District-Price-Combination)
	distReactiveCount <- eventReactive(input$buttonStartTwo, {
		req(input$street)
		req(input$number)
		
		# Pull district
		district <- addresses %>%
			filter(StrasseLang == input$street & Hnr == input$number) %>% 
			pull(QuarLang)
		
		# Pull zone BZO16
		zoneBZO16 <- addresses %>%
			filter(StrasseLang == input$street & Hnr == input$number) %>% 
			pull(ZoneBZO16Lang)
		
		# Pull zone BZO99
		zoneBZO99 <- addresses %>%
			filter(StrasseLang == input$street & Hnr == input$number) %>% 
			pull(ZoneBZO99Lang)
		
		# Price serie BZO16
		countSerieBZO16 <- series %>% 
			filter(QuarLang == district & ZoneLang == zoneBZO16,
						 Typ == "Zahl",
						 Jahr >= 2019) 
		
		# Price serie BZO99
		countSerieBZO99 <- series %>% 
			filter(QuarLang == district & ZoneLang == zoneBZO99,
						 Typ == "Zahl",
						 Jahr < 2019) 
		
		# Total series
		countSerieTotal <- bind_rows(countSerieBZO16, countSerieBZO99) %>% 
			select(-Typ, -QuarCd, -QuarLang, -ZoneSort, -ZoneLang)
		
		if(nrow(countSerieTotal)>0) {
			countDistZone <- countSerieTotal %>% 
				kable("html", col.names = c("Jahr",
				                            "Ganze\nLiegenschaft",
				                            "Stockwerk-\nEigentum",
				                            "Total   ",
				                            " ",
				                            " ",
				                            " ",
				                            " "),
				      align = c("c")) %>% 
				add_header_above(c(" ",
				                   "Anzahl Handänderungen" = 6,
				                   "Stockwerk-\nEigentum pro m2 Wohnungsfläche (alle Zonen)"),
						 font_size = 10,
						 color = "#0F05A0",
						 align = c("l")) %>% 
				kable_styling(bootstrap_options = c("hover", "condensed")) %>% 
				row_spec(0, font_size = 10) %>% 
			  column_spec(1:8, width = "10%")
			countDistZone
		} else {
			countDistZone <- NULL
		}
	})
	
	# Conditional Data Download
	output$dataTwo <- renderUI({
		availability <- dataAvailable()
		if(availability>0) {
			req(input$street)
			req(input$number)
			req(input$buttonStartTwo)
			tags$div(
				id = "h5",
				class = "h5",
				DataTwo <- paste0("Daten herunterladen")
			)
		} else {
			txt <- NULL
		}
	})
	
	# Show Output Prices
	observeEvent(input$buttonStartTwo, {
		req(input$street)
		req(input$number)
		req(input$buttonStartTwo)
		output$resultsPriceSeries <- renderText({
			outPriceSeries <- distReactivePrice()
			outPriceSeries
		})
	})
	
	# Show Output Counts
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
	
	observeEvent(input$linkCountTwoTest, {
		shinyjs::toggle('countDivTwo')
		output$resultsCountSeries <- renderText({
			outCountSeries <- distReactiveCount()
			outCountSeries
		})
		if(input$linkCountTwoTest %% 2 == 1) {
			txt <- "Anzahl Handänderungen verbergen"
			updateActionLink(session, "linkCountTwoTest", label = txt, icon = icon("angle-up"))
		} else {
			txt <- "Anzahl Handänderungen einblenden"
			updateActionLink(session, "linkCountTwoTest", label = txt, icon = icon("angle-down"))
		}
	})
	
	### Write Download Table
	## App 1
	# CSV
	output$downloadDataCSV <- downloadHandler(
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
	
	# Excel
	output$downloadDataEXCEL <- downloadHandler(
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
			write.xlsx(dataDownload(), file, row.names = FALSE, showNA = FALSE)
		}
	)
	
	## App 2
	# CSV
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
	
	# Excel
	output$downloadDataEXCELTwo <- downloadHandler(
		filename = function() {
			district <- addresses %>%
				filter(StrasseLang == input$street & Hnr == input$number) %>% 
				pull(QuarLang)
			paste0("Liegenschaftenhandel_nach_Bauzonenordnung_und_Quartier_", district, ".xlsx")
		},
		content = function(file) {
			write.xlsx(dataDownloadTwo(), file, row.names = FALSE, showNA = FALSE)
		}
	)
	
	# Conditional Data Download (title)
	observeEvent(input$buttonStartTwo, {
		output$dataTwo <- renderUI({
			availability <- dataAvailable()
			if(availability>0) {
				req(input$street)
				req(input$number)
				req(input$buttonStartTwo)
				tags$div(
					id = "h5",
					class = "h5",
					DataTwo <- paste0("Daten herunterladen")
				)
			} else {
				txt <- NULL
			}
		})
	})
	
	# Conditional Data Download (CSV link)
	output$tagCSV <- renderUI({
		availability <- dataAvailable()
		if(availability>0) {
			req(input$street)
			req(input$number)
			req(input$buttonStartTwo)
			tags$div(
				id = "downloadLinkCSV",
				class = "downloadLink",
				icon("file-csv"),
				tags$div(
					id = "downloadLinkCSVText",
					class = "downloadLinkIcon",
					downloadLink("downloadDataCSVTwo", ".csv herunterladen")
				)
			)
		} else {
			txt <- NULL
		}
	})
	
	# Conditional Data Download (Excel link)
	output$tagEXCEL <- renderUI({
		availability <- dataAvailable()
		if(availability>0) {
			req(input$street)
			req(input$number)
			req(input$buttonStartTwo)
			tags$div(
				id = "downloadLinkEXCEL",
				class = "downloadLink",
				icon("file-excel"),
				tags$div(
					id = "downloadLinkEXCELText",
					class = "downloadLinkIcon",
					downloadLink("downloadDataEXCELTwo", ".xlsx herunterladen")
				)
			)
		} else {
			txt <- NULL
		}
	})
	
	# Conditional Data Download (OGD link)
	output$tagOGD <- renderUI({
		availability <- dataAvailable()
		if(availability>0) {
			req(input$street)
			req(input$number)
			req(input$buttonStartTwo)
			tags$div(
				id = "linkOGD",
				class = "downloadLink",
				icon("database"),
				tags$div(
					id = "downloadLinkOGDText",
					class = "downloadLinkIcon",
					tags$a(
						class = "downloadLinkOGD",
						href = "https://data.stadt-zuerich.ch/dataset?tags=lima",
						target="_blank",
						"im OGD-Portal herunterladen"
					)
				)
			)
		} else {
			txt <- NULL
		}
	})
	
	
	### Change Action Query Button when first selected
	## App 1
	observe({
		req(input$buttonStart)
		updateActionButton(session, "buttonStart",
		                   label = "Erneute Abfrage",
		                   icon = icon("refresh"))
	})
	
	## App 2
	observe({
		req(input$buttonStartTwo)
		updateActionButton(session, "buttonStartTwo",
		                   label = "Erneute Abfrage",
		                   icon = icon("refresh"))
	})
}

### Run the App
shinyApp(ui = ui, server = server)
