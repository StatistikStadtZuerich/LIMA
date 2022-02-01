### Libraries
library(shiny)
library(knitr)
library(kableExtra)
library(ggplot2)
library(dplyr)
library(shinyjs)
library(dqshiny)
library(gtools)
library(xlsx)
library(RCurl)

### Data
source("DataLoad.R", local = TRUE)

#GUI
ui <- fluidPage(
	
	
	#CSS
	includeCSS("sszTheme.css"),
	
	#Title
	titlePanel("Abfragetool LIMA"),
	br(),
	p("Durch Benutzen der Applikation stimmen Sie dem Disclaimer zu. (Link zu html-Seite mit Nutzungshinweisen). Weitere Informationen (Link zu html-Seite mit zusätzlichen Informationen). Sie haben die Auswahl zwischen zwei Datenabfragen."),
	br(),
	radioButtons(inputId="query", 
							 label="Wählen Sie eine Abfrage", 
							 choices=c("Abfrage 1: Zeitreihen nach Bauzonen für ganze Stadt und Teilgebiete",
							 					"Abfrage 2: Zeitreihen für Quartiere und Bauzonen über Adresseingabe"),
							 character(0)),
	br(),
	conditionalPanel(
		condition = 'input.query == "Abfrage 1: Zeitreihen nach Bauzonen für ganze Stadt und Teilgebiete"',
		sidebarLayout(
			sidebarPanel(
				selectInput("area", 
										"Gebietsauswahl",
										choices = c(unique(zones$Gebiet_n))
				),
				radioButtons("price", 
										 "Preise",
										 choices = c(unique(zones$PreisreiheLang))
				),
				conditionalPanel(
					condition = 'input.price != "Stockwerkeigentum pro m2 Wohnungsfläche"',
					radioButtons("group", 
											 "Art",
											 choices = c("Nur ganze Liegenschaften",
											 						"Nur Stockwerkeigentum",
											 						"Alle Verkäufe")
					),
				),
				actionButton("buttonStart", 
										 "Abfrage starten", 
										 icon = icon("database")),
				br(),
				conditionalPanel(
					condition = 'input.buttonStart',
					h5("Daten herunterladen"),
					tags$div(
						id = "downloadLinkCSV",
						class = "downloadLink",
						icon("file-csv"),
						tags$div(
							id = "downloadLinkCSVText",
							class = "downloadLinkText",
							downloadLink("downloadDataCSV", ".csv herunterladen")
						)
					),
					tags$div(
						id = "downloadLinkEXCEL",
						class = "downloadLink",
						icon("file-excel"),
						tags$div(
							id = "downloadLinkEXCELText",
							class = "downloadLinkText",
							downloadLink("downloadDataEXCEL", ".xlsx herunterladen")
						)
					),
					tags$div(
						id = "linkOGD",
						class = "downloadLink",
						icon("database"),
						tags$div(
							id = "downloadLinkOGDText",
							class = "downloadLinkText",
							tags$a(
								href = "https://data.stadt-zuerich.ch/",
								"im OGD-Portal herunterladen"
							)
						)
					),
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
			mainPanel(
				tags$div(
					id = "title_id",
					class = "title_div",
					textOutput("title")
				),
				tags$div(
					id = "subtitle_id",
					class = "subtitle_div",
					textOutput("subtitle")
				),
				tags$div(
					id = "subSubtitle_id",
					class = "subSubtitle_div",
					textOutput("subSubtitle")
				),
				tags$div(
					id = "tableTitle16_id",
					class = "tableTitle_div",
					textOutput("tableTitle16")
				),
				htmlOutput("resultsPrice16"),
				tags$div(
					id = "tableTitle99_id",
					class = "tableTitle_div",
					textOutput("tableTitle99")
				),
				htmlOutput("resultsPrice99"),
				useShinyjs(),
				conditionalPanel(
					condition = 'input.buttonStart',
					actionLink("linkCount",
										 "Anzahl Handänderungen einblenden", 
										 icon = icon("angle-down"),
										 style='font-size:12px')
				),
				shinyjs::hidden(
					div(id='countDiv',
							tags$div(
								id = "tableTitleTwo16_id",
								class = "tableTitle_div",
								textOutput("tableTitleTwo16")
							),
							htmlOutput("resultsCount16"),
							tags$div(
								id = "tableTitleTwo99_id",
								class = "tableTitle_div",
								textOutput("tableTitleTwo99")
							),
							htmlOutput("resultsCount99")
					)
				)
			)
		)
	),
	conditionalPanel(
		condition = 'input.query == "Abfrage 2: Zeitreihen für Quartiere und Bauzonen über Adresseingabe" ',
		sidebarLayout(
			sidebarPanel(
				autocomplete_input("street", 
													 "Geben Sie eine Strasse ein", 
													 unique(addresses$Strasse)),
				selectizeInput("number", 
											 "Wählen Sie eine Hausnummer aus", 
											 choices = c("",sort(unique(addresses$Hausnummer))),
											 selected = NULL),
				actionButton("buttonStartTwo", 
										 "Abfrage starten", 
										 icon = icon("database")),
				br(),
				conditionalPanel(
					condition = 'input.street && input.number && input.buttonStartTwo',
					h5("Daten herunterladen"),
					tags$div(
						id = "downloadLinkCSV",
						class = "downloadLink",
						icon("file-csv"),
						tags$div(
							id = "downloadLinkCSVText",
							class = "downloadLinkText",
							downloadLink("downloadDataCSVTwo", ".csv herunterladen")
						)
					),
					tags$div(
						id = "downloadLinkEXCEL",
						class = "downloadLink",
						icon("file-excel"),
						tags$div(
							id = "downloadLinkEXCELText",
							class = "downloadLinkText",
							downloadLink("downloadDataEXCELTwo", ".xlsx herunterladen")
						)
					),
					tags$div(
						id = "linkOGD",
						class = "downloadLink",
						icon("database"),
						tags$div(
							id = "downloadLinkOGDText",
							class = "downloadLinkText",
							tags$a(
								href = "https://data.stadt-zuerich.ch/",
								"im OGD-Portal herunterladen"
							)
						)
					)
				)
			),
			mainPanel(
				br(),
				htmlOutput("resultsInfos"),
				tags$div(
					id = "info_id",
					class = "info_div",
					textOutput("info")
				),
				br(),
				htmlOutput("resultsPriceSeries"),
				useShinyjs(),
				conditionalPanel(
					condition = 'input.buttonStartTwo',
					actionLink("linkCountTwo",
										 "Anzahl Handänderungen einblenden", 
										 icon = icon("angle-down"),
										 style='font-size:12px')
				),
				shinyjs::hidden(
					div(id='countDivTwo',
							htmlOutput("resultsCountSeries")
					)
				)
			)
		)
	)
)

#Server
server <- function(input, output, session) {
	
	#Get Data for Download
	#App 1
	dataDownload <- eventReactive(input$buttonStart, {
		if(input$price == "Stockwerkeigentum pro m2 Wohnungsfläche") {
			filtered <- zones %>%
				filter(Gebiet_n == input$area,
							 PreisreiheLang == input$price) %>% 
				select(Typ, Gebiet_n, PreisreiheLang, ArtLang, BZO, Jahr, ALLE, ZE, KE, QU, W2, W23, W34, W45, W56)
			filtered
		} else {
			filtered <- zones %>%
				filter(Gebiet_n == input$area,
							 PreisreiheLang == input$price,
							 ArtLang == input$group) %>% 
				select(Typ, Gebiet_n, PreisreiheLang, ArtLang, BZO, Jahr, ALLE, ZE, KE, QU, W2, W23, W34, W45, W56)
			filtered
		}
	})
	
	#App 2
	dataDownloadTwo <- eventReactive(input$buttonStartTwo, {
		
		#Pull district
		district <- addresses %>%
			filter(Strasse == input$street & Hausnummer == input$number) %>% 
			pull(QuarLang)
		
		#Pull zone BZO16
		zoneBZO16 <- addresses %>%
			filter(Strasse == input$street & Hausnummer == input$number) %>% 
			pull(ZoneBZO16)
		
		#Pull zone BZO99
		zoneBZO99 <- addresses %>%
			filter(Strasse == input$street & Hausnummer == input$number) %>% 
			pull(ZoneBZO99)
		
		#Serie BZO16
		serieBZO16 <- series %>% 
			filter(Quartier_N == district & Zone_N == zoneBZO16,
						 Jahr >= 2019) 
		
		#Serie BZO99
		serieBZO99 <- series %>% 
			filter(Quartier_N == district & Zone_N == zoneBZO99,
						 Jahr < 2019) 
		
		#Total series
		seriesPriceCount <- bind_rows(serieBZO16, serieBZO99) %>% 
			select(-Quartier_C, -Zone_S, -Zone_N) %>% 
			arrange(factor(Typ, levels = c("Preis",
																		 "Menge")), 
							desc(Jahr))
		seriesPriceCount
	})
	
	#Tables Output
	#App 1
	#Get Data for Output Prices
	#BZO16
	priceOutput16 <- eventReactive(input$buttonStart, {
		if(input$price == "Stockwerkeigentum pro m2 Wohnungsfläche") {
			filtered <- zonesBZO16 %>%
				filter(Typ == "Preis",
							 Gebiet_n == input$area,
							 PreisreiheLang == input$price) %>% 
				select(Jahr, Total, Z, K, Q, W2, W3, W4, W5, W6) %>% 
				mutate_all(., ~replace(., is.na(.), " ")) %>% 
				kable("html",
							align = c("c")) %>% 
				kable_styling(bootstrap_options = c("hover", "condensed")) %>% 
				row_spec(0, font_size = 10) %>% 
				column_spec(1:10, width = "10%")
			filtered
		} else {
			filtered <- zonesBZO16 %>%
				filter(Typ == "Preis",
							 Gebiet_n == input$area,
							 PreisreiheLang == input$price,
							 ArtLang == input$group) %>% 
				select(Jahr, Total, Z, K, Q, W2, W3, W4, W5, W6) %>% 
				mutate_all(., ~replace(., is.na(.), " ")) %>% 
				kable("html",
							align = c("c")) %>% 
				kable_styling(bootstrap_options = c("hover", "condensed")) %>% 
				row_spec(0, font_size = 10) %>% 
				column_spec(1:10, width = "10%")
			filtered
		}
	})
	
	#BZO99
	priceOutput99 <- eventReactive(input$buttonStart, {
		if(input$price == "Stockwerkeigentum pro m2 Wohnungsfläche") {
			filtered <- zonesBZO99 %>%
				filter(Typ == "Preis",
							 Gebiet_n == input$area,
							 PreisreiheLang == input$price) %>% 
				select(Jahr, Total, Z, K, Q, ` `,W2, W3, W4, W5) %>% 
				mutate_all(., ~replace(., is.na(.), " ")) %>% 
				kable("html",
							align = c("c")) %>% 
				kable_styling(bootstrap_options = c("hover", "condensed")) %>% 
				row_spec(0, font_size = 10) %>% 
				column_spec(1:10, width = "10%")
			filtered 
		} else {
			filtered <- zonesBZO99 %>%
				filter(Typ == "Preis",
							 Gebiet_n == input$area,
							 PreisreiheLang == input$price,
							 ArtLang == input$group) %>% 
				select(Jahr, Total, Z, K, Q, ` `,W2, W3, W4, W5) %>% 
				mutate_all(., ~replace(., is.na(.), " ")) %>% 
				kable("html",
							align = c("c")) %>% 
				kable_styling(bootstrap_options = c("hover", "condensed")) %>% 
				row_spec(0, font_size = 10) %>% 
				column_spec(1:10, width = "10%")
			filtered
		}
	})
	
	#Get Data for Output Counts
	#BZO16
	countOutput16 <- eventReactive(input$buttonStart, {
		if(input$price == "Stockwerkeigentum pro m2 Wohnungsfläche") {
			filtered <- zonesBZO16 %>%
				filter(Typ == "Menge",
							 Gebiet_n == input$area,
							 PreisreiheLang == input$price) %>% 
				select(Jahr, Total, Z, K, Q, W2, W3, W4, W5, W6) %>% 
				mutate_all(., ~replace(., is.na(.), " ")) %>% 
				kable("html",
							align = c("c")) %>% 
				kable_styling(bootstrap_options = c("hover", "condensed")) %>% 
				row_spec(0, font_size = 10) %>% 
				column_spec(1:10, width = "10%")
			filtered
		} else {
			filtered <- zonesBZO16 %>%
				filter(Typ == "Menge",
							 Gebiet_n == input$area,
							 PreisreiheLang == input$price,
							 ArtLang == input$group) %>% 
				select(Jahr, Total, Z, K, Q, W2, W3, W4, W5, W6) %>% 
				mutate_all(., ~replace(., is.na(.), " ")) %>% 
				kable("html",
							align = c("c")) %>% 
				kable_styling(bootstrap_options = c("hover", "condensed")) %>% 
				row_spec(0, font_size = 10) %>% 
				column_spec(1:10, width = "10%")
			filtered
		}
	})
	
	#BZO99
	countOutput99 <- eventReactive(input$buttonStart, {
		if(input$price == "Stockwerkeigentum pro m2 Wohnungsfläche") {
			filtered <- zonesBZO99 %>%
				filter(Typ == "Menge",
							 Gebiet_n == input$area,
							 PreisreiheLang == input$price) %>% 
				select(Jahr, Total, Z, K, Q, ` `,W2, W3, W4, W5) %>% 
				mutate_all(., ~replace(., is.na(.), " ")) %>% 
				kable("html",
							align = c("c")) %>% 
				kable_styling(bootstrap_options = c("hover", "condensed")) %>% 
				row_spec(0, font_size = 10) %>% 
				column_spec(1:10, width = "10%")
			filtered
		} else {
			filtered <- zonesBZO99 %>%
				filter(Typ == "Menge",
							 Gebiet_n == input$area,
							 PreisreiheLang == input$price,
							 ArtLang == input$group) %>% 
				select(Jahr, Total, Z, K, Q, ` `,W2, W3, W4, W5) %>% 
				mutate_all(., ~replace(., is.na(.), " ")) %>% 
				kable("html",
							align = c("c")) %>% 
				kable_styling(bootstrap_options = c("hover", "condensed")) %>% 
				row_spec(0, font_size = 10) %>% 
				column_spec(1:10, width = "10%")
			filtered
		}
	})
	
	#Show Output Prices (App 1)
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
	
	#Show Output Counts (App 1)
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
	
	
	#App 2
	#Show Output Info (App 2)
	#Sort for House Number in Drop Down
	observe({
		updateSelectInput(
			session, "number",
			choices = c("",mixedsort(filter(addresses, Strasse == input$street)[,2]))
		)
	})
	
	#Get Information of Address
	infosReactive <- eventReactive(input$buttonStartTwo, {
		req(input$street)
		req(input$number)
		infosFiltered <- addresses %>%
			filter(Strasse == input$street & Hausnummer == input$number) %>% 
			mutate(Adresse = paste0(Strasse, " ", Hausnummer)) %>% 
			select(Adresse, QuarLang, Zones) %>% 
			mutate(pivot = 1) %>% 
			pivot_longer(!pivot) %>% 
			mutate(name = case_when(name == "Adresse" ~ "Die Adresse",
															name == "QuarLang" ~ "liegt im Quartier",
															name == "Zones" ~ "in folgender Zone")) %>% 
			select(-pivot) %>% 
			kable("html",
						align = "lr",
						col.names = NULL) %>% 
			kable_styling(bootstrap_options = c("condensed")) %>% 
			column_spec(2, bold = TRUE)
		infosFiltered
	})
	
	#Show Output Information Address
	observeEvent(input$buttonStartTwo, {
		output$resultsInfos <- renderText({
			outInfos <- infosReactive() 
			outInfos
		})
	})
	
	#Reactive Info
	infoReactive <- eventReactive(input$buttonStartTwo, {
		req(input$street)
		req(input$number)
		infoTitle <- paste0("In dieser Zone dieses Quartiers wurden folgende Medianpreise festgestellt")
	})
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
			filter(Strasse == input$street & Hausnummer == input$number) %>% 
			pull(QuarLang)
		
		#Pull zone BZO16
		zoneBZO16 <- addresses %>%
			filter(Strasse == input$street & Hausnummer == input$number) %>% 
			pull(ZoneBZO16)
		
		#Pull zone BZO99
		zoneBZO99 <- addresses %>%
			filter(Strasse == input$street & Hausnummer == input$number) %>% 
			pull(ZoneBZO99)
		
		#Price serie BZO16
		priceSerieBZO16 <- series %>% 
			filter(Quartier_N == district & Zone_N == zoneBZO16,
						 Typ == "Preis",
						 Jahr >= 2019) 
		
		#Price serie BZO99
		priceSerieBZO99 <- series %>% 
			filter(Quartier_N == district & Zone_N == zoneBZO99,
						 Typ == "Preis",
						 Jahr < 2019) 
		
		#Total series
		priceSerieTotal <- bind_rows(priceSerieBZO16, priceSerieBZO99) %>% 
			select(-Typ, -Quartier_C, -Quartier_N, -Zone_S, -Zone_N)
		
		priceDistZone <- priceSerieTotal %>% 
			kable("html",
						col.names = c("Jahr",
													"Ganze\nLiegenschaft",
													"Stockwerkeigentum",
													"Beides",
													"Ganze\nLiegenschaft",
													"Stockwerkeigentum",
													"Beides",
													" "),
						align = c("c")) %>% 
			add_header_above(c(" ",
												 "Gesamtpreise pro m2 Boden" = 3,
												 "Gesamtpreise pro m2 Boden abzgl. Gebäudeversicherungswert" = 3,
												 "Stockwerkeigentum pro m2 Wohnungsfläche (alle Zonen)"),
											 font_size = 10,
											 color = "#0F05A0",
											 align = c("c")) %>% 
			kable_styling(bootstrap_options = c("hover", "condensed")) %>% 
			row_spec(0, font_size = 10)
		priceDistZone
	})
	
	#Get Data for Output Counts (District-Price-Combination)
	distReactiveCount <- eventReactive(input$buttonStartTwo, {
		req(input$street)
		req(input$number)
		
		#Pull district
		district <- addresses %>%
			filter(Strasse == input$street & Hausnummer == input$number) %>% 
			pull(QuarLang)
		
		#Pull zone BZO16
		zoneBZO16 <- addresses %>%
			filter(Strasse == input$street & Hausnummer == input$number) %>% 
			pull(ZoneBZO16)
		
		#Pull zone BZO99
		zoneBZO99 <- addresses %>%
			filter(Strasse == input$street & Hausnummer == input$number) %>% 
			pull(ZoneBZO99)
		
		#Price serie BZO16
		countSerieBZO16 <- series %>% 
			filter(Quartier_N == district & Zone_N == zoneBZO16,
						 Typ == "Menge",
						 Jahr >= 2019) 
		
		#Price serie BZO99
		countSerieBZO99 <- series %>% 
			filter(Quartier_N == district & Zone_N == zoneBZO99,
						 Typ == "Menge",
						 Jahr < 2019) 
		
		#Total series
		countSerieTotal <- bind_rows(countSerieBZO16, countSerieBZO99) %>% 
			select(-Typ, -Quartier_C, -Quartier_N, -Zone_S, -Zone_N)
		
		countDistZone <- countSerieTotal %>% 
			kable("html",
						col.names = c("Jahr",
													"Ganze\nLiegenschaft",
													"Stockwerkeigentum",
													"Beides",
													"Ganze\nLiegenschaft",
													"Stockwerkeigentum",
													"Beides",
													" "),
						align = c("c")) %>% 
			add_header_above(c(" ",
												 "Gesamtpreise pro m2 Boden" = 3,
												 "Gesamtpreise pro m2 Boden abzgl. Gebäudeversicherungswert" = 3,
												 "Stockwerkeigentum pro m2 Wohnungsfläche (alle Zonen)"),
											 font_size = 10,
											 color = "#0F05A0",
											 align = c("c")) %>% 
			kable_styling(bootstrap_options = c("hover", "condensed")) %>% 
			row_spec(0, font_size = 10)
		
		countDistZone
	})
	
	#Show Output Prices
	observeEvent(input$buttonStartTwo, {
		output$resultsPriceSeries <- renderText({
			outPriceSeries <- distReactivePrice()
			outPriceSeries
		})
	})
	
	#Show Output Counts
	observeEvent(input$linkCountTwo, {
		shinyjs::toggle('countDivTwo')
		output$resultsCountSeries <- renderText({
			outCountSeries <- distReactiveCount()
			outCountSeries
		})
		if(input$linkCountTwo %% 2 == 1) {
			txt <- "Anzahl Handänderungen verbergen"
			updateActionLink(session, "linkCountTwo", label = txt, icon = icon("angle-up"))
		} else {
			txt <- "Anzahl Handänderungen einblenden"
			updateActionLink(session, "linkCountTwo", label = txt, icon = icon("angle-down"))
		}
	})
	
	#Write Download Table
	#App 1
	#CSV
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
	
	#Excel
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
	
	#App 2
	#CSV
	output$downloadDataCSVTwo <- downloadHandler(
		filename = function() {
			district <- addresses %>%
				filter(Strasse == input$street & Hausnummer == input$number) %>% 
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
				filter(Strasse == input$street & Hausnummer == input$number) %>% 
				pull(QuarLang)
			paste0("Liegenschaftenhandel_nach_Bauzonenordnung_und_Quartier_", district, ".xlsx")
		}, 
		content = function(file) {
			write.xlsx(dataDownloadTwo(), file, row.names = FALSE, showNA = FALSE)
		}
	)
	
	#Change Action Query Button
	#App 1
	observe({
		req(input$buttonStart)
		updateActionButton(session, "buttonStart",
											 label = "Erneute Abfrage",
											 icon = icon("refresh"))
	})
	
	#App 2
	observe({
		req(input$buttonStartTwo)
		updateActionButton(session, "buttonStartTwo",
											 label = "Erneute Abfrage",
											 icon = icon("refresh"))
	})
}

#Run the App
shinyApp(ui = ui, server = server)
