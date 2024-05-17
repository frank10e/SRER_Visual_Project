

server = function(input, output) {
 
  output$test_print <- renderPrint({
    
    # selected_year_data()
    !is.null(map_single_data())
    # map_single_data()
    
  })
  
  
  
  output$mPlot <- renderPlot({
    withProgress(message = 'Analysising',
                 detail = 'This may take a while...', value = 0, {
                   for (i in 1:1) {
                     incProgress(1/15)
                     Sys.sleep(0.07)
                   }
                 })
    
    tryCatch({
      # Plot the result with Transect Name
      ggplot() +
        geom_sf(data = result, aes(fill = "Transect Name")) +
        theme_minimal() +
        scale_fill_viridis(discrete = TRUE) +
        labs(fill = "Transect Name") +
        geom_point(data = full_year_data,mapping=
                     aes(color = `% Use`, x = `UTM Center X`, y =`UTM Center Y` )
                   
        ) +
        # geom_point(data = test_srer1,mapping=
        #              aes(color = `% Use`, geometry = geometry),
        #            stat = "sf_coordinates"
        # )+
        scale_color_viridis_c(option = "C")+
        guides(fill=FALSE)
    },error=function(e){
      
    })
  })
  
  
  # Reactively get the selected year's data

  
 
  

  
 
  
  # Reactively get the selected transect's data
  selected_transect_data <- reactive({
   
    test_srer_2 %>% 
      filter(Year == as.numeric(input$selectYear), Transect_Name == input$selectTransectName) %>% 
      mutate(Average_Bins = cut(Average, breaks = c(-Inf, 5, 20, 40, 60, Inf), labels = color_labels_3))
  })
  
  # Prepare the label data for the selected transect
  label_data_transect <- reactive({
     
    selected_transect_data() %>% 
      st_centroid() %>% 
      st_as_sf() %>% 
      cbind(st_coordinates(.), Average = .$Average)
  })
  # 加入一个加载过程，这是第一个加载1/10 
  # Display the map for the selected year

  
  
  selected_year_data <- reactive({
    
    df1 <- test_srer_2 %>% 
      filter(Year == as.numeric(input$selectYear)) %>%
      mutate(Average_Bins = cut(Average, breaks = c(-Inf, 5, 20, 40, 60, Inf), labels = color_labels_2),
             Label = paste(Transect_Name, round(Average, 1), sep = ": "))
    
    df2 <-  df1 %>% 
      st_centroid() %>% 
      st_as_sf() %>% 
      cbind(st_coordinates(.), Average = .$Average) %>% 
      filter(Average!=0) %>%
      mutate(Label = paste(Transect_Name, round(Average, 1), sep = ": "))
      
    
    df <- list(df1=df1,
               df2=df2)
    
    return(df)
  })%>%
    bindCache(input$selectYear)%>%
    bindEvent(input$runYear)
  
  
 
  
  
  
  output$mapPlot <- renderPlot({
    withProgress(message = 'ploting',
                 detail = 'This may take a while...', value = 0, {
                   for (i in 1:1) {
                     incProgress(1/15)
                     Sys.sleep(0.07)
                   }
                 })
    
    df <- selected_year_data()
    
    
    ggplot() +
      geom_sf(data = df$df1, aes(fill = Average_Bins)) +
      geom_text(data = df$df2, aes(x = X, y = Y, label = Label), size = 3, check_overlap = TRUE) +
      labs(title = paste("Map for Year", input$selectYear), fill = "T") +
      theme_minimal() +
      scale_fill_manual(values = color_palette_2, drop = FALSE) +
      guides(fill = guide_legend(title = "Average % Use"))
     
 
  })
  
  
  # Reactively get the average data of past three years
  avg_three_years_data <- reactive({
    
    df1 <- test_srer_2 %>% 
      filter(Year >= (as.numeric(input$selectYear) - 2) & Year <= as.numeric(input$selectYear))%>%
      group_by(Transect_Name) %>%
      summarise(Average = mean(Average, na.rm = TRUE)) %>%
      mutate(Average_Bins = cut(Average, breaks = c(-Inf, 5, 20, 40, 60, Inf), labels = color_labels_3),
             Label = paste(Transect_Name, round(Average, 1), sep = ": "))
    
    df2 <-  df1 %>% 
      st_centroid() %>% 
      st_as_sf() %>% 
      cbind(st_coordinates(.), Average = .$Average) %>% 
      filter(Average!=0) %>%
      mutate(Label = paste(Transect_Name, round(Average, 1), sep = ": "))
    
    
    
    df <- list(df1=df1,
               df2=df2)
    
    return(df)
  })%>%
    bindCache(input$selectYear)%>%
    bindEvent(input$runYear)
  
  
  
  
  # 加入一个加载过程，这是第二个加载2/10 
  # Display the map for the average of past three years
  output$avgMapPlot <- renderPlot({
    withProgress(message = 'ploting',
                 detail = 'This may take a while...', value = 0, {
                   for (i in 1:1) {
                     incProgress(1/15)
                     Sys.sleep(0.07)
                   }
                 })
  
    df <- avg_three_years_data()
    tryCatch({
      ggplot() +
        geom_sf(data = df$df1, aes(fill = Average_Bins)) +
        geom_text(data = df$df2, aes(x = X, y = Y, label = Label), size = 3, check_overlap = TRUE) +
        labs(title = paste("Average Map for Past Three Years from", as.numeric(input$selectYear) - 2, "to", input$selectYear), fill = "T") +
        theme_minimal() +
        scale_fill_manual(values = color_palette_2, drop = FALSE) +
        guides(fill = guide_legend(title = "Average % Use"))
    },error=function(e){
      
    })
  })
  
 
 
  
  map_single_data <- reactive({
    
    tryCatch(
      { 
    
    # selected_pasture <- unique(santa_rita_data$Pasture)[1] 
    # selected_year <- as.integer(year_choices[1])
    
    
    selected_pasture <- input$selectPasture
    selected_year <- as.integer(input$selectYear)
    
    
    
    if(st_crs(santa_rita_1) != st_crs(santa_rita_data_plot)) {santa_rita_data_plot <- st_transform(santa_rita_data_plot, st_crs(santa_rita_1))}
    SRER_Pastures <- st_join(santa_rita_1, santa_rita_data_plot)
    SRER_Pastures <- SRER_Pastures[!is.na(SRER_Pastures$Pasture),]
    SRER_Pastures <- SRER_Pastures %>% 
      group_by(Pasture, geometry = st_drop_geometry(.)) %>% 
      summarize()
    
    
    # transect_names <- get_transect_names_for_pasture(santa_rita_data, selected_pasture)
    transect_names <- unique(santa_rita_data[santa_rita_data$Pasture %in% selected_pasture, "Transect Name"][[1]])
   
    
    # transect_names <- get_transect_names_for_pasture(data, pasture_name)
    
    column_name <- paste0("% Use ", selected_year)
    specific_transects_values <- santa_rita_data[santa_rita_data$`Transect Name` %in% transect_names, ][[column_name]]
 
    
    
    
    specific_transects <- santa_rita_data_plot[santa_rita_data_plot$`Transect Name` %in% transect_names, ]
    
    specific_transects_values <- specific_transects_values[1:nrow(specific_transects)]
    specific_transects$Value <- specific_transects_values
    
    
    
    specific_transects <- santa_rita_data_plot[santa_rita_data_plot$`Transect Name` %in% transect_names, ]
    if(length(specific_transects_values) != nrow(specific_transects)) {stop("Length of values does not match the data frame.")}
    

   
    
    # 加入一个加载过程，这是第四个加载4/10 
    specific_transects$Value <- specific_transects_values
    grd_poly <- st_make_grid(santa_rita_data_plot, cellsize = 50000)
    grd <- st_sample(st_zm(grd_poly), size = 25000, type = "regular")
    grd_sf <- st_as_sf(grd, crs = st_crs(santa_rita_data_plot))
    
 
    
    specific_transects <- st_zm(specific_transects, drop = TRUE, what = "ZM")
    # If the data set is 0, it will return to 0
    specific_transects$Value[is.na(specific_transects$Value)] <- 0

   
      specific_transects_sp <- as(specific_transects, "Spatial")
      
      grd_sp <- as(grd_sf, "Spatial")
      
    
    proj4string(grd_sp) <- proj4string(specific_transects_sp)
    grid_spacing <- 10
    bbox <- st_bbox(santa_rita_data_plot)
    grd <- expand.grid(x = seq(bbox["xmin"] - 1000, bbox["xmax"] + 1000, by = grid_spacing),
                       y = seq(bbox["ymin"] - 1000, bbox["ymax"] + 1000, by = grid_spacing))
    coordinates(grd) <- ~x+y
    gridded(grd) <- TRUE
    proj4string(grd) <- proj4string(specific_transects_sp)
    idw_result <- idw(Value~1, locations = specific_transects_sp, newdata = grd,idp = 2.0)
    r <- raster(idw_result)
    r_clipped <- crop(r, extent(santa_rita_data_plot) + 10000)
    SRER_Pastures_polygons <- st_buffer(SRER_Pastures, dist = 10) 
    specific_transects_polygons <- st_buffer(specific_transects, dist = 10)
    SRER_Selected_Pasture <- SRER_Pastures[SRER_Pastures$Pasture == selected_pasture, ]
    SRER_Selected_Pasture <- st_zm(SRER_Selected_Pasture, drop=TRUE, what="ZM")
   
    
    r_mask <- rasterize(SRER_Selected_Pasture, r)
    r_mask_cropped <- crop(r_mask, extent(r_clipped))
    r_clipped_masked <- mask(r_clipped, r_mask_cropped)
    (r_clipped_masked+r_clipped_masked+r_clipped_masked)/3
    breaks <- c(0,5,20,40,60,100)
    colors <- c("white", "#7484D2", "#D1D1D1", "#F5CF86","#E60000")
    # Adds a new column that combines Value and Transect Name into a single string
    specific_transects$Label <- as.character(round(specific_transects$Value, 1))
     
    
    df <- list(
      r_clipped_masked=r_clipped_masked,
      SRER_Selected_Pasture=SRER_Selected_Pasture,
      breaks=breaks,
      colors=colors,
      specific_transects=specific_transects,
      specific_transects_polygons=specific_transects_polygons 
    )
    
      }, 
    error = function(e)  NULL, 
    finally = )
    
    
    return(df)
  
  }) %>%
    bindCache(input$selectYear,input$selectPasture)%>%
    bindEvent(input$runPasture)
  
  
  output$map_single <- renderTmap({
    # req(map_single_data())
    
    withProgress(message = 'ploting',
                 detail = 'This may take a while...', value = 0, {
                   for (i in 1:15) {
                     incProgress(1/15)
                     Sys.sleep(0.07)
                   }
                 })
     
    tryCatch({  
      tm_shape(map_single_data()$r_clipped_masked, 
                  bbox=raster::extent(st_sf(map_single_data()$SRER_Selected_Pasture[3,1][[2]]))+100) +
        tm_raster(n=5, breaks=map_single_data()$breaks, palette=map_single_data()$colors, title="Percent Use (%)") +
        tm_shape(map_single_data()$SRER_Selected_Pasture) +
        tm_borders(lwd = 1.5, col = "black") +
        tm_shape(map_single_data()$specific_transects) +
        tm_text("Label", size = 1.5, col = "black", bg.color = "white", auto.placement = TRUE) +
        tm_shape(map_single_data()$specific_transects_polygons) +
        tm_polygons(border.col = "black", lwd = 1.5) +
        tm_legend(legend.outside=TRUE)
    }
      , 
      error = function(e)  {
        # print("ddd")
        tm_shape(World) +
          tm_polygons()
        }, 
      finally = {
         
      } )
   
  })
  
 
  
  map_three_data <- reactive({
    
    tryCatch(
      { 
        selected_pasture <- input$selectPasture
        selected_year <- as.integer(input$selectYear)
        if(selected_year <= 2011) {
          
          runjs('document.getElementById("progressBar").style.width = "100%"; document.getElementById("progressBar").textContent = "100%";')
          
          showNotification("Three average data is not exist", type = "error")
          shinyjs::delay(1000, {
            shinyjs::hide("progressContainer")
          })
          return(NULL)
        }
        years_to_average <- (selected_year-2):selected_year
        santa_rita_data$ThreeYearAverage <- rowMeans(santa_rita_data[paste0("% Use ", years_to_average)], na.rm = TRUE)
        print("Check the data before defining specific_transects:")
        
        if(st_crs(santa_rita_1) != st_crs(santa_rita_data_plot)) {
          santa_rita_data_plot <- st_transform(santa_rita_data_plot, st_crs(santa_rita_1))}
        SRER_Pastures <- st_join(santa_rita_1, santa_rita_data_plot)
        SRER_Pastures <- SRER_Pastures[!is.na(SRER_Pastures$Pasture),]
        SRER_Pastures <- SRER_Pastures %>%
          group_by(Pasture, geometry = st_drop_geometry(.)) %>%
          summarize()
        
        
        
        
        # get_average_use_for_three_years <- function(data, pasture_name, years_to_average) {
        #   transect_names <- get_transect_names_for_pasture(data, pasture_name)
        #   data_subset <- data[data$`Transect Name` %in% transect_names, ]
        #   column_names <- paste0("% Use ", years_to_average)
        #   average_values <- rowMeans(data_subset[column_names], na.rm = TRUE)
        #   return(average_values)}
        
        transect_names <- unique(santa_rita_data[santa_rita_data$Pasture %in% selected_pasture, "Transect Name"][[1]])
        
        
        data_subset <- santa_rita_data[santa_rita_data$`Transect Name` %in% transect_names, ]
        column_names <- paste0("% Use ", years_to_average)
        specific_transects_values <- rowMeans(data_subset[column_names], na.rm = TRUE)
        
        
        
        
        specific_transects <- santa_rita_data_plot[santa_rita_data_plot$`Transect Name` %in% transect_names, ]
        
        specific_transects_values <- specific_transects_values[1:nrow(specific_transects)]
        specific_transects$Value <- specific_transects_values
        
        
        
        specific_transects <- santa_rita_data_plot[santa_rita_data_plot$`Transect Name` %in% transect_names, ]
        if(length(specific_transects_values) != nrow(specific_transects)) {stop("Length of values does not match the data frame.")}
        
        
        
        runjs('document.getElementById("progressBar").style.width = "70%"; document.getElementById("progressBar").textContent = "70%";')
        
        
        specific_transects$Value <- specific_transects_values
        grd_poly <- st_make_grid(santa_rita_data_plot, cellsize = 50000)
        grd <- st_sample(st_zm(grd_poly), size = 25000, type = "regular")
        grd_sf <- st_as_sf(grd, crs = st_crs(santa_rita_data_plot))
        length(specific_transects$Value)
        specific_transects <- st_zm(specific_transects, drop = TRUE, what = "ZM")
        # If the data set is 0, it will return to 0
        specific_transects_sp <- as(specific_transects, "Spatial")
        grd_sp <- as(grd_sf, "Spatial")
        proj4string(grd_sp) <- proj4string(specific_transects_sp)
        grid_spacing <- 10
        bbox <- st_bbox(santa_rita_data_plot)
        grd <- expand.grid(x = seq(bbox["xmin"] - 1000, bbox["xmax"] + 1000, by = grid_spacing),
                           y = seq(bbox["ymin"] - 1000, bbox["ymax"] + 1000, by = grid_spacing))
        coordinates(grd) <- ~x+y
        gridded(grd) <- TRUE
        proj4string(grd) <- proj4string(specific_transects_sp)
        
        
        runjs('document.getElementById("progressBar").style.width = "80%"; document.getElementById("progressBar").textContent = "80%";')
        
        
        idw_result <- idw(Value~1, locations = specific_transects_sp, newdata = grd,idp = 2.0)
        r <- raster(idw_result)
        r_clipped <- crop(r, extent(santa_rita_data_plot) + 10000)
        SRER_Pastures_polygons <- st_buffer(SRER_Pastures, dist = 10)
        specific_transects_polygons <- st_buffer(specific_transects, dist = 10)
        SRER_Selected_Pasture <- SRER_Pastures[SRER_Pastures$Pasture == selected_pasture, ]
        SRER_Selected_Pasture <- st_zm(SRER_Selected_Pasture, drop=TRUE, what="ZM")
        
        r_mask <- rasterize(SRER_Selected_Pasture, r)
        r_mask_cropped <- crop(r_mask, extent(r_clipped))
        r_clipped_masked <- mask(r_clipped, r_mask_cropped)
        (r_clipped_masked+r_clipped_masked+r_clipped_masked)/3
        breaks <- c(0,5,20,40,60,100)
        colors <- c("white", "#7484D2", "#D1D1D1", "#F5CF86","#E60000")
        # Adds a new column that combines Value and Transect Name into a single string
        specific_transects$Label <- as.character(round(specific_transects$Value, 1))
        
        
        df <- list(
          r_clipped_masked=r_clipped_masked,
          SRER_Selected_Pasture=SRER_Selected_Pasture,
          breaks=breaks,
          colors=colors,
          specific_transects=specific_transects,
          specific_transects_polygons=specific_transects_polygons 
        )
        
      }, 
      error = function(e)  NULL, 
      finally = )
    
    
    return(df)
    
    
    
  }) %>%
    bindCache(input$selectYear,input$selectPasture)%>%
    bindEvent(input$runPasture)   
  
  
  
  output$map_three <- renderTmap({
     
    tryCatch({ 
      tm_shape(map_three_data()$r_clipped_masked, 
               bbox=raster::extent(st_sf(map_three_data()$SRER_Selected_Pasture[3,1][[2]]))+100) +
        tm_raster(n=5, breaks=map_three_data()$breaks, palette=map_three_data()$colors, title="Average Use percentage (%)") +
        tm_shape(map_three_data()$SRER_Selected_Pasture) +
        tm_borders(lwd = 1.5, col = "black") +
        tm_shape(map_three_data()$specific_transects) +
        tm_text("Label", size = 1.5, col = "black", bg.color = "white", auto.placement = TRUE) +
        tm_shape(map_three_data()$specific_transects_polygons) +
        tm_polygons(border.col = "black", lwd = 1.5) +
        tm_legend(legend.outside=TRUE)
 
    },error=function(e){
      
      tm_shape(World) +
        tm_polygons()

    }, 
    finally ={
       
    }
      
      
    
    )
  })
  
  #################################################################
  #  sidebarPanel
  ################################################################# 
  
  
  
  
  
  output$plot1_sider <- renderUI({
    # If missing input, return to avoid error later in function 
    
    tabPanel(
      "dataview",
      
      fluidRow(
        column(6, 
               selectInput("selectYear", 
                           label = "Select a Year", 
                           choices = year_choices,
                           selected = max(year_choices)) 
        ),
        column(6,
               selectInput("selectPasture", 
                           label = "Select a Pasture", 
                           choices = unique(santa_rita_data$Pasture))
        ),
        
      ),
      fluidRow(
        column(6, 
               actionButton(
                 inputId = "runYear",
                 label = "RUN Year",
                 class = "btn-success"
               )
        ),
        column(6,
               actionButton(
                 inputId = "runPasture",
                 label = "RUN Pasture",
                 class = "btn-success"
               )
        )
      )
    )
  })
  
  output$Navbar1_item2_sider <- renderUI({
    # If missing input, return to avoid error later in function
    tabPanel(
      "dataview",
      # HTML("<strong>数据行（Row）</strong> "),
      sliderInput(
        "datarange_row",
        "Data Range:",
        label = HTML("<strong>数据行<br>（Row）</strong> "),
        min = 1,
        max = 50,
        step = 1,
        value = c(1, 20)
      )
    ) 
  })
  
  output$Navbar2_item1_sider <- renderUI({
    # If missing input, return to avoid error later in function
    tabPanel(
      "dataview",
      # HTML("<strong> Row </strong> "),
      sliderInput(
        "datarange_row",
        "Data Range:",
        label = HTML("<strong> <br>（Row）</strong> "),
        min = 1,
        max = 100,
        step = 1,
        value = c(1, 23)
      )
    ) 
  })
  
  output$Navbar2_item2_sider <- renderUI({
    # If missing input, return to avoid error later in function
    tabPanel(
      "dataview",
      # HTML("<strong>（Row）</strong> "),
      sliderInput(
        "datarange_row",
        "Data Range:",
        label = HTML("<strong> <br>（Row）</strong> "),
        min = 1,
        max = 233,
        step = 1,
        value = c(1, 35)
      )
    ) 
  })
  
  
  output$Navbar31_item1_sider <- renderUI({
    # If missing input, return to avoid error later in function
    tabPanel(
      "dataview",
      # HTML("<strong>（Row）</strong> "),
      sliderInput(
        "datarange_row",
        "Data Range:",
        label = HTML("<strong> <br>（Row）</strong> "),
        min = 1,
        max = 333,
        step = 1,
        value = c(1, 122)
      )
    ) 
  })
  
  
  output$Navbar31_item2_sider <- renderUI({
    # If missing input, return to avoid error later in function
    tabPanel(
      "dataview",
      # HTML("<strong>数据行（Row）</strong> "),
      sliderInput(
        "datarange_row",
        "Data Range:",
        label = HTML("<strong> <br>（Row）</strong> "),
        min = 1,
        max = 122,
        step = 1,
        value = c(1, 42)
      )
    ) 
  })
  
  
  output$Navbar32_item1_sider <- renderUI({
    # If missing input, return to avoid error later in function
    tabPanel(
      "dataview",
      # HTML("<strong>（Row）</strong> "),
      sliderInput(
        "datarange_row",
        "Data Range:",
        label = HTML("<strong> <br>（Row）</strong> "),
        min = 1,
        max = 333,
        step = 1,
        value = c(1, 122)
      )
    ) 
  })
  
  
  output$Navbar32_item2_sider <- renderUI({
    # If missing input, return to avoid error later in function
    tabPanel(
      "dataview",
      # HTML("<strong>（Row）</strong> "),
      sliderInput(
        "datarange_row",
        "Data Range:",
        label = HTML("<strong> <br>（Row）</strong> "),
        min = 1,
        max = 122,
        step = 1,
        value = c(1, 42)
      )
    ) 
  })
  
  
  
  output$Navbar33_item1_sider <- renderUI({
    # If missing input, return to avoid error later in function
    tabPanel(
      "dataview",
      # HTML("<strong>数据行（Row）</strong> "),
      sliderInput(
        "datarange_row",
        "Data Range:",
        label = HTML("<strong> <br>（Row）</strong> "),
        min = 1,
        max = 333,
        step = 1,
        value = c(1, 122)
      )
    ) 
  })
  
  
  output$Navbar33_item2_sider <- renderUI({
    # If missing input, return to avoid error later in function
    tabPanel(
      "dataview",
      # HTML("<strong>（Row）</strong> "),
      sliderInput(
        "datarange_row",
        "Data Range:",
        label = HTML("<strong> <br>（Row）</strong> "),
        min = 1,
        max = 122,
        step = 1,
        value = c(1, 42)
      )
    ) 
  })
  
  
  
  ################################################################# 
  # main
  #################################################################
  
  
  
  output$print_Navbar1_item1 <- renderPrint({ 
   
      print("print_Navbar1_item1") 
    
  })
  
  
  output$print_Navbar1_item2 <- renderPrint({ 
    
    print("print_Navbar1_item2") 
    
  })
  
  
  output$print_Navbar2_item1 <- renderPrint({ 
    
    print("print_Navbar2_item1") 
    
  })
  
  
  output$print_Navbar2_item2 <- renderPrint({ 
    
    print("print_Navbar2_item2") 
    
  })
  
  
  output$print_Navbar31_item1 <- renderPrint({ 
    
    print("print_Navbar31_item1") 
    
  })
  
  output$print_Navbar31_item2 <- renderPrint({ 
    
    print("print_Navbar31_item2") 
    
  })
  
  output$print_Navbar32_item1 <- renderPrint({ 
    
    print("print_Navbar32_item1") 
    
  })
 
  output$print_Navbar32_item2 <- renderPrint({ 
    
    print("print_Navbar32_item2") 
    
  })
  
  output$print_Navbar33_item1 <- renderPrint({ 
    
    print("print_Navbar33_item1") 
    
  })
  
  output$print_Navbar33_item2 <- renderPrint({ 
    
    print("print_Navbar33_item2") 
    
  })
  
  
  
  ################################################################# 
  # end
  #################################################################
  
}  #server
