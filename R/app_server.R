#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import leaflet
#' @import retirementData
#' @importFrom ggplot2 ggplot aes_string geom_point
#'
#' @noRd
app_server <- function(input, output, session) {
    # Your application server logic
    filtered <- reactive({
        retirementLoc |>
            # grouping variables
            dplyr::filter(state %in% input$state) |>
            dplyr::filter(cbsa_desig %in% input$cbsa_desig) |>
            dplyr::filter(rucc_2013 %in% input$rucc_2013) |>
            # demographic
            dplyr::filter(pop_2020 >= input$pop_2020[1] & pop_2020 <= input$pop_2020[2]) |>
            dplyr::filter(pct_pop_change >= input$pct_pop_change[1] & pct_pop_change <= input$pct_pop_change[2]) |>
            dplyr::filter(partisan_lean >= input$partisan_lean[1] & partisan_lean <= input$partisan_lean[2]) |>
            dplyr::filter(broadband_2017 >= input$broadband_2017[1] & broadband_2017 <= input$broadband_2017[2]) |>
            dplyr::filter(med_hh_inc_2019 >= input$med_hh_inc_2019[1] & med_hh_inc_2019 <= input$med_hh_inc_2019[2]) |>
            dplyr::filter(pct_bachelor >= input$pct_bachelor[1] & pct_bachelor <= input$pct_bachelor[2]) |>
            # healthcare
            dplyr::filter(life_exp >= input$life_exp[1] & life_exp <= input$life_exp[2]) |>
            dplyr::filter(violent_crime_rate >= input$violent_crime_rate[1] & violent_crime_rate <= input$violent_crime_rate[2]) |>
            dplyr::filter(average_daily_pm2_5 >= input$average_daily_pm2_5[1] & average_daily_pm2_5 <= input$average_daily_pm2_5[2]) |>
            dplyr::filter(prim_care_dr_rate >= input$prim_care_dr_rate[1] & prim_care_dr_rate <= input$prim_care_dr_rate[2]) |>
            # weather
            dplyr::filter(avg_annual_temp >= input$avg_annual_temp[1] & avg_annual_temp <= input$avg_annual_temp[2]) |>
            # home valuation
            dplyr::filter(median_home_price >= input$median_home_price[1] & median_home_price <= input$median_home_price[2]) |>
            dplyr::filter(yoy_price_chg_pct >= input$yoy_price_chg_pct[1] & yoy_price_chg_pct <= input$yoy_price_chg_pct[2]) |>
            dplyr::filter(years_to_payoff >= input$years_to_payoff[1] & years_to_payoff <= input$years_to_payoff[2])
    })
    output$map <- leaflet::renderLeaflet({
        df <- filtered()
        pal <- leaflet::colorFactor(
            palette = c("#4bbf73", "#007bff", "#e83e8c", "#fd7e14", "#f0ad4e"),
            domain = df$cbsa_desig
        )
        airplaneIcon <- leaflet::makeIcon(
            iconUrl = "./img/plane.svg",
            iconWidth = 15, iconHeight = 15,
            # iconAnchorX = 0, iconAnchorY = 0
        )
        mortarboardIcon <- leaflet::makeIcon(
            iconUrl = "./img/mortarboard.svg",
            iconWidth = 15, iconHeight = 15,
            # iconAnchorX = 0, iconAnchorY = 0
        )
        chevronIcon <- leaflet::makeIcon(
            iconUrl = "./img/chevron.svg",
            iconWidth = 15, iconHeight = 15
        )
        hospitalIcon <- leaflet::makeIcon(
            iconUrl = "./img/hospital.svg",
            iconWidth = 15, iconHeight = 15
        )
        leaflet() |>
            # addTiles() |>
            addProviderTiles(providers$CartoDB.DarkMatter) |>
            fitBounds(
                lng1 = min(df$lon, na.rm = T),
                lng2 = max(df$lon, na.rm = T),
                lat1 = min(df$lat, na.rm = T),
                lat2 = max(df$lat, na.rm = T)
            ) |>
            # setView(mean(df$lon, na.rm = T),
            #         mean(df$lat, na.rm = T),
            #         zoom = 4) |>
            addCircleMarkers(
                data = df,
                lng = ~lon,
                lat = ~lat,
                color = ~ pal(cbsa_desig),
                radius = 10,
                fillOpacity = .25,
                fill = T,
                stroke = T,
                weight = 3,
                popup = leafpop::popupTable(df,
                    row.numbers = F,
                    feature.id = F,
                    zcol = c(
                        "state",
                        "county",
                        "pop_2020",
                        "years_to_payoff"
                    )
                )
            ) |>
            addMarkers(
                data = retirementData::airportLoc,
                lng = ~lon,
                lat = ~lat,
                icon = airplaneIcon,
                group = "Airports",
                popup = leafpop::popupTable(retirementData::airportLoc,
                    zcol = c(
                        "airport",
                        "rank",
                        "hub",
                        "enplane_2020"
                    ),
                    row.numbers = F,
                    feature.id = F
                )
            ) |>
            addMarkers(
                data = retirementData::collegeLoc,
                lng = ~lon,
                lat = ~lat,
                icon = mortarboardIcon,
                group = "Colleges",
                popup = leafpop::popupTable(retirementData::collegeLoc,
                    zcol = c(
                        "name",
                        "students_2020"
                    ),
                    row.numbers = F,
                    feature.id = F
                )
            ) |>
            addMarkers(
                data = retirementData::hospitalLoc,
                lng = ~lon,
                lat = ~lat,
                icon = hospitalIcon,
                group = "Hospitals",
                popup = leafpop::popupTable(retirementData::hospitalLoc,
                    zcol = c(
                        "name",
                        "type",
                        "ownership",
                        "emer_room",
                        "stars",
                        "verify"
                    ),
                    row.numbers = F,
                    feature.id = F
                )
            ) |>
            addMarkers(
                data = retirementData::militaryBases,
                lng = ~lon,
                lat = ~lat,
                icon = chevronIcon,
                group = "Bases",
                popup = leafpop::popupTable(retirementData::militaryBases,
                    zcol = c(
                        "base",
                        "perimeter",
                        "area"
                    ),
                    row.numbers = F,
                    feature.id = F
                )
            ) |>
            addLegend("bottomright",
                pal = pal,
                values = df$cbsa_desig,
                title = "CBSA Designation",
                labFormat = labelFormat(prefix = "$"),
                opacity = 1
            ) |>
            leaflet::addLayersControl(
                overlayGroups = c("Airports", "Colleges", "Hospitals", "Bases"),
                options = layersControlOptions(collapsed = FALSE)
            ) |>
            leaflet::hideGroup("Airports") |>
            leaflet::hideGroup("Colleges") |>
            leaflet::hideGroup("Hospitals") |>
            leaflet::hideGroup("Bases")
    })
    output$tableCounties <- DT::renderDataTable({
        df <- filtered()
        DT::datatable(df,
            rownames = F,
            style = "bootstrap",
            class = "compact",
            extensions = c("Buttons", "Scroller"),
            options = list(
                dom = "Blrtip",
                scrollY = 700,
                scroller = TRUE,
                scrollX = TRUE,
                columnDefs = list(
                    list(
                        visible = TRUE,
                        targets = c(1:7)
                    )
                ),
                buttons = list(
                    I("colvis"), # turn columns on and off
                    "csv", # download as .csv
                    "excel" # download as .xlsx
                )
            ),
            colnames = c(
                "fips", "lon", "lat", "state", "county", "pop_2020", "pct_pop_change",
                "cbsa_desig", "rucc_2013", "partisan_lean", "med_hh_inc_2019",
                "pct_bachelor", "broadband_2017", "life_exp", "violent_crime_rate",
                "average_daily_pm2_5", "prim_care_dr_rate", "avg_annual_temp",
                "median_home_price", "yoy_price_chg_pct", "years_to_payoff"
            )
        )
    })
    output$plotlyScatter <- plotly::renderPlotly({
        df <- filtered()
        p <- ggplot(df, aes_string(input$xaxis,
            input$yaxis,
            color = input$groupby,
            size = input$size,
            key = "county"
        )) +
            geom_point(alpha = .5)
        # p <- p + theme_dark()
        plotly::ggplotly(p, tooltip = c(
            "key",
            "size",
            "color",
            "x",
            "y"
        ))
    })
    migration_out_filtered <- eventReactive(input$button, {
        irsMigration %>%
            dplyr::filter(state_origin == input$state2) %>%
            dplyr::filter(grepl(input$county, county_origin))
    })
    migration_in_filtered <- eventReactive(input$button, {
        irsMigration %>%
            dplyr::filter(state_target == input$state2) %>%
            dplyr::filter(grepl(input$county, county_target))
    })
    output$migration_out <- renderLeaflet({
        df.out <- migration_out_filtered()
        #lines
        lines <-
            df.out %>%
            dplyr::mutate(id = 1:dplyr::n() %>% as.character) %>%
            dplyr::select(id, lat_origin, lat_target, lon_origin, lon_target) %>%
            tidyr::pivot_longer(!id,
                         names_to = "coord",
                         values_to = "value"
            ) %>%
            tidyr::separate(coord, into = c("dir", "group"), sep = "_") %>%
            tidyr::pivot_wider(c(id, group), names_from = dir, values_from = value)
        #map
        m <-
            leaflet() %>%
            addProviderTiles(providers$CartoDB.DarkMatter) %>%
            addPolylines(data = lines,
                         lat = ~lat,
                         lng = ~lon,
                         weight = .5,
                         group = ~group) %>%
            addCircleMarkers(data = df.out,
                             lat = ~lat_target,
                             lng = ~lon_target,
                             popup = leafpop::popupTable(df.out,
                                                zcol = c("state_target",
                                                         "county_target",
                                                         "state_origin",
                                                         "county_origin",
                                                         "returns",
                                                         "exempts",
                                                         "avg_agi"),
                                                row.numbers = F,
                                                feature.id = F
                             ),
                             radius = 5
            )
        m
    })
    output$migration_in <- renderLeaflet({
        df.in <- migration_in_filtered()
        #lines
        lines <-
            df.in %>%
            dplyr::mutate(id = 1:dplyr::n() %>% as.character) %>%
            dplyr::select(id, lat_origin, lat_target, lon_origin, lon_target) %>%
            tidyr::pivot_longer(!id,
                         names_to = "coord",
                         values_to = "value"
            ) %>%
            tidyr::separate(coord, into = c("dir", "group"), sep = "_") %>%
            tidyr::pivot_wider(c(id, group), names_from = dir, values_from = value)
        #map
        m <-
            leaflet() %>%
            addProviderTiles(providers$CartoDB.DarkMatter) %>%
            addPolylines(data = lines,
                         lat = ~lat,
                         lng = ~lon,
                         weight = .5,
                         color = "#f0ad4e",
                         group = ~group) %>%
            addCircleMarkers(data = df.in,
                             lat = ~lat_origin,
                             lng = ~lon_origin,
                             color = "#f0ad4e",
                             popup = leafpop::popupTable(df.in,
                                                zcol = c("state_target",
                                                         "county_target",
                                                         "state_origin",
                                                         "county_origin",
                                                         "returns",
                                                         "exempts",
                                                         "avg_agi"),
                                                row.numbers = F,
                                                feature.id = F
                             ),
                             radius = 5
            )
        m
    })
}
