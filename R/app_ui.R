#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @importFrom shinythemes shinytheme
#' @importFrom shinyWidgets pickerInput numericRangeInput
#' @importFrom stats quantile
#'
#' @noRd
retirementLoc <- retirementData::retirementLoc
irsMigration <- retirementData::irsMigration
app_ui <- function(request) {
    tagList(
        # Leave this function for adding external resources
        golem_add_external_resources(),
        # Your application UI logic
        navbarPage(
            # settings ----
            theme = shinythemes::shinytheme("cosmo"),
            inverse = F,
            title = tags$div(
                tags$img(src = "img/balloon.png", width = 130, height = 130, style = "float:left; margin-left: 5px; margin-right: 5px; margin-top: -15px; border-radius: 50%;"),
                tags$title("Retirement Locator")
            ),
            footer = tags$footer(
                HTML(
                    "<!-- Site footer -->
<div class='footer navbar-fixed-bottom'>
<footer class='site-footer'>
      <div class='container'>
            <hr>
      </div>
      <div class='container'>
            <div class='row'>
                  <div class='col-md-8 col-sm-6 col-xs-12'>
                        <p class='copyright-text'>&copy; 2021 Rob Wiederstein under MIT
                              <a href='https://github.com/RobWiederstein/retirementDash/blob/master/LICENSE.md'>License</a>.
                             </p>
                                 </div>

                                 <div class='col-md-4 col-sm-6 col-xs-12'>
                                 <ul class='social-icons'>
                                 <li><a class='github' href='https://github.com/RobWiederstein'><i class='fa fa-github'></i></a></li>
                                 <li><a class='twitter' href='https://twitter.com/robwiederstein?lang=en'><i class='fa fa-twitter'></i></a></li>
                                 <li><a class='linkedin' href='https://www.linkedin.com/in/rob-wiederstein-797553138/'><i class='fa fa-linkedin'></i></a></li>
                                 </ul>
                                 </div>
                                 </div>
                                 </div>
                                 </footer>
                                 </div>
                                 <script src='https://use.fontawesome.com/9d03eba9c5.js'></script>"
                )
            ),
            # locator ----
            tabPanel(
                "Location",
                ## criteria ----
                column(
                    3,
                    wellPanel(
                        style = "background-color: #fff; border-color: #2c3e50; overflow-y:scroll; max-height: 800px;",
                        br(),
                        h1("Retirement Locator"),
                        tags$br(),
                        tags$p("The 3,146 US counties may be selected by 17 criteria.
                               Sliders are preset at the 2.5% and 97.5% level to eliminate
                               outliers, leaving 2,028 counties."),
                        hr(),
                        ### Input: button filter ----
                        actionButton("button_filter", "Select counties!"),
                        hr(),
                        ### Groups: ----
                        h4("Groups:"),
                        #### Input: State ----
                        pickerInput("state", "State:",
                            choices = sort(unique(retirementLoc$state)),
                            selected = unique(retirementLoc$state),
                            multiple = T,
                            options = list(
                                `actions-box` = TRUE,
                                `deselect-all-text` = "None",
                                `select-all-text` = "All",
                                `none-selected-text` = "zero"
                            )
                        ),
                        #### Input: CBSA designation ----
                        pickerInput("cbsa_desig", "CBSA Designation:",
                            choices = sort(unique(retirementLoc$cbsa_desig)),
                            selected = unique(retirementLoc$cbsa_desig),
                            multiple = T,
                            options = list(
                                `actions-box` = TRUE,
                                `deselect-all-text` = "None",
                                `select-all-text` = "All",
                                `none-selected-text` = "zero"
                            )
                        ),
                        #### Input: USDA Rural-Urban Continuum  ----
                        pickerInput("rucc_2013", "Rural-Urban Continuum (1 - most urban, 9 - least urban):",
                            choices = sort(unique(retirementLoc$rucc_2013)),
                            selected = unique(retirementLoc$rucc_2013),
                            multiple = T,
                            options = list(
                                `actions-box` = TRUE,
                                `deselect-all-text` = "None",
                                `select-all-text` = "All",
                                `none-selected-text` = "zero"
                            )
                        ),
                        br(),
                        ### Demographics ----
                        h4("Demographic"),
                        #### Input: population ----
                        numericRangeInput(
                            inputId = "pop_2020", label = "2020 Population",
                            value = c(
                                min(retirementLoc$pop_2020, na.rm = T),
                                max(retirementLoc$pop_2020, na.rm = T)
                            )
                        ),
                        #### Input: Population Change Lean  ----
                        sliderInput("pct_pop_change", "% Pop. Change 2010 to 2020:",
                            min = -35,
                            max = 140,
                            value = quantile(retirementLoc$pct_pop_change,
                                probs = c(.025, .975), na.rm = T
                            ),
                            step = 5,
                            round = 1
                        ),
                        #### Input: Partisan Lean  ----
                        sliderInput("partisan_lean", "% Partisan Lean 2020:",
                            min = 0,
                            max = 90,
                            value = quantile(retirementLoc$partisan_lean,
                                probs = c(.025, .975), na.rm = T
                            ),
                            step = 5,
                            round = 1
                        ),
                        #### Input: broadband 2017  ----
                        sliderInput("broadband_2017", "% Broadband 2017:",
                            min = round(min(retirementLoc$broadband_2017, na.rm = T), 0),
                            max = round(max(retirementLoc$broadband_2017, na.rm = T), 0),
                            value = quantile(retirementLoc$broadband_2017,
                                probs = c(.025, 1), na.rm = T
                            )
                        ),
                        #### Input: median household income ----
                        sliderInput("med_hh_inc_2019", "Median household income:",
                            min = plyr::round_any(min(retirementLoc$med_hh_inc_2019, na.rm = T), 1000),
                            max = plyr::round_any(max(retirementLoc$med_hh_inc_2019, na.rm = T), 1000),
                            value = plyr::round_any(quantile(retirementLoc$med_hh_inc_2019,
                                probs = c(.025, 1), na.rm = T
                            ), 1000),
                            step = 1000
                        ),
                        #### Input: education pct having bachelors degree ----
                        sliderInput("pct_bachelor", "% Adults Having Completed College:",
                            min = round(min(retirementLoc$pct_bachelor, na.rm = T), 0),
                            max = round(max(retirementLoc$pct_bachelor, na.rm = T), 0),
                            value = quantile(retirementLoc$pct_bachelor,
                                probs = c(.025, 1), na.rm = T
                            )
                        ),
                        br(),
                        ### Health: ----
                        h4("Health"),
                        #### Input: Life Expectancy  ----
                        sliderInput("life_exp", "Life Expectancy (years):",
                            min = round(min(retirementLoc$life_exp, na.rm = T), 0),
                            max = round(max(retirementLoc$life_exp, na.rm = T), 0),
                            value = quantile(retirementLoc$life_exp,
                                probs = c(.025, 1), na.rm = T
                            ),
                            step = 1,
                            round = 1
                        ),
                        #### Input: violent_crime_rate ----
                        sliderInput("violent_crime_rate", "Violent crime rate:",
                            min = round(min(retirementLoc$violent_crime_rate, na.rm = T), 0),
                            max = round(max(retirementLoc$violent_crime_rate, na.rm = T), 0),
                            value = c(quantile(retirementLoc$violent_crime_rate,
                                probs = c(0, .975), na.rm = T
                            ))
                        ),
                        #### Input: average daily particulate matter 2.5 microns ----
                        sliderInput("average_daily_pm2_5", "Particulate Matter 2.5:",
                            min = min(retirementLoc$average_daily_pm2_5, na.rm = T),
                            max = max(retirementLoc$average_daily_pm2_5, na.rm = T),
                            value = quantile(retirementLoc$average_daily_pm2_5,
                                probs = c(0, .975), na.rm = T
                            )
                        ),
                        #### Input: access to primary care physician ----
                        sliderInput("prim_care_dr_rate", "Primary care physicians:",
                            min = round(min(retirementLoc$prim_care_dr_rate, na.rm = T), 0),
                            max = round(max(retirementLoc$prim_care_dr_rate, na.rm = T), 0),
                            value = quantile(retirementLoc$prim_care_dr_rate,
                                probs = c(.025, 1), na.rm = T
                            )
                        ),
                        br(),
                        ### Weather: ----
                        h4("Weather"),
                        #### Input: Average annual temperature  ----
                        sliderInput("avg_annual_temp", "Avg. Annual Temp:",
                            min = round(min(retirementLoc$avg_annual_temp, na.rm = T), 0),
                            max = round(max(retirementLoc$avg_annual_temp, na.rm = T), 0),
                            value = quantile(retirementLoc$avg_annual_temp,
                                probs = c(0, 1), na.rm = T
                            )
                        ),
                        br(),
                        ### Valuation: ----
                        h4("Valuation"),
                        #### Input: median home price ----
                        sliderInput("median_home_price", "Median home price: ",
                            min = plyr::round_any(min(retirementLoc$median_home_price, na.rm = T), 1000),
                            max = plyr::round_any(max(retirementLoc$median_home_price, na.rm = T), 1000),
                            value = plyr::round_any(quantile(retirementLoc$median_home_price,
                                probs = c(.025, .975), na.rm = T
                            ), 1000),
                            step = 25000
                        ),
                        #### Input: year-over-year price change percentage ----
                        sliderInput("yoy_price_chg_pct", "Year-over-year price change (%): ",
                            min = round(min(retirementLoc$yoy_price_chg_pct, na.rm = T), 0),
                            max = round(max(retirementLoc$yoy_price_chg_pct, na.rm = T), 0),
                            value = quantile(retirementLoc$yoy_price_chg_pct,
                                probs = c(.025, .975), na.rm = T
                            )
                        ),
                        #### Input: home payoff  ----
                        sliderInput("years_to_payoff", "Home payoff in years:",
                            min = round(min(retirementLoc$years_to_payoff, na.rm = T), 0),
                            max = round(max(retirementLoc$years_to_payoff, na.rm = T), 0),
                            value = quantile(retirementLoc$years_to_payoff,
                                probs = c(0, .975), na.rm = T
                            ),
                            step = .5
                        )
                    )
                ),
                ## map ----
                column(
                    9,
                    wellPanel(
                        style = "background-color: #fff; border-color: #2c3e50;",
                        br(),
                        h1("United States"),
                        hr(),
                        leaflet::leafletOutput("map", height = "600")
                    )
                )
            ),
            # counties ----
            tabPanel(
                "Counties",
                ## counties ----
                column(
                    12,
                    wellPanel(
                        style = "background-color: #fff; border-color: #2c3e50;  overflow-y:scroll; max-height: 800px;",
                        br(),
                        h1("Selected Counties"),
                        hr(),
                        DT::DTOutput("tableCounties", height = 600)
                    )
                )
            ),
            # tab: analyzer ----
            tabPanel(
                "Analysis",
                ## sidebar: criteria ----
                column(
                    3,
                    wellPanel(
                        style = "background-color: #fff; border-color: #2c3e50; overflow-y:scroll;",
                        br(),
                        h1("Plot"),
                        hr(),
                        ### Input: x-axis ----
                        pickerInput(
                            inputId = "xaxis",
                            label = "x-axis:",
                            choices = c(
                                "average_daily_pm2_5", "avg_annual_temp", "broadband_2017",
                                "cbsa_desig", "county", "fips", "lat", "life_exp", "lon", "med_hh_inc_2019",
                                "median_home_price", "partisan_lean", "pct_bachelor", "pct_pop_change",
                                "pop_2020", "prim_care_dr_rate", "rucc_2013", "state", "violent_crime_rate",
                                "years_to_payoff", "yoy_price_chg_pct"
                            ),
                            selected = "pct_pop_change"
                        ),
                        ### Input: y-axis ----
                        pickerInput(
                            inputId = "yaxis",
                            label = "y-axis:",
                            choices = c(
                                "average_daily_pm2_5", "avg_annual_temp", "broadband_2017",
                                "cbsa_desig", "county", "fips", "lat", "life_exp", "lon", "med_hh_inc_2019",
                                "median_home_price", "partisan_lean", "pct_bachelor", "pct_pop_change",
                                "pop_2020", "prim_care_dr_rate", "rucc_2013", "state", "violent_crime_rate",
                                "years_to_payoff", "yoy_price_chg_pct"
                            ),
                            selected = "years_to_payoff"
                        ),
                        ### Input: grouping variable ----
                        pickerInput(
                            inputId = "groupby",
                            label = "Group by:",
                            choices = c("state", "cbsa_desig", "rucc_2013"),
                            selected = "state"
                        ),
                        ### Input: size variable ----
                        pickerInput(
                            inputId = "size",
                            label = "Size: ",
                            choices = c("pop_2020", "pct_pop_change", "yoy_price_chg_pct")
                        )
                    )
                ),
                ## main: scatter plot ----
                column(
                    9,
                    wellPanel(
                        style = "background-color: #fff; border-color: #2c3e50;",
                        h1("Scatter Plot"),
                        hr(),
                        plotly::plotlyOutput("plotlyScatter", height = "600px")
                    )
                )
            ),
            # migrator ----
            tabPanel(
                "Migration",
                fluidRow(
                    column(
                        3,
                        wellPanel(
                            style = "background-color: #fff; border-color: #2c3e50;",
                            br(),
                            h1("Migration"),
                            hr(),
                            tags$p("The IRS publishes county-to-county migration data based on tax returns. Counties with less than 20 returns are suppressed."),
                            tags$p("Step 1: Enter a single state."),
                            ### Input: State ----
                            pickerInput(
                                inputId = "state2",
                                label = "State: ",
                                choices = c(sort(unique(irsMigration$state_origin))),
                                selected = "FL"
                            ),
                            tags$p("Step 2: Pick a county."),
                            ### Input: county ----
                            uiOutput("county1"),
                            tags$p("Step 3:  Push the 'plot' button to see the results."),
                            ### Input: button ----
                            actionButton("button", "Plot chart!")
                        )
                    ),
                    column(
                        9,
                        wellPanel(
                            style = "background-color: #fff; border-color: #2c3e50;",
                            tabsetPanel(
                                tabPanel(
                                    "migration - out",
                                    leaflet::leafletOutput("migration_out", height = 600)
                                ),
                                tabPanel(
                                    "migration - in",
                                    leaflet::leafletOutput("migration_in", height = 600)
                                )
                            )
                        )
                    )
                )
            ),
            # overview ----
            tabPanel(
                "Overview",
                tagList(
                    br(),
                    includeMarkdown(
                        file.path(resourcePaths()["www"], "OVERVIEW.md")
                    )
                )
            )
        )
    )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
    add_resource_path(
        "www", app_sys("app/www")
    )
    add_resource_path(
        "img", app_sys("app/www/img")
    )

    tags$head(
        favicon(),
        bundle_resources(
            path = app_sys("app/www"),
            app_title = "retirementDash"
        )
        # Add here other external resources
        # for example, you can add shinyalert::useShinyalert()
    )
}
