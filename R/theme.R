theme_dashboard <- function() {
  primary <- "#005AA2"
  accent <- "#00C1D5"
  secondary <- "#DE5428"

  dashboardthemes::shinyDashboardThemeDIY(
    # General
    appFontFamily = "Helvetica",
    appFontColor = "black",
    primaryFontColor = "#7E909A",
    infoFontColor = "#7E909A",
    successFontColor = "#7E909A",
    warningFontColor = "#7E909A",
    dangerFontColor = "#7E909A",
    bodyBackColor = "#F8F8F8",

    # Header
    logoBackColor = primary,
    headerButtonBackColor = primary,
    headerButtonIconColor = "white",
    headerButtonBackColorHover = accent,
    headerButtonIconColorHover = "white",
    headerBackColor = primary,
    headerBoxShadowColor = "#F1F1F1",
    headerBoxShadowSize = "0px 0px 0px",

    # Sidebar
    sidebarBackColor = "#EDEDED",
    sidebarPadding = 5,
    sidebarMenuBackColor = "transparent",
    sidebarMenuPadding = 0,
    sidebarMenuBorderRadius = 0,
    sidebarShadowRadius = "0px 0px 0px",
    sidebarShadowColor = "#F1F1F1",
    sidebarUserTextColor = "#7E909A",
    sidebarSearchBackColor = "rgb(55,72,80)",
    sidebarSearchIconColor = "rgb(153,153,153)",
    sidebarSearchBorderColor = "rgb(55,72,80)",
    sidebarTabTextColor = "#7E909A",
    sidebarTabTextSize = 13,
    sidebarTabBorderStyle = "none none none solid",
    sidebarTabBorderColor = "transparent",
    sidebarTabBorderWidth = 5,
    sidebarTabBackColorSelected = "transparent",
    sidebarTabTextColorSelected = primary,
    sidebarTabRadiusSelected = "0px 0px 0px 0px",
    sidebarTabBackColorHover = "#EDEDED",
    sidebarTabTextColorHover = accent,
    sidebarTabBorderStyleHover = "none none none solid",
    sidebarTabBorderColorHover = secondary,
    sidebarTabBorderWidthHover = 5,
    sidebarTabRadiusHover = "0px 0px 0px 0px",

    # Boxes
    boxBackColor = "white",
    boxBorderRadius = 5,
    boxShadowSize = "0px 0px 0px",
    boxShadowColor = paste0(primary, "30"),
    boxTitleSize = 18,
    boxDefaultColor = "white",
    boxPrimaryColor = "white",
    boxInfoColor = "rgb(210,214,220)",
    boxSuccessColor = "rgba(0,255,213,1)",
    boxWarningColor = "rgb(244,156,104)",
    boxDangerColor = "rgb(255,88,55)",
    tabBoxTabColor = "white",
    tabBoxTabTextSize = 12,
    tabBoxTabTextColor = "#7E909A",
    tabBoxTabTextColorSelected = primary,
    tabBoxBackColor = "white",
    tabBoxHighlightColor = primary,
    tabBoxBorderRadius = 0,

    # Inputs
    buttonBackColor = "rgb(245,245,245)",
    buttonTextColor = "rgb(0,0,0)",
    buttonBorderColor = "rgb(200,200,200)",
    buttonBorderRadius = 5,
    buttonBackColorHover = "rgb(235,235,235)",
    buttonTextColorHover = "rgb(100,100,100)",
    buttonBorderColorHover = "rgb(200,200,200)",
    textboxBackColor = "rgb(255,255,255)",
    textboxBorderColor = "rgb(200,200,200)",
    textboxBorderRadius = 5,
    textboxBackColorSelect = "rgb(245,245,245)",
    textboxBorderColorSelect = "rgb(200,200,200)",

    # Tables
    tableBackColor = NA,
    tableBorderColor = NA,
    tableBorderTopSize = 1,
    tableBorderRowSize = 1
  ) %>%
    tagList(
      tags$style(
        HTML("
          .skin-blue .main-header .navbar {
            height: 80px !important;
          }
          .skin-blue .main-header .logo {
            height: 80px !important;
            line-height: 80px !important;
          }
          #header-logo {
            position: absolute;
            right: 20px;
            top: 10px;
            height: 60px;
            width: auto;
            z-index: 1000;
          }
          .skin-blue .main-header .navbar-custom-menu {
            display: flex;
            align-items: center;
            justify-content: flex-end;
          }
          /* Hide sidebar toggle button */
          .sidebar-toggle {
            display: none !important;
          }
          /* Adjust sidebar menu positioning */
          .main-sidebar {
            padding-top: 100px !important;
          }
          .sidebar {
            padding-top: 0 !important;
          }

        ")
      ),
      tags$script(HTML("
        $(document).ready(function() {
          var logo = $('<img>', {
            id: 'header-logo',
            src: 'SQI-watermark-vector.png'
          });
          $('.navbar').append(logo);
        });
      "))
    )
}
