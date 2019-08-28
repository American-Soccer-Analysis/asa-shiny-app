sidebar <- bs4DashSidebar(
     skin = "dark",
     status = "primary",
     title = "American Soccer Analysis",
     brandColor = "primary",
     src = "asa_icon_white.png",
     elevation = 0,
     opacity = 1,
     bs4SidebarMenu(
          id = "asa_sidebar",
          bs4SidebarHeader("Profiles"),
          bs4SidebarMenuItem(
               "Players",
               tabName = "profile_player",
               icon = "user"
          ),
          bs4SidebarMenuItem(
               "Teams",
               tabName = "profile_teams",
               icon = "shield-alt"
          ),
          bs4SidebarHeader("Header 1"),
          bs4SidebarMenuItem(
               "Item 1",
               tabName = "item1",
               icon = "sliders"
          )
     )
)