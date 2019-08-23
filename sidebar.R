sidebar <- bs4DashSidebar(
     skin = "dark",
     status = "primary",
     title = "American Soccer Analysis",
     brandColor = "primary",
     src = "asa_icon_white.png",
     elevation = 0,
     opacity = 1,
     bs4SidebarMenu(
          bs4SidebarHeader("Header 1"),
          bs4SidebarMenuItem(
               "Item 1",
               tabName = "item1",
               icon = "sliders"
          ),
          bs4SidebarMenuItem(
               "Item 2",
               tabName = "item2",
               icon = "id-card"
          ),
          bs4SidebarHeader("Header 1"),
          bs4SidebarMenuItem(
               "Item 1",
               tabName = "item1",
               icon = "sliders"
          )
     )
)