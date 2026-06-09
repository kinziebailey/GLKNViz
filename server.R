
# Server ----
server <- function(input, output, session){

# Map ----
output$map <- leaflet::renderLeaflet({
  leaflet::leaflet() |>
    leaflet::addTiles()
    # leaflet::addTiles(group = "Map", # access token issues
    #                   urlTemplate = NPSbasic,
    #                   options = tileOptions(minZoom = 8)) |>
    # leaflet::addTiles(group = "Imagery",
    #                   urlTemplate = ESRIimagery,
    #                   options = tileOptions(minZoom = 8)) |>
    # leaflet::addTiles(group = "Topo",
    #                   urlTemplate = ESRItopo,
    #                   options = tileOptions(minZoom = 8)) |>
    # leaflet::addTiles(group = "NatGeo",
    #                   urlTemplate = ESRINatGeo,
    #                   options = tileOptions(minZoom = 8)) |>
    # leaflet::addLayersControl(baseGroups = c("Map",
    #                                          "Imagery",
    #                                          "Topo",
    #                                          "NatGeo"),
    #                           options = layersControlOptions(collapsed = T))

})

}