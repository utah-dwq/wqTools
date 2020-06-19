#' Add a map reset button to a Leaflet map object
#'
#' @param leaf A Leaflet map object
#' @examples
#' reset_map=wqTools::buildMap() %>% addMapResetButton()

#' @export
addMapResetButton <- function(leaf) {
  leaf %>%
    addEasyButton(
      easyButton(
        icon = "ion-arrow-shrink", 
        title = "Reset View", 
        onClick = JS(
          "function(btn, map){ map.setView(map._initialCenter, map._initialZoom); }"
        )
      )
    ) %>% 
    htmlwidgets::onRender(
      JS(
"
function(el, x){ 
  var map = this; 
  map.whenReady(function(){
    map._initialCenter = map.getCenter(); 
    map._initialZoom = map.getZoom();
  });
}"
      )
    )
}

# See https://github.com/rstudio/leaflet/issues/623#issuecomment-493450792 for more information

