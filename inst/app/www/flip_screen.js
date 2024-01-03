$(document).on("shiny:connected", (function(e) {
  Shiny.setInputValue("orientation", screen.orientation.type)
}));

screen.orientation.addEventListener("change", function(e) {
  Shiny.onInputChange('orientation', e.target.type);
});
