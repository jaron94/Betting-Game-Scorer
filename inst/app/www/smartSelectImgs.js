$( document ).ready(function() {
  Shiny.addCustomMessageHandler('genSmartSelectImg', function(arg) {
    var button_content = document.querySelector(`#A${arg.i} .item-content`);
    var curr_av = button_content.querySelector("img");
    if (curr_av !== null) {
      curr_av.src = arg.src;
    } else {
      var av = document.createElement("img");
      av.className = "avatar";
      av.src = arg.src;
      button_content.appendChild(av);
    }    
  })
});
