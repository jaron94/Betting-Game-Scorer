function toggle_inputs_one(inputs_li, n_players) {
  for (var i = 2, len = inputs_li.length; i < len; i++) {
    if (i >= n_players) {
      inputs_li[i].style.display = "none";
    } else {
      inputs_li[i].style.display = "block";
    }
  }
}

$( document ).ready(function() {
  Shiny.addCustomMessageHandler('toggle_inputs', function(n_players) {
    var pinputs_li = document.querySelectorAll("#pinputs .list");
    toggle_inputs_one(pinputs_li, n_players);
    var ainputs_li = document.querySelectorAll("#ainputs .list");
    toggle_inputs_one(ainputs_li, n_players);
  })
});



