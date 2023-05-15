$( document ).ready(function() {
  Shiny.addCustomMessageHandler('restyle_tl_item_div', function(round) {
    const bgcolor = window.getComputedStyle(
      document.getElementsByClassName("timeline-item-divider")[0]
    ).backgroundColor;
    const tl_id = `tl_item${round}`;
    document.getElementById(tl_id).style.backgroundColor = "red";
    const beforeTempstring = `#${tl_id}:before{background: ${bgcolor}}`;
    const beforeTag = document.createElement("style");
    beforeTag.innerHTML = beforeTempstring;
    document.head.insertAdjacentElement('beforeend', beforeTag);
    const afterTempstring = `#${tl_id}:after{background: ${bgcolor}}`;
    const afterTag = document.createElement("style");
    afterTag.innerHTML = afterTempstring;
    document.head.insertAdjacentElement('beforeend', afterTag);
  })
});


