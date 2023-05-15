$( document ).ready(function() {
  const bgcolor = window.getComputedStyle(
    document.getElementsByClassName("timeline-item-divider")[0]
  ).backgroundColor
  const beforeTempstring = `#lasttime:before{background: ${bgcolor}}`;
  const beforeTag = document.createElement("style");
  beforeTag.innerHTML = beforeTempstring;
  document.head.insertAdjacentElement('beforeend', beforeTag);
  const afterTempstring = `#lasttime:after{background: ${bgcolor}}`;
  const afterTag = document.createElement("style");
  beforeTag.innerHTML = afterTempstring;
  document.head.insertAdjacentElement('beforeend', beforeTag);
});


