$( document ).ready(function() {
  const bgcolor = window.getComputedStyle(
    document.getElementsByClassName("timeline-item-divider")[0]
  ).backgroundColor
  const cssTemplateString = `#lasttime:before{background: ${bgcolor}}`;
  const styleTag = document.createElement("style");
  styleTag.innerHTML = cssTemplateString;
  document.head.insertAdjacentElement('beforeend', styleTag);
});


