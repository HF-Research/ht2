
$(document).on('shiny:sessioninitialized', function(event) {
  var isIE = window.document.documentMode > 0
  Shiny.onInputChange("check", isIE);
});