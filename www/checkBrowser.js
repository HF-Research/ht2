/*JS code to check if user is using IE browser*/
$(document).on('shiny:sessioninitialized', function(event) {
  var isIE = window.document.documentMode > 0
  Shiny.onInputChange("check", isIE);
});