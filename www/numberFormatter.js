shinyjs.numberFormatter = function(param) {
  


if(param === 'en'){
  var numFormatDef= d3.formatLocale({
  decimal: '.',
  thousands: ',',
  grouping: [3],
  currency: ['', '$']
});

  return numFormatDef.format(',');
}

var numFormatDef= d3.formatLocale({
  decimal: ',',
  thousands: '.',
  grouping: [3],
  currency: ['', 'DKK']
});
return numFormatDef.format(',');
}