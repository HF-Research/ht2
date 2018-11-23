// !preview r2d3 data=(export$`Balloon angioplasty`$age$cases[year == 2015])
//
// r2d3: https://rstudio.github.io/r2d3
//


var margin = ({top:10, right:10, bottom:40, left:60});
var Gwidth = width - margin.left - margin.right
var Gheight = height - margin.top - margin.bottom
var barPadding = 0.2;


// Get data into format for bar chart, while storing grouping and variable name 
var groupingName = Object.keys(data[0])[1]
var varName = Object.keys(data[0])[2]
for (var i = 0; i<data.length; i++) {
    data[i].grouping = data[i][groupingName]
    data[i].value = data[i][varName]
}

var initData = d3.nest().key(d => d.grouping).entries(data)



var colors = ['#bd6916', '#166abd ']

var topG = svg.append('g')
    .attr('transform', 'translate(' + margin.left + ',' + margin.top +')')


// Initial scale

// Scale between the keys (i.e. b/w age groups, edu, etc`)
var scaleX = d3.scaleBand()
  .domain(initData.map(d => d.key))
  .range([0, Gwidth])
  .padding(barPadding);

var scaleColors = d3.scaleOrdinal()
    .range(colors)

// Initial axis
var yAxis = topG.append('g')
  .attr("class", "y axis")

var xAxis = topG.append('g')
    .attr("class", "x axis")

xAxis.call(d3.axisBottom(scaleX))
    .attr("transform", 'translate(' + 0 + "," + Gheight + ')');


// Axis titles
topG.append("text")
  .attr("x", Gwidth / 2)
  .attr("y", Gheight + margin.bottom - 5)
  .attr("class", "x axisTitle")
  .text(groupingName)
  
topG.append("text")
  .attr("transform", "rotate(-90)")
  .attr("x", 0 - Gheight / 2)
  .attr("y", 0 - margin.left + 20)
  .attr("class", "y axisTitle")
  .text("Total")



var chartArea = topG.append("g");












// UPDATE FUNCTION - will be called by r2d3.onRender()
function update(inData) {

// Reshape data
  var groupingName = Object.keys(inData[0])[1]
  var varName = Object.keys(inData[0])[2]
  for (var i = 0; i<inData.length; i++) {
      inData[i].grouping = inData[i][groupingName]
      inData[i].value = inData[i][varName]
      delete inData[i][varName]
      delete inData[i][groupingName]
      
  }


  var newData = d3.nest()
    .key(d => d.grouping)
    .entries(inData);
   
  
  var maxY = d3.max(newData, d => d3.max(d.values, k => k.value));
  grouping1Names = newData.map(d => d.key);
  grouping2Names = newData[0].values.map(d => d.Sex);
  var tLong = 450;
  var tShort = 200;
  // Scales used in updates 
  var scaleY = d3.scaleLinear()
    .domain([0, maxY])
    .range([Gheight, 0]);
  
  var scaleX = d3.scaleBand()
    .domain(grouping1Names)
    .range([0, Gwidth])
    .padding(barPadding);

  var scaleX1 = d3.scaleBand()
    .domain(grouping2Names)
    .rangeRound([0, scaleX.bandwidth()]);
  
  
   // Perform the data joins
   var barGroupWithData = chartArea
      .selectAll('g')
      .data(newData, d => d.key);
   
   // Remove any bar-groups not present in incoming data
   barGroupWithData.exit()
    .transition()
    .duration(tShort)
    .ease(d3.easeLinear)
    .style('opacity', 0)
    .remove();
   
   var barsData = barGroupWithData.enter()
      .append("g")
      .merge(barGroupWithData)
      .attr("transform", d => "translate(" + scaleX(d.key) + ",0)")
      
   //barsData.transition().duration(t).
      
		var	bars = barsData.selectAll("rect")
          .data(d => Object.keys(d.values)
                          .map(k => ({ keyL2: grouping2Names[k], value: d.values[k].value }) ))
    
  bars.exit().transition().duration(tLong).attr("y", d=> scaleY(0)).remove()
      
  bars.enter()
    .append("rect")
    .attr("fill", d => scaleColors(d.keyL2))
    .attr("y", d => scaleY(0))
    .merge(bars)
    .attr("x", (d) => scaleX1(d.keyL2))
    
    .transition()
    .duration(tLong)
    .ease(d3.easeLinear)
    .attr("width", scaleX1.bandwidth())
    .attr('y', d => scaleY(d.value))
    .attr("height", d => scaleY(0) - scaleY(d.value));


   
  // Udpate axes
  yAxis.transition()
    .duration(tLong)
    .call(d3.axisLeft(scaleY))
    
  xAxis.transition()
    .duration(tLong)
    .call(d3.axisBottom(scaleX))
 
  
  topG.select(".y.axisTitle")
    .transition()
    .duration(tLong)
    .text(varName)
      .style("text-anchor", "middle");
  
  topG.select(".x.axisTitle")
    .transition()
    .duration(tLong)
    .style('opacity', 1)
    .text(groupingName)
      .style("text-anchor", "middle");
    
    
}










// When data is updated via shiny, the following code is run:
// This is also called on first load apparently
r2d3.onRender(function(newData) {
  if (newData.length > 0){
    update(newData);
  }
}); 