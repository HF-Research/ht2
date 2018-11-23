// !preview r2d3 data=dcast(export$`Balloon angioplasty`$national$cases, year ~ sex, value.var = "n_patients")
//
// r2d3: https://rstudio.github.io/r2d3
//

// steps taken from: https://bl.ocks.org/gordlea/27370d1eea8464b04538e6d8ced39e89
/* Add an SVG to draw our line chart on
1. Use the D3 standard margin convetion
2. Create an x axis
3. Create a y axis
4. Create an x scale
5. Create a y scale
6. Create a line generator
7. Create a random dataset
8. Create a path object for the line
9. Bind the data to the path object
10. Call the line generator on the data-bound path object
11. Add circles to show each datapoint
12. Add some basic styling to the chart so its easier on the eyes
*/


 /*Additional notes:
Because the plot outputs inside a Shadow-dom (whatever that really is), some funcational differnces exist. Fx. d3.selectAll doesn't seem to penetrate down into the shadowdom. Also, need to check if CSS styling will penetrate
*/

var margin = ({top:10, right:10, bottom:40, left:60});
var Gwidth = width - margin.left - margin.right
var Gheight = height - margin.top - margin.bottom
var barPadding = 0.2;
var colors = ['#bd6916', '#166abd ']

var tLong = 450;
var tShort = 200;
var cRadius = 7;

var topG = svg.append('g')
    .attr('transform', 'translate(' + margin.left + ',' + margin.top +')')


// Initial scale

// Scale between the keys (i.e. b/w age groups, edu, etc`)
var scaleX = d3.scaleLinear()
  .domain(d3.extent(data, d => d.year))
  .range([0, Gwidth])
  

var scaleColors = d3.scaleOrdinal()
    .range(colors)

// Initial axis
var yAxis = topG.append('g')
  .attr("class", "y axis")

var xAxis = topG.append('g')
    .attr("class", "x axis")

xAxis.call(d3.axisBottom(scaleX)
              .tickFormat(d3.format("")))
    .attr("transform", 'translate(' + 0 + "," + Gheight + ')')
      


// Axis titles
topG.append("text")
  .attr("x", Gwidth / 2)
  .attr("y", Gheight + margin.bottom)
  .attr("class", "x axisTitle")
  .text("Year")
  .style("text-anchor", "middle");
  
topG.append("text")
  .attr("transform", "rotate(-90)")
  .attr("x", 0 - Gheight / 2)
  .attr("y", 0 - margin.left + 20)
  .attr("class", "y axisTitle")
  .text("Total")

var chartArea = topG.append("g");

var maxY = d3.max(data, d=> Math.max(d.female, d.male))
grouping1Names = data.map(d => d.year);


// Scales used in updates 
var scaleY = d3.scaleLinear()
  .domain([0, maxY])
  .range([Gheight, 0]);

var scaleX = d3.scaleLinear()
.domain(d3.extent(data, d => d.year))
.range([0, Gwidth])

// Line generators
var valueLine1 = d3.line()
.x(d => scaleX(d.year))
.y(d => scaleY(d.female))

var valueLine2 = d3.line()
.x(d => scaleX(d.year))
.y(d => scaleY(d.male))

// Add initial line paths
 var line1 = chartArea
  .append("path")
  .datum(data) // use dataum() because appending to single svg element
  .attr("d", valueLine1)
  .attr("fill", "none")
  .attr("stroke", colors[0])
  .attr("stroke-width", 3)
  .attr("class", "line female");

var line2 = chartArea
  .append("path")
  .datum(data) // use dataum() because appending to single svg element
  .attr("d", valueLine2)
  .attr("fill", "none")
  .attr("stroke", colors[1])
  .attr("stroke-width", "3")
  .attr("class", "line male");

// Add initial circles

var circlesFemale = chartArea
  .selectAll(".dot")
    .data(data)
    .enter().append("circle")
    .attr("class", "dotfemale")
    .attr("cx", d => scaleX(d.year))
    .attr("cy", d => scaleY(d.female))
    .attr("r", cRadius)
    .attr("fill", colors[0]);

var circlesMale = chartArea
  .selectAll(".dot")
    .data(data)
    .enter().append("circle")
    .attr("class", "dotmale")
    .attr("cx", d => scaleX(d.year))
    .attr("cy", d => scaleY(d.male))
    .attr("r", cRadius)
    .attr("fill", colors[1]);


var div = svg.append("div")
    .attr('class', 'tooltip')
    .style('opacity', 0)
    .attr("position", "absolute")
    .attr("text-align", "center")
    .attr("width"," 60px")
    .attr("height", "28px")					
    .attr("padding", "2px")				
    .attr("font", "12px sans-serif")
    .attr("background", "lightsteelblue")	
    .attr("border", "0px")		
    .attr("border-radius"," 8px")			


  // UPDATE FUNCTION - will be called by r2d3.onRender()
function update(newData) {

  // Reshape data
  var maxY = d3.max(newData, d=> Math.max(d.female, d.male))
  var varName = newData[0].variable
  
  // Tooltip
  		

  
  // Scales used in updates 
  var scaleY = d3.scaleLinear()
    .domain([0, maxY])
    .range([Gheight, 0]);
  
  var scaleX = d3.scaleLinear()
  .domain(d3.extent(newData, d => d.year))
  .range([0, Gwidth])
  
  // Line generators
  var valueLine1 = d3.line()
  .x(d => scaleX(d.year))
  .y(d => scaleY(d.female))

  var valueLine2 = d3.line()
  .x(d => scaleX(d.year))
  .y(d => scaleY(d.male))
  
   // Create the paths for each series of data
  chartArea
    .select(".female")
    .transition()
    .duration(tLong)
    .attr("d", valueLine1(newData))
    
  chartArea
    .select(".male")
    .transition()
    .duration(tLong)
    .attr("d", valueLine2(newData))
  
  // Update circles
  var dotFemale = chartArea.selectAll(".dotfemale")
    .data(newData)
    
    dotFemale.transition()
    .duration(tLong)
    .attr("cx", d => scaleX(d.year))
    .attr("cy", d => scaleY(d.female))
    
    dotFemale.on("mouseover", function(d){
                          div.transition()
                          .duration(tShort)
                          .style('opacity', .9);
                          div.html(d)
                            .style("left", d3.event.pageX + "px")
                            .style("top", (d3.event.pageY - 28) + "px");
    })
   
    
  chartArea.selectAll(".dotmale")
    .data(newData)
    .transition()
    .duration(tLong)
    .attr("cx", d => scaleX(d.year))
    .attr("cy", d => scaleY(d.male))
  
  
  // Udpate axes
  yAxis.transition()
    .duration(tLong)
    .call(d3.axisLeft(scaleY))
    
  xAxis.transition()
    .duration(tLong)
    .call(d3.axisBottom(scaleX)
            .tickFormat(d3.format("")))
  
  // Update axis titles
  topG.select(".y.axisTitle")
    .transition()
    .duration(tLong)
    .text(varName)
    .style("text-anchor", "middle");

    

    
}

// When data is updated via shiny, the following code is run:
r2d3.onRender(function(newData) {
  if (newData.length > 0){
    update(newData);
  }
}); 