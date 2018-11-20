// !preview r2d3 data=export$`Balloon angioplasty`$age$cases[year == 2015]
//
// r2d3: https://rstudio.github.io/r2d3
//

var initData = d3.nest().key(d => d.grouping).entries(data)
var margin = ({top:20, right:30, bottom:30, left:60});
var Gwidth = width - margin.left - margin.right
var Gheight = height - margin.top - margin.bottom
var barPadding = 0.1;

var topG = svg.append('g')
    .attr('transform', 'translate(' + margin.left + ',' + margin.top +')')


// Initial scales
var scaleY = d3.scaleLinear()
  .domain([0, d3.max(data, d=> d.value)]).nice()
  .range([Gheight - 0]);

// Scale between the groupings (i.e. b/w age groups`)
var scaleX = d3.scaleBand()
  .domain(data.map(d => d.grouping))
  .range([0, Gwidth])
  .padding(barPadding);

/* Scale within groupings (i.e. male/female). The range is from 0 to the width of each grouping */
var scaleX1 = d3.scaleBand()
  .domain(data.map(d => d.key))
  .range(0, scaleX.bandwidth())


// Initial axis
var yAxis = topG.append('g')
  .attr("class", "y axis")
  

var xAxis = topG.append('g')
    .attr("class", "x axis")

xAxis.call(d3.axisBottom(scaleX))
    .attr("transform", 'translate(' + 0 + "," + Gheight + ')');

var chartArea = topG.append("g")
    .attr("class", "chartArea");



// UPDATE FUNCTION - will be called by r2d3.onRender()
function update(inData) {


  var newData = d3.nest().key(d => d.grouping).entries(inData);
   
  
  var maxY = d3.max(newData, d => d3.max(d.values, k => k.n_patients));
  grouping1Names = newData.map(d => d.key);
  grouping2Names = newData[0].values.map(d => d.sex);
  var t = 1000;
  // Scales used in updates (these take the updated data as arguments, as opposed to the fn() calls outside the update() )
  
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
  
  
  // Perform the data join
   
   var barGroupWithData = chartArea.selectAll('.barGroup').data(newData);
   
    barGroupWithData.exit().remove();
   
   
   var bars = barGroupWithData.enter()
      .append("g")
      .attr("transform", d => "translate(" + scaleX(d.key) + ",0)")
      .attr("fill", "steelblue")
      .merge(barGroupWithData)
      .selectAll("rect")
          .data(d => Object.keys(d.values)
                          .map(k => ({ keyL2: grouping2Names[k], value: d.values[k].n_patients }) ));
      
  bars.enter()
    .append("rect")
    .attr("fill", "steelblue")
    .attr("y", d => scaleY(0))
    .merge(bars)
    .attr("x", (d, i) => scaleX1(d.keyL2))
    .attr("width", scaleX1.bandwidth())
    .transition()
    .duration(t)
    .ease(d3.easeLinear)
    .attr('y', d => scaleY(d.value))
    .attr("height", d => scaleY(0) - scaleY(d.value));
  
  
  bars.exit().transition().style('opacity', 0).remove()
  
  // Add new elements, update existing AND new elements    
  
  /*var bars = layersWithData.enter()
      .append("g")
      .attr("transform", d => 'translate')
      .merge(layersWithData)
      
      .attr("fill", "steelblue")
     
    */  
      
      
  
   
  // Udpate axis
  
  yAxis.transition()
    .duration(t)
    .call(d3.axisLeft(scaleY))
    
    
  xAxis.transition()
    .duration(t)
    .call(d3.axisBottom(scaleX))
     
    
  
    
  
    
    
  
    
}



r2d3.onRender(function(newData) {
  update(newData);
});