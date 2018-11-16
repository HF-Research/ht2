// !preview r2d3 data=export$`Balloon angioplasty`$age$cases[year == 2015]
//
// r2d3: https://rstudio.github.io/r2d3
//


var margin = ({top:20, right:0, bottom:30, left:40});

var barPadding = 0.1;




// Initial scales
var scaleY = d3.scaleLinear()
  .domain([0, d3.max(data, d=> d.n_patients)]).nice()
  .range([height - margin.bottom, margin.top]);

// Scale between the groupings (i.e. b/w age groups`)
var scaleX = d3.scaleBand()
  .domain(data.map(d => d.grouping))
  .range([margin.left, width - margin.right])
  .padding(barPadding);

/* Scale within groupings (i.e. male/female). The range is from 0 to the width of each grouping */
var scaleX1 = d3.scaleBand()
  .domain(data.map(d => d.sex))
  .range(0, scaleX.bandwidth())


// Initial axis
svg.append('g')
  .call(d3.axisLeft(scaleY))
  .attr("class", "y axis")
  .attr("transform", "translate("+(margin.right + 4) + "0)");

svg.append('g')
    .call(d3.axisBottom(scaleX))
    .attr("class", "x axis")
    .attr("transform", `translate(0,${height - margin.bottom})`);


// Initial data join
svg.selectAll(".groups").data(data).enter()
    .append("g")
    .attr("class", "g")
    .attr("x", d => scaleX(d.grouping))
    .attr("width", scaleX.bandwidth())
    .attr("fill", "steelblue");


// UPDATE FUNCTION - will be called by r2d3.onRender()
function update(newData) {
  
  var t = 600
// Scales used in updates (these take the updated data as arguments, as opposed to the fn() calls outside the update() )
var scaleY = d3.scaleLinear()
  .domain([0, d3.max(newData, d=> d.n_patients)]).nice()
  .range([height - margin.bottom, margin.top]);

var scaleX = d3.scaleBand()
  .domain(newData.map(d => d.grouping))
  .range([margin.left, width - margin.right])
  .padding(barPadding);



// Perform the data join
var bar = svg.selectAll(".groups").data(newData);
// Remove unused elements


// Add new elements, update existing AND new elements
bar.enter()
    .append("rect")
    .merge(bar)
    .attr("x", d => scaleX(d.grouping))
    .attr("width", scaleX.bandwidth())
    .attr("fill", "steelblue");

// Transition exit not working
bar.transition()
    .duration(t)
    .ease(d3.easeLinear)
    .attr('y', d => scaleY(d.n_patients))
    .attr("height", d => scaleY(0) - scaleY(d.n_patients))
    .attr("x", d => scaleX(d.grouping))
    .attr("width", scaleX.bandwidth());

bar.exit()
  .transition()
    .duration(t)
    .style("fill-opacity", 0)
    .attr('y', d => scaleY(d.n_patients))
    .attr("height", d => scaleY(0) - scaleY(d.n_patients))
  .remove();  
  
// Udpate axis

svg.select('.y')
  .transition()
    .duration(t)
  .call(d3.axisLeft(scaleY))
  .attr("transform", "translate("+(margin.right + 4) + "0)");
  
svg.select('.x')
  .transition()
  .duration(t)
  .call(d3.axisBottom(scaleX))
  .attr("transform", `translate(0,${height - margin.bottom})`);
  

  

  
  

  
}



r2d3.onRender(function(newData) {
  update(newData);
});