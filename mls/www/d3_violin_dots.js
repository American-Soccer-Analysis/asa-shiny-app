// Set dimensions and margins -------------------
var margin = {top: 10, right: 20, bottom: 50, left: 20},
    width = width - margin.left - margin.right,
    height = height - margin.top - margin.bottom;

// Set x and y ranges ---------------------------
var x = d3.scaleLinear().range([0, width]);
var y = d3.scaleLinear().range([height, 0]);

// Set color scale for positions ----------------
var color = d3.scaleOrdinal()
              .domain(['FW', 'MF', 'DF'])
              .range(['#02ACE6', '#685786', '#CF0327']);

// Set class for tooltips -----------------------
var div = d3.select("body").append("div")
    .attr("class", "tooltip")
    .style("opacity", 0);

// Custom function for creating gridlines -------
function make_x_gridlines() {
    return d3.axisBottom(x)
        .ticks(10);
}

// Append svg to the page body ------------------
var svg = svg.append("svg")
    .attr("width", width + margin.left + margin.right)
    .attr("height", height + margin.top + margin.bottom)
  .append("g")
    .attr("transform",
          "translate(" + margin.left + "," + margin.top + ")");


// Render elements ------------------------------
r2d3.onRender(function(data, s, w, h, options) {

  // Scale the range of the data
  if (options.x_axis_absolute === true) {
     x.domain([0, d3.max(data, function(d) { return d.x_val; })]);
  } else {
     x.domain([d3.min(data, function(d) { return d.x_val; }),
               d3.max(data, function(d) { return d.x_val; })]);
  }

  y.domain([d3.min(data, function(d) { return d.y_val; }) - 1,
            d3.max(data, function(d) { return d.y_val; }) + 1]);

  // Add the x-gridlines
  svg.append("g")
      .attr("class", "grid")
      .attr("transform", "translate(0," + height + ")")
      .call(make_x_gridlines()
          .tickSize(-height)
          .tickFormat("")
      );

  // Add the x-axis
  svg.append("g")
      .attr("transform", "translate(0," + height + ")")
      .call(d3.axisBottom(x).tickFormat(d => d + options.x_axis_suffix));

  // Add the dots with tooltips
  svg.selectAll("dot")
     .data(data)
   .enter().append("circle")
     .attr("r", function(d) {
          if (d.current_player === true) { return 6 }
          else 	{ return 3.5 } })
     .attr("cx", function(d) { return x(d.x_val); })
     .attr("cy", function(d) { return y(d.y_val); })
     .attr("fill", function(d) {
          if (d.current_player === true) { return "yellow" }
          else 	{ return color(d.broad_position) } })
     .attr("stroke", function(d) {
          if (d.current_player === true) { return "black" }
          else 	{ return "none" } })
     .on("mouseover", function(d) {
       div.transition()
         .duration(200)
         .style("opacity", 1);
       div.html("<strong>" + d.x_tooltip + options.annotation_suffix + "</strong><br/>" + d.player_name)
         .style("left", (d3.event.pageX + 5) + "px")
         .style("top", (d3.event.pageY + 5) + "px")
         .style("text-align", "center")
         .style("font", "12px proxima-nova, sans-serif")
         .style("padding", "10px")
         .style("background", "white")
         .style("border", "0.5pt solid #ddd")
         .style("box-shadow", "0px 0px 5px 0px rgba(123, 123, 123, 0.25)")
         .style("position", "absolute")
         .style("pointer-events", "none")
         .style("color", "#444");
       d3.select(this)
         .style("stroke", "black");
       })
     .on("mouseout", function(d) {
       div.transition()
         .duration(500)
         .style("opacity", 0);
       d3.select(this)
         .style("stroke", function(d) {
            if (d.current_player === true) {return "black"}
            else 	{ return "none" } });
       });

  // Add the legend
  var legend = svg.append("g")
    .attr("transform", `translate(${width - margin.right - 10}, ${margin.top})`);

  // Legend title
  legend.append("text")
        .text("Position")
        .attr("font-size", "0.7em")
        .attr("font-weight", "bold")
        .attr("font-family", "proxima-nova, sans-serif");

  // Legend circles
  legend.selectAll(".legend-mark")
     .data(['FW', 'MF', 'DF'])
     .join("circle")
     .attr("class", "legend-mark")
     .attr("cx", 5)
     .attr("cy", (d, i) => (i + 1) * 15)
     .attr("fill", d => color(d))
     .attr("r", 4);

  // Legend names
  legend.selectAll(".legend-name")
     .data(['FW', 'MF', 'DF'])
     .join("text")
     .attr("class", "legend-name")
     .attr("x", 15)
     .attr("y", (d, i) => (i + 1) * 15)
     .attr("text-anchor", "start")
     .attr("alignment-baseline", "middle")
     .attr("font-size", "0.7em")
     .attr("font-family", "proxima-nova, sans-serif")
     .text(d => d);

  // Add the annotation
   var annotation = svg.append("g")
    .attr("transform", `translate(${width - margin.right - 175}, ${height - margin.bottom + 15})`);

  // Annotation circle
  annotation.selectAll(".annotation-mark")
     .data(data.filter(function(d) {return d.current_player === true;}))
     .join("circle")
     .attr("class", "annotation-mark")
     .attr("cx", 5)
     .attr("cy", (d, i) => (i + 1) * 15)
     .attr("fill", "yellow")
     .attr("stroke", "black")
     .attr("r", 4);

  // Annotation name
  annotation.selectAll(".annotation-name")
     .data(data.filter(function(d) {return d.current_player === true;}))
     .join("text")
     .attr("class", "annotation-name")
     .attr("x", 15)
     .attr("y", (d, i) => (i + 1) * 15)
     .attr("text-anchor", "start")
     .attr("alignment-baseline", "middle")
     .attr("font-size", "0.7em")
     .attr("font-weight", "bold")
     .attr("font-family", "proxima-nova, sans-serif")
     .text(function(d) { return d.player_name + " (" + d.x_tooltip + options.annotation_suffix + ")" });

  // Axis title
   svg.append("text")
    .attr("transform",
          "translate(" + (width/2) + " ," +
                         (height + margin.top + 35) + ")")
    .style("text-anchor", "middle")
    .style("font-family", "proxima-nova, sans-serif")
    .style("font-weight", "bold")
    .style("font-size", "14px")
    .text(options.x_axis_title);
});
