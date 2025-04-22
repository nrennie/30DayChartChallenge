function elPaisPlot(data) {
  
    const margin = {top: 70, right: 60, bottom: 60, left: 25};
    const width = 700 - margin.left - margin.right;
    const height = 500 - margin.top - margin.bottom;
    const bgCol = "white";

    const xScale = d3.scaleLinear()
        .domain([1979, 2016])
        .range([ 0, width ]);

    const yScale = d3.scaleLinear()
        .domain([0, 35])
        .range([ height, 0]);

    const lastData = data[data.length-1];
    const lastYear = xScale(lastData.Year);
    const lastAus = yScale(lastData.Australia);
    const lastWorld = yScale(lastData.World);

    // Plot
    const chartContainer = d3.select("#chart")
      .style('background-color', bgCol);
  
    const svg = chartContainer
      .append("svg")
      .attr("width", width + margin.left + margin.right)
      .attr("height", height + margin.top + margin.bottom)
      .append("g")
      .attr("transform", "translate(" + margin.left + "," + margin.top + ")");
    
    svg.append("g")
        .attr("transform", "translate(0," + height + ")")
        .call(d3.axisBottom(xScale)
        .tickFormat(d3.format(".0f")));

    svg.append("g")
        .call(d3.axisLeft(yScale))
        .attr('class', 'yAxisline');

    // Grid lines
    svg.append("g")
      .attr('class', 'grid-lines')
      .selectAll('line')
      .data(yScale.ticks())
      .join('line')
      .attr('x1', 0)
      .attr('x2', width)
      .attr('y1', d => yScale(d))
      .attr('y2', d => yScale(d));

    // Shaded area
    svg.append("path")
      .datum(data)
      .attr("fill", "#858585")
      .attr("fill-opacity", 0.4)
      .attr("stroke", "none")
      .attr("d", d3.area()
        .x(function(d) { return xScale(d.Year) })
        .y0(function(d) { return yScale(d.Australia) })
        .y1(function(d) { return yScale(d.World) })
        )

    // Lines
    svg.append("path")
      .datum(data)
      .attr("fill", "none")
      .attr("stroke", "steelblue")
      .attr("stroke-width", 3)
      .attr("d", d3.line()
        .x(function(d) { return xScale(d.Year) })
        .y(function(d) { return yScale(d.Australia) })
        )

    svg.append("path")
        .datum(data)
        .attr("fill", "none")
        .attr("stroke", "red")
        .attr("stroke-width", 3)
        .attr("d", d3.line()
          .x(function(d) { return xScale(d.Year) })
          .y(function(d) { return yScale(d.World) })
          );
    
    // Points
    svg.append('circle')
      .attr("cx", lastYear)
      .attr("cy", lastAus)
      .attr('r', 5)
      .attr('fill', "steelblue");

    svg.append('circle')
      .attr("cx", lastYear)
      .attr("cy", lastWorld)
      .attr('r', 5)
      .attr('fill', "red");
    
    // Text
    svg.append('text')
      .attr("x", lastYear + 10)
      .attr("y", lastAus + 5)
      .text("Australia")
      .attr('fill', "steelblue")
      .style("font-size", "12px")
      .style("font-weight", "bold");

    svg.append('text')
      .attr("x", lastYear + 10)
      .attr("y", lastWorld + 5)
      .text("World")
      .attr('fill', "red")
      .style("font-size", "12px")
      .style("font-weight", "bold");


    // Title
    svg.append("text")
        .attr("text-anchor", "left")
        .attr("y", -40)
        .attr("x", 0 - margin.left)
        .text("The share of income received by the richest 1% of the population")
        .style("font-size", "16px")
        .style("font-weight", "bold");

    // Subtitle
    svg.append("text")
        .attr("text-anchor", "left")
        .attr("y", -20)
        .attr("x", 0 - margin.left)
        .text("% of income received by richest 1%")
        .style("font-size", "12px");

    // Caption
    svg.append("text")
        .attr("text-anchor", "left")
        .attr("y", height + 50)
        .attr("x", 0 - margin.left)
        .text("Data: World Inequality Database (WID). Processed by Our World in Data")
        .style("font-size", "10px")
        .style("fill", "#858585");

  
  };

  d3.csv("data.csv", d => ({
    Year: +d.Year,
    Australia: +d.Australia,
    World: +d.World
  }))
  .then(data => {
    elPaisPlot(data);
  });