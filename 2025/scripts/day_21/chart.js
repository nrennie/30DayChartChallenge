function fossilPlot(data) {
  
    const margin = {top: 70, right: 30, bottom: 70, left: 50};
    const width = 700 - margin.left - margin.right;
    const height = 500 - margin.top - margin.bottom;
    const bgCol = "white";

    const xScale = d3.scaleLinear()
        .domain([1965, 1995])
        .range([ 0, width ]);

    const yScale = d3.scaleLinear()
        .domain([0, 20])
        .range([ height, 0]);

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
        .call(d3.axisLeft(yScale));

    // Lines
    svg.append("path")
      .datum(data)
      .attr("fill", "#046A38")
      .attr("fill-opacity", 0.1)
      .attr("stroke", "none")
      .attr("d", d3.area()
        .x(function(d) { return xScale(d.Year) })
        .y0(function(d) { return yScale(d.Income) })
        .y1(function(d) { return yScale(d.Income - d.Income) })
      )

    svg.append("path")
      .datum(data)
      .attr("stroke", "#046A38")
      .attr("stroke-width", 3)
      .attr("fill", "none")
      .attr("d", d3.line()
        .x(function(d) { return xScale(d.Year) })
        .y(function(d) { return yScale(d.Income) })
      )

    svg.append("path")
      .datum(data)
      .attr("fill", "#FF671F")
      .attr("fill-opacity", 0.1)
      .attr("stroke", "none")
      .attr("d", d3.area()
        .x(function(d) { return xScale(d.Year) })
        .y0(function(d) { return yScale(100 - d.Fossil) })
        .y1(function(d) { return yScale(d.Fossil - d.Fossil) })
      )

    svg.append("path")
        .datum(data)
        .attr("stroke", "#FF671F")
        .attr("stroke-width", 3)
        .attr("fill", "none")
        .attr("d", d3.line()
          .x(function(d) { return xScale(d.Year) })
          .y(function(d) { return yScale(100 - d.Fossil) })
        )

     // Y axis label
    svg.append("text")
      .attr("text-anchor", "middle")
      .attr("transform", "rotate(-90)")
      .attr("y", -margin.left + 20)
      .attr("x", -margin.top - height/2 + margin.bottom)
      .text("Percentage (%)")
      .style("font-size", "12px");


    svg.append("text")
        .attr("text-anchor", "left")
        .attr("y", 280)
        .attr("x", 175)
        .text("Percentage of income held by richest 1% decreases")
        .attr('fill', "#046A38")
        .style("font-size", "16px")
        .style("font-weight", "bold");

    svg.append("text")
        .attr("text-anchor", "left")
        .attr("y", 60)
        .attr("x", 110)
        .text("Percentage of non-fossil fuels increases")
        .attr('fill', "#FF671F")
        .style("font-size", "16px")
        .style("font-weight", "bold");

    // Title
    svg.append("text")
        .attr("text-anchor", "left")
        .attr("y", -40)
        .attr("x", 0 - margin.left)
        .text("Income inequality and fossil fuels in India. 1965 - 1995.")
        .style("font-size", "16px")
        .style("font-weight", "bold");

    // Subtitle
    svg.append("text")
        .attr("text-anchor", "left")
        .attr("y", -20)
        .attr("x", 0 - margin.left)
        .text("Income is measured before payment of taxes and non-pension benefits but after the payment of public and private pensions.")
        .style("font-size", "12px");

    // Caption
    svg.append("text")
        .attr("text-anchor", "left")
        .attr("y", height + 50)
        .attr("x", 0 - margin.left)
        .text("Data: World Inequality Database (WID). Processed by Our World in Data")
        .style("font-size", "10px");

  
  };

  d3.csv("data.csv", d => ({
    Year: +d.Year,
    Income: +d.Income,
    Fossil: +d.Fossil
  }))
  .then(data => {
    fossilPlot(data);
  });