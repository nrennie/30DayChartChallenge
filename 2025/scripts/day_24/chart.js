function whoPlot(data) {
  
    const margin = {top: 70, right: 30, bottom: 70, left: 50};
    const width = 700 - margin.left - margin.right;
    const height = 500 - margin.top - margin.bottom;
    const bgCol = "white";

    const xScale = d3.scaleLinear()
        .domain([2008.5, 2019.5])
        .range([ 0, width ]);

    const yScale = d3.scaleLinear()
        .domain([-40, 40])
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
        .attr("transform", "translate(0," + height / 2 + ")")
        .call(d3.axisBottom(xScale)
        .tickFormat(d3.format(".0f")));

    svg.append('g')
        .call(d3.axisLeft(yScale)
          .tickFormat(d => Math.abs(d))
        );

    // Lines
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
      .attr("fill", "#046A38")
      .attr("fill-opacity", 0.2)
      .attr("stroke", "none")
      .attr("d", d3.area()
        .x(function(d) { return xScale(d.Year) })
        .y0(function(d) { return yScale(d.Income) })
        .y1(function(d) { return yScale(0) })
      )

    svg.append("path")
        .datum(data)
        .attr("stroke", "#1B9AAA")
        .attr("stroke-width", 3)
        .attr("fill", "none")
        .attr("d", d3.line()
          .x(function(d) { return xScale(d.Year) })
          .y(function(d) { return yScale(- d.Handwashing) })
        )

    svg.append("path")
        .datum(data)
        .attr("fill", "#1B9AAA")
        .attr("fill-opacity", 0.2)
        .attr("stroke", "none")
        .attr("d", d3.area()
          .x(function(d) { return xScale(d.Year) })
          .y0(function(d) { return yScale(0) })
          .y1(function(d) { return yScale(-d.Handwashing) })
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
        .attr("text-anchor", "middle")
        .attr("y", height * 7/8)
        .attr("x", width/2)
        .text("People in rural areas without access to basic handwashing facilities (%) ")
        .attr('fill', "#1B9AAA")
        .style("font-size", "16px")
        .style("font-weight", "bold");

    svg.append("text")
        .attr("text-anchor", "middle")
        .attr("y", height * 1/8 - 15)
        .attr("x", width / 2)
        .text("Income received by richest 1% of population (%)")
        .attr('fill', "#046A38")
        .style("font-size", "16px")
        .style("font-weight", "bold");

    // Title
    svg.append("text")
        .attr("text-anchor", "left")
        .attr("y", -40)
        .attr("x", 0 - margin.left)
        .text("Income inequality and access to handwashing facilities in Mexico. 2009 - 2019")
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
        .text("Data: World Inequality Database (processed by Our World in Data). World Health Organization.")
        .style("font-size", "10px");

  
  };

  d3.csv("data.csv", d => ({
    Year: +d.Year,
    Income: +d.Income,
    Handwashing: +d.Handwashing
  }))
  .then(data => {
    whoPlot(data);
  });