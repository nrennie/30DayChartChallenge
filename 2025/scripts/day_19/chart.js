function smoothPlot(data) {
  
    const margin = {top: 70, right: 30, bottom: 70, left: 60};
    const width = 700 - margin.left - margin.right;
    const height = 500 - margin.top - margin.bottom;
    const bgCol = "white";

    const xScale = d3.scaleLinear()
        .domain([1979, 2024])
        .range([ 0, width ]);

    const yScale = d3.scaleLinear()
        .domain([0, 35])
        .range([ height, 0]);

    const loessRegression = d3.regressionLoess()
        .x(d => d.Year)
        .y(d => d.Income)
        .bandwidth(0.5);
       
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

    // Points
    svg.selectAll('circle')
      .data(data)
      .enter()
      .append('circle')
      .attr("cx", function (d) { return xScale(d.Year); } )
      .attr("cy", function (d) { return yScale(d.Income); } )
      .attr('r', 3)
      .attr('fill', "green")
      .attr('fill-opacity', 0.1);

    // Regression line
    lineGenerator = d3.line()
      .x(d => xScale(d[0]))
      .y(d => yScale(d[1]));

    svg.append("path")
      .attr("class", "regression")
      .datum(loessRegression(data))
      .attr("d", lineGenerator);

     // Y axis label
    svg.append("text")
        .attr("text-anchor", "middle")
        .attr("transform", "rotate(-90)")
        .attr("y", -margin.left + 20)
        .attr("x", -margin.top - height/2 + margin.bottom)
        .text("% of income received by richest 1%")
        .style("font-size", "12px");

    // Title
    svg.append("text")
        .attr("text-anchor", "left")
        .attr("y", -40)
        .attr("x", 0 - margin.left)
        .text("The share of income received by the richest 1% of the population.")
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
    Income: +d.Income
  }))
  .then(data => {
    smoothPlot(data);
  });