function wafflePlot(data) {
  
    const margin = {top: 70, right: 10, bottom: 40, left: 10};
    const width = 700 - margin.left - margin.right;
    const height = 380 - margin.top - margin.bottom;
    const bgCol = "white";
    const iconSize = 24;

    const xScale = d3.scaleLinear()
        .domain([0.5, 25.5])
        .range([ 0, width ]);

    const yScale = d3.scaleLinear()
        .domain([0.5, 10.5])
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

      data.forEach(d => {
        d3.xml(d.icon).then(iconXml => {
            const svgNode = iconXml.documentElement.cloneNode(true);
    
            d3.select(svgNode)
                .attr('x', xScale(d.x) - iconSize / 2)
                .attr('y', yScale(d.y) - iconSize / 2)
                .attr('width', iconSize)
                .attr('height', iconSize)
                .style('fill', d.colour)
                .style('opacity', 1);
    
            svg.node().appendChild(svgNode);
        });
    });

    // Title
    svg.append("text")
        .attr("text-anchor", "left")
        .attr("y", -40)
        .attr("x", 0 - margin.left)
        .text("1% of people have 21% of the world's wealth.")
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
        .attr("y", height + 30)
        .attr("x", 0 - margin.left)
        .text("Data: World Inequality Database (WID). Processed by Our World in Data")
        .style("font-size", "10px");

  
  };

  d3.csv("data.csv", d => ({
    x: +d.x,
    y: +d.y,
    colour: d.colour,
    icon: d.icon
  }))
  .then(data => {
    wafflePlot(data);
  });