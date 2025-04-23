function monoPlot(data) {
  
    const margin = 10;
    const width = 600 - 2*margin;
    const height = 600 - 2*margin;
    const bgCol = "#9D1B69";

    const xScale = d3.scalePow()
        .domain([-4, 4])
        .range([ 0, width ])
        .exponent(0.5);

    const yScale = d3.scalePow()
        .domain([-13, 13])
        .range([ height, 0])
        .exponent(0.5);
    
    const alphaScale = d3.scaleLinear()
        .domain([0, 25])
        .range([0, 1]);

    // Plot
    const chartContainer = d3.select("#chart")
      .style('background-color', bgCol);
  
    const svg = chartContainer
      .append("svg")
      .attr("width", width + 2*margin)
      .attr("height", height + 2*margin)
      .append("g")
      .attr("transform", "translate(" + margin + "," + margin + ")");

    svg.selectAll('circle')
        .data(data)
        .enter()
        .append('circle')
        .attr("cx", function (d) { return xScale(d.ChangeA); } )
        .attr("cy", function (d) { return yScale(d.ChangeB); } )
        .attr('r', d => 4 * d.Continent)
        .attr('fill', "#F0A8D3")
        .attr('stroke', "#F0A8D3")
        .attr('fill-opacity', function (d) { return alphaScale(d.Income); });
  
  };

  d3.csv("data.csv", d => ({
    Income: +d.Income,
    ChangeA: +d.ChangeA,
    ChangeB: +d.ChangeB,
    Continent: +d.Continent
  }))
  .then(data => {
    monoPlot(data);
  });