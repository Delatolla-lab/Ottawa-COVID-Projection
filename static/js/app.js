var hash = window.location.hash;
window.location.hash = "";

document.addEventListener("DOMContentLoaded", function() {
  updatePlotlyGraphs();

  if (hash) {
    scrollToElement(document.querySelector(hash));

    setTimeout(() => {
      history.replaceState(null, null, document.location.pathname + hash);
    });
  }

  var dropdowns = document.getElementsByClassName("navbar-dropdown");
  for (var i = 0; i < dropdowns.length; i++) {
    dropdowns.item(i).addEventListener("click", function(e) {
      var hash = new URL(e.target.getAttribute("href"), window.location.origin)
        .hash;

      var elementToScrollTo = document.querySelector(hash);
      if (elementToScrollTo) {
        e.preventDefault();

        scrollToElement(elementToScrollTo);

        history.pushState(null, null, document.location.pathname + hash);
      }
    });
  }
});

function updatePlotlyGraphs() {
  if (window.location.pathname === "/") {
    // Set the image width to 100% for the peak hispitalization grpahs so it works
    // well on mobiles
    document
      .getElementById("peak-hosp-projections")
      .querySelector("img").style = "width: 100%;";
    // Resize all plotly graphs if the user is viewing them on mobile screens
    // so they lool better
    setTimeout(() => {
      if (window.innerWidth <= 425) {
        var plotlyPlots = document.querySelectorAll(".js-plotly-plot");
        for (var i = 0; i < plotlyPlots.length; i++) {
          Plotly.relayout(plotlyPlots[i].getAttribute("id"), {
            width: window.innerWidth
          });
          // Move the legends to the top left of the graphs so the
          // graphs itself can become bigger
          Plotly.update(plotlyPlots[i].getAttribute("id"), null, {
            legend: {
              x: 0,
              y: 200
            }
          });
        }
      }
    });
  }
}

function scrollToElement(element) {
  var NavbarHeight = 70;
  window.scroll({
    top: element.offsetTop - NavbarHeight,
    left: 0,
    behavior: "smooth"
  });
}
