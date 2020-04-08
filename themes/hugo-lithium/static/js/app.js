var hash = window.location.hash;
window.location.hash = "";

document.addEventListener("DOMContentLoaded", function () {
    if (window.location.pathname === "/") {
        window.addEventListener("hashchange", function (e) {
            e.preventDefault();
        })

        // Set the image width to 100% for the peak hispitalization grpahs so it works
        // well on mobiles
        document.getElementById("peak-hospitalization-projections").querySelector("img").style = "width: 100%;"
        // Resize all plotly graphs if the user is viewing them on mobile screens
        // so they lool better
        setTimeout(() => {
            if (window.innerWidth <= 425) {
                var plotlyPlots = document.querySelectorAll(".js-plotly-plot")
                for (var i = 0; i < plotlyPlots.length; i++) {
                    Plotly.relayout(plotlyPlots[i].getAttribute("id"), {
                        width: window.innerWidth
                    })
                }
            }
        })

        if (hash) {
            var NavbarHeight = 70;
            window.scroll({
                top:
                    document.querySelector(hash).offsetTop -
                    NavbarHeight,
                left: 0,
                behavior: 'smooth',
            });

            setTimeout(() => {
                history.replaceState(null, null, document.location.pathname + hash);
            })
        }

        var vizDropdown = document.getElementById("viz-dropdown");
        vizDropdown.addEventListener("click", function (e) {
            e.preventDefault();

            var NavbarHeight = 70;
            window.scroll({
                top:
                    document.querySelector(e.target.getAttribute("href").replace("/", "")).offsetTop -
                    NavbarHeight,
                left: 0,
                behavior: 'smooth',
            });
            history.pushState(null, null, document.location.pathname + e.target.getAttribute("href").replace("/", ""));
        })
    }
})