document.addEventListener("DOMContentLoaded", function () {
    var vizDropdown = document.getElementById("viz-dropdown");
    vizDropdown.addEventListener("click", function (e) {
        e.preventDefault();

        var NavbarHeight = 70;
        window.scroll({
            top:
                document.querySelector(e.target.getAttribute("href")).offsetTop -
                NavbarHeight,
            left: 0,
            behavior: 'smooth',
        });
        history.replaceState(null, null, document.location.pathname + e.target.getAttribute("href"));
    })
})


