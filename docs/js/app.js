console.log(window.location.hash)
var hash = window.location.hash;
window.location.hash = "";

document.addEventListener("DOMContentLoaded", function () {
    if (window.location.pathname === "/") {
        window.addEventListener("hashchange", function (e) {
            e.preventDefault();
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