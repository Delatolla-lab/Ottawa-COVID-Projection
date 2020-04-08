// Store the current hash in the URL. If we are in the root page we will handle
// scrolling to this location on the webpage
var currentHash = window.location.hash;
window.location.hash = "";

document.addEventListener("DOMContentLoaded", function () {
    if (window.location.pathname === "/") {
        // We will handle all hash changes since we want to scroll to the
        // visualizations ourselves
        window.addEventListener("hashchange", function (e) {
            e.preventDefault();
        })

        // If the user clicked on a has link on the root page outside of the 
        // root page for eg. one of the visualizations dropdown links
        if (currentHash) {
            scrollToAnchorTag(currentHash)
        }

        var vizDropdown = document.getElementById("viz-dropdown");
        vizDropdown.addEventListener("click", function (e) {
            e.preventDefault();

            var anchorTag = e.target.getAttribute("href").replace("/", "")
            scrollToAnchorTag(anchorTag)
        })
    }
})

function scrollToAnchorTag(anchorTag) {
    var NavbarHeight = 70;
    window.scroll({
        top:
            document.querySelector(anchorTag).offsetTop -
            NavbarHeight,
        left: 0,
        behavior: 'smooth',
    });
    setTimeout(() => {
        history.pushState(null, null, document.location.pathname + anchorTag);
    })
}