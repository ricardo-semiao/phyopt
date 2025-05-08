document.addEventListener('DOMContentLoaded', function() {
    var navlinks = document.querySelectorAll('.nav-li a');
    for (var i = 0; i < navlinks.length; i++) {
        if (document.URL.split('/')[3] == navlinks[i].href.split('/')[3]) {
            navlinks[i].classList.add('current-page');
        }
    }
});
