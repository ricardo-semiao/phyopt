document.addEventListener("DOMContentLoaded", function() {
    // Show/hide package navbar, update content width accordingly
    const sn = document.querySelector('#show-navbar');
	const nav = document.querySelector('#rspkgdown-main nav.navbar');
    const content = document.querySelector("#rspkgdown-main > div.container");
	let isClicked = true; // Navbar starts shown

	sn.addEventListener("click", () => {
		isClicked = !isClicked;
		if (isClicked) {
			nav.style.visibility = 'visible';
			nav.style.width = 'unset';
            nav.ariaExpanded = "true";
            content.style.maxWidth = "calc(100% - 150px)";
		} else {
			nav.style.visibility = 'hidden';
			nav.style.width = '0';
            nav.ariaExpanded = "false";
            content.style.maxWidth = "100%";
		}
	});
    window.addEventListener("resize", function () {
        if (document.documentElement.clientWidth > 960) {
            nav.style.visibility = "visible";
            nav.style.width = "unset";
            content.style["max-width"] = "calc(100% - 150px)";       
            sn.style.display = "none";
            sn.ariaHidden = "true";
        } else {
            nav.style.visibility = 'hidden';
            nav.style.width = '0';
            content.style["max-width"] = "100%";
        }
    });

    // Manage the back to top button AND navbar hider, if one is present. From quarto-nav.js
    let lastScrollTop = window.pageYOffset || document.documentElement.scrollTop;
    const scrollDownBuffer = 5;
    const scrollUpBuffer = 100; // Large buffer in pkgdown
    const btt = document.getElementById("back-to-top");
    const main_height = document.querySelector("#rspkgdown-main").offsetHeight;

    window.document.addEventListener("scroll", function () {
        const currentScrollTop = window.pageYOffset || document.documentElement.scrollTop;

        // Shows and hides the button 'intelligently' as the user scrolls
        if (currentScrollTop - scrollDownBuffer > lastScrollTop) {
            if (document.documentElement.clientWidth < 960) {
                sn.style.display = "none";
                sn.ariaHidden = "true";
            }
            btt.style.display = "none";
            btt.ariaHidden = "true";
            lastScrollTop = currentScrollTop <= 0 ? 0 : currentScrollTop;
        } else if (currentScrollTop < lastScrollTop - scrollUpBuffer) {
            if (document.documentElement.clientWidth < 960) {
                sn.style.display = "inline-block";
                sn.ariaHidden = "false";
            }
            btt.style.display = "inline-block";
            btt.ariaHidden = "false";
            lastScrollTop = currentScrollTop <= 0 ? 0 : currentScrollTop;
        }

        // Show the button at the bottom, hides it at the top
        if (currentScrollTop <= 0) {
            btt.style.display = "none";
            btt.ariaHidden = "true";
        } else if (window.innerHeight + currentScrollTop >= main_height) {
            if (document.documentElement.clientWidth < 960) {
                sn.style.display = "inline-block";
                sn.ariaHidden = "false";
            }
            btt.style.display = "inline-block";
            btt.ariaHidden = "false";
        }
    }, false);
});
