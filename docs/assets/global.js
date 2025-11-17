"use strict";

// Global variables:
window.RSSITE = {
    rem: 16, // In px
    media_width_lg: 920,
    media_width_md: 740
}


// Get the viewport width that accounts for scrollbar:
// https://www.smashingmagazine.com/2023/12/new-css-viewport-units-not-solve-classic-scrollbar-problem/
new ResizeObserver(() => {
  let vw = document.documentElement.clientWidth / 100;
  document.documentElement.style.setProperty('--vw', `${vw}px`);
}).observe(document.documentElement);


// Set current page:
document.addEventListener('DOMContentLoaded', () => {
    const navLinks = document.querySelectorAll('#rssite-header a');
    for (let i = 0; i < navLinks.length; i++) {
        const link = navLinks[i];
        const currentPath = window.location.pathname.replace(/(^\/|\/$)/g, '');
        const linkPath = link.pathname.replace(/(^\/|\/$)/g, '');
        const patternLinkPath = new RegExp("^" + linkPath.replace(/[.*+?^${}()|[\]\\]/g, "\\$&") + "(/|$)");

        if (patternLinkPath.test(currentPath)) {
            link.setAttribute("aria-current", "page");

            const dropContainer = link.closest(".nav-drop");
            if (dropContainer) {
                dropContainer.querySelector("button").setAttribute("aria-current", "page");
            }
        }
    }
});


// Handle .nav-drop aria-expanded:
document.addEventListener("DOMContentLoaded", () => {
    const navDrops = document.querySelectorAll("#rssite-header .nav-drop");

    navDrops.forEach(drop => {
        const button = drop.querySelector("button")
        let isHovered = false;
        let isFocused = false;
        
        drop.addEventListener("focusin", () => {
            isFocused = true;
            button.setAttribute("aria-expanded", "true");
        });
        drop.addEventListener("focusout", () => {
            setTimeout(() => {
                isFocused = false;
                if (!isHovered) {
                    button.setAttribute("aria-expanded", "false");
                }
            }, 50);
        });
        drop.addEventListener("mouseenter", () => {
            isHovered = true;
            button.setAttribute("aria-expanded", "true");
        });
        drop.addEventListener("mouseleave", () => {
            isHovered = false;
            if (!isFocused) {
                button.setAttribute("aria-expanded", "false");
            }
        });
    });
});
