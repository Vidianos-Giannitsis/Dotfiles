/**
 * SPDX-FileCopyrightText: 2020,2021 Jens Lechtenbörger
 * SPDX-License-Identifier: GPL-3.0-or-later
 *
 * Use bit patterns to selectively hide links with URL parameter "hidelinks".
 * Currently, bits 1, 2, 4, 8, 16, 32 are used, with a max value of 63:
 * 1 - Hide backward links
 * 2 - Hide forward links
 * 4 - Hide links between presentations
 * 8 - Hide links to other resources under oer.gitlab.io
 * 16 - Hide external links
 * 32 - Hide external links beyond class topics
**/

if ( window.location.search.match( /hidelinks/gi ) ) {
    const number = parseInt((new URL(window.location.href)).searchParams.get("hidelinks")) || 31;
    let links = document.querySelectorAll("a[href]");
    const fragmentRe = new RegExp("^#");
    const httpRe = new RegExp("^https?://");
    const oerRe = new RegExp("^https://oer.gitlab.io/");
    for (let i = 0, l = links.length; i < l; i++) {
        let link = links[i];
        const href = link.getAttribute("href");
        if ( fragmentRe.test(href) ) {
            if (   ( number & 1 && link.classList.contains("backwardlink") )
                || ( number & 2 && link.classList.contains("forwardlink") ) )
                link.classList.add("hiddenlink");
        }
        else if ( httpRe.test(href) ) {
            if (   ( number & 8 && oerRe.test(href) )
                || ( number & 16 )
                || ( number & 32 && link.classList.contains("beyondlink")) )
                link.classList.add("hiddenlink");
        }
        else if ( number & 4 && href.length > 0 )
            link.classList.add("hiddenlink");
    }
}
