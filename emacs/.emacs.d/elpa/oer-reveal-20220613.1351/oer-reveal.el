;;; oer-reveal.el --- OER with reveal.js, plugins, and org-re-reveal  -*- lexical-binding: t; -*-
;; -*- Mode: Emacs-Lisp -*-
;; -*- coding: utf-8 -*-

;; SPDX-FileCopyrightText: 2017-2022 Jens Lechtenbörger
;; SPDX-License-Identifier: GPL-3.0-or-later

;; Author: Jens Lechtenbörger
;; URL: https://gitlab.com/oer/oer-reveal
;; Version: 4.5.0
;; Package-Requires: ((emacs "24.4") (org-re-reveal "3.1.0"))
;; Keywords: hypermedia, tools, slideshow, presentation, OER

;; Emacs 24.4 adds advice-add and advice-remove, which are used below.
;; Thus, Emacs should not be older.

;;; License:
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.
;; If not, see http://www.gnu.org/licenses/ or write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;; The package *oer-reveal* extends *org-re-reveal* (available on
;; MELPA and at https://gitlab.com/oer/org-re-reveal) with resources
;; and functionality that aim to simplify the creation of Open
;; Educational Resources (OER).  This package defines an Org mode
;; export backend derived from *org-re-reveal* for export to HTML with
;; reveal.js.  It provides help in installing and configuring
;; reveal.js and several of its plugins and forms the basis of
;; emacs-reveal (https://gitlab.com/oer/emacs-reveal).
;;
;; See file README.md for more information; examples are included
;; under subdirectory "examples".
;;
;; Package *oer-reveal* is really meant to be used as part of
;; emacs-reveal (https://gitlab.com/oer/emacs-reveal).
;;
;; Note that you should require oer-reveal-publish, not oer-reveal.

;;; Code:
(require 'cl-lib) ; cl-mapcar
(require 'subr-x) ; string-trim, string-join
(require 'url-util) ; url-encode-url
(require 'org)
(require 'org-re-reveal)

(defvar oer-reveal-keys) ; Silence byte compiler

(unless (fboundp 'alist-get)
  ;; Following based on subr.el, Emacs 27.0.50.  Argument testfn removed
  ;; as assoc in older Emacsen only accepts two arguments.
  (defun alist-get (key alist &optional default remove)
    "Return the value associated with KEY in ALIST.
If KEY is not found in ALIST, return DEFAULT.
Use TESTFN to lookup in the alist if non-nil.  Otherwise, use `assq'.

This is a generalized variable suitable for use with `setf'.
When using it to set a value, optional argument REMOVE non-nil
means to remove KEY from ALIST if the new value is `eql' to DEFAULT."
    (ignore remove) ;;Silence byte-compiler.
    (let ((x (assq key alist)))
      (if x (cdr x) default))))

(defun oer-reveal-define-backend ()
  "Define the back-end for export as reveal.js presentation.
Derive from 're-reveal to add further options and keywords."
  (org-export-define-derived-backend 'oer-reveal 're-reveal

    :menu-entry
    `(,(nth 0 oer-reveal-keys) "Export to reveal.js HTML Presentation"
      ((,(nth 1 oer-reveal-keys)
        "To file" oer-reveal-export-to-html)
       (,(nth 2 oer-reveal-keys)
        "To file and browse" oer-reveal-export-to-html-and-browse)
       (,(nth 3 oer-reveal-keys)
        "Current subtree to file" oer-reveal-export-current-subtree)))

    :options-alist ; See org-export-options-alist for meaning of parts.
    '((:oer-reveal-plugins "OER_REVEAL_PLUGINS" nil oer-reveal-plugins t)
      (:oer-reveal-revealjs-version "OER_REVEAL_REVEALJS_VERSION" nil
                                    oer-reveal-revealjs-version t)
      (:oer-reveal-a11y-dependency "OER_REVEAL_A11Y_DEPENDENCY" nil
                                   oer-reveal-a11y-dependency t)
      (:oer-reveal-anything-dependency "OER_REVEAL_ANYTHING_DEPENDENCY" nil
                                       oer-reveal-anything-dependency t)
      (:oer-reveal-anything-config "OER_REVEAL_ANYTHING_CONFIG" nil
                                   oer-reveal-anything-config t)
      (:oer-reveal-anything-svg-opacity "OER_REVEAL_ANYTHING_SVG_OPACITY" nil
                                        oer-reveal-anything-svg-opacity t)
      (:oer-reveal-audio-slideshow-dependency "OER_REVEAL_AUDIO_SLIDESHOW_DEPENDENCY" nil
                                              oer-reveal-audio-slideshow-dependency t)
      (:oer-reveal-audio-slideshow-config "OER_REVEAL_AUDIO_SLIDESHOW_CONFIG" nil
                                          oer-reveal-audio-slideshow-config t)
      (:oer-reveal-coursemod-dependency "OER_REVEAL_COURSEMOD_DEPENDENCY" nil
                                        oer-reveal-coursemod-dependency t)
      (:oer-reveal-coursemod-config "OER_REVEAL_COURSEMOD_CONFIG" nil
                                    oer-reveal-coursemod-config t)
      (:oer-reveal-navigation-mode "OER_REVEAL_NAVIGATION_MODE" nil
                                    oer-reveal-navigation-mode t)
      (:oer-reveal-jump-dependency "OER_REVEAL_JUMP_DEPENDENCY" nil
                                   oer-reveal-jump-dependency t)
      (:oer-reveal-quiz-dependency "OER_REVEAL_QUIZ_DEPENDENCY" nil
                                   oer-reveal-quiz-dependency t)
      (:oer-reveal-toc-progress-dependency "OER_REVEAL_TOC_PROGRESS_DEPENDENCY" nil
                                           oer-reveal-toc-progress-dependency t)
      (:oer-reveal-rdf-prefixes "OER_REVEAL_RDF_PREFIXES" nil
                                oer-reveal-rdf-prefixes t)
      (:oer-reveal-rdf-typeof "OER_REVEAL_RDF_TYPEOF" nil
                              oer-reveal-rdf-typeof t)
      (:oer-reveal-without-subtitle "OER_REVEAL_WITHOUT_SUBTITLE" nil nil t)
      (:oer-reveal-copyright "SPDX-FILECOPYRIGHTTEXT" nil nil newline)
      (:oer-reveal-license "SPDX-LICENSE-IDENTIFIER" nil nil newline))

    :translate-alist
    '((template . oer-reveal-template))))

(defun oer-reveal-define-menu (symbol value)
  "Define back-end with (new) key bindings.
SYMBOL must be `oer-reveal-keys' and VALUE its new value."
  (let ((standard (eval (car (get symbol 'standard-value)))))
    (cl-assert
     (eq symbol 'oer-reveal-keys) nil
     (format "Symbol in oer-reveal-define-menu unexpected: %s" symbol))
    (cl-assert
     (= (length standard) (length value))
     (format "Value for oer-reveal-keys must have length %s (same as standard), not %s"
             (length standard) (length value)))
    (set-default symbol value)
    (oer-reveal-define-backend)))

;;; Customizable options and variables
(defgroup org-export-oer-reveal nil
  "Options for exporting Org files to reveal.js HTML pressentations.
The options here are provided by package oer-reveal.  They extend those
of org-re-reveal."
  :tag "Org Export oer-reveal"
  :group 'org-export-re-reveal)

(defcustom oer-reveal-keys '(?w ?w ?b ?s)
  "Define keys for export with oer-reveal.
This list must contain four characters: The first one triggers export
with oer-reveal (after \\<org-mode-map> \\[org-export-dispatch]).
The remaining three charaters each invoke a different export variant.
One of those characters must be typed after the first one; the
variants are, in sequence: Export to file, export to file followed by
browsing that file, subtree export to file."
  :group 'org-export-oer-reveal
  :type '(list (character :tag "Key to trigger export with oer-reveal")
               (character :tag "Key for export to file")
               (character :tag "Key to browse file after export")
               (character :tag "Key for subtree export to file"))
  :set #'oer-reveal-define-menu)

;; The following variable with its doc string is derived from AUCTeX's
;; TeX-master.
(defcustom oer-reveal-master t
  "The master file associated with the current buffer.
If the file being edited is actually included from another file, you
can tell oer-reveal the name of the master file by setting this variable.
If there are multiple levels of nesting, specify the top level file.

If this variable is nil, oer-reveal will query you for the name.

If the variable is t, oer-reveal will assume the file is a master file
itself.

Maybe use File Variables (see Info node `(emacs)Specifying File Variables'
in the Emacs manual) to set this variable permanently for each file."
  :group 'org-export-oer-reveal
  :type '(choice (const :tag "Use current buffer" t)
                 (const :tag "Query for filename" nil)
		 (file :format "%v"))
  :package-version '(oer-reveal . "2.8.0"))
(make-variable-buffer-local 'oer-reveal-master)

(defcustom oer-reveal-revealjs-version "4"
  "Version of reveal.js.
See `org-re-reveal-revealjs-version' for possible values.
If non-nil, this value takes precedence over
`org-re-reveal-revealjs-version' during export.

Note that you must set this variable to a correct value yourself.
This package does not attempt to install reveal.js.  If you want that,
go for `emacs-reveal', see URL `https://gitlab.com/oer/emacs-reveal/'."
  :group 'org-export-oer-reveal
  :type '(choice (const nil) string)
  :package-version '(oer-reveal . "3.0.0"))

(defcustom oer-reveal-mobile-app t
  "Value to asssign to `org-re-reveal-mobile-app' in `oer-reveal--setup-env'.
If t, add meta tags to the exported HTML file, see `org-re-reveal-mobile-app'.
Set to nil to omit such tags (and, please, open an issue to let me know why
this is a bad setting)."
  :group 'org-export-oer-reveal
  :type 'boolean
  :package-version '(org-re-reveal . "3.13.0"))

(defcustom oer-reveal-navigation-mode
  "navigationMode: window.location.search.match( /default-navigation/gi ) ? 'default' : 'linear'"
  "Navigation mode of reveal.js.
By default, use linear mode, but allow URL parameter \"default-navigation\"
to revert to default mode of reveal.js."
  :group 'org-export-oer-reveal
  :type '(choice (const nil) string)
  :package-version '(oer-reveal . "3.21.0"))

(defcustom oer-reveal-warning-delay t
  "Control whether to pause after display of warnings.
You may want to set this to nil in batch mode."
  :group 'org-export-oer-reveal
  :type 'boolean
  :package-version '(oer-reveal . "2.15.0"))

(defcustom oer-reveal-plugins
  '("reveal.js-plugins" "Reveal.js-TOC-Progress" "reveal.js-jump-plugin"
    "reveal.js-quiz" "reveal.js-coursemod" "reveal-a11y")
  "List of `plugin' components to publish.
Each element here is supposed to be the directory name of the plugin.
If you remove a plugin from this list, it will no longer be published.
If you add plugins to this list, you need to provide suitable
initialization code in `oer-reveal-plugin-config', for
plugins for reveal.js 4 and later also in `oer-reveal-plugin-4-config'.
If you remove plugins here, you also should remove corresponding
lines from `oer-reveal-plugin-4-config'."
  :group 'org-export-oer-reveal
  :type '(repeat string)
  :package-version '(oer-reveal . "3.8.0"))

(defcustom oer-reveal-audio-slideshow-dependency
  "{ src: '%splugin/audio-slideshow/audio-slideshow.js', condition: function( ) { return !!document.body.classList && !Reveal.isSpeakerNotes(); } }"
  "Dependency to initialize audio-slideshow plugin."
  :group 'org-export-oer-reveal
  :type 'string)

(defcustom oer-reveal-audio-slideshow-config
  "audioStartAtFragment: true,
  audio: {
    advance: -1, autoplay: window.location.search.match( /audio-autoplay/gi ), defaultDuration: 0, defaultAudios: false, defaultAudioRate: window.location.search.match( /audio-speed/gi )? parseFloat((new URL(window.location.href)).searchParams.get('audio-speed')) : 1.0, playerOpacity: 0.8, playerStyle: 'position: fixed; bottom: 9.5vh; left: 0%; width: 30%; height:30px; z-index: 33;' }"
  "Configuration for audio-slideshow plugin:
- Do not advance after end of audio.
- Do not start playing audio automatically, but allow URL parameter
  \"audio-autoplay\" for autoplay.
- Derive playback speed for audio from URL parameter \"audio-speed\".
- Do not display controls if no local audio file is given.
- Do not try to download audio files with default names.
- Increase opacity when unfocused (students found default too easy to miss).
- Display audio controls at bottom left (to avoid overlap)."
  :group 'org-export-oer-reveal
  :type 'string
  :package-version '(oer-reveal . "3.21.0"))

(defcustom oer-reveal-anything-dependency
  "{ src: '%splugin/anything/anything.js' }"
  "Dependency to initialize anything plugin."
  :group 'org-export-oer-reveal
  :type 'string
  :package-version '(oer-reveal . "1.3.0"))

(defcustom oer-reveal-anything-config
  (format "anything: [
        // Following initialization code for class animate from anything-demo.html.
        // Copyright (c) 2016 Asvin Goel, under The MIT License (MIT).
	{className: \"animate\",  initialize: (function(container, options){
		Reveal.addEventListener( 'fragmentshown', function( event ) {
			if (typeof event.fragment.beginElement === \"function\" ) {
				event.fragment.beginElement();
			}
		});
		Reveal.addEventListener( 'fragmenthidden', function( event ) {
			if (event.fragment.hasAttribute('data-reverse') ) {
				var reverse = event.fragment.parentElement.querySelector('[id=\\\"' + event.fragment.getAttribute('data-reverse') + '\\\"]');
				if ( reverse && typeof reverse.beginElement === \"function\" ) {
					reverse.beginElement();
				}
			}
		});
		if ( container.getAttribute(\"data-svg-src\") ) {
			var xhr = new XMLHttpRequest();
			xhr.onload = function() {
				if (xhr.readyState === 4) {
					var svg = container.querySelector('svg');
					container.removeChild( svg );
					container.innerHTML = xhr.responseText + container.innerHTML;
					if ( svg ) {
						container.querySelector('svg').innerHTML = container.querySelector('svg').innerHTML + svg.innerHTML;
					}
				}
				else {
					console.warn( \"Failed to get file. ReadyState: \" + xhr.readyState + \", Status: \" + xhr.status);
				}
			};
			xhr.open( 'GET', container.getAttribute(\"data-svg-src\"), true );
			xhr.send();
		}
	}) },
	{className: \"randomPic\",
	 defaults: {imgalt: \"Dummy alt text\",
		    imgcaption: \"Image by {name}\",
		    choices: [ {name: \"dummyname\", path: \"dummypath\"} ]},
	 initialize: (function(container, options){
	     var choice = Math.trunc( Math.random()*(options.choices.length) );
	     var img = \"<img src='\" + options.choices[choice].path + \"' alt='\" + options.choices[choice].imgalt + \"' />\";
	     var caption = options.imgcaption.replace(new RegExp('\\{name\\}', 'gm'), options.choices[choice].name);
	     container.innerHTML = img + caption;
	 }) },
	{className: \"notes\",
	 initialize: (function(container, options){
	     container.addEventListener('click', function(e) { %s });
	 }) }
]"
          (if (or (not oer-reveal-revealjs-version)
                  (version< oer-reveal-revealjs-version "4"))
              "RevealNotes.open();"
            "Reveal.getPlugins().notes.open();"))
  "Configuration for anything plugin.
Currently, this sets up animation of SVG graphics,
random selection of an image among multiple ones,
and opening of speaker notes on click."
  :group 'org-export-oer-reveal
  :type 'string
  :package-version '(oer-reveal . "1.3.0"))

(defcustom oer-reveal-anything-svg-opacity
  "<script>
 // I use opacity on g elements for step-wise appearance of SVG figures.
 // When printing, make sure that all parts are visible.
 if ( window.location.search.match( /print-pdf/gi ) ) {
     var layers = document.querySelectorAll('svg.animate g[opacity]');
     for (var i=0; i < layers.length; i++) {
         var layer = layers[i];
         layer.setAttribute(\"opacity\", 1);
     }
 }
</script>"
  "Script to set opacity in SVG images to 1 for PDF export.
Set to nil if you do not want this."
  :group 'org-export-oer-reveal
  :type '(choice (const nil) string)
  :package-version '(oer-reveal . "3.21.0"))

(defcustom oer-reveal-a11y-dependency
  "{ src: '%splugin/accessibility/helper.js', async: true, condition: function() { return !!document.body.classList; } }"
  "Dependency to initialize accessibility plugin."
  :group 'org-export-oer-reveal
  :type 'string
  :package-version '(oer-reveal . "2.3.0"))

(defcustom oer-reveal-coursemod-dependency
  "{ src: '%splugin/coursemod/coursemod.js', async: true }"
  "Dependency to initialize coursemod plugin."
  :group 'org-export-oer-reveal
  :type 'string
  :package-version '(oer-reveal . "1.3.0"))

(defcustom oer-reveal-coursemod-config
  "coursemod: { enabled: true, shown: false }"
  "Configuration for coursemod plugin: Enable, but do not show it."
  :group 'org-export-oer-reveal
  :type 'string
  :package-version '(oer-reveal . "1.2.0"))

(defcustom oer-reveal-jump-dependency
  "{ src: '%splugin/jump/jump.js', async: true }"
  "Dependency to initialize jump plugin."
  :group 'org-export-oer-reveal
  :type 'string
  :package-version '(oer-reveal . "1.3.0"))

(defcustom oer-reveal-quiz-dependency
  "{ src: '%splugin/quiz/js/quiz.js', async: true, callback: function() { prepareQuizzes({preventUnanswered: true, skipStartButton: true}); } }"
  "Dependency to initialize quiz plugin.
See URL `https://gitlab.com/schaepermeier/reveal.js-quiz/blob/master/README.md'
for available options."
  :group 'org-export-oer-reveal
  :type 'string
  :package-version '(oer-reveal . "1.2.0"))

(defcustom oer-reveal-toc-progress-dependency
  (concat "{ src: '%splugin/toc-progress/toc-progress.js', async: true, callback: function() { toc_progress.initialize('reduce', 'rgba(120,138,130,0.2)'"
          (if (or (not oer-reveal-revealjs-version)
                  (version< oer-reveal-revealjs-version "4"))
              ""
            ", 'body'")
          "); toc_progress.create(); } }")
  "Dependency to initialize TOC-Progress plugin.
If there are lots of subsections, 'scroll'ing can be enabled or the font
size can be 'reduce'd.  Go for the latter with first argument.
Second arguement sets background color.
For reveal.js 4, the third argument sets the viewport."
  :group 'org-export-oer-reveal
  :type 'string
  :package-version '(oer-reveal . "3.0.0"))

(defcustom oer-reveal-plugin-config
  '(("reveal.js-plugins"
     ()
     (:oer-reveal-audio-slideshow-config :oer-reveal-anything-config))
    ("Reveal.js-TOC-Progress" (:oer-reveal-toc-progress-dependency) ())
    ("reveal.js-jump-plugin" (:oer-reveal-jump-dependency) ())
    ("reveal.js-quiz" (:oer-reveal-quiz-dependency) ())
    ("reveal.js-coursemod"
     (:oer-reveal-coursemod-dependency) (:oer-reveal-coursemod-config))
    ("reveal-a11y" (:oer-reveal-a11y-dependency) ()))
  "Initialization for reveal.js plugins in `oer-reveal-plugins'.
This is a list of triples.  Each triple consists of
- the plugin name, which must be its directory name,
- a list of symbols or strings with JavaScript dependencies; for reveal.js 4
  plugins, configuration needs to be provided with
  `oer-reveal-plugin-4-config' and this list should be empty,
- a possibly empty list of symbols or strings with configuration settings.
The symbols should occur among the options-alist of the backend `oer-reveal'
so that its value can be obtained with `plist-get' during export."
  :group 'org-export-oer-reveal
  :type '(repeat
          (list
           (string :tag "Plugin name (directory)")
           (repeat (choice
                    (symbol :tag "JavaScript dependency among options-alist")
                    (string :tag "JavaScript dependency as string")))
           (repeat (choice
                    (symbol :tag "JavaScript config among options-alist")
                    (string :tag "JavaScript config as string")))))
  :package-version '(oer-reveal . "3.8.0"))

(defcustom oer-reveal-plugin-4-config
  "audioslideshow RevealAudioSlideshow plugin/audio-slideshow/plugin.js
anything RevealAnything plugin/anything/plugin.js"
  "Initialization for reveal.js 4 plugins.
This should be a multi-line string, each line with the format of
`REVEAL_ADD_PLUGIN'.  If you want to pass initialization options to
`Reveal.initialize()', add them in `oer-reveal-plugin-config'."
  :group 'org-export-oer-reveal
  :type 'string
  :package-version '(oer-reveal . "3.8.0"))

(defcustom oer-reveal-default-figure-title "Figure"
  "Default title for figures whose metadata lacks a title.
Also used in display of short licenses."
  :group 'org-export-oer-reveal
  :type 'string)

(defcustom oer-reveal-latex-figure-float "htp"
  "Define position for floating figures in LaTeX export.
This only applies to figures exported by macros of oer-reveal.
For other figures, see `org-latex-default-figure-position'.
You may want to use \"H\" with the float package, which is used
with oer-reveal-publish based on `oer-reveal-publish-figure-float'."
  :group 'org-export-oer-reveal
  :type 'string)

(defcustom oer-reveal-license-font-factor 0.35
  "Factor for size of font for license information.
By default, oer-reveal.css uses `.35em' as font size for license
information.  The MAXHEIGHT in `oer-reveal-export-attribution' may use
different units.  If it specifies `ex', use this number to calculate
the `max-width' of rotated license information from MAXHEIGHT.
You may want to specify MAXHEIGHT with oer-reveal's unit `rh'
instead of `ex', introduced in version 3.10.0."
  :group 'org-export-oer-reveal
  :type 'number
  :package-version '(oer-reveal . "3.9.0"))

(defcustom oer-reveal-img-src "data-src"
  "Source attribute to use in HTML img tag.
By default, use \"data-src\" for lazy loading with reveal.js.
In `oer-reveal-publish-to-html', use \"src\"."
  :group 'org-export-oer-reveal
  :type 'string
  :package-version '(oer-reveal . "3.13.0"))

(defconst oer-reveal-default-slide-height 700
  "Default height of slides with reveal.js.
See URL `https://revealjs.com/presentation-size/'.")

(defvar oer-reveal-with-alternate-types '("org")
  "List of alternate types for which to create links.
Each element of this list must occur as first entry of a triple in
`oer-reveal-alternate-type-config'.
By default, this contains \"org\" to create an alternate type link to the
Org source file.  Function `oer-reveal-publish-to-reveal-and-pdf' changes
this to create links for Org source and PDF variant.")

(defconst oer-reveal-alternate-type-config
  '(("org" "text/org" sourceversion)
    ("pdf" "application/pdf" pdfversion))
  "List of triples for alternate type links in HTML presentations.
The first entry is a file extension, the second its MIME type; the third
specifies whether and how to construct a title for the link.
The title can be a string, to be used literally.  If the title's length is
zero, no title  attribute is generated in `oer-reveal-add-alternate-types'.
By default, the third entry indicates a symbol in `oer-reveal-dictionaries',
which in turn translates to a title string including a placeholder \"%s\";
depending on the export backend in use, the placeholder is translated with
`htmldoc' or `revealjsdoc'.")

(defconst oer-reveal-plugin-config-fmt "%s,\n"
  "Format string to embed a line with plugin configuration.")

;; Variables about installation location and reveal.js plugins follow.
(defconst oer-reveal-dir
  (file-name-directory (or load-file-name (buffer-file-name)))
  "Directory of oer-reveal containing code and resources.
Useful for `org-publish-all' to publish resources that are also
contained in this directory.")
(defconst oer-reveal-submodules-url
  "https://gitlab.com/oer/emacs-reveal-submodules.git"
  "Git URL for submodules of reveal.js and plugins.")
(defcustom oer-reveal-submodules-version "2.4.0"
  "Version of submodules to check out.
This can be a string, indicating a git version tag, or nil.
If nil, `oer-reveal-submodules-ok-p' always returns t, and oer-reveal does
not attempt to update the submodules, e.g., in Docker containers."
  :group 'org-export-oer-reveal
  :type '(choice (const nil) string)
  :package-version '(oer-reveal . "2.5.0"))

(defconst oer-reveal-buffer "*oer-reveal git output*"
  "Name of buffer holding Git output.")
(defcustom oer-reveal-submodules-dir
  (file-name-as-directory
   (concat (file-name-as-directory user-emacs-directory)
	  (file-name-sans-extension
	   (file-name-nondirectory oer-reveal-submodules-url))))
  "Directory with submodules of oer-reveal.
Submodules include reveal.js and its plugins.
If this directory does not exist, installation is offered.
If this directory exists, it must have been cloned via git from
`oer-reveal-submodules-url'.  If that condition is violated, strange
things may happen.
This directory must not be a relative path (but can start with \"~\")."
  :group 'org-export-oer-reveal
  :type 'directory)

;; Variables to control generation of files to include Org files.
(defcustom oer-reveal-generate-org-includes-p nil
  "Set to t for question whether to generate include files upon loading.
Used in `oer-reveal-generate-include-files'."
  :group 'org-export-oer-reveal
  :type 'boolean)

(defcustom oer-reveal-org-includes-dir
  (file-name-as-directory
   (concat (file-name-as-directory user-emacs-directory) "oer-reveal-org"))
  "Target directory for `oer-reveal-generate-include-files'."
  :group 'org-export-oer-reveal
  :type 'directory)

;; Variables to control RDFa meta-data.
(defcustom oer-reveal-rdf-prefixes
  "prefix=\"dc: http://purl.org/dc/elements/1.1/ dcterms: http://purl.org/dc/terms/ dcmitype: http://purl.org/dc/dcmitype/ cc: http://creativecommons.org/ns# schema: http://schema.org/\""
  "String with RDFa prefixes."
  :group 'org-export-oer-reveal
  :type 'string
  :package-version '(oer-reveal . "3.14.0"))

(defcustom oer-reveal-rdf-typeof
  '("dcmitype:InteractiveResource"
    "schema:PresentationDigitalDocument" "schema:LearningResource")
  "Specify RDFa types of document as list of strings.
Supercedes `oer-reveal-dcmitype' to also include LRMI vocabulary.
See URL `http://lrmi.net/about/lrmi/'
and URL `https://schema.org/LearningResource'."
  :group 'org-export-oer-reveal
  :type '(repeat string)
  :package-version '(oer-reveal . "3.14.0"))

(defcustom oer-reveal-rdf-figure-typeof "schema:ImageObject"
  "Specify one string value for RDFa type of figure.
In addition, that attribute contains
- `oer-reveal--default-figure-dcmitype', which can be overwritten per figure
  in its meta file, and
- \"schema:LearningResource\" if that is contained in `oer-reveal-rdf-typeof'
  for the embedding document."
  :group 'org-export-oer-reveal
  :type 'string
  :package-version '(oer-reveal . "3.15.0"))

(defcustom oer-reveal-rdf-caption-property "schema:caption"
  "Specify RDFa property as string for caption of figure in HTML format.
Only used if caption is non-empty."
  :group 'org-export-oer-reveal
  :type 'string
  :package-version '(oer-reveal . "3.15.0"))

(defcustom oer-reveal-dcmitype "typeof=\"dcmitype:InteractiveResource\""
  "Specify DCMI type.
See URL `https://www.dublincore.org/specifications/dublin-core/dcmi-terms/'."
  :group 'org-export-oer-reveal
  :type 'string
  :package-version '(oer-reveal . "2.0.0"))
(make-obsolete-variable 'oer-reveal-dcmitype 'oer-reveal-rdf-typeof "3.14.0")

(defcustom oer-reveal-created-template
  "<p class=\"date\">%s: <span property=\"dcterms:created\">%s</span></p>"
  "Template string for HTML \"p\" element with creation date.
Template for `oer-reveal-license-to-fmt'; that funtion replaces first \"%s\"
with language-specific word for `created' in `oer-reveal-dictionaries',
second one with creation date."
  :group 'org-export-oer-reveal
  :type 'string
  :package-version '(oer-reveal . "2.0.0"))

;; Variables to control export of figures.
(defcustom oer-reveal-figures-dir "figures/"
  "Name of directory of submodule URL `https://gitlab.com/oer/figures/'.
This variable influences the treatment of filenames in image metadata files.
If a filename starts with this directory, it is not changed.  (Thus, set
this to the empty string to leave all filenames unchanged.)
Otherwise, the filename is treated as being relative to the metadata file."
  :group 'org-export-oer-reveal
  :type 'directory
  :package-version '(oer-reveal . "3.13.0"))

(defcustom oer-reveal-copy-dir-suffix ".for-export"
  "If non-empty string, copy embedded figures into separate directory.
An oer-reveal project might embed a subset of the OER figure repository
at URL `https://gitlab.com/oer/figures/'.  To publish only those figures
that are actually used, they can be copied to a separate directory for
export.  If you embed figures, say from \"./figures\", then oer-reveal
with the default setting \".for-export\" copies each embedded figure to
the directory \"./figures.for-export\" and publishes only those copied
figures.
More precisely, `oer-reveal-copy-dir-suffix' is inserted as suffix of
the first ordinary directory component of FILENAME.  (Dots and (back-)
slashes at the beginning of the name are left unchanged.)
Set to empty string to disable this functionality."
  :group 'org-export-oer-reveal
  :type 'string
  :package-version '(oer-reveal . "2.1.0"))

;; The following options are only relevant if you use
;; oer-reveal-export-image-grid to generate image grids.
;; Then, the options control in what directory generated CSS is saved.
(defcustom oer-reveal-export-dir "public/"
  "Directory into which HTML, CSS, and Javascript is published.
The default supposes that `org-publish-all' publishes into a
subdirectory of `public/'.
This is only used to publish CSS of image grids with
`oer-reveal-export-image-grid'."
  :group 'org-export-oer-reveal
  :type 'directory)

(defcustom oer-reveal-css-filename-template
  "figures/internal_grid_css/grid%s.css"
  "Template for filename of CSS generated for image grid.
This must contain `%s' as placeholder for the grid's identifier.
Note that this filename is exported into a subdirectory of
`oer-reveal-export-dir' under the current directory."
  :group 'org-export-oer-reveal
  :type 'string)

;; Variables to control treatment of license information.
(defcustom oer-reveal-dictionaries
  '(("en" . (("CC0-1.0" . "Creative Commons license CC0 1.0")
             ("CC-BY-SA-4.0" . "Creative Commons license CC BY-SA 4.0")
             (text . "Except where otherwise noted, the work “%t”, %c, is published under the %l.")
             (license . " and the ")
             (copyright . " and ")
             (by . "by")
             (created . "Created")
             (legalese . "<div class=\"legalese\"><p><a href=\"/imprint.html\">Imprint</a> | <a href=\"/privacy.html\">Privacy Policy</a></p></div>")
             (htmldoc . "OER HTML page")
             (revealjsdoc . "OER HTML presentation with reveal.js")
             (sourceversion . "Org mode source code of this %s")
             (pdfversion . "PDF version of this %s")
             (pdffootnote . "This PDF document is an inferior version of an \\href{%s}{%s}; \\href{%s}{free/libre Org mode source repository}.")))
    ("de" . (("CC0-1.0" . "Creative-Commons-Lizenz CC0 1.0")
             ("CC-BY-SA-4.0" . "Creative-Commons-Lizenz CC BY-SA 4.0")
             (text . "Soweit nicht anders angegeben unterliegt das Werk „%t“, %c, der %l.")
             (copyright . " und ")
             (license . " und der ")
             (by . "von")
             (created . "Erzeugt")
             (legalese . "<div class=\"legalese\"><p><a href=\"/imprint.html\">Impressum</a> | <a href=\"/privacy-de.html\">Datenschutz</a></p></div>")
             (htmldoc . "OER-HTML-Seite")
             (revealjsdoc . "OER-HTML-Präsentation mit Reveal.js")
             (sourceversion . "Org-Mode-Quelltext dieser %s")
             (pdfversion . "PDF-Version dieser %s")
             (pdffootnote . "Dieses PDF-Dokument ist eine minderwertige Version einer \\href{%s}{%s}; \\href{%s}{freies Repository mit Org-Mode-Quelltexten}."))))
  "List of pairs specifying dictionaries for licensing related words.
The first component of each pair is a two-letter language identifier (as
defined with \"#+LANGUAGE\"), while the second one is a list of pairs from
identifiers to language-specific words/strings/pieces of code.
Currently, the following identifiers are used:
- \"CC0-1.0\" and \"CC-BY-SA-4.0\": Texts to display licenses
- `text': The license text with %-sequences indicating title (%t),
  copyright and author information (%c), and license information (%l)
- `copyright' and `license': Connectors to use when multiple lines
  with copyright or license information need to be combined;
  note the whitespace
- `by': Word to indicate what author created the work
- `created': Word to indicate when the work was created
- `legalese': HTML string pointing to legalese (imprint and privacy
  policy); set to empty string to avoid altogether
- `htmldoc' and `revealjsdoc': Texts to indicate type of primary
  target format, either HTML document or reveal.js presentation
- `pdffootnote': Text for a footnote in LaTeX to point to source and
  primary target files; first \"%s\" is replaced with URL of primary
  target variant, second one based on the target format with either
  `revealjsdoc' or `htmldoc', third one with URL to source code repository;
  resulting text is used in `oer-reveal-alternate-type-latex';
  set to empty string to avoid footnote
- `sourceversion' and `pdfversion': Text for source version and PDF version
  of target document; text contains \"%s\" to be replaced with
  `revealjsdoc' or `htmldoc'
If you add another language, you need to provide translations for all
identifiers.  Please create an issue (or merge request) to share your
language at URL `https://gitlab.com/oer/org-re-reveal/issues/'."
  :group 'org-export-oer-reveal
  :type '(repeat (cons
                  (string :tag "Language")
                  (repeat (cons
                           (choice symbol string)
                           (string :tag "Translation")))))
  :package-version '(oer-reveal . "3.3.0"))

(defcustom oer-reveal-licenses
  '(("CC-BY-SA-4.0" . "https://creativecommons.org/licenses/by-sa/4.0/")
    ("CC0-1.0" . "https://creativecommons.org/publicdomain/zero/1.0/"))
  "License information as list of pairs:
First, the SPDX identifier for the license; second, a URI for the license.
If you add a license here, you also need to add its identifier to
`oer-reveal-dictionaries'."
  :group 'org-export-oer-reveal
  :type '(repeat (cons
                  (string :tag "SPDX identifier")
                  (string :tag "License URI")))
  :package-version '(oer-reveal . "2.0.0"))

(defconst oer-reveal--copyright-regexp
  "^\\([-0-9, ]+\\)\\([^<]+\\)\\([<]\\([^>]+\\)[>]\\)?$"
  "Regular expression to match SPDX copyright information.
See URL `https://reuse.software/faq/#licensing'.")

(defconst oer-reveal--license-regexp "^\\(.*\\)$"
  "Regular expression to match SPDX license identifier.
See URL `https://reuse.software/faq/'.")

(defcustom oer-reveal-use-year-ranges-p t
  "If t, use ranges for copyright years.
E.g., use \"2018-2020\" instead of \"2018, 2019, 2020\".
Set to nil to use lists of years."
  :group 'org-export-oer-reveal
  :type 'boolean
  :package-version '(oer-reveal . "2.3.0"))

(defcustom oer-reveal-sort-creators-pred #'oer-reveal-sort-creators-p
  "Predicate to sort structures that represent creators.
Sorts with `oer-reveal-sort-creators-p' by default."
  :group 'org-export-oer-reveal
  :type 'function
  :package-version '(oer-reveal . "2.3.0"))

(defcustom oer-reveal-spdx-author nil
  "Author to restrict search in `oer-reveal-copyright-check'."
  :group 'org-export-oer-reveal
  :type '(choice (const nil) string)
  :package-version '(oer-reveal . "2.9.0"))

(defcustom oer-reveal-spdx-copyright-regexp "SPDX-FileCopyrightText:.*"
  "Regular expression to match copyright information."
  :group 'org-export-oer-reveal
  :type 'regexp
  :package-version '(oer-reveal . "2.9.0"))


;;; Functions to install and update submodules.
(defun oer-reveal-clone-submodules ()
  "Clone submodules from `oer-reveal-submodules-url'.
Target directory is `oer-reveal-submodules-dir'.
Output of Git goes to buffer `oer-reveal-buffer'."
  (let ((parent (file-name-directory
		 (directory-file-name oer-reveal-submodules-dir))))
    (unless (file-writable-p parent)
      (error "Directory to install submodules not writable: %s" parent))
    (save-excursion
      (pop-to-buffer (get-buffer-create oer-reveal-buffer) nil t)
      (let ((default-directory parent)
	    ;; In newer Emacsen, call-process starts in default-directory,
	    ;; which is what we want.  In Emacs 24.5.1, this does not happen.
	    ;; Instead, assign filename to buffer, from which call-process
	    ;; obtains its directory.
	    (buffer-file-name (concat parent oer-reveal-buffer)))
	(insert "Performing git clone in: ")
	(call-process "pwd" nil t t)
	(call-process "git" nil t t "clone" oer-reveal-submodules-url)
	(insert "...done\n\n")))
    (unless (file-readable-p oer-reveal-submodules-dir)
      (error "Cloning of submodules failed.  Directory not readable: %s"
	     oer-reveal-submodules-dir))))

(defun oer-reveal-git-version-string (&optional dir)
  "Get git tag with \"git describe --tags\".
If optional DIR is non-nil, determine tag in that directory;
otherwise in `oer-reveal-submodules-dir'."
  (let ((dir (or dir oer-reveal-submodules-dir)))
    (string-trim (shell-command-to-string
		  (format "cd %s; git describe --tags"
			  (shell-quote-argument (expand-file-name dir)))))))

(defun oer-reveal-submodules-ok-p ()
  "Return t if submodules have correct version.
If `oer-reveal-submodules-version' is nil, disable checks and return t.
Otherwise, check that \"git describe --tags\" in `oer-reveal-submodules-dir'
returns the version `oer-reveal-submodules-version'
and make sure that submodules have been initialized by checking the
existence of file \"reveal.js\"."
  (if oer-reveal-submodules-version
      (and
       (string= oer-reveal-submodules-version
                (oer-reveal-git-version-string))
       (let* ((subdirs `(,oer-reveal-submodules-dir "reveal.js" "js"))
              (dir (mapconcat #'file-name-as-directory subdirs "")))
         (file-readable-p (concat dir "reveal.js"))))
    t))

(defun oer-reveal-update-submodules ()
  "Update submodules for this version of oer-reveal.
Do not update if `oer-reveal-submodules-ok-p' returns t.
Output of Git goes to buffer `oer-reveal-buffer'."
  (unless (file-writable-p oer-reveal-submodules-dir)
    (error "Directory of submodules not writable: %s"
	   oer-reveal-submodules-dir))
  (when (not (oer-reveal-submodules-ok-p))
    (save-excursion
      (pop-to-buffer (get-buffer-create oer-reveal-buffer) nil t)
      (let ((default-directory
	      (file-name-as-directory oer-reveal-submodules-dir))
	    ;; As explained above, also assign value to buffer-file-name.
	    (buffer-file-name
	     (concat (file-name-as-directory oer-reveal-submodules-dir)
		     oer-reveal-buffer)))
	(insert "Performing git pull and checkout in: ")
	(call-process "pwd" nil t t)
	(call-process "git" nil t t "checkout" "master")
	(call-process "git" nil t t "pull")
	(call-process "git" nil t t "checkout" oer-reveal-submodules-version)
	(insert "...done\n\nPerforming submodule install...\n")
	(call-process "git" nil t t "submodule" "sync" "--recursive")
	(call-process "git" nil t t "submodule" "update" "--init" "--recursive")
	(insert "...done\n\n"))))
  (unless (oer-reveal-submodules-ok-p)
    (error "Submodule update failed")))

(defun oer-reveal-install-submodules ()
  "Install reveal.js and plugins as submodules.
Software is cloned from `oer-reveal-submodules-url' into
`oer-reveal-submodules-dir'."
  (oer-reveal-clone-submodules)
  (oer-reveal-update-submodules))

(defun oer-reveal-setup-submodules (&optional force)
  "Install or update submodules of oer-reveal.
If optional FORCE is t, do not ask when `oer-reveal-submodules-dir' is
missing but install submodules silently."
  (interactive "P")
  (if (file-exists-p oer-reveal-submodules-dir)
      (oer-reveal-update-submodules)
    (when (or force
	      (y-or-n-p (format "Directory \"%s\" for reveal.js and plugins does not exist.  Type \"y\" to have it set up for you (needs to download about 26 MB).  Type \"n\" to install necessary submodules yourself or customize `oer-reveal-submodules-dir'.  Your choice? "
				oer-reveal-submodules-dir)))
      (oer-reveal-install-submodules))))

(defun oer-reveal--generate-include-file (source-file type)
  "Generate include file for SOURCE-FILE.
The TYPE can be \"org\", to generate a file that includes the source file,
or \"title-slide\", to generate a files that defines \"REVEAL_TITLE_SLIDE\".
The resulting file is stored under `oer-reveal-org-includes-dir'."
  (let* ((source-base (file-name-nondirectory source-file))
         (target-org (concat (file-name-sans-extension source-base) ".org"))
	 (target-file (concat
		       (file-name-as-directory oer-reveal-org-includes-dir)
		       target-org)))
    (with-temp-file target-file
      (insert
       (format "# Generated file.  Will be overwritten without warning.\n"))
      (cond ((string= type "org")
             (insert (format "#+INCLUDE: \"%s\"\n" source-file)))
            ((string= type "title-slide")
             (insert (format "#+REVEAL_TITLE_SLIDE: %s\n" source-file)))
            (t (user-error "Unexpected type `%s' for file `%s'"
                           type source-file))))))

(defun oer-reveal-generate-include-files (&optional force)
  "Generate files that include Org configuration files of oer-reveal.
If `oer-reveal-org-includes-dir' does not exist and
`oer-reveal-generate-org-includes-p' is t, ask user whether that directory
should be created to store generated files.
If optional FORCE is t, create directory without questions.
This provides a stable location for \"#+INCLUDE\" statements in your
Org files."
  (catch 'aborted
    (if (not (file-exists-p oer-reveal-org-includes-dir))
	(if (or force
		(and oer-reveal-generate-org-includes-p
		     (y-or-n-p
		      (format "Directory \"%s\" does not exist.  Create and populate for you (if not, maybe customize `oer-reveal-generate-org-includes-p')? "
			      oer-reveal-org-includes-dir))))
	    (make-directory oer-reveal-org-includes-dir t)
	  (throw 'aborted nil)))
    (dolist (spec
             (list
              (cons "org" "\\.org$")
              (cons "title-slide" "\\.html$"))
             nil)
      (let* ((source-dir (file-name-as-directory
			  (concat (file-name-as-directory
				   (expand-file-name oer-reveal-dir))
                                  (car spec))))
	     (source-files (directory-files source-dir t (cdr spec))))
        (mapc (lambda (source-file)
                (funcall #'oer-reveal--generate-include-file
                         source-file (car spec)))
              source-files)))))

;;; Links in new browser tabs.
(defcustom oer-reveal-new-tab-url-regexp "."
  "Regular expression or nil.
If non-nil, URLs matching this pattern open in new browser tabs,
unless they also match `oer-reveal-no-new-tab-url-regexp'.
By default, all HTTP(S) links open in new tabs.
Set to nil if you do not want URLs to open in new tabs.
Changes of URLs happen via `oer-reveal-filter-parse-tree',
which is added to `org-export-filter-parse-tree-functions'
in oer-reveal-publish.  Note that this applies to links in all
backends derived from `html'."
  :group 'org-export-oer-reveal
  :type '(choice (const nil) regexp)
  :package-version '(oer-reveal . "3.22.0"))

(defcustom oer-reveal-no-new-tab-url-regexp nil
  "Regular expression or nil.
If a URL matches this pattern, it does not open in a new tab, regardless of
`oer-reveal-new-tab-url-regexp'."
  :group 'org-export-oer-reveal
  :type '(choice (const nil) regexp)
  :package-version '(oer-reveal . "3.22.0"))

(defun oer-reveal--link-in-tab (link)
  "Add attributes to LINK such that it opens in a new browser tab.
Only applies to HTTP(S) links.
See also `oer-reveal-new-tab-url-regexp'."
  (let ((type (org-element-property :type link))
        (path (org-element-property :path link)))
    (when
        (and (member type '("http" "https"))
             (or (not oer-reveal-new-tab-url-regexp)
                 (string-match oer-reveal-new-tab-url-regexp path))
             (or (not oer-reveal-no-new-tab-url-regexp)
                 (not (string-match oer-reveal-no-new-tab-url-regexp path))))
      (let ((attrs (org-export-read-attribute :attr_html link)))
        (push ":target _blank" attrs)
        (push ":rel noopenener noreferrer" attrs)
        (org-element-put-property link :attr_html attrs)))
    link))

(defun oer-reveal-filter-parse-tree (tree backend _)
  "Filter parse TREE for BACKEND.
In backends derived from `html', apply `oer-reveal--link-in-tab' to all
Org links."
  (when (org-export-derived-backend-p backend 'html)
    (org-element-map tree 'link
      (lambda (elem)
        (oer-reveal--link-in-tab elem))))
  tree)

;;; Allow colored text.
;; The FAQ at http://orgmode.org/worg/org-faq.html contains a recipe
;; based on the obsolete function (since Org 9.0) org-add-link-type.
;; Use org-link-set-parameters if it is available.
;; With Org 9.4, the :follow function needs two arguments, the
;; :export function four, see org-link-parameters.
(defun oer-reveal--color-link-follow (path &optional _)
  "Color link PATH."
  (message (concat "color "
		   (progn (add-text-properties
			   0 (length path)
			   (list 'face `((t (:foreground ,path))))
			   path)
                          path))))
(defun oer-reveal--color-link-export (path desc backend &optional _)
  "Export color link at PATH with DESC to BACKEND."
  (cond
   ((eq backend 'html)
    (format "<span style=\"color:%s;\">%s</span>" path desc))
   ((eq backend 'latex)
    (format "{\\color{%s}%s}" path desc))))

(defun oer-reveal--path-export (path desc backend hyper)
  "Export hyperlink for PATH with DESC to BACKEND.
For HTML export: If PATH ends with \".org\", replace that extension with
\".html\"; otherwise, use PATH unchanged.
For LaTeX export, replace extension with \".pdf\".
For HTML backends, create hyperlink with format string HYPER."
  (let ((extension (file-name-extension path))
        (sans-extension (file-name-sans-extension path)))
    (cond
     ((eq backend 'html)
      (let ((path (if (string= extension "org")
                      (concat sans-extension ".html")
                    path)))
        (format hyper path (or desc path))))
     ((eq backend 'latex)
      (let ((path (if (or (string= extension "html")
                          (string= extension "org"))
                      (concat sans-extension ".pdf")
                    path)))
        (format "\\href{%s}{%s}" path (or desc path)))))))

;;; Allow local links, to be exported without usual translation by Org.
;; Useful to preserve relative paths if files are included.
(defun oer-reveal--local-path-export (path desc backend &optional _)
  "Export local PATH with DESC to BACKEND, without Org interference.
This is meant for links in combination with INCLUDE statements where
Org by default may insert unwanted path components.  Here, just
the file extension in PATH may be changed.  See URL
`https://gitlab.com/oer/cs/programming/-/blob/master/texts/Git-Workflow-Instructions.org'
for examples."
  (oer-reveal--path-export path desc backend "<a href=\"%s\">%s</a>"))

;;; Create hyperlink with hasPart RDFa information.
(defun oer-reveal--haspart-export (path desc backend &optional _)
  "Export PATH with DESC to BACKEND.
For HTML export, create hyperlink for a learning resource that is
a part of the current document.  Use RDFa markup."
  (oer-reveal--path-export
   path desc backend
   "<a typeof=\"schema:LearningResource\" rel=\"schema:hasPart\" href=\"%s\">%s</a>"))

;;; Create hyperlinks with target and class attributes.
(defcustom oer-reveal-external-url-template
  "<a href=\"%s\" target=\"_blank\" rel=\"noopener noreferrer\" class=\"%s\">%s</a>"
  "Format string for external URLs with three placeholders.
The first one is the URL, the second one a class attribute, the third one
the link's text."
  :group 'org-export-oer-reveal
  :type 'string
  :package-version '(oer-reveal . "3.21.0"))

(defun oer-reveal--url-export (path desc backend type)
  "Export PATH with DESC to BACKEND for TYPE."
  (cond
   ((eq backend 'html)
    (let ((htmlclass (format "%slink" type)))
      (format oer-reveal-external-url-template
              path htmlclass (or desc path))))
   ((eq backend 'latex)
    (format "\\href{%s}{%s}" path (or desc path)))))
(defun oer-reveal--basic-url-export (path desc backend &optional _)
  "Export PATH with DESC to BACKEND.
This is meant for external hyperlinks to open in new tabs with
a class to indicate that basic topics are covered."
  (oer-reveal--url-export path desc backend "basic"))
(defun oer-reveal--beyond-url-export (path desc backend &optional _)
  "Export PATH with DESC to BACKEND.
This is meant for external hyperlinks to open in new tabs with
a class to indicate that additional topics are covered."
  (oer-reveal--url-export path desc backend "beyond"))
(defun oer-reveal--revisit-url-export (path desc backend &optional _)
  "Export PATH with DESC to BACKEND.
This is meant for external hyperlinks to open in new tabs with
a class to indicate that topics are revisited later."
  (oer-reveal--url-export path desc backend "revisit"))

(defun oer-reveal-register-link (type follow-func export-func)
  "Register Org link TYPE with FOLLOW-FUNC and EXPORT-FUNC."
  (if (fboundp #'org-link-set-parameters)
      (org-link-set-parameters type :follow follow-func :export export-func)
    (org-add-link-type type follow-func export-func)))

(oer-reveal-register-link "color"
                          #'oer-reveal--color-link-follow
                          #'oer-reveal--color-link-export)

(oer-reveal-register-link "local"
                          #'org-link-open-as-file
                          #'oer-reveal--local-path-export)

(oer-reveal-register-link "hasPart"
                          #'org-link-open-as-file
                          #'oer-reveal--haspart-export)

(oer-reveal-register-link "basic"
                          #'browse-url
                          #'oer-reveal--basic-url-export)

(oer-reveal-register-link "beyond"
                          #'browse-url
                          #'oer-reveal--beyond-url-export)

(oer-reveal-register-link "revisit"
                          #'browse-url
                          #'oer-reveal--revisit-url-export)

;;; Add alternate type links to HTML presentations and pointers to PDF.
(defconst oer-reveal-alternate-type-html
  "#+HTML_HEAD: <link rel=\"alternate\" type=\"%s\" href=\"%s\"%s/>\n"
  "Org code for HTML link element for alternate type.")
(defconst oer-reveal-alternate-type-latex
  "#+TITLE: @@latex:\\footnote{%s}@@\n"
  "Org code for LaTeX footnote on title pointing to HTML and Org variants.")
(defconst oer-reveal-gitlab-regexp
  "^\\(git@gitlab.com:\\|https://\\([^@]+@\\)?gitlab.com/\\)\\(.*?\\)\\([.]git\\)?$"
  "Regular expression matching GitLab URLs.
Group 3 matches the path.")

(defun oer-reveal--parse-git-url (&optional url)
  "Return nil or a pair of URLs for HTTPS repo and GitLab Pages.
Either create pair from optional URL or from output of \"git remote\".
Return nil if URL does not look like the URL of a GitLab repository."
  (let ((url (or url
                 (string-trim
                  (shell-command-to-string "git remote get-url origin")))))
    (when (string-match oer-reveal-gitlab-regexp url)
      (let* ((path (match-string 3 url))
             (components (split-string path "/"))
             (project-or-group (car components))
             (path-in-project (mapconcat #'identity (cdr components) "/"))
             (source-repo (concat "https://gitlab.com/" path))
             (pages-domain (format "%s.gitlab.io" project-or-group))
             (pages-url (if (string= pages-domain path-in-project)
                            (format "https://%s/" pages-domain)
                          (format "https://%s/%s"
                                  pages-domain path-in-project))))
        (cons source-repo pages-url)))))

(defun oer-reveal--relative-git-basename (filename)
  "Return relative basename of FILENAME in Git repository."
  (let ((root (string-trim
               (shell-command-to-string "git rev-parse --show-toplevel"))))
    (if (string-prefix-p "fatal" root)
        (file-name-base filename)
      (file-name-sans-extension
       (file-relative-name filename root)))))

(defun oer-reveal--alternate-link-title
    (base-title language doctype backend)
  "Create title attribute for alternate link.
Generate title from BASE-TITLE for LANGUAGE, and DOCTYPE.
Ignore BACKEND."
  (ignore backend)
  (if (stringp base-title)
      base-title
    (let ((title-spec (oer-reveal--translate language base-title)))
      (format title-spec doctype))))

(defun oer-reveal-add-alternate-types
    (types source-repo html-url basename &optional backend)
  "Construct Org code to add links for types in list TYPES.
Supported string values for TYPES are defined in
`oer-reveal-alternate-type-config', currently \"org\" and \"pdf\".
First, create HTML link elements in \"HTML_HEAD\" lines for each type
in TYPES.  For \"org\", create a link to the Org file under SOURCE-REPO.
For other types, including \"pdf\", create a link with relative path.
Second, add a LaTeX footnote to the title with href links to the source
file in SOURCE-REPO and to the HTML file under HTML-URL.
BASENAME is the relative source filename without file extension in
SOURCE-REPO.
Optional BACKEND is the export backend, `re-reveal' by default."
  (let* ((language (oer-reveal--language))
         (backend (or backend 're-reveal))
         (doctype (if (org-export-derived-backend-p backend 're-reveal)
                      (oer-reveal--translate language 'revealjsdoc)
                    (oer-reveal--translate language 'htmldoc))))
    (concat
     (mapconcat
      (lambda (type)
        (let* ((triple (assoc type oer-reveal-alternate-type-config))
               (mime-type (nth 1 triple))
               (title (oer-reveal--alternate-link-title
                       (nth 2 triple) language doctype backend))
               (title-attr (if (< 0 (length title))
                               (format " title=\"%s\"" title)
                             ""))
               (filename (concat basename "." type))
               (url (cond
                     ((equal type "org")
                      ;; Absolute link to source repository.
                      (concat source-repo "/blob/master/" filename))
                     ((equal type "pdf")
                      ;; Relative link to PDF in same directory.
                      (file-name-nondirectory filename))
                     (t (error "Unknown alternate type: `%s'" type)))))
          (format oer-reveal-alternate-type-html
                  mime-type url title-attr)))
      types "")
     (if (member "pdf" types)
         (let* ((targeturl
                 (concat (if (string-suffix-p "/" html-url)
                             html-url
                           (concat html-url "/"))
                         basename ".html"))
                (footnote (oer-reveal--translate language 'pdffootnote)))
           (if (< 0 (length footnote))
               (format oer-reveal-alternate-type-latex
                       (format footnote targeturl doctype source-repo))
             ""))
       ""))))

(defun oer-reveal-insert-alternate-types (backend)
  "Insert Org code to add links for alternate MIME types.
Call `oer-reveal-add-alternate-types' with `oer-reveal-with-alternate-types',
SOURCE-REPO and HTML-URL derived from the URL of the GitLab repository,
BASENAME derived from the name of the buffer's file.
Insert resulting Org code at end of current buffer.
BACKEND defaults to `re-reveal' but can indicate `html' as well."
  (let* ((pair (oer-reveal--parse-git-url))
         (source-repo (car pair))
         (html-url (cdr pair))
         (filename (buffer-file-name))
         (basename (and filename
                        (oer-reveal--relative-git-basename filename))))
    (when (and pair basename)
      (save-excursion
        (goto-char (point-max))
        (insert
         "\n\n"
         (oer-reveal-add-alternate-types
          oer-reveal-with-alternate-types source-repo html-url basename
          (or backend 're-reveal)))))))

;;; Function to generate proper CC attribution for images.
;; Function oer-reveal-export-attribution is used in macros in org/config.org.
;; See emacs-reveal-howto for sample use:
;; https://gitlab.com/oer/emacs-reveal-howto
;;;###autoload
(defun oer-reveal-export-attribution (&rest args)
  "Generate HTML and LaTeX code for image with license attribution.
Essentially, this function calls `oer-reveal--export-attribution-helper'
\(where arguments ARGS are documented), but makes sure that macro
arguments are properly expanded to work with all Org versions,
also after an incompatible change with Org 9.2."
  ;; The first argument is the file name for metadata.  If that
  ;; starts with a quotation mark, arguments have been quoted.
  ;; (You don't start file names with quotation marks, do you?)
  (let ((metadata (car args)))
    (if (string-prefix-p "\"" metadata)
	(apply #'oer-reveal--export-attribution-helper
	       (mapcar #'oer-reveal--read-from-string args))
      (apply #'oer-reveal--export-attribution-helper args))))

(defun oer-reveal--check-symbol (object)
  "Helper function for `oer-reveal--read-from-string'.
Different Org versions treat macro arguments differently.  Check whether
OBJECT is a quoted symbol, where no quoting is necessary.  Notify user
if applicable.  Raise `user-error' in case of unknown type."
  (if (and (consp object) (eq 'quote (car object)))
      (progn
	(message
	 "Explicit quoting of symbol in `%s' not necessary (with your Org version)"
	 object)
	(when oer-reveal-warning-delay (sit-for 2))
	(cadr object))
    (user-error "Unexpected type `%s' in `%s'" (type-of object) object)))

(defun oer-reveal--read-from-string (object)
  "Undo potential quoting in OBJECT for strings with Org 9.2.
If OBJECT is a string, then use `read-from-string' to return
a boolean, integer, string, or symbol.
If OBJECT is not a string, return it unchanged."
  (if (stringp object)
      (if (= 0 (length object))
	  nil
	(let ((first (car (read-from-string object))))
	  (if (or (booleanp first) (integerp first) (stringp first)
		  (symbolp first))
	      first
	    (oer-reveal--check-symbol first))))
    object))

(defun oer-reveal--export-attribution-helper
    (metadata
     &optional caption maxheight divclasses shortlicense embed-svg extra-attrs)
  "Display image from METADATA.
Produce string for HTML and LaTeX exports to be embedded in Org files.
METADATA is a text file including licensing information.
If optional CAPTION is not nil, it can either be a string or t.  In that
case, display text underneath the image: If CAPTION is t, display whatever
the meta-data knows as title, otherwise display the string CAPTION, but
replace cite-links if present.  If CAPTION is t, the title is not repeated
as part of the license information.
If CAPTION is nil, a LaTeX caption is generated anyways to have a numbered
figure (and frequently to also display license information).
Optional MAXHEIGHT restricts the height of the image and of the license
information in HTML.  MAXHEIGHT needs be a full specification including
the unit, e.g. `50vh'.  Actually, I stopped using viewport heights:
With scaling of reveal.js, the viewport size is not calculated properly
\(this is visible for screens with much larger resolutions than the
presentation's resolution), and images may overlap the footer.
The recommended unit for use with oer-reveal as of version 3.10.0 is
`rh' (reveal.js height), which is meant as replacement of `vh' to work
correctly with reveal.js, taking scaling into account.  Thus, `50rh'
should cover 50% of a slide's height.
If you use `ex' as unit, note that licence information is displayed with
a smaller font.  To account for this difference,
`oer-reveal-license-font-factor' determines the maximum width of license
information.
If present, optional DIVCLASSES must be a string with space separated
classes for the div element, including `figure'.
If optional SHORTLICENSE is the symbol `none', do not display license
text (useful if image license agrees with document license);
if it is t, display license based on `oer-reveal--short-license-template'
\(instead of default (long) license text).  As this does not include
the author, it must not be used with attribution licenses.  (Instead,
this is meant to indicate public domain licensing.)
If optional EMBED-SVG is non-nil, embed XML code of SVG image directly.  In
this case, the maximum height on the image does not have any effect.
For LaTeX, the METADATA file may specify a texwidth, which is embedded in
the width specification as fraction of `linewidth'; 0.9 by default.
Optional EXTRA-ATTRS are assigned to the div element."
  (let ((org (oer-reveal--attribution-strings
	      metadata caption maxheight divclasses shortlicense
              embed-svg extra-attrs)))
    (concat (if caption
		(concat "@@html: </p><div class=\"imgcontainer\">"
			(car org)
			"</div><p>@@")
	      (concat "@@html: </p>" (car org) "<p>@@"))
	    "\n"
	    (cdr org))))

(defconst oer-reveal--legal-html-template
  "<div class=\"rdfa-license\" about=\"%s\"%s%s><p>%s</p>%s</div>"
  "Outer \"div\" element to hold copyright and license information.
First %s is the URI or path of the document, second and third one may hold
prefixes and DCMI type from `oer-reveal-rdf-prefixes' and
`oer-reveal-dcmitype'; fourth holds contents, fifth an optional creation
timestamp.")
(defconst oer-reveal--title-html-template
  "<span property=\"dcterms:title\">%s</span>")
(defconst oer-reveal--short-license-template
  (concat "[[%s][" oer-reveal-default-figure-title "]] under [[%s][%s]]"))
(defconst oer-reveal--license-html-template
  "<a rel=\"license\" href=\"%s\">%s</a>")
(defconst oer-reveal--source-html-template
  "<a rel=\"dcterms:source\" href=\"%s\">%s</a>")
;; Use dc for rights, not dcterms, as the latter cannot be used with literal
;; values, see: https://github.com/tdwg/rdf/blob/master/DublinCore.md
(defconst oer-reveal--rights-html-template
  "<span property=\"dc:rights\">%s</span>")
(defconst oer-reveal--copyright-string "©")
(defconst oer-reveal--datecopy-html-template
  (concat oer-reveal--copyright-string
          " <span property=\"dcterms:dateCopyrighted\">%s</span>"))
(defconst oer-reveal--creator-html-template
  "<a rel=\"cc:attributionURL dcterms:creator\" href=\"%s\" property=\"cc:attributionName\">%s</a>")
(defconst oer-reveal--href-pdf-template "\\href{%s}{%s}")
(defconst oer-reveal--attribution-html-template
  "<span property=\"cc:attributionName\">%s</span>")
(defconst oer-reveal--figure-div-template "<div about=\"%s\" typeof=\"%s\" class=\"%s\"%s><p><img %s=\"%s\" alt=\"%s\"%s /></p>%s%s</div>")
(defconst oer-reveal--svg-div-template    "<div about=\"%s\" typeof=\"%s\" class=\"%s\"%s><p>%s</p>%s%s</div>")
(defconst oer-reveal--figure-latex-caption-template "#+BEGIN_EXPORT latex\n\\begin{figure}[%s] \\centering\n  \\includegraphics[width=%s\\linewidth]{%s} \\caption{%s (%s)}\n  \\end{figure}\n#+END_EXPORT\n")
(defconst oer-reveal--figure-latex-template "         #+BEGIN_EXPORT latex\n     \\begin{figure}[%s] \\centering\n       \\includegraphics[width=%s\\linewidth]{%s} \\caption{%s}\n     \\end{figure}\n         #+END_EXPORT\n")
(defconst oer-reveal--figure-external-latex-template "         #+BEGIN_EXPORT latex\n     \\textbf{Warning!} External figure \\textbf{not} included: %s \\newline (See HTML presentation instead.)\n         #+END_EXPORT\n")
(defconst oer-reveal--figure-unsupported-latex-template "         #+BEGIN_EXPORT latex\n     \\textbf{Warning!} Figure omitted as %s format \\textbf{not} supported in \\LaTeX: “%s”\\newline (See HTML presentation instead.)\n         #+END_EXPORT\n")
(defconst oer-reveal--unsupported-tex-figure-formats '("gif"))
(defconst oer-reveal--default-copyright "by")
(defconst oer-reveal--default-figure-dcmitype "StillImage")

;; Image grid variables
(defconst oer-reveal--css-grid-img-class-template "grid%s-img%d"
  "Template for name of grid class.")
(defconst oer-reveal--css-grid-img-template
  (concat "." oer-reveal--css-grid-img-class-template
	  " { grid-area: ga%d; }")
  "Template for CSS of img element.")
(defconst oer-reveal--css-repeat-template "repeat(%s)"
  "Template for size of rows and columns.")
(defconst oer-reveal--css-grid-template ".grid%s {
  display: grid;
  height: %s;
  max-width: 90%%;
  grid-template-columns: %s;
  grid-template-rows: %s;
  grid-gap: 5px;
  align-items: start;
  grid-template-areas: %s; }
"
  "Template for CSS of grid.")
(defconst oer-reveal--css-grid-img-all ".grid-img img { }"
  "CSS for all images of grid.")

(defun oer-reveal-http-url-p (string)
  "Return t if STRING is an HTTP(S) URL."
  (string-match-p "^https?://" string))

(defun oer-reveal--export-figure-latex
    (filename texwidth texfilename texlicense &optional latexcaption)
  "Generate LaTeX for figure at FILENAME.
If FILENAME is a full HTTP(S) URL, use
`oer-reveal--figure-external-latex-template' as placeholder.
If FILENAME has an unsupported extension (included in
`oer-reveal--unsupported-tex-figure-formats'), use
`oer-reveal--figure-unsupported-latex-template' as placeholder.
Otherwise, include graphics at TEXFILENAME of width TEXWIDTH
with caption TEXLICENSE.  Optional LATEXCAPTION determines whether
`oer-reveal--figure-latex-template' or
`oer-reveal--figure-latex-caption-template' is used to generate LaTeX code."
  (cond ((oer-reveal-http-url-p filename)
	 (format oer-reveal--figure-external-latex-template texlicense))
	((member (file-name-extension filename)
		 oer-reveal--unsupported-tex-figure-formats)
	 (format oer-reveal--figure-unsupported-latex-template
		 (file-name-extension filename) texlicense))
	(latexcaption
	 (format oer-reveal--figure-latex-caption-template
		 oer-reveal-latex-figure-float
		 texwidth texfilename latexcaption texlicense))
	(t (format oer-reveal--figure-latex-template
		   oer-reveal-latex-figure-float
		   texwidth texfilename texlicense))))

(defun oer-reveal--export-figure-html
    (filename dcmitype divclasses htmlcaption htmllicense imgalt h-image
	      &optional embed-svg extra-attrs)
  "Generate HTML for figure at FILENAME.
Assign DCMITYPE and DIVCLASSES to \"typeof\" and \"class\" attributes.
HTMLCAPTION and HTMLLICENSE specify caption and license information for
the figure in HTML format.
If optional EMBED-SVG is non-nil, the file must be an SVG image
which is embedded directly.  SVG images are also embedded directly if
single file export is requested, which fails if a H-IMAGE is given.
Otherwise, an img tag is used, for which optional parameter IMGALT provides
the text for the alt attribute, while H-IMAGE specifies the height of the
image.
If optional EXTRA-ATTRS is non-nil, it must be a string to be assigned
as extra attributes to the figure's HTML element.
Templates `oer-reveal--svg-div-template' and
`oer-reveal--figure-div-template' specify the general HTML format."
  (let* ((extension (file-name-extension filename))
	 (external (oer-reveal-http-url-p filename))
	 (issvg (and (string= "svg" extension) (not external)))
         (info (org-export-get-environment 'oer-reveal))
	 (issingle (plist-get info :reveal-single-file))
         (rdf-typeof (oer-reveal--rdf-typeof info))
         (typeof (concat (org-re-reveal--if-format
                          "%s " oer-reveal-rdf-figure-typeof)
                         (if (and rdf-typeof
                                  (string-match-p "schema:LearningResource"
                                                  rdf-typeof))
                             "schema:LearningResource "
                           "")
                         dcmitype))
         (encoded-url (url-encode-url filename)))
    (if (and issvg issingle (not embed-svg))
	(user-error "Cannot produce single file without embedding SVG: %s"
		    filename)
      (if embed-svg
	  ;; Embed SVG's XML directly.
	  (format oer-reveal--svg-div-template
		  encoded-url typeof divclasses extra-attrs
		  (oer-reveal--file-as-string filename t)
		  htmlcaption htmllicense)
	(format oer-reveal--figure-div-template
		encoded-url typeof divclasses extra-attrs
                oer-reveal-img-src
		(if (and issingle (not external))
		    ;; Insert base64 encoded image as single line.
		    (concat "data:image/" extension ";base64,"
			    (with-temp-buffer
			      (insert-file-contents-literally filename)
			      (base64-encode-region 1 (point-max) t)
			      (buffer-string)))
		  encoded-url)
		imgalt h-image htmlcaption htmllicense)))))

(defun oer-reveal--export-no-newline (string backend)
  "Call `org-export-string-as' on STRING, BACKEND, and t;
remove newline characters and, in case of HTML, surrounding p tags,
and return as result."
  (string-trim
   (replace-regexp-in-string "\n\\|<p>\\|</p>" " "
			     (org-export-string-as string backend t))))

(defun oer-reveal--file-as-string (filename &optional no-newlines)
  "Return contents of FILENAME as string.
If optional NO-NEWLINES is non-nil, return result without newlines."
  (with-temp-buffer
    (insert-file-contents-literally filename)
    (let ((decoded (decode-coding-region (point-min) (point-max) 'utf-8 t)))
      (if no-newlines
	  (replace-regexp-in-string "\n" " " decoded)
        decoded))))

(defun oer-reveal--attribute-author
    (attributionname attributionurl copyright backend)
  "Create attribution string with author and copyright information.
If ATTRIBUTIONNAME is non-nil it is the name of the author to which the work
should be attributed.  In that case, ATTRIBUTIONURL can specify a URL to
create a hyperlink to the author.
COPYRIGHT can either be the string `oer-reveal--default-copyright', which
indicates that no copyright is necessary, or an arbitrary Org string.
If ATTRIBUTIONNAME (maybe with ATTRIBUTIONURL) is non-nil, preprend
COPYRIGHT to author information.
If ATTRIBUTIONURL is nil and COPYRIGHT equals `oer-reveal--default-copyright',
return the empty string.
Otherwise, return COPYRIGHT information.
BACKEND must be `org' or `html'."
  (let ((copyright
	 (if (eq backend 'org)
	     copyright
	   (oer-reveal--export-no-newline copyright 'html))))
    (cond ((and attributionname attributionurl)
	   (format (if (eq backend 'org)
		       "%s [[%s][%s]]"
		     (concat "%s " oer-reveal--creator-html-template))
		   copyright attributionurl attributionname))
	(attributionname
	 (format (if (eq backend 'org)
		     "%s %s"
		   (concat "%s " oer-reveal--attribution-html-template))
		   copyright attributionname))
	((string= copyright oer-reveal--default-copyright) "")
	(t copyright))))

(defconst oer-reveal--copy-regexp
  "\\([./\\]*/\\)?\\([^/\\]+\\)\\([/\\]\\)\\(.*\\)"
  "Regular expression to match filename components.
Group 2 is the first orginary directory name, possibly after dots
and (back-) slashes in group 1.")

(defun oer-reveal--copy-for-export (filename)
  "Copy FILENAME depending on `oer-reveal-copy-dir-suffix'."
  (when (and (< 0 (length oer-reveal-copy-dir-suffix))
             (not (oer-reveal-http-url-p filename)))
    (unless (string-match oer-reveal--copy-regexp filename)
      (user-error "Unable to create target path for figure: %s" filename))
    (let* ((target (replace-match
                    (concat "\\1\\2" oer-reveal-copy-dir-suffix "\\3\\4")
                    t nil filename))
           (target-dir (file-name-directory target)))
      (make-directory target-dir t)
      (copy-file filename target-dir t t))))

(defun oer-reveal--license-width (maxheight)
  "Compute `max-width' for license given MAXHEIGHT.
If MAXHEIGHT is a string ending in \"ex\", return corresponding string
where the number is divided by `oer-reveal-license-font-factor'."
  (if (and (stringp maxheight)
           (string-suffix-p "ex" maxheight))
      (format "%.2fex"
              (/ (string-to-number
                  (substring maxheight 0 -2))
                 oer-reveal-license-font-factor))
    maxheight))

(defun oer-reveal--perc-height-to-pixels (perc)
  "Compute number of pixels for PERC of height.
PERC is a percentage value, either a number of a string without unit.
Given the slides' height, return number of pixels of this percentage
\(as string, with suffix \"px\")."
  (let ((height (or (plist-get (org-export-get-environment 're-reveal)
			       :reveal-height)
                    oer-reveal-default-slide-height))
        (perc (if (stringp perc)
                  (string-to-number perc)
                perc)))
    (format "%dpx" (* 0.01 height perc))))

(defun oer-reveal--image-height (maxheight)
  "Compute height unit for image given MAXHEIGHT.
If MAXHEIGHT is a string ending in \"rh\", treat it as percentage
value for the slides' height with reveal.js and return height with
unit `px'.  E.g., with the default height of 700, a unit of `10rh'
results in `70px'."
  (if (and (stringp maxheight)
           (string-suffix-p "rh" maxheight))
      (oer-reveal--perc-height-to-pixels (substring maxheight 0 -2))
    maxheight))

(defun oer-reveal--figure-path (filename metaname)
  "Return path for figure FILENAME given METANAME.
If FILENAME is a URI or starts as relative path with directory
`oer-reveal-figures-dir', return unchanged.
Otherwise, return path by treating as FILENAME relative to the directory
of METANAME.
To return all filenames unchanged, customize `oer-reveal-figures-dir' to
the empty string."
  (if (or (string-match "\\`\\(file\\|ftp\\|https?\\)://" filename)
          (string-match (concat "\\`\\([.]/\\)?" oer-reveal-figures-dir)
                        filename))
      filename
    (concat (file-name-directory metaname) filename)))

(defun oer-reveal--attribution-strings
    (metadata &optional caption maxheight divclasses shortlicense
    embed-svg extra-attrs)
  "Helper function.
See `oer-reveal-export-attribution' and
`oer-reveal--export-attribution-helper' for description of arguments
CAPTION, MAXHEIGHT, DIVCLASSES, SHORTLICENSE, EMBED-SVG, EXTRA-ATTRS.
Return cons cell whose car is the HTML representation for METADATA
and whose cdr is the LaTeX representation.
As side effect, copy figure as described for `oer-reveal-copy-dir-suffix'."
  (let* ((org-export-with-sub-superscripts nil)
	 (alist (read (oer-reveal--file-as-string metadata)))
	 (filename (oer-reveal--figure-path
                    (alist-get 'filename alist) metadata))
         (dependencies (alist-get 'dependencies alist))
	 (texfilename (file-name-sans-extension filename))
	 (licenseurl (alist-get 'licenseurl alist))
	 (licensetext (alist-get 'licensetext alist))
	 (permit (if (alist-get 'permit alist)
		     (format ". %s" (alist-get 'permit alist))
		   ""))
	 (attributionname (alist-get 'cc:attributionName alist))
	 (attributionurl (alist-get 'cc:attributionURL alist))
	 (copyright (alist-get 'copyright alist oer-reveal--default-copyright))
	 (dcmitype (format "dcmitype:%s"
                           (alist-get 'dcmitype alist
                                      oer-reveal--default-figure-dcmitype)))
	 (orgauthor (oer-reveal--attribute-author
		     attributionname attributionurl copyright 'org))
	 (htmlauthor (oer-reveal--attribute-author
		      attributionname attributionurl copyright 'html))
	 (title (alist-get 'dc:title alist oer-reveal-default-figure-title))
	 (realcaption (when caption
			(if (stringp caption)
			    caption
			  title)))
	 (htmlcaption (format
                       "<p%s>%s</p>"
                       (if realcaption
                           (org-re-reveal--if-format
                            " property=\"%s\"" oer-reveal-rdf-caption-property)
                         "")
		       (if realcaption
			   (oer-reveal--export-no-newline realcaption 'html)
			 "")))
	 (latexcaption (when realcaption
			 (oer-reveal--export-no-newline realcaption 'latex)))
	 (htmltitle (format oer-reveal--title-html-template
			    (oer-reveal--export-no-newline title 'html)))
	 (imgalt (or (alist-get 'imgalt alist)
		     title))
	 (imgadapted (alist-get 'imgadapted alist "from"))
	 (sourceuri (alist-get 'dc:source alist))
	 (sourcetext (alist-get 'sourcetext alist))
         (sourcelink (format oer-reveal--source-html-template
                             sourceuri sourcetext))
         (sourceshortlink (format oer-reveal--source-html-template
                                  sourceuri oer-reveal-default-figure-title))
	 (sourcehtml (format "; %s %s"
			     (oer-reveal--export-no-newline imgadapted 'html)
			     sourcelink))
	 (divclasses (if divclasses
			 divclasses
		       "figure"))
         (extra-attrs (if extra-attrs
                          (concat " " extra-attrs)
                        ""))
	 (texwidth (alist-get 'texwidth alist 0.9))
         (maxheight (oer-reveal--image-height maxheight))
	 (h-image (if maxheight
		      (format " style=\"max-height:%s\"" maxheight)
		    ""))
         (maxwidth (oer-reveal--license-width maxheight))
	 (h-license (if maxwidth
			(format " style=\"max-width:%s\"" maxwidth)
		      ""))
	 (license (if licensetext
		      (if licenseurl
			  (format " under [[%s][%s]];" licenseurl licensetext)
			(format " under %s" licensetext))
		    (if (< 0 (length permit))
                        ""
                      (user-error "Neither `licensetext' nor `permit' given in: %s" metadata))))
	 (orglicense (cond ((eq shortlicense 'none) "")
			   (shortlicense
                            (cl-assert (and sourceuri licenseurl) nil
                                       "Short license requires URLs for source and license.  Invalid in: %s"
                                       metadata)
                            (format oer-reveal--short-license-template
			            sourceuri licenseurl licensetext))
			   (t (concat
			       (format "“%s” %s" title orgauthor)
			       license
			       (format
                                " %s [[%s][%s]]%s"
				imgadapted sourceuri sourcetext permit)))))
         (htmllicensetag (if licensetext
                             (concat
                              " under "
			      (if licenseurl
                                  (format
                                   oer-reveal--license-html-template
				   licenseurl licensetext)
				licensetext))
			   ""))
	 (htmllicense (cond ((eq shortlicense 'none) "")
			    (shortlicense (format
			                   "<p%s>%s%s</p>"
                                           h-license
			                   sourceshortlink
                                           htmllicensetag))
			    (t (concat
				(format "<p%s>" h-license)
				;; If title is part of the requested
				;; caption, use placeholder in license.
				(if (and caption (booleanp caption))
				    (format "%s "
                                            oer-reveal-default-figure-title)
				  (format "&ldquo;%s&rdquo; " htmltitle))
				htmlauthor
				htmllicensetag
				(format "%s%s</p>" sourcehtml
					(oer-reveal--export-no-newline
					 permit 'html))))))
	 (texlicense (if (< 0 (length orglicense))
			 (oer-reveal--export-no-newline orglicense 'latex)
		       (oer-reveal--export-no-newline title 'latex))))
    (oer-reveal--copy-for-export filename)
    (mapc #'oer-reveal--copy-for-export
          (mapcar (lambda (dependency)
                    (oer-reveal--figure-path dependency metadata))
                  dependencies))
    (if (stringp caption)
	(cons (oer-reveal--export-figure-html
	       filename dcmitype divclasses htmlcaption htmllicense
               imgalt h-image embed-svg extra-attrs)
	      (oer-reveal--export-figure-latex
	       filename texwidth texfilename texlicense latexcaption))
      (cons (oer-reveal--export-figure-html
	     filename dcmitype divclasses htmlcaption
	     htmllicense imgalt h-image embed-svg extra-attrs)
	    (oer-reveal--export-figure-latex
	     filename texwidth texfilename texlicense
	     ;; Similar to above case.  However, a LaTeX caption is always
	     ;; generated via texlicense.
	     ;; Only use latexcaption when shortlicense is t
	     ;; (but not if it is none).
	     (when (and shortlicense (booleanp shortlicense))
	       latexcaption))))))

;;; Function to create a grid of images with license information in HTML.
;; Function oer-reveal-export-image-grid is used in macro in org/config.org.
;; See emacs-reveal-howto for sample use:
;; https://gitlab.com/oer/emacs-reveal-howto
;;;###autoload
(defun oer-reveal-export-image-grid (&rest args)
  "Generate HTML for image grid.
Essentially, this function calls `oer-reveal--export-image-grid-helper'
\(where arguments ARGS are documented), but makes sure that macro
arguments are properly expanded to work with all Org versions,
also after an incompatible change with Org 9.2."
  ;; The first argument is an integer ID.  If that is a string,
  ;; arguments have been quoted.
  (if (stringp (car args))
      (apply #'oer-reveal--export-image-grid-helper
	     (mapcar #'oer-reveal--read-from-string args))
    (apply #'oer-reveal--export-image-grid-helper args)))

(defun oer-reveal--export-image-grid-helper
    (grid-id grid-images height no-columns no-rows template-areas
             &optional fragment)
  "Create HTML to display grid with id GRID-ID of GRID-IMAGES.
The grid has a HEIGHT (percentage of viewport height without unit),
NO-COLUMNS columns, NO-ROWS rows; positioning is specified by TEMPLATE-AREAS.
If optional FRAGMENT is the symbol `grid', add \"fragment\" as class to the
div element containing the grid.  If it is t, add \"fragment\" as class to
each individual image in the grid."
  (let* ((images (read (oer-reveal--file-as-string grid-images)))
	 (no-images (length images))
	 (numbered (cl-mapcar #'cons (number-sequence 1 no-images) images))
	 (row-height (/ (* 0.95 height) no-rows))
         (height-px (oer-reveal--perc-height-to-pixels height))
	 (image-heights (oer-reveal--compute-image-heights template-areas))
         (frag-class (if (eq 'grid fragment) " fragment" "")))
    (oer-reveal--save-image-grid-css
     grid-id images height-px no-columns no-rows template-areas)
    (concat (format "#+REVEAL_EXTRA_CSS: %s\n"
		    (format oer-reveal-css-filename-template grid-id))
	    (format "@@html: </p><div class=\"grid%s%s\">" grid-id frag-class)
	    (mapconcat (lambda (pair)
			 (oer-reveal--export-grid-image
			  grid-id row-height image-heights
			  (car pair) (cdr pair) fragment))
		       numbered " ")
	    "</div><p>@@"
	    "\n"
	    "@@latex: Presentation contains image grid.  \\LaTeX{} export not supported.@@")))

(defun oer-reveal--generate-grid-img (grid-id no)
  "Create CSS class assigning grid-area NO to image NO in grid GRID-ID."
  (format oer-reveal--css-grid-img-template grid-id no no))

(defun oer-reveal--generate-grid-imgs (grid-id no-images)
  "Create CSS classes for GRID-ID assigning grid areas for NO-IMAGES images."
  (mapconcat (lambda (no) (oer-reveal--generate-grid-img grid-id no))
	     (number-sequence 1 no-images) "\n"))

(defun oer-reveal--generate-grid
    (grid-id height no-columns no-rows template-areas)
  "Create CSS for grid layout of GRID-ID.
Layout based on `oer-reveal--css-grid-template' requires HEIGHT,
NO-COLUMNS, NO-ROWS, TEMPLATE-AREAS."
  (format oer-reveal--css-grid-template grid-id height
	  (format oer-reveal--css-repeat-template
                  (format "%d, %d%%" no-columns (/ 100 no-columns)))
	  (format oer-reveal--css-repeat-template
                  (format "%d, %d%%" no-rows (/ 100 no-rows)))
	  template-areas))

(defun oer-reveal--save-image-grid-css
    (grid-id images height no-columns no-rows template-areas)
  "Save CSS for GRID-ID with IMAGES to file.
Helper function for `oer-reveal-export-image-grid', see there for
documentation of arguments HEIGHT, NO-COLUMNS, NO-ROWS, TEMPLATE-AREAS.
Construct name of file in `oer-reveal-export-dir' with
`oer-reveal-css-filename-template', create directories if necessary,
remove possibly previously existing file, write CSS to new file, and
return it's name."
  (let* ((no-images (length images))
	 (filename (expand-file-name
		    (format oer-reveal-css-filename-template grid-id)
		    oer-reveal-export-dir))
	 (dirname (file-name-directory filename))
	 (css (concat (oer-reveal--generate-grid-imgs grid-id no-images)
		      "\n"
		      (oer-reveal--generate-grid
		       grid-id height no-columns no-rows template-areas)
		      oer-reveal--css-grid-img-all "\n")))
    (mkdir dirname t)
    (when (file-readable-p filename)
      (delete-file filename))
    (append-to-file css nil filename)
    filename))

(defun oer-reveal--compute-image-heights (template-areas)
  "Create hash table with heights of cells in TEMPLATE-AREAS."
  (let ((rows (delete "" (delete " " (split-string template-areas "\""))))
	(result (make-hash-table :test 'equal)))
    (dolist (row rows result)
      (let ((cells (delete-dups (split-string row " "))))
	(dolist (cell cells)
	  (puthash cell (+ 1 (gethash cell result 0)) result))))))

(defun oer-reveal--export-grid-image
    (grid-id row-height image-heights no image &optional fragment)
  "Create HTML for IMAGE number NO in GRID-ID.
The height of the row is ROW-HEIGHT, heights of images are given by
IMAGE-HEIGHTS.  If optional FRAGMENT is t, add \"fragment\"
as class attribute.
Call `oer-reveal--attribution-strings' with proper metadata."
  (let ((area (format "ga%d" no))
        (frag-class (if (and fragment (booleanp fragment)) " fragment" "")))
    (car (oer-reveal--attribution-strings
	  image nil
          (oer-reveal--perc-height-to-pixels
           (* (gethash area image-heights) row-height))
	  (concat "figure grid-img "
		  (format oer-reveal--css-grid-img-class-template
			  grid-id no)
                  frag-class)))))

;;; Functionality to display language-specific license information
;;; in HTML with RDFa and in PDF.
(defun oer-reveal--translate (language identifier)
  "Return text under IDENTIFIER for LANGUAGE in `oer-reveal-dictionaries'."
  (let ((dictionary (assoc language oer-reveal-dictionaries)))
    (unless dictionary
      (user-error
       "Language `%s' unknown.  Customize `oer-reveal-dictionaries'" language))
    (let ((text (assoc identifier dictionary)))
      (unless text
        (user-error
         "Identifier `%s' unknown in language `%s'.  Customize `oer-reveal-dictionaries'"
         identifier language))
      (cdr text))))

(defun oer-reveal-sort-creators-p (first second)
  "Sort creators FIRST and SECOND.
Creators are sorted by date, then by name."
  (or (string< (nth 1 first) (nth 1 second))
      (and (string= (nth 1 first) (nth 1 second))
           (string< (nth 0 first) (nth 0 second)))))

(defun oer-reveal--explode-range (range)
  "Turn RANGE into list of numbers.
RANGE must be a string, which can either be a single number of a range
such as \"7-13\"."
  (let* ((parts (split-string range "-" t " "))
         (numbers (mapcar #'string-to-number parts)))
    (if (= 1 (length numbers))
        numbers
      (unless (= 2 (length numbers))
        (user-error "Invalid range: %s" range))
      (unless (< (car numbers) (cadr numbers))
        (user-error "First year of range not smaller than second: %s" range))
      (number-sequence (car numbers) (cadr numbers)))))

(defun oer-reveal--merge-years (strings)
  "Merge STRINGS, which represent years, into single string."
  (let ((union (sort
                (delete-dups (mapcan #'oer-reveal--explode-range strings))
                #'<))
         ;; Variable start remembers the beginning of a range,
         ;; current the max value so far
        start current result)
    (mapconcat #'identity
     (if oer-reveal-use-year-ranges-p
         (reverse
          (dolist (year (append union '(nil)) result)
            (if (and year current (= year (+ 1 current)))
                ;; Ordinary year.  If one larger than current, continue range.
                (setq current (+ 1 current))
              ;; Else, close range if current has value.
              (when current
                (if (> current start)
                    (push (format "%s-%s" start current) result)
                  (push (format "%s" start) result)))
              (setq current year)
              (setq start year))))
       (mapcar #'number-to-string union))
     ", ")))

(defun oer-reveal--convert-creators (creators fmt connective)
  "Convert CREATORS to FMT with CONNECTIVE.
CREATORS is a list of values of SPDX-FileCopyrightText lines;
FMT must be `html' or `pdf'; CONNECTIVE is a word to connect multiple
creators if necessary."
  (let (lines)
    (maphash (lambda (key value)
               (push (list key (oer-reveal--merge-years (car value))
                           (cdr value))
                     lines))
             (oer-reveal--aggregate-creators creators))
    (let ((sorted (sort lines oer-reveal-sort-creators-pred)))
      (mapconcat
       (lambda (entry)
         (let* ((name (nth 0 entry))
                (years (nth 1 entry))
                (uri (nth 2 entry))
                (html-template
                 (format oer-reveal--rights-html-template
                         (concat oer-reveal--datecopy-html-template
                                 " "
                                 (if uri
                                     oer-reveal--creator-html-template
                                   oer-reveal--attribution-html-template))))
                (pdf-template (concat oer-reveal--copyright-string
                                      " %s "
                                      (if uri
                                          oer-reveal--href-pdf-template
                                        "%s")))
                (template (cond ((eq fmt 'html) html-template)
                                ((eq fmt 'pdf) pdf-template)
                                (t (error "Format `%s' not supported" fmt)))))
           (if uri
               (format template years uri name)
             (message "If you used a URL in the SPDX copyright header, an attributionURL could be generated.")
	     (when oer-reveal-warning-delay (sit-for 2))
             (format template years name))))
       sorted connective))))

(defun oer-reveal--aggregate-creators (lines)
  "Group creators in SPDX-FileCopyrightText LINES.
A single creator may occur in multiple lines, typically with different pieces
of year information.  Aggregate those years per unique creator.
Return a hash table mapping each copyright holder to a pair consisting
of a list of years and a URI (or nil)."
  (let ((result (make-hash-table :test 'equal)))
    (dolist (line lines result)
      (unless (string-match oer-reveal--copyright-regexp line)
        (error "Copyright line not matched: %s" line))
      (let* ((years (match-string 1 line))
             (name (match-string 2 line))
             ;; URI is optional, enclosed in <...>.
             (uri (match-string 4 line))
             ;; On Emacs 24, string-trim destroys match data;
             ;; thus trim after final match-string.
             (years (split-string
                     (string-trim years)
                     "," t " "))
             (name (string-trim name))
             ;; URI may be an e-mail address, which would be useless.
             (isurl (and uri (oer-reveal-http-url-p uri)))
             (info (gethash name result)))
        (if info
            (let* ((iyears (car info))
                   (iuri (or (cdr info)
                             (and isurl (string-trim uri)))))
              (when (and uri iuri (not (equal uri iuri)))
                (user-error
                 "Different URIs (%s vs %s) for %s.  Unify SPDX headers?"
                 uri iuri name))
              (puthash name
                       (cons (append years iyears) iuri)
                       result))
          (puthash name
                   (cons years (and isurl (string-trim uri)))
                   result))))))

(defun oer-reveal--convert-license (value fmt language)
  "Convert license specified by VALUE to FMT in LANGUAGE."
  (unless (string-match oer-reveal--license-regexp value)
    (user-error "License line not matched: %s" value))
  (let* ((spdx (string-trim (match-string 1 value)))
         (license-pair (assoc spdx oer-reveal-licenses)))
    (unless license-pair
      (user-error
       "License `%s' unknown.  Customize `oer-reveal-licenses'" spdx))
    (let ((phrase (oer-reveal--translate language spdx))
          (template (if (eq fmt 'html)
                        oer-reveal--license-html-template
                      oer-reveal--href-pdf-template)))
      (format template (cdr license-pair) phrase))))

(defun oer-reveal--convert-spdx-header (header lines fmt language)
  "Convert LINES of SPDX HEADER to FMT with LANGUAGE.
HEADER indicates one of the two types of SPDX headers, namely `copyright'
or `license'.  LINES are newline-separated lines of such headers.
FMT specifies `html' or `pdf', while LANGUAGE is a two-letter language
identifier in `oer-reveal-dictionaries'."
  (let ((connective (oer-reveal--translate language header))
        (line-list (delete-dups (split-string lines "\n" t " "))))
    (cond ((eq header 'copyright)
           (oer-reveal--convert-creators line-list fmt connective))
          ((eq header 'license)
           (mapconcat (lambda (line)
                        (oer-reveal--convert-license line fmt language))
                      line-list connective))
          (t (error "Unknown SPDX header type: `%s'" header)))))

(defun oer-reveal--convert-title (title fmt)
  "Convert TITLE to FMT."
  (cond ((eq fmt 'html) (format oer-reveal--title-html-template title))
        (t title)))

(defun oer-reveal--convert-created (timestamp fmt language)
  "Convert TIMESTAMP to FMT in LANGUAGE."
  (let ((created (oer-reveal--translate language 'created)))
    (cond ((eq fmt 'html)
           (format oer-reveal-created-template created timestamp))
          (t (format "\n\n%s: %s" created timestamp)))))

(defun oer-reveal--language ()
  "Return two-letter language identifier of source document."
  (org-export-with-buffer-copy
   ;; The language might be set in an included file.
   (org-export-expand-include-keyword)
   (let* ((info (org-export-get-environment 'oer-reveal))
          (lang (or (plist-get info :language) "en")))
     ;; Keep relevant prefix of languages such as de, de-de, or de_DE.
     (downcase (car (split-string lang "[-_]"))))))

(defun oer-reveal--rdf-typeof (info &optional text-p)
  "Return RDFa typeof attribute as string or nil from INFO.
Look for keyword \"OER_REVEAL_RDF_TYPEOF\" or use `oer-reveal-rdf-typeof'.
If optional TEXT-P is non-nil, remove \"schema:PresentationDigitalDocument\"
from `oer-reveal-rdf-typeof' (if present) and add
\"schema:TextDigitalDocument\"."
  (let* ((cand (org-re-reveal--parse-listoption info :oer-reveal-rdf-typeof))
         (typeof (if text-p
                     (cons "schema:TextDigitalDocument"
                           (remove "schema:PresentationDigitalDocument"
                                   cand))
                   cand)))
    (when typeof
      (format "typeof=\"%s\"" (string-join typeof " ")))))

(defun oer-reveal-license-to-fmt
    (fmt &optional with-dccreated about with-prefix with-typeof with-legalese
         text-p without-subtitle)
  "Create license information in FMT for file of current buffer.
FMT must be `html' or `pdf'.  PDF output uses LaTeX text (with \"\\href\"
hyperlinks where appropriate).
HTML contains \"div\" elements with RDFa markup from SPDX headers as
defined in the REUSE standard (see URL `https://reuse.software/')
and with pointers to legalese under identifier `legalese' in
`oer-reveal-dictionaries'.
When optional WITH-DCCREATED is non-nil, add time when output was created,
in HTML with \"dcterms:created\" property.
Optional attributes ABOUT, WITH-PREFIX, WITH-TYPEOF, WITH-LEGALESE, TEXT-P
affect HTML output only.
If optional ABOUT is nil, derive value for \"about\" attribute from
base name of published file.
When arguments WITH-PREFIX or WITH-TYPEOF are non-nil, the \"div\"
element receives \"prefix\" or \"typeof\" attributes based on
`oer-reveal-rdf-prefixes' and `oer-reveal-rdf-typeof'.
If WITH-LEGALESE is non-nil, add a \"div\" element with pointers to legalese.
If optional TEXT-P is non-nil, produce RDFa typeof information for a text
document (see `oer-reveal--rdf-typeof').
If optional WITHOUT-SUBTITLE or OER_REVEAL_WITHOUT_SUBTITLE are non-nil,
ignore subtitle; otherwise, concatenate title and subtitle for
license information."
  (let* ((pages-url (cdr (oer-reveal--parse-git-url)))
         (uri (or about
                  (concat pages-url
                          (if (string-suffix-p "/" pages-url)
                              ""
                            "/")
                          (oer-reveal--relative-git-basename (buffer-file-name))
                          ".html")))
         (info (org-export-get-environment 'oer-reveal))
         (language (oer-reveal--language))
         (template (oer-reveal--translate language 'text))
         (legalese (oer-reveal--translate language 'legalese))
         (atitle (car (plist-get info :title)))
         (subtitle (plist-get info :subtitle))
         (without-subtitle (or without-subtitle
                               (plist-get info :oer-reveal-without-subtitle)))
         (title (if (and subtitle (not without-subtitle))
                    (format "%s%s" atitle
                            (org-re-reveal--if-format " %s" (car subtitle)))
                  atitle))
         (copyright (plist-get info :oer-reveal-copyright))
         (license (plist-get info :oer-reveal-license))
         (prefix (if with-prefix
                     (concat " " (plist-get info :oer-reveal-rdf-prefixes))
                   ""))
         (rdf-typeof (oer-reveal--rdf-typeof info text-p))
         (typeof (if with-typeof
                     (if rdf-typeof
                         (concat " " rdf-typeof)
                       (concat " " oer-reveal-dcmitype))
                     ""))
         (now (if (stringp with-dccreated)
                  with-dccreated
                (format-time-string "%Y-%m-%d %a %H:%M"))))
    (unless copyright
      (user-error
       "Org file does not specify copyright information!  Use \"#+SPDX-FileCopyrightText:\" header"))
    (unless license
      (user-error
       "Org file does not specify license information!  Use \"#+SPDX-License-Identifier:\" header"))
    (unless title
      (user-error
     "Org file does not specify title!  Use TITLE header"))
    (let* ((creator (oer-reveal--convert-spdx-header 'copyright copyright fmt language))
           (license (oer-reveal--convert-spdx-header 'license license fmt language))
           (title (oer-reveal--convert-title (string-trim title) fmt))
           (created (if with-dccreated
                        (oer-reveal--convert-created now fmt language)
                      ""))
           (text (format-spec template
                              `((?c . ,creator)
                                (?l . ,license)
                                (?t . ,title)))))
      (cond ((eq fmt 'html)
             (concat
              (format oer-reveal--legal-html-template
                      uri prefix typeof text created)
              (if (and with-legalese (< 0 (length legalese)))
                  (concat "\n" legalese)
                "")))
            ((eq fmt 'pdf)
             (concat text created))))))

(defun oer-reveal--copyright-is-current-p (&optional year)
  "Return t if optional YEAR is part of copyright information.
If YEAR is nil, use current year.
A file without SPDX copyright information is always current.
Use `oer-reveal-spdx-author' to restrict search."
  (let* ((year (or year (format-time-string "%Y" (current-time))))
         (year-regexp (if oer-reveal-spdx-author
                          (format "%s.+%s" year oer-reveal-spdx-author)
                        year))
         matches)
    (save-match-data
      (save-excursion
        (save-restriction
          (widen)
          (goto-char (point-min))
          (while (search-forward-regexp oer-reveal-spdx-copyright-regexp 800 t)
            (push (match-string 0) matches)))))
    (if matches
        (delete nil
                (mapcar (lambda (line)
                          (string-match-p year-regexp line))
                        matches))
      t)))

(defun oer-reveal-copyright-check ()
  "Show message box if year of SPDX copyright header is not current.
If your files contain SPDX-FileCopyrightText headers for multiple
authors, customize `oer-reveal-spdx-author' with your own name.  Then,
check whether a copyright header with your name is current.
After showing the message box, move point to first line with a
copyright header.

Use this function as `after-save-hook', with the message box
serving as reminder to update a SPDX-FileCopyrightText header."
  (let ((year (format-time-string "%Y" (current-time))))
    (unless (oer-reveal--copyright-is-current-p year)
      (message-box
       (concat
        "Current year %s not found in SPDX-FileCopyrightText headers"
        (if oer-reveal-spdx-author
            (format " (for %s)" oer-reveal-spdx-author)
          ""))
       year)
      (goto-char (point-min))
      (re-search-forward oer-reveal-spdx-copyright-regexp)
      (beginning-of-line))))

;;; Functionality to make org-html-link use org-re-reveal's ID format.
;; This is useful when publishing with org-html-publish-to-html
;; where the HTML file is supposed to link into presentations.
;; Sample use: https://gitlab.com/oer/OS/blob/master/elisp/publish.el
(defun oer-reveal--rewrite-link (old-fun &rest arguments)
  "Combine OLD-FUN on ARGUMENTS with `org-re-reveal--maybe-replace-in-link'."
  (let ((orig (apply old-fun arguments)))
    (org-re-reveal--maybe-replace-in-link orig t)))

(defun oer-reveal--add-advice-link (&rest arguments)
  "Extend `org-html-link' with advice for org-re-reveal's anchor ID format.
ARGUMENTS are unused (but present to allow invocation as preparation
function during Org export, which passes an argument)."
  (ignore arguments) ; Silence byte compiler
  (advice-add #'org-html-link :around #'oer-reveal--rewrite-link))

(defun oer-reveal--remove-advice-link (&rest arguments)
  "Remove advice on `org-html-link'.
ARGUMENTS are unused (but present to allow invocation as completion
function during Org export, which passes an argument)."
  (ignore arguments) ; Silence byte compiler
  (advice-remove #'org-html-link #'oer-reveal--rewrite-link))

;;; Export and publication functionality.
(defun oer-reveal--setup-env (func)
  "Setup environment for oer-reveal export, then execute FUNC with ARGS."
    (let ((table-html-th-rows 1)
	  (table-html-table-attribute "class=\"emacs-table\"")
          (org-entities-user '(("textbackslash" "\\textbackslash{}" nil "\\" "\\" "\\" "\\")))
          (org-html-table-default-attributes nil)
          (org-html-container-element oer-reveal-publish-html-container-element)
          (org-html-divs oer-reveal-publish-html-divs)
	  (org-html-doctype oer-reveal-publish-html-doctype)
	  (org-html-postamble oer-reveal-publish-html-postamble)
          (org-html-text-markup-alist oer-reveal-publish-html-text-markup-alist)
          (org-descriptive-links oer-reveal-publish-descriptive-links)
          (org-re-reveal-mobile-app oer-reveal-mobile-app)
          (org-re-reveal-revealjs-version oer-reveal-revealjs-version)
	  (oer-reveal-latex-figure-float oer-reveal-publish-figure-float)
	  (org-latex-pdf-process oer-reveal-publish-pdf-process)
	  (org-latex-default-packages-alist
	   (append oer-reveal-publish-latex-packages
		   org-latex-default-packages-alist))
          (org-export-filter-parse-tree-functions
           (if oer-reveal-new-tab-url-regexp
               (cons #'oer-reveal-filter-parse-tree
                     org-export-filter-parse-tree-functions)
             org-export-filter-parse-tree-functions)))
      (funcall func)))

(defun oer-reveal--master-buffer ()
  "Return master buffer for export of current buffer.
Use `oer-reveal-master' to determine what buffer to export."
  (unless oer-reveal-master
    (setq oer-reveal-master (read-file-name "Master file: ")))
  (if (stringp oer-reveal-master)
      (let ((buffer (find-buffer-visiting oer-reveal-master)))
        (if buffer
            buffer
          (error "You must load file of `oer-reveal-master': %s"
                 oer-reveal-master)))
    (current-buffer)))

(defun oer-reveal-export-to-html
    (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer to a reveal.js HTML file.
Optional ASYNC, SUBTREEP, VISIBLE-ONLY, BODY-ONLY, EXT-PLIST are passed
to `org-re-reveal-export-to-html'."
  (interactive)
  (oer-reveal--setup-env
   (lambda ()
     (let ((master-buffer (oer-reveal--master-buffer)))
       (save-excursion
         (with-current-buffer master-buffer
           (org-re-reveal-export-to-html
            async subtreep visible-only body-only ext-plist 'oer-reveal)))))))

(defun oer-reveal-export-to-html-and-browse
    (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer to a reveal.js and browse HTML file.
Optional ASYNC, SUBTREEP, VISIBLE-ONLY, BODY-ONLY, EXT-PLIST are passed
to `oer-reveal-export-to-html'."
  (interactive)
  (browse-url-of-file
   (expand-file-name
    (oer-reveal-export-to-html
     async subtreep visible-only body-only ext-plist))))

(defun oer-reveal-export-current-subtree
    (&optional async subtreep visible-only body-only ext-plist)
  "Export current subtree to a reveal.js HTML file.
Optional ASYNC, SUBTREEP, VISIBLE-ONLY, BODY-ONLY, EXT-PLIST are passed
to `oer-reveal-export-to-html'."
  (interactive)
  (org-narrow-to-subtree)
  (let ((ret (oer-reveal-export-to-html
              async subtreep visible-only body-only
              (plist-put ext-plist :reveal-subtree t))))
    (widen)
    ret))

;;;###autoload
(defun oer-reveal-publish-to-reveal
    (plist filename pub-dir)
  "Publish an Org file to HTML.
FILENAME is the filename of the Org file to be published.  PLIST
is the property list for the given project.  PUB-DIR is the
publishing directory.
Return output file name."
  (oer-reveal--setup-env
   (lambda ()
     (org-re-reveal-publish-to-reveal plist filename pub-dir 'oer-reveal))))

;;;###autoload
(defun oer-reveal-publish-to-reveal-and-pdf
    (plist filename pub-dir)
  "Publish an Org file to HTML.
FILENAME is the filename of the Org file to be published.  PLIST
is the property list for the given project.  PUB-DIR is the
publishing directory.
Return output file name."
  (oer-reveal--setup-env
   (lambda ()
     (let ((oer-reveal-with-alternate-types '("org" "pdf")))
       (org-re-reveal-publish-to-reveal plist filename pub-dir 'oer-reveal)
       (org-latex-publish-to-pdf plist filename pub-dir)))))

;;;###autoload
(defun oer-reveal-publish-to-reveal-client
    (plist filename pub-dir)
  "Publish an Org file to HTML as multiplex client.
FILENAME is the filename of the Org file to be published.  PLIST
is the property list for the given project.  PUB-DIR is the
publishing directory.
If `org-re-reveal-client-multiplex-filter' is a regular expression (not
nil), only publish FILENAME if it matches this regular expression.
Return output file name."
  (oer-reveal--setup-env
   (lambda ()
     (org-re-reveal-publish-to-reveal-client plist filename pub-dir 'oer-reveal))))

(defun oer-reveal-publish-to-html
    (plist filename pub-dir)
  "Call `org-html-publish-to-html' with PLIST, FILENAME, PUB-DIR.
Before that,
- reset `org-ref-ref-html' to its default value,
- set `oer-reveal-img-src' to \"src\",
- set `oer-reveal-license-font-factor' to 0.8.
Meant for ordinary HTML documents in contrast to reveal.js presentations."
  (oer-reveal--setup-env
   (lambda ()
     (let ((org-ref-ref-html
            "<a class='org-ref-reference' href=\"#%s\">[%s]</a>")
           (oer-reveal-img-src "src")
           (oer-reveal-license-font-factor 0.8))
       (org-html-publish-to-html plist filename pub-dir)))))

(defun oer-reveal-publish-to-pdf (plist filename pub-dir)
  "Call `org-latex-publish-to-pdf' with PLIST, FILENAME, PUB-DIR.
Before that, adjust settings for oer-reveal."
  (oer-reveal--setup-env
   (lambda ()
     (org-latex-publish-to-pdf plist filename pub-dir))))

(defun oer-reveal-publish-to-html-and-pdf
    (plist filename pub-dir)
  "Publish HTML and PDF with PLIST, FILENAME, PUB-DIR.
Set up `oer-reveal-with-alternate-types' to produce source and PDF
links in HTML and source and HTML links in PDF."
  (let ((oer-reveal-with-alternate-types '("org" "pdf")))
    (oer-reveal-publish-to-html plist filename pub-dir)
    (oer-reveal-publish-to-pdf plist filename pub-dir)))

;;; Functionality to set up export.
(defun oer-reveal--string-or-value (thing info)
  "Return THING if it is a string.
Otherwise, return value of property THING in plist INFO."
  (if (stringp thing)
      thing
    (plist-get info thing)))

(defun oer-reveal--plugin-config (info)
  "Build initialization string for reveal.js plugins based on INFO."
  (let* ((init-script (plist-get info :reveal-init-script))
         (plugins (org-re-reveal--read-list
                   (plist-get info :oer-reveal-plugins)))
         (config (apply #'append
                        (cl-mapcar
                         (lambda (plugin)
                           (nth 2 (assoc plugin oer-reveal-plugin-config)))
                         plugins)))
         (config-parts (if init-script
                           (cons init-script config)
                         config)))
    (when config-parts
      (mapconcat (lambda (part)
                   (format oer-reveal-plugin-config-fmt
                           (oer-reveal--string-or-value part info)))
                 config-parts ""))))

(defun oer-reveal--plugin-dependencies (info)
  "Build dependency list for reveal.js plugins based on INFO."
  (let* ((plugins
          (org-re-reveal--read-list (plist-get info :oer-reveal-plugins)))
         (external-plugins
          (org-re-reveal--external-plugins-maybe-from-file info))
         (dependencies
          (apply
           #'append
           (cl-mapcar
            (lambda (plugin)
              (mapcar (lambda (dep)
                        (cons 'dummy (oer-reveal--string-or-value dep info)))
                      (nth 1 (assoc plugin oer-reveal-plugin-config))))
           plugins))))
    (append external-plugins dependencies)))

(defun oer-reveal--concat-props (prop1 prop2 info &optional delim)
  "Concatenate properties PROP1 and PROP2 in INFO with DELIM.
If either property is nil, return only the other one without delimiter."
  (let* ((prop1 (plist-get info prop1))
         (prop1str (or prop1 ""))
         (prop2 (plist-get info prop2))
         (prop2str (or prop2 ""))
         (delim (or delim "")))
    (if (and prop1 prop2)
        (concat prop1 delim prop2)
      (concat prop1str prop2str))))

(defun oer-reveal--postscript (info)
  "Extend `:reveal-postscript' based on INFO.
Add contents of `oer-reveal-anything-svg-opacity' if non-nil."
  (oer-reveal--concat-props :reveal-postscript
                            :oer-reveal-anything-svg-opacity
                            info))

(defun oer-reveal--extra-options (info)
  "Extend `:reveal-extra-options' based on INFO.
Add contents of `oer-reveal-navigation-mode' if non-nil."
  (oer-reveal--concat-props :reveal-extra-options
                            :oer-reveal-navigation-mode
                            info ",\n"))

(defun oer-reveal-template (contents info)
  "Return complete document string after HTML conversion.
CONTENTS is the transcoded contents string.
INFO is a plist holding export options.
Setup plugin and export configuration, then call `org-re-reveal-template'."
  (let ((plugin-dependencies (oer-reveal--plugin-dependencies info))
        (plugin-config (oer-reveal--plugin-config info))
        (add-plugins (concat
                      (or (plist-get info :reveal-add-plugin) "")
                      "\n"
                      oer-reveal-plugin-4-config))
	(org-re-reveal-body-attrs (concat
                                   (plist-get info :oer-reveal-rdf-prefixes)
                                   " "
                                   (oer-reveal--rdf-typeof info))))
    (plist-put info :reveal-external-plugins plugin-dependencies)
    (plist-put info :reveal-init-script plugin-config)
    (plist-put info :reveal-extra-options (oer-reveal--extra-options info))
    (plist-put info :reveal-add-plugin add-plugins)
    (plist-put info :reveal-postscript (oer-reveal--postscript info))
    (org-re-reveal-template contents info)))

(provide 'oer-reveal)
;;; oer-reveal.el ends here
