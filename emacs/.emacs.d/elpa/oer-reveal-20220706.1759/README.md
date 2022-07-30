<!--- Local IspellDict: en -->
<!--- SPDX-FileCopyrightText: 2018-2022 Jens Lechtenbörger -->
<!--- SPDX-License-Identifier: CC0-1.0 -->

[![MELPA Stable](https://stable.melpa.org/packages/oer-reveal-badge.svg)](https://stable.melpa.org/#/oer-reveal)
[![MELPA](https://melpa.org/packages/oer-reveal-badge.svg)](https://melpa.org/#/oer-reveal)

# Relationships between projects

| Package                                                                 | Description                                                                                                                                      |
|-------------------------------------------------------------------------|--------------------------------------------------------------------------------------------------------------------------------------------------|
| [org-reveal](https://github.com/yjwen/org-reveal)                       | Origin of org-re-reveal                                                                                                                          |
| [org-re-reveal](https://gitlab.com/oer/org-re-reveal)                   | Fork of org-reveal, initially to add audio fragments, now with [various changes](https://gitlab.com/oer/org-re-reveal/blob/master/CHANGELOG.org) |
| [org-re-reveal-ref](https://gitlab.com/oer/org-re-reveal-ref)           | Addon to org-re-reveal for bibliography slide based on [org-ref](https://github.com/jkitchin/org-ref)                                            |
| [org-re-reveal-citeproc](https://gitlab.com/oer/org-re-reveal-citeproc) | Alternative to org-re-reveal-ref for bibliography slide based on syntax of Org mode 9.5                                                          |
| [oer-reveal](https://gitlab.com/oer/oer-reveal)                         | Export backend derived from org-re-reveal; functionality for installation of reveal.js and plugins; simplification of licensing for OER          |
| [emacs-reveal](https://gitlab.com/oer/emacs-reveal/)                    | Bundling of org-re-reveal, org-re-reveal-citeproc, org-re-reveal-ref, and oer-reveal                                                             |

Originally, *emacs-reveal* was created to enhance *org-reveal*, while
it is now a bundle of several packages (including *org-re-reveal-citeproc*,
*org-re-reveal-ref*, and *oer-reveal*).
Whether anyone wants to use those package in isolation is up to them.
I recommend to go for *emacs-reveal*, for which a
[howto](https://oer.gitlab.io/emacs-reveal-howto/howto.html) is
available.  Note that as of October 2019 the submodules installed by
*emacs-reveal* have a size of 226 MB; hence, initial installation
takes some time.

The package *oer-reveal* was created for use in conjunction with
*emacs-reveal* (available at https://gitlab.com/oer/emacs-reveal, not
on MELPA) but can also be used individually if you know what to
expect:

1. Submodules (for reveal.js and plugins) are *not* installed
   automatically, stable paths to contained resources are *not*
   created automatically.  See file
   [emacs-reveal.el](https://gitlab.com/oer/emacs-reveal/blob/master/emacs-reveal.el)
   for sample initialization code.
   A howto for emacs-reveal is available, which serves as sample
   emacs-reveal presentation:
   - [Org source](https://gitlab.com/oer/emacs-reveal-howto)
   - [Reveal.js presentation](https://oer.gitlab.io/emacs-reveal-howto/howto.html)
2. Publishing of Org *projects* is different from export of
   individual Org *files*.  Export can be triggered with the usual
   key binding `C-c C-e` (see below), while publishing is performed
   with `oer-reveal-publish-all`, which requires setup as described
   under 1.  In particular, `oer-reveal-publish-all` publishes
   entire projects (with all necessary resources) based on
   `org-publish-project-alist`, while interactive export of
   individual Org files only creates an HTML file, for which
   necessary resources must be available locally.
3. In your `org` files, you may want to include
   [org/config.org](org/config.org), which (a) defines several
   macros and (b) contains customizations for reveal.js plugins
   and PDF export via LaTeX.

# Functionality

This repository provides *oer-reveal*, a package to extend
[org-re-reveal](https://gitlab.com/oer/org-re-reveal)
with resources and functionality that aim to simplify the creation of
[Open Educational Resources (OER)](https://en.wikipedia.org/wiki/Open_educational_resources).
This package defines an Org mode export backend derived from
*org-re-reveal* for export to HTML with reveal.js.  It provides help
in installing and configuring reveal.js and several of its plugins.
This package forms the basis of [emacs-reveal](https://gitlab.com/oer/emacs-reveal).

More specifically, *oer-reveal* provides:
- Installation and configuration of reveal.js with several plugins,
  e.g., for audio explanations, for quizzes, to see notes, to display
  a hyperlinked table of contents as footer
- An Org export backend that extends *org-re-reveal*
  - With keys `C-c C-e w w` and `C-c C-e w b`
- Org macros to embed OER figures with proper license attribution (in
  machine-readable RDFa format for HTML export)
- Org templates to generate license information from SPDX headers
  (used with [REUSE 3.0](https://reuse.software/)

[Sample Org files](https://gitlab.com/oer/oer-reveal/tree/master/examples)
for *oer-reveal* are available in its repository, along with sample code
[publish.el](https://gitlab.com/oer/oer-reveal/blob/master/examples/publish.el)
to publish *oer-reveal* presentations.  Before you try to publish
*oer-reveal* presentations, please check out
[emacs-reveal](https://gitlab.com/oer/emacs-reveal) and the
[howto for emacs-reveal](https://oer.gitlab.io/emacs-reveal-howto/howto.html),
which showcases how to publish presentations in a GitLab CI/CD
infrastructure.

As usual for Org export, use `C-c C-e` to start an export, followed by
backend specific key bindings.  With *oer-reveal*, the default
bindings are `C-c C-e w w` and `C-c C-e w b`, which can be customized
with `oer-reveal-keys`.  (Actually, "ö" seems preferable to "w", if it
exists on your keyboard.)

Notably, *oer-reveal* simplifies two traditionally cumbersome task
for OER creators, namely (1) specification of copyright and license
information and (2) the re-use of figures under free licenses
that require proper attribution.  For (1), *oer-reveal* recognizes
SPDX headers (as advertised by the [REUSE project](https://reuse.software/));
see file [examples/test-license.org](examples/test-license.org) for an
example.  For (2), macros `revealimg`, `reveallicense`, and
`revealgrid` are defined and documented in file
[org/config.org](org/config.org).

# Usage

Variable `oer-reveal-dir` points to the directory of *oer-reveal* and
its embedded resources.  You may want to use that variable in your
own publication code, for which some pointers are provided in
function `oer-reveal-publish-all` of file
[oer-reveal-publish.el](oer-reveal-publish.el).
Note that subdirectory "title-slide" contains some variants for
title slides of presentations, and subdirectory "css" contains
sample CSS.  Subdirectory "org" contains Org files to embed in
presentations, in particular file "org/config.org", which also adds
style files for the accessibility plugin and the TOC-progress that are
used with oer-reveal by default.
Please be warned that included resources, in
particular CSS files, may change in incompatible ways.  You may
want to work with your own copies.

Function `oer-reveal-setup-submodules` downloads and installs
reveal.js and some of its plugins into the directory
`oer-reveal-submodules-dir`.  If you use
[emacs-reveal](https://gitlab.com/oer/emacs-reveal), such updates as
well as updates of *oer-reveal* itself happen automatically.  Function
`oer-reveal-generate-include-files` generates Org files under
`oer-reveal-org-includes-dir`, which include Org files coming with
*oer-reveal*; when installing *oer-reveal* from MELPA (with changing
directories upon updates) you can include those generated files at
stable locations in your own Org files.

Function `oer-reveal-publish-setq-defaults` changes variables from
other packages, which may offer some suggestions what to adapt in
your own configuration.

Again, note that the file
[emacs-reveal.el](https://gitlab.com/oer/emacs-reveal/blob/master/emacs-reveal.el),
hosted at https://gitlab.com/oer/emacs-reveal, provides the following sample
initialization code for *oer-reveal*, and the howto at
https://gitlab.com/oer/emacs-reveal-howto offers a sample presentation
using this code.

```
(require 'oer-reveal-publish)
(oer-reveal-setup-submodules t)
(oer-reveal-generate-include-files t)
(oer-reveal-publish-setq-defaults)
```

# Excerpt of customizable options

Variable `oer-reveal-reveals-version` allows to specify the version of
reveal.js (which introduced incompatible changes in version 4.0).  It
takes precedence of `org-re-reveal-revealjs-version`.  However, this
setting only affects some settings in generated HTML code; it does not
aim to install reveal.js for you.  In contrast, the software bundle
[emacs-reveal](https://gitlab.com/oer/emacs-reveal) contains reveal.js.

Variable `oer-reveal-plugins` lists reveal.js plugins to be
activated.  To configure those plugins, customize
`oer-reveal-plugin-config` and `oer-reveal-plugin-4-config`, which in
turn point to customizable variables for individual plugins.

When generating image grids, `oer-reveal-export-dir` specifies
the directory into which to generate CSS code.  This should
probably be the directory into which you publish your HTML code.
I set this to "./" before exporting with `C-c C-e w b`.
The names of generated CSS files for image grids are determined by
`oer-reveal-css-filename-template`.

For language-specific license information generated from SPDX headers,
see `oer-reveal-dictionaries`, `oer-reveal-licenses`,
`oer-reveal-use-year-ranges-p`.

For help in keeping copyright years current, see function
`oer-reveal-copyright-check` and variables `oer-reveal-spdx-author`,
`oer-reveal-spdx-copyright-regexp`.

If you include Org files from each other, customizable variable
`oer-reveal-master` allows to trigger export of the main file from
included files (inspired by AUCTeX’s master functionality).

To see all customizable variables, check out customization group
`org-export-oer-reveal`:
`M-x customize-group` followed by `org-export-oer-reveal`.
In particular, `oer-reveal-publish-babel-languages` can be used to
activate Babel languages during HTML (and PDF) export, e.g., to
generate figures from embedded sources (e.g., dot/graphviz or ditaa).
