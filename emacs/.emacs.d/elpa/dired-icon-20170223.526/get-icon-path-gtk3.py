#!/usr/bin/python3

# Copyright (C) 2016  Hong Xu <hong@topbug.net>

# This file is not part of GNU Emacs.

# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

# Read list of files from stdin, locate their corresponding icon file paths and print them to
# stdout.

from __future__ import print_function

import sys, gi

gi.require_version('Gtk', '3.0')
gi.require_version('Gio', '2.0')

from gi.repository import Gtk, Gio

image_size = -1

if len(sys.argv) == 2:
    # If we only need to test the availability of gtk libraries, exit now.
    if sys.argv[1] == 'test':
        sys.exit(0)
    else:
        image_size = int(sys.argv[1])

for line in sys.stdin:
    mimetype = line.strip()
    if not mimetype:  # ignore empty mime types
        print()
        continue

    icon = Gio.content_type_get_icon(mimetype)
    theme = Gtk.IconTheme.get_default()
    info = theme.choose_icon(icon.get_names(), image_size, 0)
    if info:
        print(info.get_filename())
    else:
        print()
