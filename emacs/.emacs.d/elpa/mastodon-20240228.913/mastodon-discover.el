;;; mastodon-discover.el --- Use Mastodon.el with discover.el  -*- lexical-binding: t -*-

;; Copyright (C) 2019 Johnson Denen
;; Copyright (C) 2020-2022 Marty Hiatt
;; Author: Johnson Denen <johnson.denen@gmail.com>
;;         Marty Hiatt <martianhiatus@riseup.net>
;; Maintainer: Marty Hiatt <martianhiatus@riseup.net>
;; Homepage: https://codeberg.org/martianh/mastodon.el

;; This file is not part of GNU Emacs.

;; This file is part of mastodon.el.

;; mastodon.el is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; mastodon.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with mastodon.el.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This adds optional functionality that can be used if the dicover package
;; is present.
;;
;; See the README file for how to use this.

;;; Code:

(declare-function discover-add-context-menu "discover")

(autoload 'mastodon-kill-window "mastodon")

(defun mastodon-discover ()
  "Plug Mastodon functionality into `discover'."
  (interactive)
  (when (require 'discover nil :noerror)
    (discover-add-context-menu
     :bind "?"
     :mode 'mastodon-mode
     :mode-hook 'mastodon-mode-hook
     :context-menu
     '(mastodon
       (description "Mastodon feed viewer")
       (actions
        ("Toots"
         ("A" "View profile of author" mastodon-profile--get-toot-author)
         ("b" "Boost" mastodon-toot--boost)
         ("f" "Favourite" mastodon-toot--favourite)
         ("c" "Toggle hidden text (CW)" mastodon-tl--toggle-spoiler-text-in-toot)
         ("k" "Bookmark toot" mastodon-toot--toggle-bookmark)
         ("v" "Vote on poll" mastodon-tl--poll-vote)
         ("n" "Next" mastodon-tl--goto-next-item)
         ("p" "Prev" mastodon-tl--goto-prev-item)
         ("TAB" "Next link item" mastodon-tl--next-tab-item)
         ("S-TAB" "Prev link item" mastodon-tl--previous-tab-item)
         ;; NB: (when (require 'mpv etc. calls don't work here
         ("C-RET" "Play media" mastodon-tl--mpv-play-video-at-point)
         ("t" "New toot" mastodon-toot)
         ("r" "Reply" mastodon-toot--reply)
         ("C" "Copy toot URL" mastodon-toot--copy-toot-url)
         ("o" "Open toot URL" mastodon-toot--open-toot-url)
         ("d" "Delete (your) toot" mastodon-toot--delete-toot)
         ("D" "Delete and redraft (your) toot" mastodon-toot--delete-toot)
         ("e" "Edit (your) toot" mastodon-toot--edit-toot-at-point)
         ("E" "View edits of (your) toot" mastodon-toot--view-toot-edits)
         ("i" "Pin/Unpin (your) toot" mastodon-toot--pin-toot-toggle)
         ("P" "View user profile" mastodon-profile--show-user)
         ("a" "Translate toot at point" mastodon-toot--translate-toot-text)
         ("T" "View thread" mastodon-tl--thread)
         ("v" "Vote on poll" mastodon-tl--poll-vote)
         ("," "View toot's favouriters" mastodon-toot--list-toot-favouriters)
         ("." "View toot's boosters" mastodon-toot--list-toot-boosters)
         ("/" "Switch buffers" mastodon-switch-to-buffer))
        ("Views"
         ("h/?" "View mode help/keybindings" describe-mode)
         ("#" "Tag search" mastodon-tl--get-tag-timeline)
         ("\"" "List followed tags" mastodon-tl--list-followed-tags)
         ("'" "Followed tags timeline" mastodon-tl--followed-tags-timeline)
         ("F" "Federated" mastodon-tl--get-federated-timeline)
         ("H" "Home" mastodon-tl--get-home-timeline)
         ("L" "Local" mastodon-tl--get-local-timeline)
         ("N" "Notifications" mastodon-notifications-get)
         ("@" "Notifications with mentions" mastodon-notifications--get-mentions)
         ("g/u" "Update timeline" mastodon-tl--update)
         ("s" "Search" mastodon-search--query)
         ("O" "Jump to your profile" mastodon-profile--my-profile)
         ("U" "Update your profile note" mastodon-profile--update-user-profile-note)
         ("K" "View bookmarks" mastodon-profile--view-bookmarks)
         ("V" "View favourites" mastodon-profile--view-favourites)
         ("R" "View follow requests" mastodon-profile--view-follow-requests)
         ("G" "View follow suggestions" mastodon-tl--get-follow-suggestions)
         ("I" "View filters" mastodon-tl--view-filters)
         ("X" "View lists" mastodon-tl--view-lists)
         ("S" "View scheduled toots" mastodon-tl--view-scheduled-toots)
         (";" "View instance description" mastodon-tl--view-instance-description))
        ("Users"
         ("W" "Follow" mastodon-tl--follow-user)
         ("C-S-W" "Unfollow" mastodon-tl--unfollow-user)
         ("M" "Mute" mastodon-tl--mute-user)
         ("C-S-M" "Unmute" mastodon-tl--unmute-user)
         ("B" "Block" mastodon-tl--block-user)
         ("C-S-B" "Unblock" mastodon-tl--unblock-user))
        ("Images"
         ;; RET errors here also :/
         ("<return>/i" "Load full image in browser" 'shr-browse-image)
         ("r" "rotate" 'image-rotate)
         ("+" "zoom in" 'image-increase-size)
         ("-" "zoom out" 'image-decrease-size)
         ("u" "copy URL" 'shr-maybe-probe-and-copy-url))
        ("Profile view"
         ("C-c C-c" "Cycle profile views" mastodon-profile--account-view-cycle))
        ("Quit"
         ("q" "Quit mastodon and bury buffer." kill-this-buffer)
         ("Q" "Quit mastodon buffer and kill window." mastodon--kill-window)
         ("M-C-q" "Quit mastodon and kill all buffers." mastodon-kill-all-buffers)))))))

(provide 'mastodon-discover)
;;; mastodon-discover.el ends here
