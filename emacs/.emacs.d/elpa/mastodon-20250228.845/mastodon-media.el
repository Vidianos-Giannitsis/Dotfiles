;;; mastodon-media.el --- Functions for inlining Mastodon media  -*- lexical-binding: t -*-

;; Copyright (C) 2017-2019 Johnson Denen
;; Copyright (C) 2020-2024 Marty Hiatt
;; Author: Johnson Denen <johnson.denen@gmail.com>
;;         Marty Hiatt <mousebot@disroot.org>
;; Maintainer: Marty Hiatt <mousebot@disroot.org>
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

;; mastodon-media.el provides functions for inlining media.

;; Known bug gnutls -12 when trying to access images on some systems.
;; It looks like their may be a version mismatch between the encryption
;; required by the server and client.

;;; Code:
(require 'url-cache)
(require 'mm-decode)
(require 'image-mode)

(autoload 'mastodon-tl--propertize-img-str-or-url "mastodon-tl")
(autoload 'mastodon-tl--image-trans-check "mastodon-tl")

(defvar url-show-status)

(defvar mastodon-tl--shr-image-map-replacement)

(defgroup mastodon-media nil
  "Inline Mastadon media."
  :prefix "mastodon-media-"
  :group 'mastodon)

(defcustom mastodon-media--avatar-height 20
  "Height of the user avatar images (if shown)."
  :type 'integer)

(defcustom mastodon-media--preview-max-height 250
  "Max height of any media attachment preview to be shown in timelines."
  :type 'integer)

(defcustom mastodon-media--enable-image-caching nil
  "Whether images should be cached."
  :type 'boolean)

(defcustom mastodon-media--hide-sensitive-media t
  "Whether media marked as sensitive should be hidden."
  :type 'boolean)

(defvar mastodon-media--generic-avatar-data
  (base64-decode-string
   "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")
  "The PNG data for a generic 100x100 avatar.")

(defvar mastodon-media--generic-broken-image-data
  (base64-decode-string
   "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")
  "The PNG data for a generic 200x200 \"broken image\" view.")

(defvar mastodon-media--sensitive-image-data
  (base64-decode-string
   "iVBORw0KGgoAAAANSUhEUgAAAMgAAADICAYAAACtWK6eAAAA6npUWHRSYXcgcHJvZmlsZSB0eXBl
IGV4aWYAAHjajVHbjcQwCPx3FVcCr/hRjvOSroMtfyc2Ts4rrXRIxjAQPEzC8fo9w89lkiXYknIs
MRLMihWpCDJ1W5tnsuabbaPGMx7uggBS3NrTIo4fwBGz58X7efSPQSPgimh5CrU6vs746gMlfw5y
Bsr9Zdr9Ax+k4oxsXi2WnKbV9o1my88xTRKXyMngTSilWBBnIUvQc7+InpuUNmjpgt7AyEergJMc
ykrwqtZZ6nVMK+7YvAU0skMMb9qFJ/xKUADz4g9VusX8q82j0Rf7z1rhDfqGdxgpcULlAAABhWlD
Q1BJQ0MgcHJvZmlsZQAAeJx9kT1Iw0AcxV8/tKJVBzuIOGSoTnZREd1qFYpQIdQKrTqYXPohNGlI
UlwcBdeCgx+LVQcXZ10dXAVB8APE2cFJ0UVK/F9SaBHjwXE/3t173L0D/PUyU81gHFA1y0gnE0I2
tyKEXtGJIHowgz6JmfqsKKbgOb7u4ePrXYxneZ/7c/QqeZMBPoE4znTDIl4nntq0dM77xBFWkhTi
c+Ixgy5I/Mh12eU3zkWH/TwzYmTSc8QRYqHYxnIbs5KhEk8SRxVVo3x/1mWF8xZntVxlzXvyF4bz
2vIS12kOI4kFLEKEABlVbKAMCzFaNVJMpGk/4eEfcvwiuWRybYCRYx4VqJAcP/gf/O7WLEyMu0nh
BNDxYtsfI0BoF2jUbPv72LYbJ0DgGbjSWv5KHZj+JL3W0qJHQP82cHHd0uQ94HIHGHzSJUNypABN
f6EAvJ/RN+WAgVuge9XtrbmP0wcgQ12lboCDQ2C0SNlrHu/uau/t3zPN/n4Ag31yra/8+kkAAA14
aVRYdFhNTDpjb20uYWRvYmUueG1wAAAAAAA8P3hwYWNrZXQgYmVnaW49Iu+7vyIgaWQ9Ilc1TTBN
cENlaGlIenJlU3pOVGN6a2M5ZCI/Pgo8eDp4bXBtZXRhIHhtbG5zOng9ImFkb2JlOm5zOm1ldGEv
IiB4OnhtcHRrPSJYTVAgQ29yZSA0LjQuMC1FeGl2MiI+CiA8cmRmOlJERiB4bWxuczpyZGY9Imh0
dHA6Ly93d3cudzMub3JnLzE5OTkvMDIvMjItcmRmLXN5bnRheC1ucyMiPgogIDxyZGY6RGVzY3Jp
cHRpb24gcmRmOmFib3V0PSIiCiAgICB4bWxuczp4bXBNTT0iaHR0cDovL25zLmFkb2JlLmNvbS94
YXAvMS4wL21tLyIKICAgIHhtbG5zOnN0RXZ0PSJodHRwOi8vbnMuYWRvYmUuY29tL3hhcC8xLjAv
c1R5cGUvUmVzb3VyY2VFdmVudCMiCiAgICB4bWxuczpkYz0iaHR0cDovL3B1cmwub3JnL2RjL2Vs
ZW1lbnRzLzEuMS8iCiAgICB4bWxuczpHSU1QPSJodHRwOi8vd3d3LmdpbXAub3JnL3htcC8iCiAg
ICB4bWxuczp0aWZmPSJodHRwOi8vbnMuYWRvYmUuY29tL3RpZmYvMS4wLyIKICAgIHhtbG5zOnht
cD0iaHR0cDovL25zLmFkb2JlLmNvbS94YXAvMS4wLyIKICAgeG1wTU06RG9jdW1lbnRJRD0iZ2lt
cDpkb2NpZDpnaW1wOmYyYjU4MzUwLTc3ZWMtNDAxNC1hNDVlLTE1N2QyZjljOGM5NyIKICAgeG1w
TU06SW5zdGFuY2VJRD0ieG1wLmlpZDowOTk5MzZhMi1jOGM5LTRkYTAtYTI0Yi02YTM1MmUyNmNi
NmUiCiAgIHhtcE1NOk9yaWdpbmFsRG9jdW1lbnRJRD0ieG1wLmRpZDphMDliYmZhMi03MzA2LTQ3
NWQtOGExNC05YzA3ZTE1NmFiMTYiCiAgIGRjOkZvcm1hdD0iaW1hZ2UvcG5nIgogICBHSU1QOkFQ
ST0iMi4wIgogICBHSU1QOlBsYXRmb3JtPSJMaW51eCIKICAgR0lNUDpUaW1lU3RhbXA9IjE3MTc1
MDI1MDIzNDQ1NzIiCiAgIEdJTVA6VmVyc2lvbj0iMi4xMC4zNCIKICAgdGlmZjpPcmllbnRhdGlv
bj0iMSIKICAgeG1wOkNyZWF0b3JUb29sPSJHSU1QIDIuMTAiCiAgIHhtcDpNZXRhZGF0YURhdGU9
IjIwMjQ6MDY6MDRUMTQ6MDE6NDArMDI6MDAiCiAgIHhtcDpNb2RpZnlEYXRlPSIyMDI0OjA2OjA0
VDE0OjAxOjQwKzAyOjAwIj4KICAgPHhtcE1NOkhpc3Rvcnk+CiAgICA8cmRmOlNlcT4KICAgICA8
cmRmOmxpCiAgICAgIHN0RXZ0OmFjdGlvbj0ic2F2ZWQiCiAgICAgIHN0RXZ0OmNoYW5nZWQ9Ii8i
CiAgICAgIHN0RXZ0Omluc3RhbmNlSUQ9InhtcC5paWQ6NTRmM2I5NDktOTlkMS00Mzk2LWI2NzIt
Y2ZkYjRlZWFiYTA1IgogICAgICBzdEV2dDpzb2Z0d2FyZUFnZW50PSJHaW1wIDIuMTAgKExpbnV4
KSIKICAgICAgc3RFdnQ6d2hlbj0iMjAyNC0wNi0wNFQxNDowMTo0MiswMjowMCIvPgogICAgPC9y
ZGY6U2VxPgogICA8L3htcE1NOkhpc3Rvcnk+CiAgPC9yZGY6RGVzY3JpcHRpb24+CiA8L3JkZjpS
REY+CjwveDp4bXBtZXRhPgogICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAg
ICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAg
ICAgCiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAg
ICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAKICAgICAgICAg
ICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAg
ICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIAogICAgICAgICAgICAgICAgICAgICAg
ICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAg
ICAgICAgICAgICAgICAgICAgICAgCiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAg
ICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAg
ICAgICAgICAKICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAg
ICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIAogICAg
ICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAg
ICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgCiAgICAgICAgICAgICAgICAg
ICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAg
ICAgICAgICAgICAgICAgICAgICAgICAgICAKICAgICAgICAgICAgICAgICAgICAgICAgICAgICAg
ICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAg
ICAgICAgICAgICAgIAogICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAg
ICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAg
CiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAg
ICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAKICAgICAgICAgICAg
ICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAg
ICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIAogICAgICAgICAgICAgICAgICAgICAgICAg
ICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAg
ICAgICAgICAgICAgICAgICAgCiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAg
ICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAg
ICAgICAKICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAg
ICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIAogICAgICAg
ICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAg
ICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgCiAgICAgICAgICAgICAgICAgICAg
ICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAg
ICAgICAgICAgICAgICAgICAgICAgICAKICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAg
ICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAg
ICAgICAgICAgIAogICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAg
ICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgCiAg
ICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAg
ICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAKICAgICAgICAgICAgICAg
ICAgICAgICAgICAgCjw/eHBhY2tldCBlbmQ9InciPz6w3d0DAAAABmJLR0QA/wD/AP+gvaeTAAAA
CXBIWXMAAC4jAAAuIwF4pT92AAAAB3RJTUUH6AYEDAEq/VtQSwAAABl0RVh0Q29tbWVudABDcmVh
dGVkIHdpdGggR0lNUFeBDhcAAAtOSURBVHja7dvbT5R3Hsfxz5xwhtOsjkgFGQZRTlFOCjJSkpVe
YFltmpZNuo3t9qLdu/4Pa/+L3jVZL9pEU2q7aGirEkUkWw+IM5SpOELxgICcUXjmsBfKBOooo2Za
3H2/Ei+EeRif74/3c5gnmj47fDgqAHGZGQFAIACBAAQCEAhAIACBAAQCEAhAIAAIBCAQgEAAAgEI
BCAQgEAAAgEIBCAQAAQCEAhAIACBAAQCEAhAIACBAAQCgEAAAgEIBCAQgEAAAgEIBCAQgEAAAgFA
IACBAAQCEAhAIACBAAQCEAhAIAAIBCAQgEAAAgEIBCAQgEAAAgEIBCAQAAQCEAhAIACBAAQCEAjw
CrIm+w0ikYiCwaB8Pp8GBweVk5OjoqIiOZ1OjY+Pq6qq6g8fwuLiov7d1qbFhQUdOHhQaampSd0O
BBLT2dmp8+fP6/3331dzc7Oi0ahu3bql1tZW7dixY00MIRqNKhQKyWyxyPQ7bAcCkSSNjY3p7Nmz
anzjDeXl5cW+7vF41NLSos7OzjUxhHXr1umvLS2/23bgHkSSNDk5+ahC65Md5uTkaMuyaID/u0BS
UlIkSd0XLmhubu6J73vr6tb8gO7fv69gMMhvyhqfV7LeN6mBZGdnKysrS1NTU2r95htNTU29Uou9
uLiojo4OhUIhfvPX8LyS+b6mzw4fjibzHz8+Pq6jx45pfGxMqamp+suBA9pWWCiTKf5tbSgU0vXr
1+X3+3Xnzh1t2bJFFRUV8ng8kqSBgQF99dVXkiTXxo36xyefaGBgQD6/X0ODg8rNzVVdXZ1ycnJW
DNDn86nv55/1YH5excXF8nq9slgsikajmpiY0PDwsPx+v5qbm5WZmamJiQmdOHlSN4NB2Ww2paWn
a252Vk1NTSovL3/qdsePH1dfX5/WrVsnm82mSCSiAwcOqKCgQF1dXeru7pbNZpNhGCoqKlJzc3NC
+53oJW0gEND1gQGNjY4qNzdXZWVlKikpic07EolocHBQgUBAwWBQVqtVnoICFW3frry8vNjrnnfO
q80rWeub6Pu+KMu+ffv+mcxAUlNTVVxUpLm5Od2+fVt+n0/z8/PavHlz7BJsiWEYOnXqlDwej6qr
q1VVVaUHDx+qtbVVm7KztdHlktPp1I4dOxQIBDQ5OSmTySSXy6Vd1dWqrKxU8OZNnTl9Wrt27Yrd
+5w5c0YOh0NvNDYqPz9f7e3tqq6ultVq1YMHD+T3+3X+/HmNjIyourpaDodDDodDmzZt0uXLl/XO
O+9o//79qq+vV3Z2tiQ9dbvCwkLl5ubqypUrcjqd+vjjj+VyuSRJW7ZsUVlZmX755Rft3r1bDQ0N
MpvNCe33avoDAXV0dKi2pkY1u3erpLRU83Nz8vv9Ki4ultVq1eLiotrb2zU8PKz6+nrt3btXZWVl
j87wra0yDENut1tms/m557zavJK1vom875q9xFqSkZGhgwcP6t1335XdbtelS5f0xRdf6N69eyte
19PTo/T0dL322muyWCxKSUlRdVWVcnJzdaKtTXNzc7JYLHK5XMrKypLNZtOePXvkdrtjw6qsqJBh
GLo3Oho7e3R3dys9I0MWi0UbN25UQ0ODotFoLOC6ujo1NDQ8eXp9fDQ1m81xw4+3ndVqVWFhocrL
y3Xv3r0V914mk0l2u102m01VVVWyWCwJ7/ezDA0N6djRo3pz/365XC6ZzWb9yenU66+/rg8++EB2
u12SdOHCBQUCATU1NWnDhg2yWCxKTU1VbW2tGhsb1d3drf/89NOjI+dzznm1eSVrfRN53zX7Me+K
Es1mFRcXKycnR6dOnZLP59OXX36pjz76SJmZmVpYWNCPP/6ocDisjo6OuD/jRjConY+fnVgsltiQ
l3M4HLEj/NLrsrOz9e3x42ppaZHb7Y77cNJifbFRPG276upqXb16VYFAQLW1tbGvDwwMaE9dXezs
9rz7/VuRSEQ//PCDamtrtWHDhqf+O2dmZnTu3DnVeb1KS0t74vuVlZU6e/asTp86pfKdO2OvSXTO
q0nW+ibb7xbI8rNJc3OzZDLJd+2aenp61NDQoKmpKYXDYR06dEhut1vRaDR2lF86SjztviXeUT8S
DscG/fbbb+vosWM6cuSIvF6v9uzZo9QkP/XevHmztm/fro6ODu0sL5fDblcoFNJPFy/qb++9F3vd
y+735OSk7t69q5plEcaz9AFJZkZG3O/b7Xbt2LlTly9d0vT0dNyInjXn1SRrfV/pj3mnp6cViUSe
+LrNZtNer1eS1HvtWuz6dPkATCaTzGZz7E8iw5MUG/ryZy8ul0t///BD7WtsVFdXl/515IhmZmaS
OliTyaSamhoZhqEbAwOxS6GqykrZbLYV1+Uvs98PHz58tJAJzmd2dvbpB6/09IT3L96cnyWZ6/vK
BjI4NPTEfcaStMeLsbQo6x5fJ48uu7Z8EeHHR5blv4ThcFh2u13eujodOnRI42Njunr1asI/MxJ9
sQ/68vLylJObq3OdnVpcXNTFixe1bdu2Fa952f1eugQZGxt75uucTuejA1Jvb+yXNd6zBEnKzMx8
oTk/a17JXN+XXac/LJBUh0NdXV1xP58ef7ygFRUVkqQN69fLnZ+vzs7OuEe5Bw8famJiYsXfw3FO
s4uLiyuOMAsLC7p0+XLs+263W6VlZU9su3SmW37aX7qJfjA/v+KsuPw18bZbvv1er1fjY2P6/vvv
lZ+fH7uGXvK8+/1b69ev1+acHHV2dsZ9znT37l3Nzs4qIyND9fX1mpmZkc/ni3updu3aNf15374V
l1eJznm1eSVrfRNdpzUZSHp6uvr6+vTtd99pZGREoVBIhmFoeHhYJ0+eVEVFhUpLS2M38fubmhQO
h/XN8eMaGRlROBxWKBTSnTt3dDMYjB0FZ2ZmNPzrrzIMY8WlkmEYGhwcjIURjUZltVp15vRp3b59
W5FIRGNjYxq8eVNFRUUrbiBHRkZig10KOjMzU66NG+Xz+zU/P6/Z2Vn19vauut1yHo9HTqdTPT09
KikpifvhRaL7HfdDAotFb+7fL5vNphMnTmh0dFSRSESGYSgYDOr+/ftKf3yW9nq9qqysVFtbm65c
uRL7JRwdHVXbiRPaXVOj2pqaFTf2ic55tXkla30TWac1+6BwYWFBP/f3y5mZqVu3bmngxg3NTE/L
4/GoqLhYWwsKYvUvL7+3t1d9fX2ypaRoa0GBSkpKlJWVJUnq6+vT119/vWKbt956S263W59//nns
5xmGIYfDoU8//VTt7e1KS0tTf3+/srKytGvXLuXm5kqSgsGg2tralJKSonA4LMMw5PF4dPDgwUeX
LuPjOt/ZqZGREe0sL1d1VZVSUlJW3W45n9+vyYkJ1dfXP/N+7Vn7vZqJiQn19vaqv79fZrNZhYWF
Ki0tfeJ5QCQS0dDQkAKBgG7cuCFJ2rp1q4qKimLPQF50ziaT6anzSub6JvK+a/ZJejIsfQKy/IYP
/ztzXkvra30VFy7RjwTxas55La0v/+UWIBCAQAACAQgEIBCAQAACAQgEIBAABAIQCEAgAIEABAIQ
CEAgAIEABAIQCAACAQgEIBCAQAACAQgEIBCAQAACAUAgAIEABAIQCEAgAIEABAIQCEAgAIEAIBCA
QAACAQgEIBCAQAACAQgEIBAABAIQCEAgAIEABAIQCEAgAIEABAIQCAACAQgEIBCAQAACAQgEIBCA
QAACAUAgAIEABAIQCEAgAIEABAIQCEAgAIEAIBCAQAACAQgEIBCAQAACAQgEIBAABAIQCEAgAIEA
BAIQCEAgAIEABAIQCAACAQgEIBCAQAACAQgEIBCAQAACAQgEAIEABAIQCEAgAIEABAIQCEAgAIEA
IBCAQICX9F8/bNVInwJ8BAAAAABJRU5ErkJggg==")
  "The PNG data for a sensitive image placeholder.")

(defun mastodon-media--process-image-response
    (status-plist url marker image-options region-length)
  "Callback function processing the url retrieve response for URL.
STATUS-PLIST is the usual plist of status events as per `url-retrieve'.
IMAGE-OPTIONS are the precomputed options to apply to the image.
MARKER is the marker to where the response should be visible.
REGION-LENGTH is the length of the region that should be replaced
with the image."
  (when (marker-buffer marker) ; if buffer hasn't been killed
    (let ((url-buffer (current-buffer))
          (is-error-response-p (eq :error (car status-plist))))
      (let* ((data (unless is-error-response-p
                     (goto-char (point-min))
                     (and (search-forward "\n\n" nil t)
                          (buffer-substring (point) (point-max)))))
             (image (when data
                      (apply #'create-image data ;; inbuilt scaling in 27.1:
                             (when (version< emacs-version "27.1")
                               (when image-options 'imagemagick))
                             t image-options))))
        (when mastodon-media--enable-image-caching
          (unless (url-is-cached url)   ; cache if not already cached
            (url-store-in-cache url-buffer)))
        (with-current-buffer (marker-buffer marker)
          ;; Save narrowing in our buffer
          (let ((inhibit-read-only t))
            (save-restriction
              (widen)
              (put-text-property marker
                                 (+ marker region-length) 'media-state 'loaded)
              (when image
                ;; We only set the image to display if we could load
                ;; it; we already have set a default image when we
                ;; added the tag.
                (mastodon-media--display-image-or-sensitive
                 marker region-length image))
              ;; We are done with the marker; release it:
              (set-marker marker nil)))
          (kill-buffer url-buffer))))))

(defun mastodon-media--display-image-or-sensitive (marker region-length image)
  "Display image using display property, or add sensitive mask.
MARKER, REGION-LENGTH and IMAGE are from
`mastodon-media--process-image-response'.
If the image is marked sensitive, the image is stored in
image-data prop so it can be toggled."
  (if (or (not (eq t (get-text-property marker 'sensitive)))
          (not mastodon-media--hide-sensitive-media))
      ;; display image
      (put-text-property marker (+ marker region-length)
                         'display image)
    ;; display sensitive placeholder and save image data as prop:
    (add-text-properties marker (+ marker region-length)
                         `(display
                           ;; (image :type png :data ,mastodon-media--sensitive-image-data)
                           ,(create-image
                             mastodon-media--sensitive-image-data nil t)
                           sensitive-state hidden image-data ,image))))

(defun mastodon-media--process-full-sized-image-response (status-plist url)
  ;; FIXME: refactor this with but not into
  ;; `mastodon-media--process-image-response'.
  "Callback function processing the `url-retrieve' response for URL.
URL is a full-sized image URL attached to a timeline image.
STATUS-PLIST is a plist of status events as per `url-retrieve'."
  (if-let* ((error-response (plist-get status-plist :error)))
      (user-error "error in loading image: %S" error-response)
    (when mastodon-media--enable-image-caching
      (unless (url-is-cached url) ;; cache if not already cached
        (url-store-in-cache)))
    ;; thanks to rahguzar for this idea:
    ;; https://codeberg.org/martianh/mastodon.el/issues/540
    (let* ((handle (mm-dissect-buffer t))
           (image (mm-get-image handle))
           (str (image-property image :data)))
      (with-current-buffer (get-buffer-create "*masto-image*")
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert-image image str)
          (special-mode) ; prevent image-mode loop bug
          (image-mode)
          (goto-char (point-min))
          (switch-to-buffer-other-window (current-buffer))
          (image-transform-fit-both))))))

(defun mastodon-media--image-or-cached (url process-fun args)
  "Fetch URL from cache or fro host.
Call PROCESS-FUN on it with ARGS, a list of callback args as
specified by `url-retrieve'."
  (if (and mastodon-media--enable-image-caching
           (url-is-cached url)) ;; if cached, decompress and use:
      (with-current-buffer (url-fetch-from-cache url)
        (set-buffer-multibyte nil)
        (goto-char (point-min))
        (zlib-decompress-region
         (goto-char (search-forward "\n\n")) (point-max))
        (apply process-fun args)) ;; no status-plist arg from cache
    ;; fetch as usual and process-image-response will cache it:
    ;; cbargs fun will be called with status-plist by url-retrieve:
    (url-retrieve url process-fun (cdr args))))

(defun mastodon-media--load-image-from-url (url media-type start region-length)
  "Take a URL and MEDIA-TYPE and load the image asynchronously.
MEDIA-TYPE is a symbol and either `avatar' or `media-link'.
START is the position where we start loading the image.
REGION-LENGTH is the range from start to propertize."
  (let ((image-options
         (when (mastodon-tl--image-trans-check)
           (cond ((eq media-type 'avatar)
                  `(:height ,mastodon-media--avatar-height))
                 ((eq media-type 'media-link)
                  `(:max-height ,mastodon-media--preview-max-height)))))
        (buffer (current-buffer))
        (marker (copy-marker start))
        (url-show-status nil)) ; stop url.el from spamming us about connecting
    (condition-case nil
        ;; catch errors in url-retrieve to not break our caller
        (mastodon-media--image-or-cached
         url
         #'mastodon-media--process-image-response
         (list nil url marker image-options region-length))
      (error (with-current-buffer buffer
               ;; TODO: Add retries
               (put-text-property marker (+ marker region-length)
                                  'media-state 'loading-failed)
               :loading-failed)))))

(defun mastodon-media--select-next-media-line (end-pos)
  "Find coordinates of the next media to load before END-POS.
Returns the list of (`start' . `end', `media-symbol') points of
that line and string found or nil no more media links were
found."
  (let ((next-pos (point)))
    (while
        (and
         (setq next-pos (next-single-property-change next-pos 'media-state))
         (or (not (eq 'needs-loading (get-text-property next-pos 'media-state)))
             (null (get-text-property next-pos 'media-url))
             (null (get-text-property next-pos 'media-type))))
      ;; do nothing - the loop will proceed
      )
    (when (and next-pos (< next-pos end-pos))
      (let ((media-type (get-text-property next-pos 'media-type)))
        (cond
         ((eq media-type 'avatar) ; avatars are one character
          (list next-pos (+ next-pos 1) 'avatar))
         ((eq media-type 'media-link) ; media links are 5 characters: [img]
          (list next-pos (+ next-pos 5) 'media-link)))))))

(defun mastodon-media--valid-link-p (link)
  "Check if LINK is valid.
Checks to make sure the missing string has not been returned."
  (and link
       (> (length link) 8)
       (or (string= "http://" (substring link 0 7))
           (string= "https://" (substring link 0 8)))))

(defun mastodon-media--inline-images (search-start search-end)
  "Find all `Media_Links:' in the range from SEARCH-START to SEARCH-END.
Replace them with the referenced image."
  (save-excursion
    (goto-char search-start)
    (let (line-details)
      (while (setq line-details
                   (mastodon-media--select-next-media-line search-end))
        (let* ((start (car line-details))
               (end (cadr line-details))
               (media-type (cadr (cdr line-details)))
               (type (get-text-property start 'mastodon-media-type))
               (image-url (get-text-property start 'media-url)))
          (if (not (mastodon-media--valid-link-p image-url))
              ;; mark it at least as not needing loading any more
              (put-text-property start end 'media-state 'invalid-url)
            ;; proceed to load this image asynchronously
            (put-text-property start end 'media-state 'loading)
            (mastodon-media--load-image-from-url
             image-url media-type start (- end start))
            (when (or (string= type "gifv")
                      (string= type "video"))
              (mastodon-media--moving-image-overlay start end))))))))

;; (defvar-local mastodon-media--overlays nil
;;   "Holds a list of overlays in the buffer.")

(defun mastodon-media--moving-image-overlay (start end)
  "Add play symbol overlay to moving image media items.
START and END are the beginning and end of the media item to overlay."
  (let ((ov (make-overlay start end)))
    (overlay-put
     ov
     'after-string
     (propertize ""
                 'help-echo "Video"
                 'face
                 '((:height 3.5 :inherit mastodon-toot-docs-face))))))
;; (cl-pushnew ov mastodon-media--overlays)))

(defun mastodon-media--get-avatar-rendering (avatar-url)
  "Return the string to be written that renders the avatar at AVATAR-URL."
  ;; We use just an empty space as the textual representation.
  ;; This is what a user will see on a non-graphical display
  ;; where not showing an avatar at all is preferable.
  (let ((image-options (when (mastodon-tl--image-trans-check)
                         `(:height ,mastodon-media--avatar-height))))
    (concat
     (propertize " "
                 'media-url avatar-url
                 'media-state 'needs-loading
                 'media-type 'avatar
                 'display
                 (apply #'create-image mastodon-media--generic-avatar-data
                        ;; inbuilt scaling in 27.1
                        (when (version< emacs-version "27.1")
                          (when image-options 'imagemagick))
                        t image-options))
     " ")))

(defun mastodon-media--get-media-link-rendering
    (media-url &optional full-remote-url type caption sensitive)
  "Return the string to be written that renders the image at MEDIA-URL.
FULL-REMOTE-URL is used for `shr-browse-image'.
TYPE is the attachment's type field on the server.
CAPTION is the image caption if provided.
SENSITIVE is a flag from the item's JSON data."
  (let* ((help-echo-base
          (substitute-command-keys
           (concat "\\`RET'/\\`i': load full image (prefix: copy URL), \\`+'/\\`-': zoom,\
 \\`r': rotate, \\`o': save preview"
                   (when (not (eq sensitive :json-false))
                     ", \\`S': toggle sensitive media"))))
         (help-echo (if caption
                        (concat help-echo-base
                                "\n\"" caption "\"")
                      help-echo-base)))
    (concat
     (mastodon-tl--propertize-img-str-or-url
      "[img]" media-url full-remote-url type help-echo
      (create-image mastodon-media--generic-broken-image-data nil t)
      nil caption sensitive)
     " ")))

(provide 'mastodon-media)
;;; mastodon-media.el ends here
