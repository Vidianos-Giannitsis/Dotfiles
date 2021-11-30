(require 'lispy)
(require 'hydra)

(defhydra evil-lispy/hydra-navigation ()
  "
Navigation cheat sheet
----------------------

                       Starting from _|_, navigate to:
_h_: backward & up    |
_j_: next             |  _h_(->
_k_: previous         |       _k_(+ 2 3)
_l_: forward & up     |       _|_(+ _i_(+ 2 1))_o_
_o_: other side       |       _j_(* 8))_l_
_i_: inside           |


Miscellaneous
-------------
_q_: jump to ()
_-_: go to subword


"
  ("h" special-lispy-left)
  ("j" special-lispy-down)
  ("k" special-lispy-up)
  ("l" special-lispy-right)
  ("o" special-lispy-different)
  ("|" identity :color teal)
  ("i" special-lispy-flow)
  ("q" special-lispy-ace-paren)
  ("-" special-lispy-ace-subword))

(defhydra evil-lispy/hydra ()
  "
evil-lispy cheat sheet
----------------------

_N_ Navigation cheat sheet

^Refactor^               ^Evaluate^
----------------------------------------------------
_t_  teleport            _e_  eval
_d_  drag                _E_  eval and insert
_c_  clone
_w_  move up
_s_  move down
_r_  raise
_O_  to oneliner
_M_  to multiline


"
  ("N" evil-lispy/hydra-navigation/body :color teal)
  ("h" special-lispy-left)
  ("j" special-lispy-down)
  ("k" special-lispy-up)
  ("l" special-lispy-right)
  ("o" special-lispy-different)
  ("i" special-lispy-flow)
  ("q" special-lispy-ace-paren)
  ("-" special-lispy-ace-subword)

  ("c" special-lispy-clone)
  ("t" special-lispy-teleport)
  ("d" special-lispy-other-mode)
  ("w" special-lispy-move-up)
  ("s" special-lispy-move-down)
  ("r" special-lispy-raise)
  ("O" special-lispy-oneline)
  ("M" lispy-multiline)

  ("e" special-lispy-eval)
  ("E" special-lispy-eval-and-insert))

(defun evil-lispy-show-help ()
  (interactive)
  (evil-lispy/hydra/body))

(provide 'evil-lispy-help)
