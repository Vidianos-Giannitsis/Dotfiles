;;; string-inflection-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "string-inflection" "string-inflection.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from string-inflection.el

(autoload 'string-inflection-ruby-style-cycle "string-inflection" "\
foo_bar => FOO_BAR => FooBar => foo_bar" t nil)

(autoload 'string-inflection-python-style-cycle "string-inflection" "\
foo_bar => FOO_BAR => FooBar => foo_bar" t nil)

(autoload 'string-inflection-java-style-cycle "string-inflection" "\
fooBar => FOO_BAR => FooBar => fooBar" t nil)

(autoload 'string-inflection-all-cycle "string-inflection" "\
foo_bar => FOO_BAR => FooBar => fooBar => foo-bar => Foo_Bar => foo_bar" t nil)

(autoload 'string-inflection-toggle "string-inflection" "\
toggle foo_bar <=> FooBar" t nil)

(autoload 'string-inflection-camelcase "string-inflection" "\
FooBar format" t nil)

(autoload 'string-inflection-lower-camelcase "string-inflection" "\
fooBar format" t nil)

(autoload 'string-inflection-underscore "string-inflection" "\
foo_bar format" t nil)

(autoload 'string-inflection-capital-underscore "string-inflection" "\
Foo_Bar format" t nil)

(autoload 'string-inflection-upcase "string-inflection" "\
FOO_BAR format" t nil)

(autoload 'string-inflection-kebab-case "string-inflection" "\
foo-bar format" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "string-inflection" '("string-inflection-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; string-inflection-autoloads.el ends here
