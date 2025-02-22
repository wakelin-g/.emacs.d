(straight-use-package
 '(lua-mode
   :type git
   :files (:defaults (:exclude "init-tryout.el") "lua-mode-pkg.el")
   :host github :repo "immerrr/lua-mode"))
