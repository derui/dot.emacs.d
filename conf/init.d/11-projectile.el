;;; -*- lexical-binding: t -*-
;; モードラインに関係するパッケージの設定
(eval-when-compile
  (require 'cl-lib)
  (require 'use-package))

(use-package projectile
  :hook
  ((after-init . projectile-mode))
  :custom
  (projectile-completion-system 'ivy)
  :config
  (projectile-register-project-type
   'yarn
   '("package.json")
   :compile "yarn build"
   :test "yarn test"
   :run "yarn start"
   :test-suffix ".test"))
