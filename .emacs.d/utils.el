(defun ensure-installed (package)
  "Install a package using ELPA if it's not already installed"
  (unless (package-installed-p package) (package-install package))
  (require package))
