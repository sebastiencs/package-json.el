 
# package-json.el

Manage your package.json files.  

![package-json](https://github.com/sebastiencs/package-json.el/raw/screenshots/screenshot.gif)

It informs you about the dependencies, if there are availables updates and provide some usefull functions.  
It fetchs the datas directly from https://registry.npmjs.org/, so an internet connection is necessary.

### Installation
``` el
(use-package package-json
  :commands package-json-mode
  :hook (json-mode . my-package-json-start)
  :init
  (defun my-package-json-start nil
    (and buffer-file-name
         (equal (file-name-nondirectory buffer-file-name) "package.json")
         (package-json-mode))))
```

### Functions

- `package-json-open-readme-at-point`:
   Open the package's README in a buffer
- `package-json-browse-package-homepage-at-point`
   Open the package's homepage in your browser

### Todos:

-  Add a completion backend to complete dependencies from https://registry.npmjs.org/
-  Add function to run `npm --update`,...
