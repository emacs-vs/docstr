[![Build Status](https://travis-ci.com/jcs-elpa/docstr.svg?branch=master)](https://travis-ci.com/jcs-elpa/docstr)
[![Release Tag](https://img.shields.io/github/tag/jcs-elpa/docstr.svg?label=release)](https://github.com/jcs-elpa/docstr/releases/latest)
[![Emacs Ver.](https://img.shields.io/badge/Emacs-25.1+-7B59B0.svg)](https://www.gnu.org/software/emacs/)
[![License: GPL v3](https://img.shields.io/badge/License-GPL%20v3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)

# docstr
> A document string minor mode.

This package provides a simple solution to insert document string into the code.

## Usage

You can enable this package to a specific major mode, like

```el
;; Enable `docstr' inside these major modes.
(add-hook 'actionscript-mode-hook (lambda () (docstr-mode 1)))
(add-hook 'java-mode-hook (lambda () (docstr-mode 1)))
```

If you just want to enable it inside a specific buffer just call `docstr-mode`
as a command throught minibuffer.

```
M-x docstr-mode
```

To enable this package through out all the buffers by default. Call global
version of the minor mode; then just put the following code snippet to your
configuration.

```el
(global-docstr-mode 1)
```

## Supported Langauges

Here is a list of all languages that support by this package.

* ActionScript
* C
* C++
* C#
* Golang
* Groovy
* Java
* JavaScript
* JSX
* Lua
* PHP
* Python
* Rust
* Scala
* TypeScript

You can customize `docstr-writers-alist` variable to add your own document
string support for your favourite language. Just add a cons cell like `(mode-name . docstr-writer-name)`.

To create your own document string writer, you would need to create a function
that takes in one argument. For instance,

```el
(defun my-docstr-writer (search-string)
  ;; Insert document stirng here.
  )
```

Argument `search-string` will be populated by the triggeration associated list.
See variable `docstr-trigger-alist` for more information. For instance,
a `C#` trigger data is like this

```el
(csharp-mode . ("/" docstr-trigger-csharp))
```

And the triggeration function will look like this

```el
(defun docstr-trigger-csharp (&rest _)
  "Trigger document string inside C#."
  (when (and (docstr--doc-valid-p) (looking-back "///" 3))
    (save-excursion
      (insert " <summary>\n")
      (insert "/// \n")
      (insert "/// </summary>"))
    (forward-line 1)
    (end-of-line)
    (docstr--insert-doc-string (docstr--c-style-search-string 2))))
```

The document string is triggered by certain conditions are met; and it will
finally calls the function `docstr--insert-doc-string` for document string
insertion. In this example, `(docstr--c-style-search-string 2)` is the
`search-string` ready for document string writer to write a proper document
string base on the `search-string` information. Then this is the result.

<p align="center">
  <img src="./etc/csharp/csharp-vs-doc-demo.gif" width="450" height="172"/>
</p>

## Before/After Insertion

You can customize document before or after the document string insertion.
There are total two hooks you can customize.

* `docstr-before-insert-hook`
* `docstr-after-insert-hook`

The usage of this is to customize the document string with set up. For instance,
some programming languages would add `@desc` before an actual description. Do
the following can implement this.

```el
(add-hook 'docstr-before-insert-hook (lambda (search-string) (insert "@desc ")))
```

Of course, I would recommend you add it locally so it is language specific
document style. Let's try apply to language `TypeScript` only within
`typescript-mode`.

```el
(defun my-typescript-mode-hook ()
  (add-hook 'docstr-before-insert-hook
            (lambda (search-string) (insert "@desc "))
            nil t))

(add-hook 'typescript-mode-hook #'my-typescript-mode-hook)
```

## Advance Implementation

You are able to customize document string by running `before`/`after` hooks.
The following are advance examples for document string in `C++`.

| Preprocessor                          | Enumerator                          |
|:--------------------------------------|:------------------------------------|
| <img src="./etc/c++/cpp-define.gif"/> | <img src="./etc/c++/cpp-enum.gif"/> |

| Structure                             | Class                                |
|:--------------------------------------|:-------------------------------------|
| <img src="./etc/c++/cpp-struct.gif"/> | <img src="./etc/c++/cpp-class.gif"/> |

## Document String

You are able to customize default document string by tweaking these variables.

### Varialbe Name

* `docstr-format-param` - Format string contain parameter data.
* `docstr-format-return`

### Type Name

* `docstr-show-type-name`

If you don't want the type name to be shown; then set `docstr-show-type-name`
to `nil` will make the trick. For instance, if you don't want the type name
to be shown in `java-mode`; do the following.

```el
(add-hook 'java-mode-hook (lambda () (setq-local docstr-show-type-name nil)))
```

* `docstr-default-typename`

The default type name is `typename` that is assigned by `docstr-default-typename`.
You can change this value as well if you don't like the default type name. This
variable is generally use for programming languages that aren't strong type.
Like `Python`, `JavaScript`, `PHP`, etc.

## Configure Faces

This package provides a way to customize the document string faces in a
consistent way yet this is just an optional choice. If you find want the
this feature, you can put the following code snippet into your configuration.

```el
(docstr-faces-apply)
```

There are three faces that you can customize for document string.

* `docstr-faces-tag-face` - Highlight the tag face; like `@param`, `@return`, etc.
* `docstr-faces-type-face` - Highlight the type name face.
* `docstr-faces-value-face` - Highlight the variable name face.

## Contribution

If you would like to contribute to this project, you may either
clone and make pull requests to this repository. Or you can
clone the project and establish your own branch of this tool.
Any methods are welcome!
