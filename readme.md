# LaTeX Labeler - Simplify equation labeling in LaTeX

**LaTeX Labeler** is an Emacs Lisp package designed to streamline the
process of labeling equations in LaTeX documents. This package enables
numbering equations sequentially in a LaTeX file from top to bottom. The
main purpose of LaTeX Labeler is to synchronize equation labels in a
LaTeX file and a compiled document. You can avoid the hassle of manually
managing label naming. This is a simple yet powerful solution that saves
you time and eliminates labeling confusion.

**Note**: We would like to emphasize that this package is inspired by
[texlabel.el](https://www.emacswiki.org/emacs/texlabel.el), created by
tama.sh. While LaTeX Labeler offers similar functionality to
texlabel.el, it includes several enhanced and improved features.

# Features

-   **Automatic labeling**: LaTeX Labeler automatically generates and
    updates equation labels in your LaTeX documents.
-   **Reference updates**: When labels are updated, corresponding
    references are automatically updated.

# Important note

While LaTeX Labeler works seamlessly with
[AUCTeX](https://www.gnu.org/software/auctex/documentation.html) and
[YaTeX](https://www.yatex.org), it may encounter unexpected errors when
used with the built-in `latex-mode`, especially when labeling
equations within `subequations` environment. We recommend
using LaTeX Labeler with AUCTeX or YaTeX.

# Installation

1.  LaTeX Labeler is available on
    [MELPA](https://melpa.org/#/latex-labeler):

    `M-x` `package-install` `RET`
    `latex-labeler` `RET`.

2.  If you use AUCTeX, add the following code to your Emacs
    configuration file (e.g. `~/.emacs.d/init.el`):

    ``` elisp
    (with-eval-after-load 'latex (require 'latex-labeler))
    ```

    If you use YaTeX instead of AUCTeX, add the following code:

    ``` elisp
    (with-eval-after-load 'yatex (require 'latex-labeler))
    ```

# Usage

LaTeX Labeler provides three commands for labeling equations and
updating references.

-   `latex-labeler-update`: Update equation labels and
    references that match the predefined format in your LaTeX document.
-   `latex-labeler-update-force`: Forcefully update all
    labels and references, regardless of the format.
-   `latex-labeler-change-prefix-and-update`: Change the
    label prefix interactively and updates labels. When you change the
    prefix, LaTeX Labeler appends the necessary local variables
    configuration to your LaTeX file.

## Example

1.  Assume you have a LaTeX file shown as follows. The default label
    prefix is set to `eq:`.

    ![](https://github.com/X9hRRDys/latex-labeler/blob/screenshots/screenshots/demo.png)

2.  Use `M-x` `latex-labeler-update` command to
    update labels that match the \"eq: + number\" format. In the place
    where there are no labels, new labels are inserted in the \"eq: +
    number\" format. This will make the labels in your LaTeX file match
    the equation numbers in the PDF. Additionally, a reference using
    `eqref` is updated.

    ![](https://github.com/X9hRRDys/latex-labeler/blob/screenshots/screenshots/latex_labeler_update.png)

3.  Use `M-x` `latex-labeler-update-force` command
    to update all labels and references, regardless of the format.

    ![](https://github.com/X9hRRDys/latex-labeler/blob/screenshots/screenshots/latex_labeler_update_force.png)

4.  If you want to change the label prefix, use `M-x`
    `latex-labeler-change-prefix-and-update` command. You are
    prompted to enter a new prefix in the minibuffer. The labels are
    updated with the new prefix, and a local variable setting is added
    to the file.

    ![](https://github.com/X9hRRDys/latex-labeler/blob/screenshots/screenshots/latex_labeler_change_prefix_and_update.png)

## Label format with section counters

When working with a LaTeX file that includes sections, you can include
the section number in equation numbering in the compiled document. This
can be achieved by adding the following lines to the LaTeX preamble:

``` latex
\numberwithin{equation}{section}
```

For LaTeX files edited in Emacs, you can also append section numbers to
equation labels. To enable this feature, set the custom variable
`latex-labeler-with-section-counter` to `t`. If
you want to set it for a particular file, add the following lines at the
end of the LaTeX file:

``` latex
% local variables:
% latex-labeler-with-section-counter: t
% end:
```

If you prefer to set this configuration globally, add the following line
to your Emacs configuration file:

``` elisp
(setq latex-labeler-with-section-counter t)
```

After the `\appendix` command, appropriate letters are added as section counters instead of numbers.

![](https://github.com/X9hRRDys/latex-labeler/blob/screenshots/screenshots/label_with_section_numbers.png)

## Note for multi-file documents

Please note that LaTeX Labeler updates labels and references only within
the currently edited file. It does not affect labels and references in
other files included using `\input{...}` or
`\include{...}`. If you work with a large document spanning
multiple files, we recommend the following workflow:

1.  Divide your LaTeX document into files at units where equation labels
    are reset, such as chapters or sections.

2.  Use `latex-labeler-change-prefix-and-update` to set a
    label prefix for each individual file.

3.  For equations that are referenced in other files, assign unique
    labels that do not follow the prefix + number format. Use
    `latex-labeler-update` to update labels within the file.

4.  If you need to modify label names that do not follow the prefix +
    number format, use built-in commands in Emacs. For instance, when
    managing LaTeX files as a project, you can use
    `project-query-replace-regexp`, which is bound to `C-x` `p` `r`.
    This command replaces multiple files within the project.

# Examples of customization

-   To assign key bindings to the three main functions, you can set them
    up as follows. If you use AUCTeX,

    ``` elisp
    (with-eval-after-load 'latex
      (define-key LaTeX-mode-map (kbd "C-c t u") #'latex-labeler-update)
      (define-key LaTeX-mode-map (kbd "C-c t f") #'latex-labeler-update-force)
      (define-key LaTeX-mode-map (kbd "C-c t p") #'latex-labeler-change-prefix-and-update))
    ```

    If you use YaTeX,

    ``` elisp
    (with-eval-after-load 'yatex
      (define-key YaTeX-mode-map (kbd "C-c C-t u") #'latex-labeler-update)
      (define-key YaTeX-mode-map (kbd "C-c C-t f") #'latex-labeler-update-force)
      (define-key YaTeX-mode-map (kbd "C-c C-t p") #'latex-labeler-change-prefix-and-update))
    ```

-   You can add math environments to be labeled. If you want to label
    equations within a math environment `\begin{newenv}` ...
    `\end{newenv}`, configure it as follows:

    ``` elisp
    (add-to-list 'latex-labeler-math-envs "newenv" t)
    ```

    Replace \"newenv\" with the actual name of the environment you want
    to label.

-   If you want to achieve line breaks before labels and apply
    indentation, configure it as follows:

    ``` elisp
    (setq latex-labeler-string-before-label "\n")
    (setq latex-labeler-label-with-indent t)
    ```

    ![](https://github.com/X9hRRDys/latex-labeler/blob/screenshots/screenshots/line_breaks.png)

-   If you prefer nested equation labels in the subequations environment
    to be in the format `eq:1-1`, `eq:1-2`,...
    instead of `eq:1a`, `eq:1b`,..., configure it
    as follows:

    ``` elisp
    (setq latex-labeler-initial-subcounter 1)
    (setq latex-labeler-subformat-separator "-")
    ```

    It is important to set `(setq latex-labeler-subformat-separator
     "-")`. Without this, you cannot distinguish whether, for
    example, `\label{eq:31}` is a label for the first
    subequation of equation 3 or the label for the 31st equation.

    ![](https://github.com/X9hRRDys/latex-labeler/blob/screenshots/screenshots/subequations.png)

-   If you want to use labels like `foo1`, `foo2`,
    ... instead of `eq:1`, `eq:2`, ... for a
    specific file, add the following to the bottom of the LaTeX file:

    ``` latex
    % local variables:
    % latex-labeler-prefix: "foo"
    % latex-labeler-prefix-separator: ""
    % end:
    ```

# Custom variables

-   `latex-labeler-math-envs`: A list of math environments
    that should be labeled. Default is
    `'("align" "equation" "eqnarray" "gather"
     "multline" "subequations" "alignat" "flalign")` .

-   `latex-labeler-nonumber-at-linebreaks`: Math environments
    where line breaks occur without numbering. Default is `'("multline"
     "subequations")` .

-   `latex-labeler-refs`: A list of reference command types.
    Default is `'("eqref" "ref" "pageref")`.

-   `latex-labeler-commands-containing-linebreaks`: List of
    commands that can have line breaks `\\`. Line breaks in
    the commands will be ignored in the search. Default is
    `'("substack")`

-   `latex-labeler-initial-equation-number`: The initial
    equation number. Default value is `1`.

-   `latex-labeler-initial-subcounter`: The initial
    subcounter for subequations environments. Default is
    `"a"`.

-   `latex-labeler-prefix`: The prefix for generated labels.
    Default is `"eq"`.

-   `latex-labeler-prefix-separator`: The separator between
    the prefix and counter. Default is `":"`.

-   `latex-labeler-subformat-separator`: The separator
    between subformat and counter. Default is an empty string
    `""`.

-   `latex-labeler-string-before-label`: A string inserted
    before the label. Default is an empty space `" "`.

-   `latex-labeler-string-after-label`: A string inserted
    after the label. Default is an empty string `""`.

-   `latex-labeler-label-with-indent`: If `t`,
    perform indentation after labeling. Default is `nil`.

-   `latex-labeler-preserve-local-prefix`: If `t`,
    the local prefix setting is added when updating. Default is
    `t`.

-   `latex-labeler-update-reftex`: Update RefTeX after
    labeling if `t` and reftex-mode is enabled. Default is
    `nil`.

-   `latex-labeler-with-section-counter`: If the value is t,
    include a section counter in the label format. Default is
    `nil`.

-   `latex-labeler-with-appendix-letter`: If `t`,
    include an appendix letter in the label format. It is assumed that
    each appendix is separated by a section. When the value of
    `latex-labeler-with-section-counter` is `t`,
    each label in the appendix sections is appended with the appendix
    letter, regardless of the value of
    `latex-labeler-with-appendix-letter`. Default is
    `nil`.

-   `latex-labeler-initial-section-counter`: Initial section
    counter. Default is `1`.
