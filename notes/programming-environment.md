# Programming Environment

We will be using Common Lisp. To write code we need to setup an environment with a suitable editor. You can use any editor you like, but following features are necessary (in decreasing importance):

* highlighting of matching parentheses
* automatic indentation
* syntax highlighting
* electric parenthesization (you only type the opening paren, the other pair is inserted by the editor).
* ability to send selection of code or a single function to SBCL.

As long as you have these, you can use any editor and tool you like.

We suggest EMACS + SLIME. Here's a short guide for the setup.

## Setup for Windows
You can follow installation steps from this [video](https://www.youtube.com/watch?v=VnWVu8VVDbI&t=2s).
The steps are as follows:
1. Go to Emacs download [page](https://ftp.gnu.org/gnu/emacs/windows/emacs-27/)
2. Get emacs-27.2-x86_64-installer.exe (60M) (assuming you have a 64-bit system)
3. Follow installer instructions and finish installing Emacs
4. Download SBCL [here](http://prdownloads.sourceforge.net/sbcl/sbcl-2.2.2-x86-windows-binary.msi)
5. Follow installer instructions
6. Open a terminal window and type sbcl to test if your lisp is working.

You can create a home directory by providing a system variable (a folder path with an alias you can use system-wide). To do so:

7. start -> right click 'computer' -> click 'advanced system settings' -> find the 'environment variables' under 'advanced' -> create new home variable -> variable name HOME, and value is your desired home directory (it can be C:\home)

There is also another system variable called 'Path'. This enables you to run programs using terminal by calling their names. if you find it under system variables and double click it, you can see that there are multiple paths that are in the 'Path', sbcl is automatically added to it, such that you can simply type sbcl to run SBCL from command window. It would be nice to have an emacs version of that, so we'll add the directory where emacs executables are located.

8. Add 'C:\Program Files\Emacs\x86_64\bin' to the 'Path' list.
9. open a new terminal window and type emacs, you'll have your gui opened.

Now we need MELPA, a package repository for emacs. To be able to use it we need to have an .emacs file in our home directory, with requirements for melpa typed in.

In (vanilla) emacs there are two important keys: C (ctrl) and M (alt). To find (open) a file we use C-x C-f (hold ctrl and press x and f in order). Use C-g to quit if you want to cancel your command.

~ (tilde) symbol is also used for home. We'll find-file ~/.emacs (same with c:/home/.emacs)

10. Do C-x C-f and type ~/.emacs
11. Paste the following code into that file. See this [page](https://melpa.org/#/getting-started) for details.
```lisp
(require 'package)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(package-initialize)
```
Now we need the changes to get effective, so we can simply quit and restart emacs, or reload settings.

12. Quit Emacs by C-x C-c and restart. Or use M-x load file and type ~/.emacs
13. Type M-x package-install RET (return) slime RET. This should get you slime. For more info see this [page](https://slime.common-lisp.dev/doc/html/Installation.html).

We need to tell slime which lisp to use.

14. Find your .emacs file again, and add the following line.
```lisp
(setq inferior-lisp-program "sbcl")
```
15. Reload .emacs (M-x load-file ...).
16. M-x slime will get you started.

## Setup for MacOS
1. Install [Homebrew](https://brew.sh).
2. Install sbcl using brew. (See [here](https://formulae.brew.sh/formula/sbcl)).
3. For installing Emacs there are other options, but using Homebrew is ok. See [here](https://www.gnu.org/software/emacs/download.html).
4. Install slime. Follow instructions [here](https://slime.common-lisp.dev/doc/html/Installation.html#Installation).