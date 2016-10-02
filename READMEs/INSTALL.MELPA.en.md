# How to install DDSKK using MELPA

On Decenmber 2014, DDSKK joined to MELPA repository and DDSKK is possible to be installed
on GNU Emacs with `package.el`.

 * MELPA: Milkypostman's Emacs Lisp Package Archive (https://melpa.org/)

 * package.el is bundled with Emacs 24 or later, but it’s not bound to Emacs 23 and paclage.el is an external package;
 see http://wikemacs.org/wiki/Package.el

## 1. Setting for package.el

To use MELPA add following lines to your `~/.emacs.d/init.el`

```
(when (require 'package nil t)
  (add-to-list 'package-archives
    '("melpa" . "https://melpa.org/packages/") t))
```

If you use Emacs 23, add following line.

```
(package-initialize)
```

## 2. Installing DDSKK

Then, submit `M-x list-packages` to install DDSKK using MELPA, and Package Menu is indicated. 

```
-------------------- Package Menu mode --------------------
  Package   Version      Status Archive Description
  darkroom  0.1          new    gnu     Remove visual ...
* ddskk     20141227.828 new    melpa   Simple Kana to ...
  form-feed 20141228.... new    melpa   Display ^L glyphs as ...
-------------------- Package Menu mode --------------------
```

Place the cursor on ddskk and type `i` and the `I` will be marked the beggining of the line.

Again, type `x`, Emacs askes in the mini buffer if this packege will be installed.

```
------ Minibuffer -------
Install package `ddskk-20141227.828'? (yes or no)
------ Minibuffer -------
```


To build and install the ddskk package and the involved packages, ccc and cdb, type `y e s`.

Package.el installs also `skk.texi`, so info manual can be read with typing `M-x info`.  

** A limit to install the package **

The directories such like `doc/` or `etc/` included in distribution of DDSKK will not be installed by MELPA 
except `skk.info` and `skk.xpm`.

The files contained install package are described on recipe of MELPA.

Refer the detailed recipe on https://github.com/milkypostman/melpa/blob/master/recipes/ddskk

## 3. Setting of DDSKK

`leim-list.el` and `skk-setup.el` are absent, different from Makefile to install, therefor
you should set a key bind on `~/.emacs.d/init.el` as follows;

```
(global-set-key (kbd "C-x C-j") 'skk-mode)
```

After running skk-mode, Emacs read `~/.skk`, skk initializing file , so 
set such like dictionary place.

After installed DDSKK, put `M-x skk-get` and the dictionaries are automatically downloaded.
The dictionary files are in the directory indicated by `skk-get-jisyo-directory`, which default is `~/.emacs.d/skk-get-jisyo`.

## 4. Upgrading DDSKK

MELPA seems to check the updating of github repository of skk-dev/ddskk.

You can check updating of DDSKK on MELPA by typing `U` on Package Menu mode.

If there exist the updates, the target packages are indicated as follows.

```
-------------------- Echo Area --------------------
N package marked for upgrading.
-------------------- Echo Area --------------------
```

`I` is marked and then updating will start after type `x`.

Finaly, you are asked if delete old package, answer it.
