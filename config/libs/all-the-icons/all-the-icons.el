; -*- lexical-binding:nil -*-

; Currently, this package provides an interface to the following Icon Fonts

; - Atom File Icons,       found at https://atom.io/packages/file-icons
; - FontAwesome Icons,     found at http://fontawesome.io/
; - GitHub Octicons,       found at http://octicons.github.com
; - Material Design Icons, found at http://google.github.io/material-design-icons/
; - Weather Icons,         found at https://erikflowers.github.io/weather-icons/
; - AllTheIcons,           a custom Icon Font maintained as part of this package

; The simplest usage for this package is to use the following functions;

;   `all-the-icons-icon-for-buffer'
;   `all-the-icons-icon-for-dir'
;   `all-the-icons-icon-for-file'
;   `all-the-icons-icon-for-mode'
;   `all-the-icons-icon-for-url'

;   #("some-icon" 0 1 (display (raise -0.24) face (:family "dev-icons" :height 1.08 :foreground "#FFD446")))

; You can also insert icons directly using the individual icon family
; functions

;   `all-the-icons-alltheicon'     // Custom font with fewest icons
;   `all-the-icons-devicon'        // Developer Icons
;   `all-the-icons-faicon'         // Font Awesome Icons
;   `all-the-icons-fileicon'       // File Icons from the Atom File Icons package
;   `all-the-icons-octicon'        // GitHub Octicons
;   `all-the-icons-material'       // Material Design Icons
;   `all-the-icons-wicon'          // Weather Icons

; You can call these functions with the icon name you want to insert, e.g.

;   (all-the-icons-octicon "file-binary")  // GitHub Octicon for Binary File
;   (all-the-icons-faicon  "cogs")         // FontAwesome icon for cogs
;   (all-the-icons-wicon   "tornado")      // Weather Icon for tornado

; A list of all the icon names for the different font families can be
; found in the data directory, or by inspecting the alist variables.
; All the alist variables are prefixed with `all-the-icons-data/'

; TODO
; There absolutely has to be a way to set :height and 'display (raise) props
; programmatically - there are far too many icons to do this by hand.
; For now that's what I know:
; The best info I can get from lisp is from font-info, for example:
;
;               Material Icons  JBM_NL_Full_1,15  JetBrains Mono NL
; size          20              20                20
; height        20              32                26
; max-width     20              38                38
; ascent        20              23                20
; descent       0               9                 6
; space-width   20              12                12
; average-width 20              12                12
;
; but it's still not enough to calculate props, because, for example:
;
; not really important widths:
; (get_string_pixel_width (propertize " " 'face '(:family "Material Icons")))    = 21
; (get_string_pixel_width (propertize " " 'face '(:family "JBM_NL_Full_1,15")))  = 13
; (get_string_pixel_width (propertize " " 'face '(:family "JetBrains Mono NL"))) = 13
;
; important heights:
; (get_string_pixel_height (propertize " " 'face '(:family "Material Icons")))    = 21
; (get_string_pixel_height (propertize " " 'face '(:family "JBM_NL_Full_1,15")))  = 35
; (get_string_pixel_height (propertize " " 'face '(:family "JetBrains Mono NL"))) = 27
;
; indicate that there is still some "hidden" property.
; Raise doesn't seem to make a difference in line height when baseline descent = 0,
; which is the case for Material Icons.
; Maybe it's normal ascent / icon ascent?
; Ok so for example assuming default font JBM_NL_Full_1,15,
; (all-the-icons-material "lock_outline" :height (/ 23.0 20) :v-adjust (- (/ 9.0 23)))
; gives somewhat ok results.
; 23 is desired ascent - max that can be achieved w/o making line taller,
; 20 is icon's ascent and 9 is default font's descent.
; So we make icon bigger by sizing it up to have ascent of the default font,
; and raising negatively to have descent of the default font.
;
; And is the best we can do right now at "maximizing" icon to some font.
;
; Obviously that's not ideal - if we want full control over the icon in line,
; we should be able to make it ascent + descent tall and raise negatively by
; descent, not make it ascent tall and raise negatively by descent like now,
; because there is descent tall blank space above the icon, and we can't
; do anything about this (in lisp in current emacs version).
;
; So let's change emacs C display code logic to take 'display '(raise ...)
; property into account when calculating row height!

(defconst all-the-icons-color-icons t
"Whether or not to include a foreground colour when formatting the icon.
Default: t.")

(defconst all-the-icons-scale-factor 1.0
"The base Scale Factor for the `height' face property of an icon.
Default: 1.2.")

(defconst all-the-icons-default-adjust 0.0
"The default adjustment to be made to the `raise' display property of an icon.
This is multiplied by all-the-icons-scale-factor (for some reason).
This is also completely overriden by icons that specify their v-adjust.
Default: -0.2.")

(defconst all-the-icons-align-width (* (default-font-width) 3)
"Number of pixels that `all-the-icons-align' should make icon wide by default.
It should be >= as the usual icon width.")

(defconst all-the-icons-fonts-subdirectory nil
"The subdirectory within the system fonts folder where the icons are installed.
Default: nil.")

(defconst allTheIcons::DEFAULT_FILE_ICON
	'(all-the-icons-faicon "file-o" :v-adjust 0.0 :face all-the-icons-dsilver))

(defconst allTheIcons::DEFAULT_MODE_ICON allTheIcons::DEFAULT_FILE_ICON)

(defconst allTheIcons::DEFAULT_URL_ICON '(all-the-icons-faicon "globe"))

; Define faces.
(let ((define_face_all_foreground
		(lambda (face foreground)
			(define_face face `((t :foreground ,foreground)))))
	  (define_face_dark_and_light_foreground
		(lambda (face dark_foreground light_foreground)
			(define_face face
				`((((background dark)) :foreground ,dark_foreground)
				  (((background light)) :foreground ,light_foreground))))))

	; red
	(funcall define_face_all_foreground 'all-the-icons-red "#AC4142")
	(funcall define_face_all_foreground 'all-the-icons-lred "#EB595A")
	(funcall define_face_all_foreground 'all-the-icons-dred "#843031")
	(funcall define_face_dark_and_light_foreground 'all-the-icons-red-alt "#ce5643" "#843031")

	; green
	(funcall define_face_all_foreground 'all-the-icons-green "#90A959")
	(funcall define_face_dark_and_light_foreground 'all-the-icons-lgreen "#C6E87A" "#3D6837")
	(funcall define_face_all_foreground 'all-the-icons-dgreen "#6D8143")

	; yellow
	(funcall define_face_dark_and_light_foreground 'all-the-icons-yellow "#FFD446" "#FFCC0E")
	(funcall define_face_dark_and_light_foreground 'all-the-icons-lyellow "#FFC16D" "#FF9300")
	(funcall define_face_all_foreground 'all-the-icons-dyellow "#B48D56")
	(funcall define_face_all_foreground 'all-the-icons-yellow1 "#b88838") ; clion h file

	; blue
	(funcall define_face_all_foreground 'all-the-icons-blue "#6A9FB5")
	(funcall define_face_all_foreground 'all-the-icons-blue-alt "#2188b6")
	(funcall define_face_dark_and_light_foreground 'all-the-icons-lblue "#8FD7F4" "#677174")
	(funcall define_face_all_foreground 'all-the-icons-dblue "#446674")

	; maroon
	(funcall define_face_all_foreground 'all-the-icons-maroon "#8F5536")
	(funcall define_face_all_foreground 'all-the-icons-lmaroon "#CE7A4E")
	(funcall define_face_all_foreground 'all-the-icons-dmaroon "#72584B")

	; purple
	(funcall define_face_dark_and_light_foreground 'all-the-icons-purple "#AA759F" "#68295B")
	(funcall define_face_all_foreground 'all-the-icons-purple-alt "#5D54E1")
	(funcall define_face_all_foreground 'all-the-icons-lpurple "#E69DD6")
	(funcall define_face_all_foreground 'all-the-icons-dpurple "#694863")
	(funcall define_face_all_foreground 'all-the-icons-purple1 "#8e79bb") ; clion c file
	(funcall define_face_all_foreground 'all-the-icons-purple2 "#755cac")

	; orange
	(funcall define_face_all_foreground 'all-the-icons-orange "#D4843E")
	(funcall define_face_all_foreground 'all-the-icons-lorange "#FFA500")
	(funcall define_face_all_foreground 'all-the-icons-dorange "#915B2D")

	; cyan
	(funcall define_face_all_foreground 'all-the-icons-cyan "#75B5AA")
	(funcall define_face_dark_and_light_foreground 'all-the-icons-cyan-alt "#61dafb" "#0595bd")
	(funcall define_face_dark_and_light_foreground 'all-the-icons-lcyan "#A5FDEC" "#2C7D6E")
	(funcall define_face_all_foreground 'all-the-icons-dcyan "#48746D")

	; pink
	(funcall define_face_dark_and_light_foreground 'all-the-icons-pink "#F2B4B8" "#FC505B")
	(funcall define_face_dark_and_light_foreground 'all-the-icons-lpink "#FFBDC1" "#FF505B")
	(funcall define_face_dark_and_light_foreground 'all-the-icons-dpink "#B18286" "#7E5D5F")

	; silver
	(funcall define_face_all_foreground 'all-the-icons-silver "#716E68")
	(funcall define_face_dark_and_light_foreground 'all-the-icons-lsilver "#B9B6AA" "#7F7869")
	(funcall define_face_all_foreground 'all-the-icons-dsilver "#838484"))

(require 'data-alltheicons  "./data/data-alltheicons")
(require 'data-faicons      "./data/data-faicons")
(require 'data-fileicons    "./data/data-fileicons")
(require 'data-octicons     "./data/data-octicons")
(require 'data-weathericons "./data/data-weathericons")
(require 'data-material     "./data/data-material")

(defvar allTheIcons::iconAlist::extension
	'(
		("fish"			all-the-icons-alltheicon	"terminal"			:face all-the-icons-lpink)
		("zsh"			all-the-icons-alltheicon	"terminal"			:face all-the-icons-lcyan)
		("sh"			all-the-icons-alltheicon	"terminal"			:face all-the-icons-purple)
		; Meta
		("tags"			all-the-icons-octicon		"tag"				:face all-the-icons-blue		:v-adjust 0.0)
		("log"			all-the-icons-octicon		"bug"				:face all-the-icons-maroon		:v-adjust 0.0)
		; Config
		("node"			all-the-icons-alltheicon	"nodejs"			:face all-the-icons-green)
		("babelrc"		all-the-icons-fileicon		"babel"				:face all-the-icons-yellow)
		("bashrc"		all-the-icons-alltheicon	"script"			:face all-the-icons-dpink		:height 0.9)
		("bowerrc"		all-the-icons-alltheicon	"bower"				:face all-the-icons-silver		:v-adjust 0.0)
		("cr"			all-the-icons-fileicon		"crystal"			:face all-the-icons-yellow		:v-adjust 0.0)
		("ecr"			all-the-icons-fileicon		"crystal"			:face all-the-icons-yellow		:v-adjust 0.0)
		("ini"			all-the-icons-octicon		"settings"			:face all-the-icons-yellow		:v-adjust 0.0)
		("eslintignore"	all-the-icons-fileicon		"eslint"			:face all-the-icons-purple		:height 0.9)
		("eslint"		all-the-icons-fileicon		"eslint"			:face all-the-icons-lpurple		:height 0.9)
		("git"			all-the-icons-alltheicon	"git"				:face all-the-icons-lred)
		("mk"			all-the-icons-fileicon		"gnu"				:face all-the-icons-dorange)
		("cmake"		all-the-icons-fileicon		"cmake"				:face all-the-icons-red)
		("dockerignore"	all-the-icons-fileicon		"dockerfile"		:face all-the-icons-dblue		:height 1.2)
		("xml"			all-the-icons-faicon		"file-code-o"		:face all-the-icons-lorange		:height 0.95)
		("json"			all-the-icons-octicon		"settings"			:face all-the-icons-yellow		:v-adjust 0.0)
		("cson"			all-the-icons-octicon		"settings"			:face all-the-icons-yellow		:v-adjust 0.0)
		("yml"			all-the-icons-octicon		"settings"			:face all-the-icons-dyellow		:v-adjust 0.0)
		("yaml"			all-the-icons-octicon		"settings"			:face all-the-icons-dyellow		:v-adjust 0.0)
		; ?
		("pkg"			all-the-icons-octicon		"package"			:face all-the-icons-dsilver		:v-adjust 0.0)
		("rpm"			all-the-icons-octicon		"package"			:face all-the-icons-dsilver		:v-adjust 0.0)
		("pkgbuild"		all-the-icons-octicon		"package"			:face all-the-icons-dsilver		:v-adjust 0.0)
		("elc"			all-the-icons-octicon		"file-binary"		:face all-the-icons-dsilver		:v-adjust 0.0)
		("gz"			all-the-icons-octicon		"file-binary"		:face all-the-icons-lmaroon		:v-adjust 0.0)
		("zip"			all-the-icons-octicon		"file-zip"			:face all-the-icons-lmaroon		:v-adjust 0.0)
		("7z"			all-the-icons-octicon		"file-zip"			:face all-the-icons-lmaroon		:v-adjust 0.0)
		("dat"			all-the-icons-faicon		"bar-chart"			:face all-the-icons-cyan		:height 0.9)
		("dmg"			all-the-icons-octicon		"tools"				:face all-the-icons-lsilver		:v-adjust 0.0)
		("dll"			all-the-icons-faicon		"cogs"				:face all-the-icons-silver)
		("ds_store"		all-the-icons-faicon		"cogs"				:face all-the-icons-silver)
		; Source Codes
		("scpt"			all-the-icons-fileicon		"apple"				:face all-the-icons-pink)
		("aup"			all-the-icons-fileicon		"audacity"			:face all-the-icons-yellow)
		("elm"			all-the-icons-fileicon		"elm"				:face all-the-icons-blue)
		("erl"			all-the-icons-alltheicon	"erlang"			:face all-the-icons-red			:v-adjust -0.1 :height 0.9)
		("hrl"			all-the-icons-alltheicon	"erlang"			:face all-the-icons-dred		:v-adjust -0.1 :height 0.9)
		("eex"			all-the-icons-alltheicon	"elixir"			:face all-the-icons-lorange		:v-adjust -0.1 :height 0.9)
		("leex"			all-the-icons-alltheicon	"elixir"			:face all-the-icons-lorange		:v-adjust -0.1 :height 0.9)
		("heex"			all-the-icons-alltheicon	"elixir"			:face all-the-icons-lorange		:v-adjust -0.1 :height 0.9)
		("ex"			all-the-icons-alltheicon	"elixir"			:face all-the-icons-lpurple		:v-adjust -0.1 :height 0.9)
		("exs"			all-the-icons-alltheicon	"elixir"			:face all-the-icons-lred		:v-adjust -0.1 :height 0.9)
		("java"			all-the-icons-alltheicon	"java"				:face all-the-icons-purple)
		("gradle"		all-the-icons-fileicon		"gradle"			:face all-the-icons-silver)
		("ebuild"		all-the-icons-fileicon		"gentoo"			:face all-the-icons-cyan)
		("eclass"		all-the-icons-fileicon		"gentoo"			:face all-the-icons-blue)
		("go"			all-the-icons-fileicon		"go"				:face all-the-icons-blue)
		("jl"			all-the-icons-fileicon		"julia"				:face all-the-icons-purple		:v-adjust 0.0)
		("magik"		all-the-icons-faicon		"magic"				:face all-the-icons-blue)
		("matlab"		all-the-icons-fileicon		"matlab"			:face all-the-icons-orange)
		("nix"			all-the-icons-fileicon		"nix"				:face all-the-icons-blue)
		("pl"			all-the-icons-alltheicon	"perl"				:face all-the-icons-lorange)
		("pm"			all-the-icons-alltheicon	"perl"				:face all-the-icons-lorange)
		("pl6"			all-the-icons-fileicon		"raku"				:face all-the-icons-cyan)
		("pm6"			all-the-icons-fileicon		"raku"				:face all-the-icons-pink)
		("pod"			all-the-icons-alltheicon	"perldocs"			:face all-the-icons-lgreen		:height 1.2)
		("php"			all-the-icons-fileicon		"php"				:face all-the-icons-lsilver)
		("pony"			all-the-icons-fileicon		"pony"				:face all-the-icons-maroon)
		("ps1"			all-the-icons-fileicon		"powershell"		:face all-the-icons-blue)
		("pro"			all-the-icons-alltheicon	"prolog"			:face all-the-icons-lmaroon		:height 1.1)
		("proog"		all-the-icons-alltheicon	"prolog"			:face all-the-icons-lmaroon		:height 1.1)
		("py"			all-the-icons-alltheicon	"python"			:face all-the-icons-dblue)
		("idr"			all-the-icons-fileicon		"idris"				:face all-the-icons-red)
		("ipynb"		all-the-icons-fileicon		"jupyter"			:face all-the-icons-dorange)
		("gem"			all-the-icons-alltheicon	"ruby-alt"			:face all-the-icons-red)
		("raku"			all-the-icons-fileicon		"raku"				:face all-the-icons-cyan)
		("rakumod"		all-the-icons-fileicon		"raku"				:face all-the-icons-pink)
		("rb"			all-the-icons-octicon		"ruby"				:face all-the-icons-lred		:v-adjust 0.0)
		("rs"			all-the-icons-alltheicon	"rust"				:face all-the-icons-maroon		:height 1.2)
		("rlib"			all-the-icons-alltheicon	"rust"				:face all-the-icons-dmaroon		:height 1.2)
		("r"			all-the-icons-fileicon		"R"					:face all-the-icons-lblue)
		("rd"			all-the-icons-fileicon		"R"					:face all-the-icons-lblue)
		("rdx"			all-the-icons-fileicon		"R"					:face all-the-icons-lblue)
		("rsx"			all-the-icons-fileicon		"R"					:face all-the-icons-lblue)
		("beancount"	all-the-icons-faicon		"credit-card"		:face all-the-icons-lgreen)
		("ledger"		all-the-icons-faicon		"credit-card"		:face all-the-icons-lgreen)
		("svelte"		all-the-icons-fileicon		"svelte"			:face all-the-icons-red			:v-adjust 0.0)
		("gql"			all-the-icons-fileicon		"graphql"			:face all-the-icons-dpink)
		("graphql"		all-the-icons-fileicon		"graphql"			:face all-the-icons-dpink)
		("c"			all-the-icons-alltheicon	"c-line"			:face all-the-icons-blue-alt	:v-adjust -0.05 :height 1.2)
		("h"			all-the-icons-alltheicon	"c-line"			:face all-the-icons-yellow1		:v-adjust -0.05 :height 1.2)
		("m"			all-the-icons-fileicon		"apple"												:v-adjust 0.0)
		("mm"			all-the-icons-fileicon		"apple"												:v-adjust 0.0)

		("cc"			all-the-icons-alltheicon	"cplusplus-line"	:face all-the-icons-blue-alt	:v-adjust -0.05 :height 1.2)
		("cpp"			all-the-icons-alltheicon	"cplusplus-line"	:face all-the-icons-blue-alt	:v-adjust -0.05 :height 1.2)
		("cxx"			all-the-icons-alltheicon	"cplusplus-line"	:face all-the-icons-blue-alt	:v-adjust -0.05 :height 1.2)
		("hh"			all-the-icons-alltheicon	"cplusplus-line"	:face all-the-icons-purple		:v-adjust -0.05 :height 1.2)
		("hpp"			all-the-icons-alltheicon	"cplusplus-line"	:face all-the-icons-purple		:v-adjust -0.05 :height 1.2)
		("hxx"			all-the-icons-alltheicon	"cplusplus-line"	:face all-the-icons-purple		:v-adjust -0.05 :height 1.2)
		; Lisps
		("cl"			all-the-icons-fileicon		"clisp"				:face all-the-icons-lorange)
		("l"			all-the-icons-fileicon		"lisp"				:face all-the-icons-orange)
		("lisp"			all-the-icons-fileicon		"lisp"				:face all-the-icons-orange)
		("hy"			all-the-icons-fileicon		"hy"				:face all-the-icons-blue)
		("el"			all-the-icons-fileicon		"elisp"				:face all-the-icons-purple		:v-adjust -0.15 :height 1.1)
		("clj"			all-the-icons-alltheicon	"clojure-line"		:face all-the-icons-blue		:v-adjust 0.0)
		("cljc"			all-the-icons-alltheicon	"clojure-line"		:face all-the-icons-blue		:v-adjust 0.0)
		("cljs"			all-the-icons-fileicon		"cljs"				:face all-the-icons-dblue		:v-adjust 0.0)
		("coffee"		all-the-icons-alltheicon	"coffeescript"		:face all-the-icons-maroon)
		("iced"			all-the-icons-alltheicon	"coffeescript"		:face all-the-icons-lmaroon)
		("dart"			all-the-icons-fileicon		"dart"				:face all-the-icons-blue		:v-adjust 0.0)
		("rkt"			all-the-icons-fileicon		"racket"			:face all-the-icons-red			:height 1.2)
		("scrbl"		all-the-icons-fileicon		"racket"			:face all-the-icons-blue		:height 1.2)
		; Stylesheeting
		("css"			all-the-icons-alltheicon	"css3"				:face all-the-icons-yellow)
		("scss"			all-the-icons-alltheicon	"sass"				:face all-the-icons-pink)
		("sass"			all-the-icons-alltheicon	"sass"				:face all-the-icons-dpink)
		("less"			all-the-icons-alltheicon	"less"				:face all-the-icons-dyellow		:height 0.8)
		("postcss"		all-the-icons-fileicon		"postcss"			:face all-the-icons-dred)
		("pcss"			all-the-icons-fileicon		"postcss"			:face all-the-icons-dred)
		("sss"			all-the-icons-fileicon		"postcss"			:face all-the-icons-dred)
		("styl"			all-the-icons-alltheicon	"stylus"			:face all-the-icons-lgreen)
		("csv"			all-the-icons-octicon		"graph"				:face all-the-icons-dblue		:v-adjust 0.0)
		; haskell
		("hs"			all-the-icons-alltheicon	"haskell"			:face all-the-icons-red)
		("chs"			all-the-icons-alltheicon	"haskell"			:face all-the-icons-red)
		("lhs"			all-the-icons-alltheicon	"haskell"			:face all-the-icons-red)
		("hsc"			all-the-icons-alltheicon	"haskell"			:face all-the-icons-red)
		; Web modes
		("inky-haml"	all-the-icons-fileicon		"haml"				:face all-the-icons-lyellow)
		("haml"			all-the-icons-fileicon		"haml"				:face all-the-icons-lyellow)
		("htm"			all-the-icons-alltheicon	"html5"				:face all-the-icons-orange)
		("html"			all-the-icons-alltheicon	"html5"				:face all-the-icons-orange)
		("inky-er"		all-the-icons-alltheicon	"html5"				:face all-the-icons-lred)
		("inky-erb"		all-the-icons-alltheicon	"html5"				:face all-the-icons-lred)
		("erb"			all-the-icons-alltheicon	"html5"				:face all-the-icons-lred)
		("hbs"			all-the-icons-fileicon		"moustache"			:face all-the-icons-green)
		("inky-slim"	all-the-icons-octicon		"dashboard"			:face all-the-icons-yellow		:v-adjust 0.0)
		("slim"			all-the-icons-octicon		"dashboard"			:face all-the-icons-yellow		:v-adjust 0.0)
		("jade"			all-the-icons-fileicon		"jade"				:face all-the-icons-red)
		("pug"			all-the-icons-fileicon		"pug-alt"			:face all-the-icons-red)
		; Javascript
		("d3js"			all-the-icons-alltheicon	"d3"				:face all-the-icons-lgreen		:height 0.8)
		("re"			all-the-icons-fileicon		"reason"			:face all-the-icons-red-alt)
		("rei"			all-the-icons-fileicon		"reason"			:face all-the-icons-dred)
		("ml"			all-the-icons-fileicon		"ocaml"				:face all-the-icons-lpink)
		("mli"			all-the-icons-fileicon		"ocaml"				:face all-the-icons-dpink)
		("react"		all-the-icons-alltheicon	"react"				:face all-the-icons-lblue		:height 1.1)
		("ts"			all-the-icons-fileicon		"typescript"		:face all-the-icons-blue-alt	:v-adjust -0.1)
		("js"			all-the-icons-alltheicon	"javascript"		:face all-the-icons-yellow		:v-adjust 0.0)
		("mjs"			all-the-icons-alltheicon	"javascript"		:face all-the-icons-yellow		:v-adjust 0.0)
		("es"			all-the-icons-alltheicon	"javascript"		:face all-the-icons-yellow		:v-adjust 0.0)
		("jsx"			all-the-icons-fileicon		"jsx-2"				:face all-the-icons-cyan-alt	:v-adjust -0.1)
		("tsx"			all-the-icons-fileicon		"tsx"				:face all-the-icons-cyan-alt	:v-adjust -0.1)
		("njs"			all-the-icons-alltheicon	"nodejs"			:face all-the-icons-lgreen		:height 1.2)
		("vue"			all-the-icons-fileicon		"vue"				:face all-the-icons-lgreen)
		("wasm"			all-the-icons-fileicon		"wasm"				:face all-the-icons-purple-alt)
		("wat"			all-the-icons-fileicon		"wasm"				:face all-the-icons-purple-alt)

		("sbt"			all-the-icons-fileicon		"sbt"				:face all-the-icons-red)
		("scala"		all-the-icons-alltheicon	"scala"				:face all-the-icons-red)
		("scm"			all-the-icons-fileicon		"scheme"			:face all-the-icons-red			:height 1.2)
		("swift"		all-the-icons-alltheicon	"swift"				:face all-the-icons-green		:v-adjust -0.1)

		("tcl"			all-the-icons-fileicon		"tcl"				:face all-the-icons-dred)

		("tf"			all-the-icons-fileicon		"terraform"			:face all-the-icons-purple-alt)
		("tfvars"		all-the-icons-fileicon		"terraform"			:face all-the-icons-purple-alt)
		("tfstate"		all-the-icons-fileicon		"terraform"			:face all-the-icons-purple-alt)

		("asm"			all-the-icons-fileicon		"assembly"			:face all-the-icons-blue)
		; Verilog(-AMS) and SystemVerilog(-AMS)
		("v"			all-the-icons-fileicon		"verilog"			:face all-the-icons-red			:v-adjust -0.2)
		("vams"			all-the-icons-fileicon		"verilog"			:face all-the-icons-red			:v-adjust -0.2)
		("sv"			all-the-icons-fileicon		"verilog"			:face all-the-icons-red			:v-adjust -0.2)
		("sva"			all-the-icons-fileicon		"verilog"			:face all-the-icons-red			:v-adjust -0.2)
		("svh"			all-the-icons-fileicon		"verilog"			:face all-the-icons-red			:v-adjust -0.2)
		("svams"		all-the-icons-fileicon		"verilog"			:face all-the-icons-red			:v-adjust -0.2)
		; VHDL(-AMS)
		("vhd"			all-the-icons-fileicon		"vhdl"				:face all-the-icons-blue)
		("vhdl"			all-the-icons-fileicon		"vhdl"				:face all-the-icons-blue)
		("vhms"			all-the-icons-fileicon		"vhdl"				:face all-the-icons-blue)
		; Cabal
		("cabal"		all-the-icons-fileicon		"cabal"				:face all-the-icons-lblue)
		; Kotlin
		("kt"			all-the-icons-fileicon		"kotlin"			:face all-the-icons-orange)
		("kts"			all-the-icons-fileicon		"kotlin"			:face all-the-icons-orange)
		; Nimrod
		("nim"			all-the-icons-fileicon		"nimrod"			:face all-the-icons-yellow)
		("nims"			all-the-icons-fileicon		"nimrod"			:face all-the-icons-yellow)
		; SQL
		("sql"			all-the-icons-octicon		"database"			:face all-the-icons-silver)
		; Styles
		("styles"		all-the-icons-material		"style"				:face all-the-icons-red)
		; Lua
		("lua"			all-the-icons-fileicon		"lua"				:face all-the-icons-dblue)
		; ASCII doc
		("adoc"			all-the-icons-fileicon		"asciidoc"			:face all-the-icons-lblue)
		("asciidoc"		all-the-icons-fileicon		"asciidoc"			:face all-the-icons-lblue)
		; Puppet
		("pp"			all-the-icons-fileicon		"puppet"			:face all-the-icons-yellow)
		; Jinja
		("j2"			all-the-icons-fileicon		"jinja"				:face all-the-icons-silver)
		("jinja2"		all-the-icons-fileicon		"jinja"				:face all-the-icons-silver)
		; Docker
		("dockerfile"	all-the-icons-fileicon		"dockerfile"		:face all-the-icons-cyan)
		; Vagrant
		("vagrantfile"	all-the-icons-fileicon		"vagrant"			:face all-the-icons-blue)
		; GLSL
		("glsl"			all-the-icons-fileicon		"vertex-shader"		:face all-the-icons-blue)
		("vert"			all-the-icons-fileicon		"vertex-shader"		:face all-the-icons-blue)
		("tesc"			all-the-icons-fileicon		"vertex-shader"		:face all-the-icons-purple)
		("tese"			all-the-icons-fileicon		"vertex-shader"		:face all-the-icons-dpurple)
		("geom"			all-the-icons-fileicon		"vertex-shader"		:face all-the-icons-green)
		("frag"			all-the-icons-fileicon		"vertex-shader"		:face all-the-icons-red)
		("comp"			all-the-icons-fileicon		"vertex-shader"		:face all-the-icons-dblue)
		; CUDA
		("cu"			all-the-icons-fileicon		"nvidia"			:face all-the-icons-green)
		("cuh"			all-the-icons-fileicon		"nvidia"			:face all-the-icons-green)
		; Fortran
		("f90"			all-the-icons-fileicon		"fortran"			:face all-the-icons-purple)
		; C#
		("cs"			all-the-icons-alltheicon	"csharp-line"		:face all-the-icons-dblue)
		("csx"			all-the-icons-alltheicon	"csharp-line"		:face all-the-icons-dblue)
		; F#
		("fs"			all-the-icons-fileicon		"fsharp"			:face all-the-icons-blue-alt)
		("fsi"			all-the-icons-fileicon		"fsharp"			:face all-the-icons-blue-alt)
		("fsx"			all-the-icons-fileicon		"fsharp"			:face all-the-icons-blue-alt)
		("fsscript"		all-the-icons-fileicon		"fsharp"			:face all-the-icons-blue-alt)
		; zig
		("zig"			all-the-icons-fileicon		"zig"				:face all-the-icons-orange)
		; odin
		("odin"			all-the-icons-fileicon		"odin"				:face all-the-icons-lblue		:height 1.1)
		; File Types
		("ico"			all-the-icons-octicon		"file-media"		:face all-the-icons-blue		:v-adjust 0.0)
		("png"			all-the-icons-octicon		"file-media"		:face all-the-icons-orange		:v-adjust 0.0)
		("gif"			all-the-icons-octicon		"file-media"		:face all-the-icons-green		:v-adjust 0.0)
		("jpeg"			all-the-icons-octicon		"file-media"		:face all-the-icons-dblue		:v-adjust 0.0)
		("jpg"			all-the-icons-octicon		"file-media"		:face all-the-icons-dblue		:v-adjust 0.0)
		("webp"			all-the-icons-octicon		"file-media"		:face all-the-icons-dblue		:v-adjust 0.0)
		; Audio
		("mp3"			all-the-icons-faicon		"volume-up"			:face all-the-icons-dred)
		("wav"			all-the-icons-faicon		"volume-up"			:face all-the-icons-dred)
		("m4a"			all-the-icons-faicon		"volume-up"			:face all-the-icons-dred)
		("ogg"			all-the-icons-faicon		"volume-up"			:face all-the-icons-dred)
		("flac"			all-the-icons-faicon		"volume-up"			:face all-the-icons-dred)
		("opus"			all-the-icons-faicon		"volume-up"			:face all-the-icons-dred)
		("au"			all-the-icons-faicon		"volume-up"			:face all-the-icons-dred)
		("aif"			all-the-icons-faicon		"volume-up"			:face all-the-icons-dred)
		("aifc"			all-the-icons-faicon		"volume-up"			:face all-the-icons-dred)
		("aiff"			all-the-icons-faicon		"volume-up"			:face all-the-icons-dred)
		("svg"			all-the-icons-alltheicon	"svg"				:face all-the-icons-lgreen		:height 0.9)
		; Video
		("mov"			all-the-icons-faicon		"film"				:face all-the-icons-blue)
		("mp4"			all-the-icons-faicon		"film"				:face all-the-icons-blue)
		("ogv"			all-the-icons-faicon		"film"				:face all-the-icons-dblue)
		("mpg"			all-the-icons-faicon		"film"				:face all-the-icons-blue)
		("mpeg"			all-the-icons-faicon		"film"				:face all-the-icons-blue)
		("flv"			all-the-icons-faicon		"film"				:face all-the-icons-blue)
		("ogv"			all-the-icons-faicon		"film"				:face all-the-icons-dblue)
		("mkv"			all-the-icons-faicon		"film"				:face all-the-icons-blue)
		("webm"			all-the-icons-faicon		"film"				:face all-the-icons-blue)
		("dav"			all-the-icons-faicon		"film"				:face all-the-icons-blue)
		; Fonts
		("ttf"			all-the-icons-fileicon		"font"				:face all-the-icons-dcyan		:v-adjust 0.0)
		("woff"			all-the-icons-fileicon		"font"				:face all-the-icons-cyan		:v-adjust 0.0)
		("woff2"		all-the-icons-fileicon		"font"				:face all-the-icons-cyan		:v-adjust 0.0)
		; Doc
		("pdf"			all-the-icons-octicon		"file-pdf"			:face all-the-icons-dred		:v-adjust 0.0)
		("text"			all-the-icons-octicon		"file-text"			:face all-the-icons-cyan		:height 1.15 :v-adjust -0.05)
		("txt"			all-the-icons-octicon		"file-text"			:face all-the-icons-cyan		:height 1.15 :v-adjust -0.05)
		("doc"			all-the-icons-fileicon		"word"				:face all-the-icons-blue)
		("docx"			all-the-icons-fileicon		"word"				:face all-the-icons-blue)
		("docm"			all-the-icons-fileicon		"word"				:face all-the-icons-blue)
		("eml"			all-the-icons-faicon		"envelope"			:face all-the-icons-blue)
		("msg"			all-the-icons-faicon		"envelope"			:face all-the-icons-blue)
		("texi"			all-the-icons-fileicon		"tex"				:face all-the-icons-lred)
		("tex"			all-the-icons-fileicon		"tex"				:face all-the-icons-lred)
		("md"			all-the-icons-octicon		"markdown"			:face all-the-icons-lblue		:v-adjust 0.0)
		("bib"			all-the-icons-fileicon		"bib"				:face all-the-icons-maroon)
		("org"			all-the-icons-fileicon		"org"				:face all-the-icons-lgreen)
		("pps"			all-the-icons-fileicon		"powerpoint"		:face all-the-icons-orange)
		("ppt"			all-the-icons-fileicon		"powerpoint"		:face all-the-icons-orange)
		("pptsx"		all-the-icons-fileicon		"powerpoint"		:face all-the-icons-orange)
		("pptx"			all-the-icons-fileicon		"powerpoint"		:face all-the-icons-orange)
		("knt"			all-the-icons-fileicon		"powerpoint"		:face all-the-icons-cyan)
		("xlsx"			all-the-icons-fileicon		"excel"				:face all-the-icons-dgreen)
		("xlsm"			all-the-icons-fileicon		"excel"				:face all-the-icons-dgreen)
		("xlsb"			all-the-icons-fileicon		"excel"				:face all-the-icons-dgreen)
		("xltx"			all-the-icons-fileicon		"excel"				:face all-the-icons-dgreen)
		("xltm"			all-the-icons-fileicon		"excel"				:face all-the-icons-dgreen)
		("ly"			all-the-icons-faicon		"music"				:face all-the-icons-green)

		("key"			all-the-icons-octicon		"key"				:face all-the-icons-lblue		:v-adjust 0.0)
		("pem"			all-the-icons-octicon		"key"				:face all-the-icons-orange		:v-adjust 0.0)
		("p12"			all-the-icons-octicon		"key"				:face all-the-icons-dorange		:v-adjust 0.0)
		("crt"			all-the-icons-octicon		"key"				:face all-the-icons-lblue		:v-adjust 0.0)
		("pub"			all-the-icons-octicon		"key"				:face all-the-icons-blue		:v-adjust 0.0)
		("gpg"			all-the-icons-octicon		"key"				:face all-the-icons-lblue		:v-adjust 0.0)
		("cache"		all-the-icons-octicon		"database"			:face all-the-icons-green		:v-adjust 0.0)
	)
)

(defvar all-the-icons-regexp-icon-alist
	'(
		("^TAGS$"					all-the-icons-octicon		"tag"				:face all-the-icons-blue		:v-adjust 0.0)
		("^TODO$"					all-the-icons-octicon		"checklist"			:face all-the-icons-lyellow		:v-adjust 0.0)
		("^LICENSE$"				all-the-icons-octicon		"book"				:face all-the-icons-blue		:v-adjust 0.0)
		("^readme"					all-the-icons-octicon		"book"				:face all-the-icons-lcyan		:v-adjust 0.0)
		; Config
		("nginx$"					all-the-icons-fileicon		"nginx"				:face all-the-icons-dgreen		:height 0.9)
		("apache$"					all-the-icons-alltheicon	"apache"			:face all-the-icons-dgreen		:height 0.9)
		; C
		("^Makefile$"				all-the-icons-fileicon		"gnu"				:face all-the-icons-dorange)
		("^CMakeLists.txt$"			all-the-icons-fileicon		"cmake"				:face all-the-icons-red)
		("^CMakeCache.txt$"			all-the-icons-fileicon		"cmake"				:face all-the-icons-blue)
		("^meson.build$"			all-the-icons-fileicon		"meson"				:face all-the-icons-purple)
		("^meson_options.txt$"		all-the-icons-fileicon		"meson"				:face all-the-icons-purple)
		; Docker
		("^\\.?Dockerfile"			all-the-icons-fileicon		"dockerfile"		:face all-the-icons-blue)
		; Homebrew
		("^Brewfile$"				all-the-icons-faicon		"beer"				:face all-the-icons-lsilver)
		; AWS
		("^stack.*.json$"			all-the-icons-alltheicon	"aws"				:face all-the-icons-orange)
		("^serverless\\.yml$"		all-the-icons-faicon		"bolt"				:face all-the-icons-yellow		:v-adjust 0.0)
		; lock files
		("~$"						all-the-icons-octicon		"lock"				:face all-the-icons-maroon		:v-adjust 0.0)
		; Source Codes
		("^mix.lock$"				all-the-icons-alltheicon	"elixir"			:face all-the-icons-lyellow		:v-adjust -0.1 :height 0.9)
		; Ruby
		("^Gemfile\\(\\.lock\\)?$"	all-the-icons-alltheicon	"ruby-alt"			:face all-the-icons-red)
		("_?test\\.rb$"				all-the-icons-fileicon		"test-ruby"			:face all-the-icons-red			:v-adjust 0.0)
		("_?test_helper\\.rb$"		all-the-icons-fileicon		"test-ruby"			:face all-the-icons-dred		:v-adjust 0.0)
		("_?spec\\.rb$"				all-the-icons-fileicon		"test-ruby"			:face all-the-icons-red			:v-adjust 0.0)
		("_?spec_helper\\.rb$"		all-the-icons-fileicon		"test-ruby"			:face all-the-icons-dred		:v-adjust 0.0)

		("-?spec\\.ts$"				all-the-icons-fileicon		"test-typescript"	:face all-the-icons-blue		:v-adjust 0.0)
		("-?test\\.ts$"				all-the-icons-fileicon		"test-typescript"	:face all-the-icons-blue		:v-adjust 0.0)
		("-?spec\\.js$"				all-the-icons-fileicon		"test-js"			:face all-the-icons-lpurple		:v-adjust 0.0)
		("-?test\\.js$"				all-the-icons-fileicon		"test-js"			:face all-the-icons-lpurple		:v-adjust 0.0)
		("-?spec\\.jsx$"			all-the-icons-fileicon		"test-react"		:face all-the-icons-blue-alt	:v-adjust 0.0)
		("-?test\\.jsx$"			all-the-icons-fileicon		"test-react"		:face all-the-icons-blue-alt	:v-adjust 0.0)
		; Git
		("^MERGE_"					all-the-icons-octicon		"git-merge"			:face all-the-icons-red			:v-adjust 0.0)
		("^COMMIT_EDITMSG"			all-the-icons-octicon		"git-commit"		:face all-the-icons-red			:v-adjust 0.0)
		; Stylesheeting
		("stylelint"				all-the-icons-fileicon		"stylelint"			:face all-the-icons-lyellow)
		; JavaScript
		("^package.json$"			all-the-icons-fileicon		"npm"				:face all-the-icons-red)
		("^package.lock.json$"		all-the-icons-fileicon		"npm"				:face all-the-icons-dred)
		("\\.npmignore$"			all-the-icons-fileicon		"npm"				:face all-the-icons-dred)
		("^yarn\\.lock"				all-the-icons-fileicon		"yarn"				:face all-the-icons-blue-alt)
		("^bower.json$"				all-the-icons-alltheicon	"bower"				:face all-the-icons-lorange		:v-adjust 0.0)
		("^gulpfile"				all-the-icons-alltheicon	"gulp"				:face all-the-icons-lred)
		("^gruntfile"				all-the-icons-alltheicon	"grunt"				:face all-the-icons-lyellow		:v-adjust -0.1)
		("^webpack"					all-the-icons-fileicon		"webpack"			:face all-the-icons-lblue)
		; Go
		("^go.mod$"					all-the-icons-fileicon		"config-go"			:face all-the-icons-blue-alt)
		("^go.work$"				all-the-icons-fileicon		"config-go"			:face all-the-icons-blue-alt)
		; Emacs
;		("bookmark"					all-the-icons-octicon		"bookmark"			:face all-the-icons-lpink		:v-adjust 0.0 :height 1.1)

		("^\\*scratch\\*$"			all-the-icons-faicon		"sticky-note"		:face all-the-icons-lyellow)
		("^\\*scratch.*"			all-the-icons-faicon		"sticky-note"		:face all-the-icons-yellow)
		("^\\*new-tab\\*$"			all-the-icons-material		"star"				:face all-the-icons-cyan)

		("^\\."						all-the-icons-octicon		"gear"												:v-adjust 0.0)
	)
)

(defvar all-the-icons-dir-icon-alist
	'(
		("trash"			all-the-icons-faicon		"trash-o"			:v-adjust -0.1 :height 1.2)
		("dropbox"			all-the-icons-faicon		"dropbox"			:v-adjust -0.1)
		("google[ _-]drive"	all-the-icons-alltheicon	"google-drive"		:v-adjust -0.1)
		("^atom$"			all-the-icons-alltheicon	"atom"				:v-adjust -0.1 :height 1.2)
		("documents"		all-the-icons-faicon		"book"				:v-adjust -0.1)
		("download"			all-the-icons-faicon		"cloud-download"	:v-adjust -0.1 :height 0.9)
		("desktop"			all-the-icons-octicon		"device-desktop"	:v-adjust -0.1)
		("pictures"			all-the-icons-faicon		"picture-o"			:v-adjust -0.2 :height 0.9)
		("photos"			all-the-icons-faicon		"camera-retro"		:v-adjust -0.1)
		("music"			all-the-icons-faicon		"music"				:v-adjust -0.1)
		("movies"			all-the-icons-faicon		"film"				:v-adjust -0.1 :height 0.9)
		("code"				all-the-icons-octicon		"code"				:v-adjust -0.1 :height 1.1)
		("workspace"		all-the-icons-octicon		"code"				:v-adjust -0.1 :height 1.1)
		("test"				all-the-icons-fileicon		"test-dir"			:height 0.9)
		("\\.git"			all-the-icons-alltheicon	"git")
		(".?"				all-the-icons-octicon		"file-directory"	:v-adjust -0.1)
	)
)

(defvar all-the-icons-weather-icon-alist
	'(
		("tornado"					all-the-icons-wicon "tornado")
		("hurricane"				all-the-icons-wicon "hurricane")
		("thunderstorms"			all-the-icons-wicon "thunderstorm")
		("sunny"					all-the-icons-wicon "day-sunny")
		("rain.*snow"				all-the-icons-wicon "rain-mix")
		("rain.*hail"				all-the-icons-wicon "rain-mix")
		("sleet"					all-the-icons-wicon "sleet")
		("hail"						all-the-icons-wicon "hail")
		("drizzle"					all-the-icons-wicon "sprinkle")
		("rain"						all-the-icons-wicon "showers" :height 1.1 :v-adjust 0.0)
		("showers"					all-the-icons-wicon "showers")
		("blowing.*snow"			all-the-icons-wicon "snow-wind")
		("snow"						all-the-icons-wicon "snow")
		("dust"						all-the-icons-wicon "dust")
		("fog"						all-the-icons-wicon "fog")
		("haze"						all-the-icons-wicon "day-haze")
		("smoky"					all-the-icons-wicon "smoke")
		("blustery"					all-the-icons-wicon "cloudy-windy")
		("windy"					all-the-icons-wicon "cloudy-gusts")
		("cold"						all-the-icons-wicon "snowflake-cold")
		("partly.*cloudy.*night"	all-the-icons-wicon "night-alt-partly-cloudy")
		("partly.*cloudy"			all-the-icons-wicon "day-cloudy-high")
		("cloudy.*night"			all-the-icons-wicon "night-alt-cloudy")
		("cxloudy.*day"				all-the-icons-wicon "day-cloudy")
		("cloudy"					all-the-icons-wicon "cloudy")
		("clear.*night"				all-the-icons-wicon "night-clear")
		("fair.*night"				all-the-icons-wicon "stars")
		("fair.*day"				all-the-icons-wicon "horizon")
		("hot"						all-the-icons-wicon "hot")
		("not.*available"			all-the-icons-wicon "na")
	)
)

(defvar all-the-icons-mode-icon-alist
	'(
		(emacs-lisp-mode           all-the-icons-fileicon "elisp"              :face all-the-icons-purple :v-adjust -0.15 :height 1.1)
		(circe-server-mode         all-the-icons-faicon "commenting-o"         :v-adjust 0.0)
		(circe-channel-mode        all-the-icons-faicon "commenting-o"         :v-adjust 0.0)
		(circe-query-mode          all-the-icons-faicon "commenting-o"         :v-adjust 0.0)
		(crystal-mode              all-the-icons-fileicon "crystal"            :v-adjust 0.0 :face all-the-icons-yellow)
		(erc-mode                  all-the-icons-faicon "commenting-o"         :v-adjust 0.0)
		(inferior-emacs-lisp-mode  all-the-icons-fileicon "elisp"              :face all-the-icons-lblue :v-adjust -0.15 :height 1.1)
		(dired-mode                all-the-icons-octicon "file-directory"      :v-adjust 0.0)
		(wdired-mode               all-the-icons-octicon "file-directory"      :v-adjust 0.0 :face all-the-icons-dcyan)
		(lisp-interaction-mode     all-the-icons-fileicon "lisp"               :v-adjust -0.1 :face all-the-icons-orange)
		(sly-mrepl-mode            all-the-icons-fileicon "clisp"              :v-adjust -0.1 :face all-the-icons-orange)
		(slime-repl-mode           all-the-icons-fileicon "clisp"              :v-adjust -0.1 :face all-the-icons-orange)
		(org-mode                  all-the-icons-fileicon "org"                :v-adjust 0.0 :face all-the-icons-lgreen)
		(typescript-mode           all-the-icons-fileicon "typescript"         :v-adjust -0.1 :face all-the-icons-blue-alt)
		(typescript-ts-mode        all-the-icons-fileicon "typescript"         :v-adjust -0.1 :face all-the-icons-blue-alt)
		(typescript-tsx-mode       all-the-icons-fileicon "tsx"                :v-adjust -0.1 :face all-the-icons-cyan-alt)
		(tsx-ts-mode               all-the-icons-fileicon "tsx"                :v-adjust -0.1 :face all-the-icons-cyan-alt)
		(js-mode                   all-the-icons-alltheicon "javascript"       :v-adjust -0.1 :face all-the-icons-yellow)
		(js-ts-mode                all-the-icons-alltheicon "javascript"       :v-adjust -0.1 :face all-the-icons-yellow)
		(js-jsx-mode               all-the-icons-alltheicon "javascript"       :v-adjust -0.1 :face all-the-icons-yellow)
		(js2-mode                  all-the-icons-alltheicon "javascript"       :v-adjust -0.1 :face all-the-icons-yellow)
		(js3-mode                  all-the-icons-alltheicon "javascript"       :v-adjust -0.1 :face all-the-icons-yellow)
		(rjsx-mode                 all-the-icons-fileicon "jsx-2"              :v-adjust -0.1 :face all-the-icons-cyan-alt)
		(term-mode                 all-the-icons-octicon "terminal"            :v-adjust 0.2)
		(vterm-mode                all-the-icons-octicon "terminal"            :v-adjust 0.2)
		(eshell-mode               all-the-icons-octicon "terminal"            :v-adjust 0.0 :face all-the-icons-purple)
		(magit-refs-mode           all-the-icons-octicon "git-branch"          :v-adjust 0.0 :face all-the-icons-red)
		(magit-process-mode        all-the-icons-octicon "mark-github"         :v-adjust 0.0)
		(magit-diff-mode           all-the-icons-octicon "git-compare"         :v-adjust 0.0 :face all-the-icons-lblue)
		(ediff-mode                all-the-icons-octicon "git-compare"         :v-adjust 0.0 :Face all-the-icons-red)
		(comint-mode               all-the-icons-faicon "terminal"             :v-adjust 0.0 :face all-the-icons-lblue)
		(eww-mode                  all-the-icons-faicon "firefox"              :v-adjust -0.1 :face all-the-icons-red)
		(org-agenda-mode           all-the-icons-octicon "checklist"           :v-adjust 0.0 :face all-the-icons-lgreen)
		(cfw:calendar-mode         all-the-icons-octicon "calendar"            :v-adjust 0.0)
		(ibuffer-mode              all-the-icons-faicon "files-o"              :v-adjust 0.0 :face all-the-icons-dsilver)
		(messages-buffer-mode      all-the-icons-fileicon "elisp"              :face all-the-icons-dsilver :v-adjust -0.15 :height 1.1)
		(help-mode                 all-the-icons-faicon "info"                 :v-adjust -0.1 :face all-the-icons-purple)
		(helpful-mode              all-the-icons-faicon "info"                 :v-adjust -0.1 :face all-the-icons-purple)
		(benchmark-init/tree-mode  all-the-icons-octicon "dashboard"           :v-adjust 0.0)
		(jenkins-mode              all-the-icons-fileicon "jenkins"            :face all-the-icons-blue)
		(magit-popup-mode          all-the-icons-alltheicon "git"              :face all-the-icons-red)
		(magit-status-mode         all-the-icons-alltheicon "git"              :face all-the-icons-lred)
		(magit-log-mode            all-the-icons-alltheicon "git"              :face all-the-icons-green)
		(mu4e-compose-mode         all-the-icons-octicon "pencil"              :v-adjust 0.0)
		(mu4e-headers-mode         all-the-icons-octicon "mail"                :v-adjust 0.0)
		(mu4e-main-mode            all-the-icons-octicon "mail"                :v-adjust 0.0)
		(mu4e-view-mode            all-the-icons-octicon "mail-read"           :v-adjust 0.0)
		(sieve-mode                all-the-icons-octicon "mail"                :v-adjust 0.0)
		(gnus-group-mode           all-the-icons-octicon "mail"                :v-adjust 0.0)
		(gnus-summary-mode         all-the-icons-octicon "mail"                :v-adjust 0.0)
		(gnus-article-mode         all-the-icons-octicon "mail-read"           :v-adjust 0.0)
		(message-mode              all-the-icons-octicon "pencil"              :v-adjust 0.0)
		(package-menu-mode         all-the-icons-faicon "archive"              :v-adjust 0.0 :face all-the-icons-silver)
		(paradox-menu-mode         all-the-icons-faicon "archive"              :v-adjust 0.0 :face all-the-icons-silver)
		(Custom-mode               all-the-icons-octicon "settings"            :v-adjust -0.1)

		; Special matcher for Web Mode based on the `web-mode-content-type' of the current buffer.
		(web-mode all-the-icons--web-mode-icon)

		(fundamental-mode                   all-the-icons-fileicon "elisp"            :face all-the-icons-dsilver :v-adjust -0.15 :height 1.1)
		(special-mode                       all-the-icons-fileicon "elisp"            :face all-the-icons-yellow :v-adjust -0.15 :height 1.1)
		(text-mode                          all-the-icons-octicon "file-text"         :v-adjust 0.0 :face all-the-icons-cyan)
		(enh-ruby-mode                      all-the-icons-alltheicon "ruby-alt"       :face all-the-icons-lred)
		(ruby-mode                          all-the-icons-alltheicon "ruby-alt"       :face all-the-icons-lred)
		(ruby-ts-mode                       all-the-icons-alltheicon "ruby-alt"       :face all-the-icons-lred)
		(inf-ruby-mode                      all-the-icons-alltheicon "ruby-alt"       :face all-the-icons-red)
		(projectile-rails-compilation-mode  all-the-icons-alltheicon "ruby-alt"       :face all-the-icons-red)
		(rspec-compilation-mode             all-the-icons-alltheicon "ruby-alt"       :face all-the-icons-red)
		(rake-compilation-mode              all-the-icons-alltheicon "ruby-alt"       :face all-the-icons-red)
		(sh-mode                            all-the-icons-alltheicon "terminal"       :face all-the-icons-purple)
		(bash-ts-mode                       all-the-icons-alltheicon "terminal"       :face all-the-icons-purple)
		(shell-mode                         all-the-icons-alltheicon "terminal"       :face all-the-icons-purple)
		(fish-mode                          all-the-icons-alltheicon "terminal"       :face all-the-icons-lpink)
		(nginx-mode                         all-the-icons-fileicon "nginx"            :height 0.9 :face all-the-icons-dgreen)
		(apache-mode                        all-the-icons-alltheicon "apache"         :height 0.9 :face all-the-icons-dgreen)
		(makefile-mode                      all-the-icons-fileicon "gnu"              :face all-the-icons-dorange)
		(cmake-mode                         all-the-icons-fileicon "cmake"            :face all-the-icons-red)
		(cmake-ts-mode                      all-the-icons-fileicon "cmake"            :face all-the-icons-red)
		(dockerfile-mode                    all-the-icons-fileicon "dockerfile"       :face all-the-icons-blue)
		(dockerfile-ts-mode                 all-the-icons-fileicon "dockerfile"       :face all-the-icons-blue)
		(docker-compose-mode                all-the-icons-fileicon "dockerfile"       :face all-the-icons-lblue)
		(nxml-mode                          all-the-icons-faicon "file-code-o"        :height 0.95 :face all-the-icons-lorange)
		(json-mode                          all-the-icons-octicon "settings"          :face all-the-icons-yellow)
		(json-ts-mode                       all-the-icons-octicon "settings"          :face all-the-icons-yellow)
		(jsonian-mode                       all-the-icons-octicon "settings"          :face all-the-icons-yellow)
		(yaml-mode                          all-the-icons-octicon "settings"          :v-adjust 0.0 :face all-the-icons-dyellow)
		(yaml-ts-mode                       all-the-icons-octicon "settings"          :v-adjust 0.0 :face all-the-icons-dyellow)
		(elisp-byte-code-mode               all-the-icons-octicon "file-binary"       :v-adjust 0.0 :face all-the-icons-dsilver)
		(archive-mode                       all-the-icons-octicon "file-zip"          :v-adjust 0.0 :face all-the-icons-lmaroon)
		(elm-mode                           all-the-icons-fileicon "elm"              :face all-the-icons-blue)
		(erlang-mode                        all-the-icons-alltheicon "erlang"         :face all-the-icons-red :v-adjust -0.1 :height 0.9)
		(elixir-mode                        all-the-icons-alltheicon "elixir"         :face all-the-icons-lorange :v-adjust -0.1 :height 0.9)
		(java-mode                          all-the-icons-alltheicon "java"           :face all-the-icons-purple)
		(java-ts-mode                       all-the-icons-alltheicon "java"           :face all-the-icons-purple)
		(go-mode                            all-the-icons-fileicon "go"               :face all-the-icons-blue)
		(go-ts-mode                         all-the-icons-fileicon "go"               :face all-the-icons-blue)
		(go-mod-ts-mode                     all-the-icons-fileicon "config-go"        :face all-the-icons-blue-alt)
		(go-dot-mod-mode                    all-the-icons-fileicon "config-go"        :face all-the-icons-blue-alt)
		(go-dot-work-mode                   all-the-icons-fileicon "config-go"        :face all-the-icons-blue-alt)
		(graphql-mode                       all-the-icons-fileicon "graphql"          :face all-the-icons-dpink)
		(matlab-mode                        all-the-icons-fileicon "matlab"           :face all-the-icons-orange)
		(nix-mode                           all-the-icons-fileicon "nix"              :face all-the-icons-blue)
		(perl-mode                          all-the-icons-alltheicon "perl"           :face all-the-icons-lorange)
		(cperl-mode                         all-the-icons-alltheicon "perl"           :face all-the-icons-lorange)
		(php-mode                           all-the-icons-fileicon "php"              :face all-the-icons-lsilver)
		(prolog-mode                        all-the-icons-alltheicon "prolog"         :height 1.1 :face all-the-icons-lmaroon)
		(python-mode                        all-the-icons-alltheicon "python"         :face all-the-icons-dblue)
		(python-ts-mode                     all-the-icons-alltheicon "python"         :face all-the-icons-dblue)
		(inferior-python-mode               all-the-icons-alltheicon "python"         :face all-the-icons-dblue)
		(racket-mode                        all-the-icons-fileicon "racket"           :height 1.2 :face all-the-icons-red)
		(rust-mode                          all-the-icons-alltheicon "rust"           :height 1.2 :face all-the-icons-maroon)
		(rustic-mode                        all-the-icons-alltheicon "rust"           :height 1.2 :face all-the-icons-maroon)
		(rust-ts-mode                       all-the-icons-alltheicon "rust"           :height 1.2 :face all-the-icons-maroon)
		(scala-mode                         all-the-icons-alltheicon "scala"          :face all-the-icons-red)
		(scheme-mode                        all-the-icons-fileicon   "scheme"         :height 1.2 :face all-the-icons-red)
		(swift-mode                         all-the-icons-alltheicon "swift"          :v-adjust -0.1 :face all-the-icons-green)
		(svelte-mode                        all-the-icons-fileicon "svelte"           :v-adjust 0.0 :face all-the-icons-red)
		(c-mode                             all-the-icons-alltheicon "c-line"         :face all-the-icons-blue-alt :v-adjust -0.05 :height 1.2)
		(c-ts-mode                          all-the-icons-alltheicon "c-line"         :face all-the-icons-blue-alt :v-adjust -0.05 :height 1.2)
		(c++-mode                           all-the-icons-alltheicon "cplusplus-line" :face all-the-icons-blue-alt :v-adjust -0.05 :height 1.2)
		(c++-ts-mode                        all-the-icons-alltheicon "cplusplus-line" :face all-the-icons-blue-alt :v-adjust -0.05 :height 1.2)
		(csharp-mode                        all-the-icons-alltheicon "csharp-line"    :face all-the-icons-dblue)
		(csharp-ts-mode                     all-the-icons-alltheicon "csharp-line"    :face all-the-icons-dblue)
		(clojure-mode                       all-the-icons-alltheicon "clojure"        :face all-the-icons-blue)
		(cider-repl-mode                    all-the-icons-alltheicon "clojure"        :face all-the-icons-green)
		(clojurescript-mode                 all-the-icons-fileicon "cljs"             :face all-the-icons-dblue)
		(coffee-mode                        all-the-icons-alltheicon "coffeescript"   :face all-the-icons-maroon)
		(lisp-mode                          all-the-icons-fileicon "lisp"             :face all-the-icons-orange)
		(css-mode                           all-the-icons-alltheicon "css3"           :face all-the-icons-yellow)
		(css-ts-mode                        all-the-icons-alltheicon "css3"           :face all-the-icons-yellow)
		(scss-mode                          all-the-icons-alltheicon "sass"           :face all-the-icons-pink)
		(sass-mode                          all-the-icons-alltheicon "sass"           :face all-the-icons-dpink)
		(less-css-mode                      all-the-icons-alltheicon "less"           :height 0.8 :face all-the-icons-dyellow)
		(stylus-mode                        all-the-icons-alltheicon "stylus"         :face all-the-icons-lgreen)
		(csv-mode                           all-the-icons-octicon "graph"             :v-adjust 0.0 :face all-the-icons-dblue)
		(haskell-mode                       all-the-icons-alltheicon "haskell"        :face all-the-icons-red)
		(haskell-c2hs-mode                  all-the-icons-alltheicon "haskell"        :face all-the-icons-red)
		(literate-haskell-mode              all-the-icons-alltheicon "haskell"        :face all-the-icons-red)
		(haml-mode                          all-the-icons-fileicon "haml"             :face all-the-icons-lyellow)
		(html-mode                          all-the-icons-alltheicon "html5"          :face all-the-icons-orange)
		(html-ts-mode                       all-the-icons-alltheicon "html5"          :face all-the-icons-orange)
		(rhtml-mode                         all-the-icons-alltheicon "html5"          :face all-the-icons-lred)
		(mustache-mode                      all-the-icons-fileicon "moustache"        :face all-the-icons-green)
		(slim-mode                          all-the-icons-octicon "dashboard"         :v-adjust 0.0 :face all-the-icons-yellow)
		(jade-mode                          all-the-icons-fileicon "jade"             :face all-the-icons-red)
		(pug-mode                           all-the-icons-fileicon "pug"              :face all-the-icons-red)
		(react-mode                         all-the-icons-alltheicon "react"          :height 1.1 :face all-the-icons-lblue)
		(image-mode                         all-the-icons-octicon "file-media"        :v-adjust 0.0 :face all-the-icons-blue)
		(texinfo-mode                       all-the-icons-fileicon "tex"              :face all-the-icons-lred)
		(markdown-mode                      all-the-icons-octicon "markdown"          :v-adjust 0.0 :face all-the-icons-lblue)
		(bibtex-mode                        all-the-icons-fileicon "bib"              :face all-the-icons-maroon)
		(org-mode                           all-the-icons-fileicon "org"              :face all-the-icons-lgreen)
		(compilation-mode                   all-the-icons-faicon "cogs"               :v-adjust 0.0)
		(objc-mode                          all-the-icons-faicon "apple"              :v-adjust 0.0)
		(tuareg-mode                        all-the-icons-fileicon "ocaml"            :v-adjust 0.0)
		(purescript-mode                    all-the-icons-fileicon "purescript"       :v-adjust 0.0)
		(verilog-mode                       all-the-icons-fileicon "verilog"          :v-adjust -0.2 :face all-the-icons-red)
		(vhdl-mode                          all-the-icons-fileicon "vhdl"             :face all-the-icons-blue)
		(haskell-cabal-mode                 all-the-icons-fileicon "cabal"            :face all-the-icons-lblue)
		(kotlin-mode                        all-the-icons-fileicon "kotlin"           :face all-the-icons-orange)
		(kotlin-ts-mode                     all-the-icons-fileicon "kotlin"           :face all-the-icons-orange)
		(nim-mode                           all-the-icons-fileicon "nimrod"           :face all-the-icons-yellow)
		(sql-mode                           all-the-icons-octicon  "database"         :face all-the-icons-silver)
		(lua-mode                           all-the-icons-fileicon "lua"              :face all-the-icons-dblue)
		(adoc-mode                          all-the-icons-fileicon "asciidoc"         :face all-the-icons-lblue)
		(puppet-mode                        all-the-icons-fileicon "puppet"           :face all-the-icons-yellow)
		(jinja2-mode                        all-the-icons-fileicon "jinja"            :face all-the-icons-silver)
		(powershell-mode                    all-the-icons-fileicon "powershell"       :face all-the-icons-blue)
		(tex-mode                           all-the-icons-fileicon "tex"              :face all-the-icons-lred)
		(latex-mode                         all-the-icons-fileicon "tex"              :face all-the-icons-lred)
		(dart-mode                          all-the-icons-fileicon "dart"             :face all-the-icons-blue)
		(fsharp-mode                        all-the-icons-fileicon "fsharp"           :face all-the-icons-blue)
		(asm-mode                           all-the-icons-fileicon "assembly"         :face all-the-icons-blue)
		(nasm-mode                          all-the-icons-fileicon "assembly"         :face all-the-icons-blue)
		(tcl-mode                           all-the-icons-fileicon "tcl"              :face all-the-icons-dred)
		(cuda-mode                          all-the-icons-fileicon "nvidia"           :face all-the-icons-green)
		(f90-mode                           all-the-icons-fileicon "fortran"          :face all-the-icons-purple)
		(hy-mode                            all-the-icons-fileicon "hy"               :face all-the-icons-blue)
		(glsl-mode                          all-the-icons-fileicon "vertex-shader"    :face all-the-icons-green)
		(zig-mode                           all-the-icons-fileicon "zig"              :face all-the-icons-orange)
		(exwm-mode                          all-the-icons-octicon "browser"           :v-adjust 0.2 :face all-the-icons-purple)
		(beancount-mode                     all-the-icons-faicon "credit-card"        :face all-the-icons-lgreen)
		(ledger-mode                        all-the-icons-faicon "credit-card"        :face all-the-icons-lgreen)
		(odin-mode                          all-the-icons-fileicon "odin"             :height 1.1 :face all-the-icons-lblue)
		(pdf-view-mode                      all-the-icons-octicon  "file-pdf"         :v-adjust 0.0 :face all-the-icons-dred)
		(elfeed-search-mode                 all-the-icons-faicon   "rss-square"       :face all-the-icons-orange)
		(elfeed-show-mode                   all-the-icons-faicon   "rss"              :face all-the-icons-orange)
		(emms-browser-mode                  all-the-icons-faicon   "music"            :face all-the-icons-silver)
		(emms-lyrics-mode                   all-the-icons-faicon   "music"            :face all-the-icons-silver)
		(emms-show-all-mode                 all-the-icons-faicon   "music"            :face all-the-icons-silver)
		(emms-metaplaylist-mode             all-the-icons-faicon   "music"            :face all-the-icons-silver)
		(emms-tag-editor-mode               all-the-icons-faicon   "music"            :face all-the-icons-silver)
		(emms-playlist-mode                 all-the-icons-faicon   "music"            :face all-the-icons-silver)
		(lilypond-mode                      all-the-icons-faicon   "music"            :face all-the-icons-green)
		(magik-session-mode                 all-the-icons-alltheicon "terminal"       :face all-the-icons-blue)
		(magik-cb-mode                      all-the-icons-faicon "book"               :face all-the-icons-blue)
		(meson-mode                         all-the-icons-fileicon "meson"            :face all-the-icons-purple)
		(man-common                         all-the-icons-fileicon "man-page"         :face all-the-icons-blue)
		(ess-r-mode                         all-the-icons-fileicon "R"                :face all-the-icons-lblue)
	)
)

(defvar all-the-icons-url-alist
	'(
		; Social media and communities
		("^\\(https?://\\)?\\(www\\.\\)?del\\.icio\\.us" all-the-icons-faicon "delicious")
		("^\\(https?://\\)?\\(www\\.\\)?behance\\.net" all-the-icons-faicon "behance")
		("^\\(https?://\\)?\\(www\\.\\)?dribbble\\.com" all-the-icons-faicon "dribbble")
		("^\\(https?://\\)?\\(www\\.\\)?facebook\\.com" all-the-icons-faicon "facebook-official")
		("^\\(https?://\\)?\\(www\\.\\)?glide\\.me" all-the-icons-faicon "glide-g")
		("^\\(https?://\\)?\\(www\\.\\)?plus\\.google\\.com" all-the-icons-faicon "google-plus")
		("linkedin\\.com" all-the-icons-faicon "linkedin")
		("^\\(https?://\\)?\\(www\\.\\)?ok\\.ru" all-the-icons-faicon "odnoklassniki")
		("^\\(https?://\\)?\\(www\\.\\)?reddit\\.com" all-the-icons-faicon "reddit-alien")
		("^\\(https?://\\)?\\(www\\.\\)?slack\\.com" all-the-icons-faicon "slack")
		("^\\(https?://\\)?\\(www\\.\\)?snapchat\\.com" all-the-icons-faicon "snapchat-ghost")
		("^\\(https?://\\)?\\(www\\.\\)?weibo\\.com" all-the-icons-faicon "weibo")
		("^\\(https?://\\)?\\(www\\.\\)?twitter\\.com" all-the-icons-faicon "twitter")
		; Blogging
		("joomla\\.org" all-the-icons-faicon "joomla")
		("^\\(https?://\\)?\\(www\\.\\)?medium\\.com" all-the-icons-faicon "medium")
		("tumblr\\.com" all-the-icons-faicon "tumblr")
		("^wordpress\\.com" all-the-icons-faicon "wordpress")
		; Programming
		("^\\(https?://\\)?\\(www\\.\\)?bitbucket\\.org" all-the-icons-faicon "bitbucket")
		("^\\(https?://\\)?\\(www\\.\\)?codepen\\.io" all-the-icons-faicon "codepen")
		("^\\(https?://\\)?\\(www\\.\\)?codiepie\\.com" all-the-icons-faicon "codiepie")
		("^\\(https?://\\)?\\(www\\.\\)?gist\\.github\\.com" all-the-icons-octicon "gist")
		("^\\(https?://\\)?\\(www\\.\\)?github\\.com" all-the-icons-octicon "mark-github")
		("^\\(https?://\\)?\\(www\\.\\)?gitlab\\.com" all-the-icons-faicon "gitlab")
		("^\\(https?://\\)?\\(www\\.\\)?news\\.ycombinator\\.com" all-the-icons-faicon "hacker-news")
		("^\\(https?://\\)?\\(www\\.\\)?jsfiddle\\.net" all-the-icons-faicon "jsfiddle")
		("^\\(https?://\\)?\\(www\\.\\)?maxcdn\\.com" all-the-icons-faicon "maxcdn")
		("^\\(https?://\\)?\\(www\\.\\)?stackoverflow\\.com" all-the-icons-faicon "stack-overflow")
		; Video
		("^\\(https?://\\)?\\(www\\.\\)?twitch\\.tv" all-the-icons-faicon "twitch")
		("^\\(https?://\\)?\\(www\\.\\)?vimeo\\.com" all-the-icons-faicon "vimeo")
		("^\\(https?://\\)?\\(www\\.\\)?youtube\\.com" all-the-icons-faicon "youtube")
		("^\\(https?://\\)?\\(www\\.\\)?youtu\\.be" all-the-icons-faicon "youtube")
		("^\\(https?://\\)?\\(www\\.\\)?vine\\.co" all-the-icons-faicon "vine")
		; Sound
		("^\\(https?://\\)?\\(www\\.\\)?last\\.fm" all-the-icons-faicon "lastfm")
		("^\\(https?://\\)?\\(www\\.\\)?mixcloud\\.com" all-the-icons-faicon "mixcloud")
		("^\\(https?://\\)?\\(www\\.\\)?soundcloud\\.com" all-the-icons-faicon "soundcloud")
		("spotify\\.com" all-the-icons-faicon "spotify")
		; Shopping
		("^\\(https?://\\)?\\(www\\.\\)?amazon\\." all-the-icons-faicon "amazon")
		("^\\(https?://\\)?\\(www\\.\\)?opencart\\.com" all-the-icons-faicon "opencart")
		("^\\(https?://\\)?\\(www\\.\\)?paypal\\.com" all-the-icons-faicon "paypal")
		("^\\(https?://\\)?\\(www\\.\\)?shirtsinbulk\\.com" all-the-icons-faicon "shitsinbulk")
		; Images
		("^\\(https?://\\)?\\(www\\.\\)?500px\\.com" all-the-icons-faicon "500px")
		("^\\(https?://\\)?\\(www\\.\\)?deviantart\\.com" all-the-icons-faicon "deviantart")
		("^\\(https?://\\)?\\(www\\.\\)?flickr\\.com" all-the-icons-faicon "flickr")
		("^\\(https?://\\)?\\(www\\.\\)?instagram\\.com" all-the-icons-faicon "instagram")
		("^\\(https?://\\)?\\(www\\.\\)?pinterest\\." all-the-icons-faicon "pinterest")
		; Information and books
		("^\\(https?://\\)?\\(www\\.\\)?digg\\.com" all-the-icons-faicon "digg")
		("^\\(https?://\\)?\\(www\\.\\)?foursquare\\.com" all-the-icons-faicon "foursquare")
		("^\\(https?://\\)?\\(www\\.\\)?getpocket\\.com" all-the-icons-faicon "get-pocket")
		("^\\(https?://\\)?\\(www\\.\\)?scribd\\.com" all-the-icons-faicon "scribd")
		("^\\(https?://\\)?\\(www\\.\\)?slideshare\\.net" all-the-icons-faicon "slideshare")
		("stackexchange\\.com" all-the-icons-faicon "stack-exchange")
		("^\\(https?://\\)?\\(www\\.\\)?stumbleupon\\.com" all-the-icons-faicon "stumbleupon")
		("^\\(https?://\\)?\\(www\\.\\)?tripadvisor\\." all-the-icons-faicon "tripadvisor")
		("^\\(https?://\\)?\\(www\\.\\)?yelp\\." all-the-icons-faicon "yelp")

		("wikipedia\\.org" all-the-icons-faicon "wikipedia-w")
		; Various companies and tools
		("^\\(https?://\\)?\\(www\\.\\)?angel\\.co" all-the-icons-faicon "angellist")
		("^\\(https?://\\)?\\(www\\.\\)?apple\\.com" all-the-icons-faicon "apple")
		("^\\(https?://\\)?\\(www\\.\\)?buysellads\\.com" all-the-icons-faicon "buysellads")
		("^\\(https?://\\)?\\(www\\.\\)?connectdevelop\\.com" all-the-icons-faicon "connectdevelop")
		("^\\(https?://\\)?\\(www\\.\\)?dashcube\\.com" all-the-icons-faicon "dashcube")
		("^\\(https?://\\)?\\(www\\.\\)?dropbox\\.com" all-the-icons-faicon "dropbox")
		("^\\(https?://\\)?\\(www\\.\\)?enviragallery\\.com" all-the-icons-faicon "envira")
		("^\\(https?://\\)?\\(www\\.\\)?fortawesome\\.com" all-the-icons-faicon "fort-awesome")
		("^\\(https?://\\)?\\(www\\.\\)?forumbee\\.com" all-the-icons-faicon "forumbee")
		("^\\(https?://\\)?\\(www\\.\\)?gratipay\\.com" all-the-icons-faicon "gratipay")
		("^\\(https?://\\)?\\(www\\.\\)?modx\\.com" all-the-icons-faicon "modx")
		("^\\(https?://\\)?\\(www\\.\\)?pagelines\\.com" all-the-icons-faicon "pagelines")
		("^\\(https?://\\)?\\(www\\.\\)?producthunt\\.com" all-the-icons-faicon "product-hunt")
		("sellsy\\.com" all-the-icons-faicon "sellsy")
		("^\\(https?://\\)?\\(www\\.\\)?simplybuilt\\.com" all-the-icons-faicon "simplybuilt")
		("^\\(https?://\\)?\\(www\\.\\)?skyatlas\\.com" all-the-icons-faicon "skyatlas")
		("^\\(https?://\\)?\\(www\\.\\)?skype\\.com" all-the-icons-faicon "skype")
		("steampowered\\.com" all-the-icons-faicon "steam")
		("^\\(https?://\\)?\\(www\\.\\)?themeisle\\.com" all-the-icons-faicon "themeisle")
		("^\\(https?://\\)?\\(www\\.\\)?trello\\.com" all-the-icons-faicon "trello")
		("^\\(https?://\\)?\\(www\\.\\)?whatsapp\\.com" all-the-icons-faicon "whatsapp")
		("^\\(https?://\\)?\\(www\\.\\)?ycombinator\\.com" all-the-icons-faicon "y-combinator")
		("yahoo\\.com" all-the-icons-faicon "yahoo")
		("^\\(https?://\\)?\\(www\\.\\)?yoast\\.com" all-the-icons-faicon "yoast")
		; Catch all
		("android" all-the-icons-faicon "android")
		("creativecommons" all-the-icons-faicon "creative-commons")
		("forums?" all-the-icons-octicon "comment-discussion")
		("\\.pdf$" all-the-icons-octicon "file-pdf" :v-adjust 0.0 :face all-the-icons-dred)
		("google" all-the-icons-faicon "google")
		("\\.rss" all-the-icons-faicon "rss")
	)
)


(defun allTheIcons::find_regex_in_alist (file alist)
"Match FILE against an entry in ALIST using `string-match-p'."
	(cdr (assoc file alist #'string-match-p)))

(defun allTheIcons::get_raw_icon_merged_with_overrides (raw_icon overrides)
	(if overrides
		(append (list (pop raw_icon) (car raw_icon)) overrides (cdr raw_icon))
		raw_icon))

; Main functions.

(defun all-the-icons-icon-for-dir (dir &rest overrides)
"Get the formatted icon for DIR.
OVERRIDES should be a plist containining `:height',
`:v-adjust' or `:face' properties like in the normal icon inserting functions.

Note: If you want chevron, please use `all-the-icons-icon-for-dir-with-chevron'."
	(let ((raw_icon
			(allTheIcons::get_raw_icon_merged_with_overrides
				(allTheIcons::find_regex_in_alist (file-name-base (directory-file-name dir)) all-the-icons-dir-icon-alist)
				overrides)))
		(if (file-remote-p dir) ; Don't call expand-file-name on a remote dir as this can make emacs hang.
			(apply #'all-the-icons-octicon "terminal" (nthcdr 2 raw_icon))
			(let ((path (expand-file-name dir)))
				(cond
					((file-symlink-p path)
						(apply #'all-the-icons-octicon "file-symlink-directory" (nthcdr 2 raw_icon)))
					; If directory is a git submodule.
					((if-let ((gitmodule_dir (locate-dominating-file path ".gitmodules"))
							  ((file-exists-p (format "%s/.git" path))))
							(withTempBuffer
								(insert-file-contents (expand-file-name (concat gitmodule_dir ".gitmodules")))
								(re-search-forward (concat "submodule \".*?" (file-name-base path) "\"") nil t)))
						(apply #'all-the-icons-octicon "file-submodule" (nthcdr 2 raw_icon)))
					((file-exists-p (concat path "/.git"))
						(apply #'all-the-icons-octicon "repo" (nthcdr 2 raw_icon)))
					(t (apply (car raw_icon) (cdr raw_icon))))))))

(defun all-the-icons-icon-for-dir-with-chevron (dir &optional chevron padding)
"Format an icon for DIR with CHEVRON similar to tree based directories.

If PADDING is provided, it will prepend and separate
the chevron and directory with PADDING."
	(unless padding (setq padding "\t"))
	(concat
		padding
		(if chevron
			(all-the-icons-octicon (concat "chevron-" chevron) :height 0.8 :v-adjust -0.1))
		padding
		(all-the-icons-icon-for-dir dir)
		padding))

(defun allTheIcons::get_raw_icon_for_file (file)
	(or
		(allTheIcons::find_regex_in_alist file all-the-icons-regexp-icon-alist)
		(when-let ((extension (file-name-extension file)))
			(cdr (assoc (downcase extension) allTheIcons::iconAlist::extension)))
		allTheIcons::DEFAULT_FILE_ICON))

(defun allTheIcons::get_raw_icon_for_mode (mode)
	(or
		(cdr (assq mode all-the-icons-mode-icon-alist))
		(cdr (assq (get mode 'derived-mode-parent) all-the-icons-mode-icon-alist))
		allTheIcons::DEFAULT_MODE_ICON))

(defun all-the-icons-icon-for-file (file &rest overrides)
"Get the formatted icon for FILE.
overrides should be a plist containining `:height',
`:v-adjust' or `:face' properties like in the normal icon inserting functions."
	(let ((raw_icon
			(allTheIcons::get_raw_icon_merged_with_overrides
				(allTheIcons::get_raw_icon_for_file file)
				overrides)))
		(apply (car raw_icon) (cdr raw_icon))))

(defun all-the-icons-icon-for-mode (mode &rest overrides)
"Get the formatted icon for MODE.
overrides should be a plist containining `:height',
`:v-adjust' or `:face' properties like in the normal icon inserting functions."
	(let ((raw_icon
			(allTheIcons::get_raw_icon_merged_with_overrides
				(allTheIcons::get_raw_icon_for_mode mode)
				overrides)))
		(apply (car raw_icon) (cdr raw_icon))))

(defun all-the-icons-icon-for-url (url &rest overrides)
"Get the formatted icon for URL.
overrides should be a plist containining `:height',
`:v-adjust' or `:face' properties like in the normal icon inserting functions."
	(let ((raw_icon
			(allTheIcons::get_raw_icon_merged_with_overrides
				(or
					(allTheIcons::find_regex_in_alist url all-the-icons-url-alist)
					allTheIcons::DEFAULT_URL_ICON)
				overrides)))
		(apply (car raw_icon) (cdr raw_icon))))

(defun all-the-icons-icon-for-weather (weather)
"Get an icon for a WEATHER status, or nil."
	(if-let ((raw_icon
				(allTheIcons::find_regex_in_alist weather all-the-icons-weather-icon-alist)))
		(apply (car raw_icon) (cdr raw_icon))))

; Functions for easily getting probably the best icon for current buffer.

; They take icon for file only if buffer's mode matches
; auto-mode-alist entry for it's file, otherwise they take icon for mode.
; NOT TRUE ANYMORE - now they just take icon for file if buffer has a file,
; otherwise icon for mode.
; This is because auto-mode-alist has entries like c-or-c++-mode, so it's
; not that easy to check if buffer's mode match it.

(defun all-the-icons--icon-info-for-buffer (&optional f)
"Get icon info for the current buffer.
When F is provided, the info function is calculated with the format
`all-the-icons-icon-%s-for-file' or `all-the-icons-icon-%s-for-mode'."
	(let ((base-f (concat "all-the-icons-icon" (when f (concat "-" f)))))
		(if buffer-file-name
			(funcall (intern (concat base-f "-for-file"))
				(file-name-nondirectory buffer-file-name))
			(funcall (intern (concat base-f "-for-mode")) major-mode))))

(defun all-the-icons-icon-for-buffer ()
"Get the formatted icon for the current buffer.

If buffer has file, take icon for it, otherwise take icon for buffer's mode."
	(all-the-icons--icon-info-for-buffer))

(defun all-the-icons-icon-family-for-buffer ()
"Get the icon font family for the current buffer.

If buffer has file, take icon's family for it, otherwise take icon's family for buffer's mode."
	(all-the-icons--icon-info-for-buffer "family"))

; Getting :family property for icons.

(defun all-the-icons-icon-family-for-file (file) "Get the icons font family for FILE."
	(funcall (intern (concat (symbol-name (car (allTheIcons::get_raw_icon_for_file file))) "-family"))))

(defun all-the-icons-icon-family-for-mode (mode) "Get the icons font family for MODE."
	(funcall (intern (concat (symbol-name (car (allTheIcons::get_raw_icon_for_mode mode))) "-family"))))

(defun all-the-icons-icon-family (icon) "Get :family attribute from a propertized string icon."
	(plist-get (get-text-property 0 'face icon) :family))

; Add some crappy cache to functions.
; Even order of arguments to those functions, that normally doesn't matter,
; doesn't match the cache.

(defconst allTheIcons::CACHE_SIZE_LIMIT 2048
"Maximum cache size for functions cached below.
Default: 2048.")

(dolist (
	fn_symbol
	(list
		#'all-the-icons-icon-for-dir
		#'all-the-icons-icon-for-file
		#'all-the-icons-icon-for-mode
		#'all-the-icons-icon-for-url
		#'all-the-icons-icon-family-for-file
		#'all-the-icons-icon-family-for-mode
		#'all-the-icons-icon-family)
)
	(fset fn_symbol
		(let ((cache (make-hash-table :test 'equal :size allTheIcons::CACHE_SIZE_LIMIT)))
			`(lambda (&rest args)
				(or
					(gethash args ,cache)
					(progn
						(when (> (hash-table-count ,cache) allTheIcons::CACHE_SIZE_LIMIT)
							(clrhash ,cache))
						(puthash
							args (apply #',(symbol-function fn_symbol) args) ,cache)))))))

; Web crap.

(defvar web-mode-content-type) ; Silence byte-compiler warning.

(defun all-the-icons--web-mode (&optional family overrides)
"Return icon or FAMILY for `web-mode' based on `web-mode-content-type'.
Providing overrides will modify the creation of the icon."
	(let ((non-nil-args
			(cl-reduce
				(lambda (acc it) (if it (append acc (list it)) acc))
				overrides
				:initial-value nil)))
		(cond
			((equal web-mode-content-type "jsx")
				(if family
					(all-the-icons-fileicon-family)
					(apply #'all-the-icons-fileicon "jsx-2" non-nil-args)))
			((equal web-mode-content-type "javascript")
				(if family
					(all-the-icons-alltheicon-family)
					(apply #'all-the-icons-alltheicon "javascript" non-nil-args)))
			((equal web-mode-content-type "json")
				(if family
					(all-the-icons-alltheicon-family)
					(apply #'all-the-icons-alltheicon "less" non-nil-args)))
			((equal web-mode-content-type "xml")
				(if family
					(all-the-icons-faicon-family)
					(apply #'all-the-icons-faicon "file-code-o" non-nil-args)))
			((equal web-mode-content-type "css")
				(if family
					(all-the-icons-alltheicon-family)
					(apply #'all-the-icons-alltheicon "css3" non-nil-args)))
			(t
				(if family
					(all-the-icons-alltheicon-family)
					(apply #'all-the-icons-alltheicon "html5" non-nil-args))))))

(defun all-the-icons--web-mode-icon (&rest overrides)
"Get icon for a `web-mode' buffer with overrides."
	(all-the-icons--web-mode nil overrides))
(defun all-the-icons--web-mode-icon-family ()
"Get icon family for a `web-mode' buffer."
	(all-the-icons--web-mode t))

; Icon insertion functions.

(defun all-the-icons--function-name (name)
"Get the symbol for an icon function name for icon set NAME."
	(intern (concat "all-the-icons-" (downcase (symbol-name name)))))

(defun all-the-icons--family-name (name)
"Get the symbol for an icon family function for icon set NAME."
	(intern (concat "all-the-icons-" (downcase (symbol-name name)) "-family")))

(defun all-the-icons--data-name (name)
"Get the symbol for an icon family function for icon set NAME."
	(intern (concat "all-the-icons-" (downcase (symbol-name name)) "-data")))

(defun all-the-icons--insert-function-name (name)
"Get the symbol for an icon insert function for icon set NAME."
	(intern (concat "all-the-icons-insert-" (downcase (symbol-name name)))))

(defun all-the-icons--family-scale-factor (family)
	(intern (concat "all-the-icons-" (symbol-name family) "-scale-factor")))

(defun all-the-icons--family-adjust (family)
	(intern (concat "all-the-icons-default-" (symbol-name family) "-adjust")))


(defun all-the-icons--read-candidates-for-family (family &optional show-family)
"Helper to build read candidates for FAMILY.
If SHOW-FAMILY is non-nil, displays the icons family in the candidate string."
	(let ((data (funcall (all-the-icons--data-name family)))
		  (icon-f (all-the-icons--function-name family)))
		(mapcar
			(lambda (it)
				(let* ((icon-name (car it))
					   (icon-name-head (substring icon-name 0 1))
					   (icon-name-tail (substring icon-name 1))
					   (icon-display (propertize icon-name-head 'display (format "%s\t%s" (funcall icon-f icon-name) icon-name-head)))
					   (icon-family (if show-family (format "\t[%s]" family) ""))
					   (candidate-name (format "%s%s%s" icon-display icon-name-tail icon-family))
					   (candidate-icon (funcall (all-the-icons--function-name family) icon-name)))
					(cons candidate-name candidate-icon)))
			data)))

(defvar all-the-icons-font-families nil "List of defined icon font families.")

(defun all-the-icons-insert (&optional arg family)
"Interactive icon insertion function.
When Prefix ARG is non-nil, insert the propertized icon.
When FAMILY is non-nil, limit the candidates to the icon set matching it."
	(interactive "P")
	(let* (
		(standard-output (current-buffer))
		(candidates
			(if family
				(all-the-icons--read-candidates-for-family family)
				(cl-reduce #'append
					(mapcar
						(lambda (it) (all-the-icons--read-candidates-for-family it t))
						all-the-icons-font-families))))
		(prompt
			(if family
				(format "%s Icon: " (funcall (all-the-icons--family-name family)))
				"Icon: "))
		(selection (completing-read prompt candidates nil t))
		(result (cdr (assoc selection candidates))))
		(if arg
			(prin1 result)
			(insert result))))

(defun all-the-icons-insert-icons-for (family &optional height duration)
"Insert all of the available icons associated with FAMILY.
If a HEIGHT is provided it will render the icons at this height.
This is useful both to see the icons more clearly and to test
different height rendering. If DURATION is provided, it will
pause for DURATION seconds between printing each character."
	(let* ((data-f (all-the-icons--data-name family))
		   (insert-f (all-the-icons--function-name family))
		   (height (or height 2.0))
		   (data (funcall data-f)))
		(mapc
			(lambda (it)
				(insert (format "%s - %s\n" (funcall insert-f (car it) :height height) (car it)))
				(if duration
					(sit-for duration)))
			data)))

; Align icons.

(defconst all-the-icons-align-cache (make-hash-table :test 'equal)
"Hash table for icons width.
Value - pixel width, key - [char family height].")

(defun all-the-icons-align (icon &optional width)
"Return a new string with ICON centered horizontally in WIDTH space.
`all-the-icons-align-width' is the default value of WIDTH.
This is done by creating a new string with ICON in the middle,
surrounded by two identical spaces with \\='display \\='(:space width X)
properties.

ICON should have the usual face property structure with (:family F :height H),
but actually even simple chars in for example default face will somewhat work too."
	(unless width (setq width all-the-icons-align-width))
	(let (
		(space
			(propertize " "
				'display
				`(
					space
					:width
					(
						,(max
							0
							(/
								(-
									width
									(let (
										(key
											(let ((face (get-text-property 0 'face icon)))
												(vector
													(string-to-char icon)
													(plist-get face :family)
													(plist-get face :height))))
									)
										(or
											(gethash key all-the-icons-align-cache)
											(puthash
												key
												(get_string_pixel_width icon)
												all-the-icons-align-cache))))
								2.0))
					)
				)))
	)
		(concat space icon space)))

; Define functions.

(defmacro all-the-icons-define-icon (name alist family &optional font-name)
"Macro to generate functions for inserting icons for icon set NAME.

NAME defines is the name of the iconset and will produce a
function of the for `all-the-icons-NAME'.

ALIST is the alist containing maps between icon names and the
unicode for the character. All of these can be found in the data
directory of this package.

FAMILY is the font family to use for the icons.
FONT-NAME is the name of the .ttf file providing the font, defaults to FAMILY."
	`(progn
		(push ',name all-the-icons-font-families)
		(defconst ,(all-the-icons--family-scale-factor name) 1.0
			,(format "The additional `height' face property Scale Factor for %s icons. Default: 1.0."
				(symbol-name name)))
		(defconst ,(all-the-icons--family-adjust name) 0.0
			,(format "The additional `raise' display property adjustment for %s icons. Default: 0.0."
				(symbol-name name)))
		(defun ,(all-the-icons--family-name name) () ,family)
		(defun ,(all-the-icons--data-name name) () ,alist)
		(defun ,(all-the-icons--function-name name) (icon-name &rest args)
			(let* ((icon
					(or
						(cdr (assoc icon-name ,alist))
						(error
							(format "Unable to find icon with name `%s' in icon set `%s'"
								icon-name ',name))))
				   (other-face (if all-the-icons-color-icons (plist-get args :face)))
				   (height
					(*
						all-the-icons-scale-factor
						,(all-the-icons--family-scale-factor name)
						(or (plist-get args :height) 1.0)))
				   (v-adjust
					(*
						all-the-icons-scale-factor
						,(all-the-icons--family-scale-factor name)
						(+
							(or (plist-get args :v-adjust) all-the-icons-default-adjust)
							,(all-the-icons--family-adjust name))))
				   (face
					(if other-face
						(list :inherit other-face :family ,family :height height)
						(list :family ,family :height height))))
				(propertize icon
					'face face ; So that this works without `font-lock-mode' enabled.
					'font-lock-face face ; So that `font-lock-mode' leaves this alone.
					'display (list 'raise v-adjust)
					'rear-nonsticky t)))
		(defun ,(all-the-icons--insert-function-name name) (&optional arg)
			,(format "Insert a %s icon at point." family)
			(interactive "P")
			(all-the-icons-insert arg ',name))))

(all-the-icons-define-icon alltheicon all-the-icons-data/alltheicons-alist    "all-the-icons")
(all-the-icons-define-icon fileicon   all-the-icons-data/file-icon-alist      "file-icons")
(all-the-icons-define-icon faicon     all-the-icons-data/fa-icon-alist        "FontAwesome")
(all-the-icons-define-icon octicon    all-the-icons-data/octicons-alist       "github-octicons" "octicons")
(all-the-icons-define-icon wicon      all-the-icons-data/weather-icons-alist  "Weather Icons"   "weathericons")
(all-the-icons-define-icon material   all-the-icons-data/material-icons-alist "Material Icons"  "material-design-icons")

(unintern 'all-the-icons-define-icon nil)

(provide 'all-the-icons)
