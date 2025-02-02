; -*- lexical-binding:nil -*-

(setq-default auto-mode-alist '(
("\\.gpg\\(~\\|\\.~[0-9]+~\\)?\\'" nil epa-file)
("\\.elc\\'" . elisp-byte-code-mode)
("\\.zst\\'" nil jka-compr)
("\\.dz\\'" nil jka-compr)
("\\.xz\\'" nil jka-compr)
("\\.lzma\\'" nil jka-compr)
("\\.lz\\'" nil jka-compr)
("\\.g?z\\'" nil jka-compr)
("\\.bz2\\'" nil jka-compr)
("\\.Z\\'" nil jka-compr)
("\\.vr[hi]?\\'" . vera-mode)
("\\(?:\\.\\(?:rbw?\\|ru\\|rake\\|thor\\|jbuilder\\|rabl\\|gemspec\\|podspec\\)\\|/\\(?:Gem\\|Rake\\|Cap\\|Thor\\|Puppet\\|Berks\\|Brew\\|Vagrant\\|Guard\\|Pod\\)file\\)\\'" . ruby-mode)
("\\.re?st\\'" . rst-mode)
("\\.py[iw]?\\'" . python-mode)
("\\.m\\'" . octave-maybe-mode)
("\\.less\\'" . less-css-mode)
("\\.scss\\'" . scss-mode)
("\\.cs\\'" . csharp-mode)
("\\.awk\\'" . awk-mode)
("\\.\\(u?lpc\\|pike\\|pmod\\(\\.in\\)?\\)\\'" . pike-mode)
("\\.idl\\'" . idl-mode)
("\\.java\\'" . java-mode)
("\\.m\\'" . objc-mode)
("\\.ii\\'" . c++-mode)
("\\.i\\'" . c-mode)
("\\.lex\\'" . c-mode)
("\\.y\\(acc\\)?\\'" . c-mode)
("\\.h\\'" . c-or-c++-mode)
("\\.c\\'" . c-mode)
("\\.\\(ixx\\|h\\(pp\\|xx\\|\\+\\+\\)?\\|c\\(pp\\|xx\\|\\+\\+\\)m?\\)\\'" . c++-mode)
("\\.\\(bat\\|cmd\\)\\'" . bat-mode)
("\\.[sx]?html?\\(\\.[a-zA-Z_]+\\)?\\'" . mhtml-mode)
(#1="\\.svgz?\\'" . image-mode)
(#1# . xml-mode)
(#2="\\.x[bp]m\\'" . image-mode)
(#2# . c-mode)
("\\.p[bpgn]m\\'" . image-mode)
("\\.tiff?\\'" . image-mode)
("\\.gif\\'" . image-mode)
("\\.png\\'" . image-mode)
("\\.jpe?g\\'" . image-mode)
(#3="\\.webp\\'" . image-mode)
("\\.te?xt\\'" . text-mode)
("\\.[tT]e[xX]\\'" . tex-mode)
("\\.ins\\'" . tex-mode)
("\\.ltx\\'" . latex-mode)
("\\.dtx\\'" . doctex-mode)
("\\.org\\'" . org-mode)
("\\.dir-locals\\(?:-2\\)?\\.el\\'" . lisp-data-mode)
("\\.eld\\'" . lisp-data-mode)
("eww-bookmarks\\'" . lisp-data-mode)
("tramp\\'" . lisp-data-mode)
("/archive-contents\\'" . lisp-data-mode)
("places\\'" . lisp-data-mode)
("\\.emacs-places\\'" . lisp-data-mode)
("\\.el\\'" . emacs-lisp-mode)
("Project\\.ede\\'" . emacs-lisp-mode)
("\\.\\(scm\\|sls\\|sld\\|stk\\|ss\\|sch\\)\\'" . scheme-mode)
("\\.l\\'" . lisp-mode)
("\\.li?sp\\'" . lisp-mode)
("\\.[fF]\\'" . fortran-mode)
("\\.for\\'" . fortran-mode)
("\\.p\\'" . pascal-mode)
("\\.pas\\'" . pascal-mode)
("\\.\\(dpr\\|DPR\\)\\'" . delphi-mode)
("\\.\\([pP]\\([Llm]\\|erl\\|od\\)\\|al\\)\\'" . perl-mode)
("Imakefile\\'" . makefile-imake-mode)
("Makeppfile\\(?:\\.mk\\)?\\'" . makefile-makepp-mode)
("\\.makepp\\'" . makefile-makepp-mode)
("\\.mk\\'" . makefile-gmake-mode)
("\\.make\\'" . makefile-gmake-mode)
("[Mm]akefile\\'" . makefile-gmake-mode)
("\\.am\\'" . makefile-automake-mode)
("\\.texinfo\\'" . texinfo-mode)
("\\.te?xi\\'" . texinfo-mode)
("\\.[sS]\\'" . asm-mode)
("\\.asm\\'" . asm-mode)
("\\.css\\'" . css-mode)
("\\.mixal\\'" . mixal-mode)
("\\.gcov\\'" . compilation-mode)
("/\\.[a-z0-9-]*gdbinit" . gdb-script-mode)
("-gdb\\.gdb" . gdb-script-mode)
("[cC]hange\\.?[lL]og?\\'" . change-log-mode)
("[cC]hange[lL]og[-.][0-9]+\\'" . change-log-mode)
("\\$CHANGE_LOG\\$\\.TXT" . change-log-mode)
("\\.scm\\.[0-9]*\\'" . scheme-mode)
("\\.[ckz]?sh\\'\\|\\.shar\\'\\|/\\.z?profile\\'" . sh-mode)
("\\.bash\\'" . sh-mode)
("/PKGBUILD\\'" . sh-mode)
("\\(/\\|\\`\\)\\.\\(bash_\\(profile\\|history\\|log\\(in\\|out\\)\\)\\|z?log\\(in\\|out\\)\\)\\'" . sh-mode)
("\\(/\\|\\`\\)\\.\\(shrc\\|zshrc\\|m?kshrc\\|bashrc\\|t?cshrc\\|esrc\\)\\'" . sh-mode)
("\\(/\\|\\`\\)\\.\\([kz]shenv\\|xinitrc\\|startxrc\\|xsession\\)\\'" . sh-mode)
("\\.m?spec\\'" . sh-mode)
("\\.m[mes]\\'" . nroff-mode)
("\\.man\\'" . nroff-mode)
("\\.sty\\'" . latex-mode)
("\\.cl[so]\\'" . latex-mode)
("\\.bbl\\'" . latex-mode)
("\\.bib\\'" . bibtex-mode)
("\\.bst\\'" . bibtex-style-mode)
("\\.sql\\'" . sql-mode)
("\\(acinclude\\|aclocal\\|acsite\\)\\.m4\\'" . autoconf-mode)
("\\.m[4c]\\'" . m4-mode)
("\\.mf\\'" . metafont-mode)
("\\.mp\\'" . metapost-mode)
("\\.vhdl?\\'" . vhdl-mode)
("\\.article\\'" . text-mode)
("\\.letter\\'" . text-mode)
("\\.i?tcl\\'" . tcl-mode)
("\\.exp\\'" . tcl-mode)
("\\.itk\\'" . tcl-mode)
("\\.icn\\'" . icon-mode)
("\\.sim\\'" . simula-mode)
("\\.mss\\'" . scribe-mode)
("\\.f9[05]\\'" . f90-mode)
("\\.f0[38]\\'" . f90-mode)
("\\.indent\\.pro\\'" . fundamental-mode)
("\\.\\(pro\\|PRO\\)\\'" . idlwave-mode)
("\\.srt\\'" . srecode-template-mode)
("\\.prolog\\'" . prolog-mode)
("\\.tar\\'" . tar-mode)
("\\.\\(arc\\|zip\\|lzh\\|lha\\|zoo\\|[jew]ar\\|xpi\\|rar\\|cbr\\|7z\\|squashfs\\|ARC\\|ZIP\\|LZH\\|LHA\\|ZOO\\|[JEW]AR\\|XPI\\|RAR\\|CBR\\|7Z\\|SQUASHFS\\)\\'" . archie-mode)
("\\.oxt\\'" . archive-mode)
("\\.\\(deb\\|[oi]pk\\)\\'" . archive-mode)
("\\`/tmp/Re" . text-mode)
("/Message[0-9]*\\'" . text-mode)
("\\`/tmp/fol/" . text-mode)
("\\.oak\\'" . scheme-mode)
("\\.sgml?\\'" . sgml-mode)
("\\.x[ms]l\\'" . xml-mode)
("\\.dbk\\'" . xml-mode)
("\\.dtd\\'" . sgml-mode)
("\\.ds\\(ss\\)?l\\'" . dsssl-mode)
("\\.js[mx]?\\'" . javascript-mode)
("\\.har\\'" . javascript-mode)
("\\.json\\'" . js-json-mode)
("\\.[ds]?va?h?\\'" . verilog-mode)
("\\.by\\'" . bovine-grammar-mode)
("\\.wy\\'" . wisent-grammar-mode)
("\\.erts\\'" . erts-mode)
("[:/\\]\\..*\\(emacs\\|gnus\\|viper\\)\\'" . emacs-lisp-mode)
("\\`\\..*emacs\\'" . emacs-lisp-mode)
("[:/]_emacs\\'" . emacs-lisp-mode)
("/crontab\\.X*[0-9]+\\'" . shell-script-mode)
("\\.ml\\'" . lisp-mode)
("\\.ld[si]?\\'" . ld-script-mode)
("ld\\.?script\\'" . ld-script-mode)
("\\.xs\\'" . c-mode)
("\\.x[abdsru]?[cnw]?\\'" . ld-script-mode)
("\\.zone\\'" . dns-mode)
("\\.soa\\'" . dns-mode)
("\\.asd\\'" . lisp-mode)
("\\.\\(asn\\|mib\\|smi\\)\\'" . snmp-mode)
("\\.\\(as\\|mi\\|sm\\)2\\'" . snmpv2-mode)
("\\.\\(diffs?\\|patch\\|rej\\)\\'" . diff-mode)
("\\.\\(dif\\|pat\\)\\'" . diff-mode)
("\\.[eE]?[pP][sS]\\'" . ps-mode)
("\\.\\(?:PDF\\|EPUB\\|CBZ\\|FB2\\|O?XPS\\|DVI\\|OD[FGPST]\\|DOCX\\|XLSX?\\|PPTX?\\|pdf\\|epub\\|cbz\\|fb2\\|o?xps\\|djvu\\|dvi\\|od[fgpst]\\|docx\\|xlsx?\\|pptx?\\)\\'" . doc-view-mode-maybe)
("configure\\.\\(ac\\|in\\)\\'" . autoconf-mode)
("\\.s\\(v\\|iv\\|ieve\\)\\'" . sieve-mode)
("BROWSE\\'" . ebrowse-tree-mode)
("\\.ebrowse\\'" . ebrowse-tree-mode)
("#\\*mail\\*" . mail-mode)
("\\.g\\'" . antlr-mode)
("\\.mod\\'" . m2-mode)
("\\.ses\\'" . ses-mode)
("\\.docbook\\'" . sgml-mode)
("\\.com\\'" . dcl-mode)
("/config\\.\\(?:bat\\|log\\)\\'" . fundamental-mode)
("/\\.\\(authinfo\\|netrc\\)\\'" . authinfo-mode)
("\\.\\(?:[iI][nN][iI]\\|[lL][sS][tT]\\|[rR][eE][gG]\\|[sS][yY][sS]\\)\\'" . conf-mode)
("\\.la\\'" . conf-unix-mode)
("\\.ppd\\'" . conf-ppd-mode)
("java.+\\.conf\\'" . conf-javaprop-mode)
("\\.properties\\(?:\\.[a-zA-Z0-9._-]+\\)?\\'" . conf-javaprop-mode)
("\\.toml\\'" . conf-toml-mode)
("\\.desktop\\'" . conf-desktop-mode)
("/\\.redshift\\.conf\\'" . conf-windows-mode)
("\\`/etc/\\(?:DIR_COLORS\\|ethers\\|.?fstab\\|.*hosts\\|lesskey\\|login\\.?de\\(?:fs\\|vperm\\)\\|magic\\|mtab\\|pam\\.d/.*\\|permissions\\(?:\\.d/.+\\)?\\|protocols\\|rpc\\|services\\)\\'" . conf-space-mode)
("\\`/etc/\\(?:acpid?/.+\\|aliases\\(?:\\.d/.+\\)?\\|default/.+\\|group-?\\|hosts\\..+\\|inittab\\|ksysguarddrc\\|opera6rc\\|passwd-?\\|shadow-?\\|sysconfig/.+\\)\\'" . conf-mode)
("[cC]hange[lL]og[-.][-0-9a-z]+\\'" . change-log-mode)
("/\\.?\\(?:gitconfig\\|gnokiirc\\|hgrc\\|kde.*rc\\|mime\\.types\\|wgetrc\\)\\'" . conf-mode)
("/\\.mailmap\\'" . conf-unix-mode)
("/\\.\\(?:asound\\|enigma\\|fetchmail\\|gltron\\|gtk\\|hxplayer\\|mairix\\|mbsync\\|msmtp\\|net\\|neverball\\|nvidia-settings-\\|offlineimap\\|qt/.+\\|realplayer\\|reportbug\\|rtorrent\\.\\|screen\\|scummvm\\|sversion\\|sylpheed/.+\\|xmp\\)rc\\'" . conf-mode)
("/\\.\\(?:gdbtkinit\\|grip\\|mpdconf\\|notmuch-config\\|orbital/.+txt\\|rhosts\\|tuxracer/options\\)\\'" . conf-mode)
("/\\.?X\\(?:default\\|resource\\|re\\)s\\>" . conf-xdefaults-mode)
("/X11.+app-defaults/\\|\\.ad\\'" . conf-xdefaults-mode)
("/X11.+locale/.+/Compose\\'" . conf-colon-mode)
("/X11.+locale/compose\\.dir\\'" . conf-javaprop-mode)
("\\.~?[0-9]+\\.[0-9][-.0-9]*~?\\'" nil t)
("\\.\\(?:orig\\|in\\|[bB][aA][kK]\\)\\'" nil t)
("[/.]c\\(?:on\\)?f\\(?:i?g\\)?\\(?:\\.[a-zA-Z0-9._-]+\\)?\\'" . conf-mode-maybe)
("\\.[1-9]\\'" . nroff-mode)
("\\.art\\'" . image-mode)
("\\.avs\\'" . image-mode)
("\\.bmp\\'" . image-mode)
("\\.cmyk\\'" . image-mode)
("\\.cmyka\\'" . image-mode)
("\\.crw\\'" . image-mode)
("\\.dcr\\'" . image-mode)
("\\.dcx\\'" . image-mode)
("\\.dng\\'" . image-mode)
("\\.dpx\\'" . image-mode)
("\\.fax\\'" . image-mode)
("\\.heic\\'" . image-mode)
("\\.hrz\\'" . image-mode)
("\\.icb\\'" . image-mode)
("\\.icc\\'" . image-mode)
("\\.icm\\'" . image-mode)
("\\.ico\\'" . image-mode)
("\\.icon\\'" . image-mode)
("\\.jbg\\'" . image-mode)
("\\.jbig\\'" . image-mode)
("\\.jng\\'" . image-mode)
("\\.jnx\\'" . image-mode)
("\\.miff\\'" . image-mode)
("\\.mng\\'" . image-mode)
("\\.mvg\\'" . image-mode)
("\\.otb\\'" . image-mode)
("\\.p7\\'" . image-mode)
("\\.pcx\\'" . image-mode)
("\\.pdb\\'" . image-mode)
("\\.pfa\\'" . image-mode)
("\\.pfb\\'" . image-mode)
("\\.picon\\'" . image-mode)
("\\.pict\\'" . image-mode)
("\\.rgb\\'" . image-mode)
("\\.rgba\\'" . image-mode)
("\\.tga\\'" . image-mode)
("\\.wbmp\\'" . image-mode)
(#3# . image-mode)
("\\.wmf\\'" . image-mode)
("\\.wpg\\'" . image-mode)
("\\.xcf\\'" . image-mode)
("\\.xmp\\'" . image-mode)
("\\.xwd\\'" . image-mode)
("\\.yuv\\'" . image-mode)
("\\.tgz\\'" . tar-mode)
("\\.tbz2?\\'" . tar-mode)
("\\.txz\\'" . tar-mode)
("\\.tzst\\'" . tar-mode)))

(provide 'myFileAssociations)
