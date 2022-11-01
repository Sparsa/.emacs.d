# Emacs Configuration For \LaTeX Documents
This is a very simple emacs configuration to process \LaTeX documents,
in order to use it, just clone this repo to your home directory which
will create a .emacs.d directory and it contains two files, this
README.md and init.el
## Package Requirements
This configuration now uses Emacs application frame work services.
Please install Emacs application framework as instructed in the
repository:
https://github.com/emacs-eaf/emacs-application-framework

This configuration requires some additional melpa packages, and all of them should be
installed automatically when the emacs is started with this
configuration, here I list some of the key packages this
configuration uses.
### Auctex
	The most important package in the latex editing mode. It detects
	*.tex file and enables \LaTeX mode. It provides nice shortcuts for
	compiling,
	C-c C-a --- will automatically save, compile and show the document
	in another buffer
	C-c C-c will only compile/ if compiled it will display
	C-c ` will show the errors during compilation

### Synctex

	This package is responsible for interlinking the text and the pdf,
	when you double click a word/letter in pdf it should take you to
	the text at that particular position.
	Similarly pressing C-g C-a will point the respective location in
	the pdf document, it is not always accurate but is is enough to
	work with.

### reftex 
	This is useful for reference management system, if you use bibtex/biber
	for reference management.
	C-c [ will start reg-ex search for the bibtex entries
### aspell,flyspell 
	This packages will automatically check for any spelling mistakes
	in the \LaTeX document
### Flymake
	This package will check syntax errors on the fly
### Company
	This package will show possible completion of commands math
	symbols, citations or labels. 
	For example, if you write \ref{abc, then it will give possible
	completion of abc from the bibliographic entry.
	Similarly in math mode it will try to complete \alpha when you
	write \alph, it works even for the user defined formulas.
	
	
Some other interesting features of this configuration is:
	* It check for updates of the packages every 14 days
	* It asks for your permission before updating the packages
	
