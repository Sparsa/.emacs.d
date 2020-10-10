# Emacs Configuration For \LaTeX Documents
This is a very simple emacs configuration to process \LaTeX documents,
in order to use it, just clone this repo to your home directory which
will create a .emacs.d directory and it contains two files, this
README.md and init.el
## Package Requirements
This configuration requires some packages, and all of them should be
installed automatically when the emacs is started with this
configurations, here I list some of the key packages this
configuration uses.
### Auctex
	The most important package in the latex editing mode. It detects
	*.tex file and enables \LaTeX mode. It provides nice shortcuts for
	compiling,
	C-c C-a --- will automatically save, compile and show the document
	in another buffer
	C-c C-c will only compile/ if compiled it will display
	C-c ` will show the errors during compilation
### Pdf-view 
	This package is responsible for the pdf-viewer inside emacs, you
	can easily open any pdf document in emacs and read
	in this mode press 's' then 'b' to strip margins of a pdf while
	viewing
### Pdf-view continuous 

Now enjoy continuous pdf scrolling experience on Pdf-tools (pdf-view) thanks to https://github.com/dalanicolai/pdf-continuous-scroll-mode.el. 

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

Some other interesting features of this configuration is:
	* It check for updates of the packages every 14 days
	* It asks for your permission before updating the packages
	
