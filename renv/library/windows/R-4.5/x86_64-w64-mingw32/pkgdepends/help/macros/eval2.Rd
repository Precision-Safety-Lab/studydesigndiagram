\renewcommand{\eval}{\Sexpr[stage=render,results=rd]{pkgdepends:::#1}}
\renewcommand{\evalatinstall}{\Sexpr[stage=install,results=rd]{pkgdepends:::#1}}
\renewcommand{\evaltop}{\Sexpr[stage=render,results=rd]{pkgdepends:::#1}}
\renewcommand{\evalatinstalltop}{\Sexpr[stage=install,results=rd]{pkgdepends:::#1}}
