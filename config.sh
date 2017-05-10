DIR=$(dirname $0)

if [ $(ocaml $DIR/check402.ml) = ok ]; then
    rm -f $DIR/bytes.ml $DIR/bytes.mli
else
    cp $DIR/bytes.mlp $DIR/bytes.ml
    cp $DIR/bytes.mlip $DIR/bytes.mli
fi
PGM="hevea.byte hacha.byte esponja.byte bibhva.byte"
PGMNATIVE="hevea.native hacha.native esponja.native bibhva.native"
BINDIR=/usr/local/bin
LIBDIR=/usr/local/lib/hevea
LATEXLIBDIR=/usr/local/lib/hevea
OCAMLFLAGS="-w +a-4-9-41-45"
OCBFLAGS="-j 4 -classic-display"
ALLLIB="alltt.hva amsmath.hva articlecommon.hva babel.hva bookcommon.hva booktabs.hva comment.hva compat.hva hyperref.hva hrlang.hva ifthen.hva index.hva iso-symb.hva keyval.hva latexcommon.hva listings.hva lstlang1.hva lstlang2.hva lstlang3.hva makeidx.hva mathop.hva moreverb.hva multibib.hva multind.hva natbib-common.hva packages.hva plain.hva program.hva spaces.hva supertabular.hva underscore.hva url.hva verbatim.hva french-common.hva german-common.hva english.hva czech.hva portuguese.hva ragged2e.hva chapterbib.hva deepcut.hva figcut.hva longtable.hva eurosym.hva isolatin1.hva textcomp.hva chngcntr.hva ifpdf.hva theorem.hva xspace.hva latexsym.hva iso-html.hva iso-text.hva winstyles.hva winfonts.hva epsfig.hva inputenc.hva thai.hva import.hva hanging.hva lstlang1.sty lstlang2.sty lstlang3.sty labeltype.hva crlang.hva cleveref.hva"
HTMLLIB="amssymb.hva amsfonts.hva article.hva austrian.hva book.hva color.hva colortbl.hva commongraphic.hva fancysection.hva fancyvrb.hva french.hva german.hva graphics.hva graphicx.hva hevea.hva common-math.hva mathpartir.hva natbib.hva png.hva gif.hva svg.hva report.hva seminar.hva sword.hva symb-eng.hva symb-ent.hva symb-fra.hva symb-mathml.hva symb-text.hva urlhref.hva xypic.hva undersection.hva"
TEXTLIB="article.hva book.hva color.hva colortbl.hva fancysection.hva hevea.hva report.hva seminar.hva french.hva austrian.hva german.hva natbib.hva"
INFOLIB="article.hva book.hva hevea.hva report.hva seminar.hva"
