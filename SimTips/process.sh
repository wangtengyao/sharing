pdflatex --shell-escape img.tex \
&& pdftk img-pics.pdf cat 1-endeast output img-rotated.pdf \
&& pdftk img-rotated.pdf burst output fig%01d.pdf \
&& rm *.aux *.log img*.pdf doc_data.txt
