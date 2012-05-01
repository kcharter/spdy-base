.PHONY: clean

PRES=efpug-spdy

$(PRES).html: $(PRES).txt
	pandoc -t s5 --self-contained -s $(PRES).txt -o $(PRES).html

clean:
	rm -f *.html
