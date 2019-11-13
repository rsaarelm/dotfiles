FONTS := $(wildcard *.pcf.gz)

fonts.dir: $(FONTS)
	mkfontdir
	fc-cache -f

fonts.scale: fonts.dir
	mkfontscale

clean:
	rm -f fonts.dir fonts.cache
