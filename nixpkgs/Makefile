HOSTNAME = $(shell hostname)

home.nix: $(HOSTNAME).nix
	ln -s $< $@

clean:
	rm -f home.nix
