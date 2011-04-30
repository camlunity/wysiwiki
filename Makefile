all:
	$(MAKE) -C src byte

run:
	ocsigenserver -c eliom.conf

