all:
	$(MAKE) -C src all

run:
	ocsigenserver -c eliom.conf

