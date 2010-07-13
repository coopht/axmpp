all:
	gprbuild -p -Pgnat/axmpp.gpr

clean:
	gprclean -Pgnat/axmpp.gpr