all:
	gprbuild -p -Pgnat/agnutls.gpr
	gprbuild -p -Pgnat/axmpp.gpr
	gprbuild -p -Pgnat/con_cli.gpr

clean:
	gprclean -Pgnat/agnutls.gpr
	gprclean -Pgnat/axmpp.gpr
	gprclean -Pgnat/con_cli.gpr