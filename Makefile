all:
	gprbuild -p -Pgnat/axmpp.gpr
	gprbuild -p -Pgnat/con_cli.gpr
clean:
	gprclean -Pgnat/axmpp.gpr
	gprclean -Pgnat/con_cli.gpr