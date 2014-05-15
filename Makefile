RELEASE := kakaranet
COOKIE  := node_runner
VER     := 1.0.0
APP     := apps/web/priv/static/nitrogen

default: get-deps compile static-link
static-link:
	rm -rf $(APP)
	ln -s ../../../../deps/n2o/priv $(APP)

include otp.mk
