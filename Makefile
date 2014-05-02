RELEASE := kakaranet
COOKIE  := node_runner
APPS    := kernel avz stdlib sasl gproc cowboy cowlib ranch erlydtl n2o db sync server web
VER     := 1.0.0
VM      := rels/web/files/vm.args
SYS     := rels/web/files/sys.config
PLT_NAME := ~/.n2o_dialyzer.plt
ERL_ARGS := -args_file $(VM) -config $(SYS)
RUN_DIR ?= rels/web/devbox
LOG_DIR ?= rels/web/devbox/logs
APP     := apps/web/priv/static/nitrogen

default: get-deps compile static-link
static-link:
	rm -rf $(APP)
	ln -s ../../../../deps/n2o_scripts/n2o $(APP)

include otp.mk
