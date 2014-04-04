RELEASE := kakaranet
COOKIE  := sample
APPS    := kernel stdlib sasl sync gproc cowboy mimetypes ranch erlydtl n2o face
VER     := 1.0.0
VM      := rels/web/files/vm.args
SYS     := rels/web/files/sys.config
PLT_NAME := ~/.n2o_dialyzer.plt
ERL_ARGS := -args_file $(VM) -config $(SYS)
RUN_DIR ?= rels/web/devbox
LOG_DIR ?= rels/web/devbox/logs
N2O     := deps/n2o/priv/static
APP     := apps/face/priv/static/nitrogen

default: get-deps compile static-link
static-link:
	rm -rf $(N2O)
	rm -rf $(APP)
	ln -s ../../n2o_scripts $(N2O)
	ln -s ../../../../deps/n2o/priv/static/n2o $(APP)

include otp.mk