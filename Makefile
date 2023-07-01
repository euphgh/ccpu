BUILD_DIR = ./build
MainModule = mycpu

export PATH := $(PATH):$(abspath ./utils)

test:
	mill -i $(MainModule).test

verilog:
	mkdir -p $(BUILD_DIR)
	mill -i $(MainModule).runMain Elaborate -td $(BUILD_DIR)

help:
	mill -i $(MainModule).test.runMain Elaborate --help

compile:
	mill -i $(MainModule).compile

macro:
	rm -rf ./out
	mill -i $(MainModule).test

bsp:
	mill -i mill.bsp.BSP/install

clean:
	-rm -rf $(BUILD_DIR)

.PHONY: test verilog help compile bsp clean
