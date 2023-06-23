BUILD_DIR = ./build

export PATH := $(PATH):$(abspath ./utils)

test:
	mill -i __.test

verilog:
	mkdir -p $(BUILD_DIR)
	mill -i mycpu.runMain Elaborate -td $(BUILD_DIR)

help:
	mill -i __.test.runMain Elaborate --help

compile:
	mill -i __.compile

bsp:
	mill -i mill.bsp.BSP/install

reformat:
	mill -i __.reformat

checkformat:
	mill -i __.checkFormat

clean:
	-rm -rf $(BUILD_DIR)

.PHONY: test verilog help compile bsp reformat checkformat clean
