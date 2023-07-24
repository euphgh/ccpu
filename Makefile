BUILD_DIR = ./build
TEST_DIR = ./test_run_dir
MainModule = mycpu
TOPNAME = CCPU
VSRC := $(BUILD_DIR)/mycpu_top.v
SSRC := $(shell find . -path '*/src/*' -name '*.scala')

export PATH := $(PATH):$(abspath ./utils)

test:
	mill -i $(MainModule).test

verilog:
	mkdir -p $(BUILD_DIR)
	mill -i $(MainModule).runMain SubMain -td $(BUILD_DIR)


$(VSRC): $(SSRC) ./application.properties
	mkdir -p $(BUILD_DIR)
	mill -i $(MainModule).runMain Elaborate -td $(BUILD_DIR)
	mv $(BUILD_DIR)/$(TOPNAME).v $(BUILD_DIR)/mycpu_top.v
	python3 utils/MycpuReplace.py $(BUILD_DIR)/mycpu_top.v $(TOPNAME)

mycpu: $(VSRC)

sim: $(VSRC)
	cd hitd; nix develop --command make -j 32 sim $(if $(SS), SS=$(SS),);

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
	-rm -rf $(TEST_DIR)

.PHONY: test verilog help compile bsp clean mycpu
