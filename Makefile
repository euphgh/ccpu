BUILD_DIR = ./build
TEST_DIR = ./test_run_dir
MainModule = mycpu
NUR_VIVADO = ~/nur-vivado-2019.2
SUBMISSION = ~/nscscc2023_mips_group_qualifier_submission
REPOT_PATH = ~/thuthesis
TOPNAME = CCPU
VSRC := $(BUILD_DIR)/mycpu_top.sv
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
	mv $(BUILD_DIR)/$(TOPNAME).v $@
	python3 utils/MycpuReplace.py $@ $(TOPNAME)

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

package: 
	bash ./scripts/package.sh 100 ~/nscscc2023_mips_group_qualifier_submission ~/nur-vivado-2019.2


clean:
	-rm -rf $(BUILD_DIR)
	-rm -rf $(TEST_DIR)

.PHONY: test verilog help compile bsp clean mycpu
