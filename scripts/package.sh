FREQ=$1
SUBMISSION_PATH=$2
VIVADO_PATH=$3
PERF_DIR=$SUBMISSION_PATH/release_project/perf_test_v0.01/soc_axi_perf
CUR_DIR=$(pwd)

# prepare source
mv application.properties old.properties
echo "DEBUG = false" >> application.properties
export LD_LIBRARY_PATH=/run/current-system/sw/share/nix-ld/lib; make mycpu
mv old.properties application.properties
cp ./build/mycpu_top.sv $PERF_DIR/rtl/myCPU
cp ./utils/Multiplier.xci $PERF_DIR/rtl/myCPU

unset LD_LIBRARY_PATH
PLL_CONFIG=./scripts/configure_pll.tcl
org_cmd=$(head -n 1 $PLL_CONFIG)
new_cmd="${org_cmd% *} $FREQ"
sed -i "1s/.*/$new_cmd/" "$PLL_CONFIG"

echo "frequence is $FREQ, start generate perf_clk_pll.xci"
echo -e "exit\n" | nix run --impure $VIVADO_PATH#default -- -mode tcl -source $PLL_CONFIG $PERF_DIR/run_vivado/mycpu_prj1/mycpu.xpr > /dev/null
echo "finish generate perf_clk_pll.xci"

SRC_PATH=./scripts/tmp/src
mkdir -p $SRC_PATH
mkdir -p $SRC_PATH/mycpu
cp $PERF_DIR/rtl/xilinx_ip/clk_pll/clk_pll.xci $SRC_PATH/perf_clk_pll.xci

cp ./build/mycpu_top.sv $SRC_PATH/mycpu/
cp ./utils/Multiplier.xci $SRC_PATH/mycpu/
cp -r $SRC_PATH $SUBMISSION_PATH/submission/MIPS_MOU_1_zhangsan/

bash ./scripts/bitstreams.sh &
wait