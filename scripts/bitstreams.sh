SUBMISSION_PATH=$1
VIVADO_PATH=$2
PERF_DIR=$SUBMISSION_PATH/release_project/perf_test_v0.01/soc_axi_perf

cd $SUBMISSION_PATH/script
echo "start generate bitstream"
echo -e "init all\nrun all 2\nexit\n" | nix run --impure $VIVADO_PATH#default -- -mode batch -source script.tcl > /dev/null
echo "finish generate bit stream"