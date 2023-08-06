
# variables
V_MIN=80
V_MAX=90

TIMING_LIKE="Post Routing Timing Summary"
SUCCESS_LIKE="design met the timing requirement"
FAILED_LIKE="design did not meet timing requirements"

base_dir=./utils
result_base_dir=./result
src_dir=./mycpu
src_prj_dir=$base_dir/submission/ccpu/src/mycpu

ci_script_dir=$base_dir/.ci-scripts
script_dir=$base_dir/script
vivado_prj_dir=$base_dir/release_project/perf_test_v0.01/soc_axi_perf
result_dir=$result_base_dir/$(date +%s)
log_file=$result_dir/perf.log
tmp_dir=$result_base_dir/tmp-$(date +%Y%m%d)


# init
cp -r utils.old utils
cp -r $src_dir $src_prj_dir/..

mkdir -p $result_dir
mkdir -p $tmp_dir
touch $log_file
chmod +x $result_dir

echo $script_dir

# run
# nix run --impure ./utils/script#default -- -mode batch -source ./utils/script/script.tcl

config_file=$ci_script_dir/configure_pll.tcl
found_flag=0
final_freq=$V_MIN

# bs
while (( V_MIN <= V_MAX ))
do
    # update
    failed=1
    V_MID=$(( (V_MIN + V_MAX) / 2 ))

    # update pll_freq
    org_cmd=$(head -n 1 "$config_file")
    new_cmd="${org_cmd% *} $V_MID"
    sed -i "1s/.*/$new_cmd/" "$config_file"

    # update .xci
    echo $(date) >> $log_file
    echo -e "pll_freq $V_MID start testing..." >> $log_file
    echo -e "exit\n" | nix run --impure $base_dir#default -- -mode tcl -source $config_file $vivado_prj_dir/run_vivado/mycpu_prj1/mycpu.xpr
    # cat $config_file && touch ./vivado.log
    cp $vivado_prj_dir/rtl/xilinx_ip/clk_pll/clk_pll.xci $src_prj_dir/../perf_clk_pll.xci

    # synth/impl/gen bitstream
    echo -e "init all\nrun all 2\nexit\n" | nix run --impure $script_dir#default -- -mode batch -source $script_dir/script.tcl
    # bash ./test.sh $V_MID && touch ./vivado.log

    # WNS check
    while IFS= read -r line || [[ -n "$line" ]]
    do
        if [[ $line == *"$TIMING_LIKE"* ]]; then
            :
        elif [[ $line == *"$SUCCESS_LIKE"* ]]; then
            # guaranteed to be higher than V_MIN
            found_flag=1
            failed=0
        elif [[ $line == *"$FAILED_LIKE"* ]]; then
            :
        else
            continue
        fi
        echo $line >> $log_file

    done < $script_dir/result/ccpu/perf_run.log
    echo "" >> $log_file

    # cond
    if [ $failed -eq 0 ]; then
        # increase
        cp $script_dir/result/ccpu/perf.bit $result_dir/
        final_freq=$V_MID
        V_MIN=$(( V_MID + 1 ))
    else
        V_MAX=$(( V_MID - 1 ))
    fi
    cp -r $script_dir/project/ccpu/perf/template $tmp_dir
    mv  $tmp_dir/template  $tmp/project-$V_MID
    # clean
    rm -rf $script_dir/project
    rm -rf $script_dir/result

    # inter-product
    rm vivado*

done

# finish
if [ $found_flag -eq 1 ]; then
    echo "---------------------------------------------------" >> $log_file
    echo "final freq is $final_freq" >> $log_file
else
    echo "no available freq found" >> $log_file
fi

rm -rf $base_dir
rm -rf Modeltech*
