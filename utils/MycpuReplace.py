import re
import sys
import pdb


# 获取命令行参数作为输入文件名
if len(sys.argv) > 1:
    input_file = sys.argv[1]
else:
    print("Error: Please provide an input file name as the first command line argument.")
    sys.exit(1)

TopName = sys.argv[2]

def replace(match):
    if match.group(1) == ',':
        return "aresetn" + match.group(1)
    else:
        return "(!aresetn)" + match.group(1)

# 需要替换的正则
patterns = [
    (r"module "+TopName, "module mycpu_top"),
    (r"(?<!\.)\bclock\b", "aclk"), # clock -> aclk, but .clock not replace
    (r"(?<!\.)\breset\s*(,|\S)", replace),  # reset -> aresetn, but .reset not replace
    (r"\bbus_(\w+)\b", r"\1"),  # prefix_xxx -> xxx
]

# 读取输入文件
with open(input_file, "r") as f:
    verilog_code = f.read()

# 查找名为"CCPU"的模块
ccpu_module = re.search(r"module\s+CCPU\s*\(", verilog_code)

if ccpu_module:
    # 提取模块内部的代码
    ccpu_code = verilog_code[ccpu_module.start():]
    # 查找模块的结束语句"endmodule"
    ccpu_end = re.search(r"endmodule", ccpu_code)
    if ccpu_end:
        # 在模块内部查找并替换所有变量"clock"为"aclk"
        ccpu_internal = ccpu_code[0:ccpu_end.start()]
        # pdb.set_trace()
        for pattern, replacement in patterns:
            ccpu_internal = re.sub(pattern, replacement, ccpu_internal)
        # ccpu_internal = re.sub(r"(?<!\.)\bclock\b", r"aclk", ccpu_internal)
        # 将替换后的代码写回到原文件中
        modified_code = verilog_code[:ccpu_module.start()] + \
                        ccpu_internal + \
                        ccpu_code[ccpu_end.start():]
        with open(input_file, "w") as f:
            f.write(modified_code)
    else:
        print("Error: {0} module definition is missing the 'endmodule' statement.".format(TopName))
else:
    print("Error: {0} module not found.".format(TopName))


with open(input_file, 'r') as f:
    lines = f.readlines()

endmodule_index = -1
for i in range(len(lines)-1, -1, -1):
    line = lines[i].strip()
    if line.startswith("endmodule"):
        endmodule_index = i
        break

if endmodule_index == -1:
    print("Error: No endmodule found in file")
    exit()

new_lines = lines[:endmodule_index+1]
new_content = "".join(new_lines)

with open(input_file, 'w') as f:
    f.write(new_content)