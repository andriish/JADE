import docker
import glob
import regex
import os
import shutil

c_name = "jade_soft"

#Path Variables
pwd = "/home"
bin_dir = f'{pwd}/installedGNU32/bin'
source_dir = f'{pwd}/convert_data_files'
binary_dir = f'{pwd}/build/test'
work_dir = f'{pwd}/convert_data_files'
temp_dir = f'{pwd}/convert_data_files/temp'
data_dir = './../test/jadetapes'

client = docker.from_env()
jade_cont = client.containers.get(c_name)
def jc_run(cmd, env = None):
    return jade_cont.exec_run(cmd, workdir=temp_dir, environment=env)

#get data files
data_files = glob.glob(f"{data_dir}/*/*")
for path in data_files:
    name_full = regex.findall(r"JAD\d*\/file\d*$", path)[0] #get data_file name to rename it later
    name = name_full.split("/")
    if not os.path.exists(f"./{name[0]}"):
        os.mkdir(f"./{name[0]}")
    else:
        pass

    if not os.path.exists("./temp"):
        os.mkdir("./temp")
    else:
        pass

    #copy file

    shutil.copy2(path, "temp/data_file") 

    #convert data file

    exec_str = r"export GFORTRAN_CONVERT_UNIT=native ;"
    exec_str = fr'{exec_str} sh {binary_dir}/wrapper.sh {source_dir}/fptobos.card {bin_dir}/fptobos'
    jc_run(f'bash -c "{exec_str}"')

    exec_str = r"export GFORTRAN_CONVERT_UNIT=big_endian\;native:2 ;"
    exec_str = fr'{exec_str} sh {binary_dir}/wrapper.sh {source_dir}/superv.card {bin_dir}/superv'
    jc_run(f'bash -c "{exec_str}"')

    exec_str = r"export GFORTRAN_CONVERT_UNIT=big_endian ;"
    exec_str = fr'{exec_str} sh {binary_dir}/wrapper.sh {source_dir}/ze4v.card {bin_dir}/ze4v'
    jc_run(f'bash -c "{exec_str}"')

    exec_str = r"export GFORTRAN_CONVERT_UNIT=native ;"
    exec_str = fr'{exec_str} sh {binary_dir}/wrapper.sh {source_dir}/jzread.card {bin_dir}/jzread'
    jc_run(f'bash -c "{exec_str}"')

    exec_str = r"export GFORTRAN_CONVERT_UNIT=native\;big_endian:2,22 ;"
    exec_str = fr'{exec_str} sh {binary_dir}/wrapper.sh {source_dir}/jtjob.card {bin_dir}/jtjob'
    jc_run(f'bash -c "{exec_str}"')

    jc_run(f"h2root {temp_dir}/data_file_jtjob.hbook {work_dir}/{name_full}.root")

    shutil.rmtree("./temp")
