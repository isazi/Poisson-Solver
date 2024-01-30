from kernel_tuner import tune_kernel
from kernel_tuner.utils.directives import extract_directive_signature, extract_directive_code, generate_directive_function, extract_directive_data, allocate_signature_memory

sizes = dict()
sizes["n_rows"] = 1024
sizes["n_cols"] = 1024
sizes["grid_size"] = sizes["n_cols"] * sizes["n_rows"]

with open("../src/gauss_seidel_mod.F90") as file:
    source = file.read()

signatures = extract_directive_signature(source)
functions = extract_directive_code(source)
data = extract_directive_data(source)

for function in signatures.keys():
    print(f"Tuning {function}")

    args = allocate_signature_memory(data[function])
    code = generate_directive_function("", signatures[function], functions[function])
    
    tune_params = dict()
    tune_params["ngangs"] = [2**i for i in range(0, 15)]
    tune_params["vlength"] = [2**i for i in range(0, 11)]

    tune_kernel(
        function,
        code,
        0,
        args,
        tune_params,
        compiler_options=["-fast", "-acc=gpu"],
        compiler="nvfortran",
    )
