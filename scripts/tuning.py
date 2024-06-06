from kernel_tuner import tune_kernel
from kernel_tuner.utils.directives import (
    Code,
    OpenACC,
    Fortran,
    extract_directive_signature,
    extract_directive_code,
    generate_directive_function,
    extract_directive_data,
    allocate_signature_memory,
    extract_initialization_code,
)

sizes = dict()
sizes["n_rows"] = 1024
sizes["n_cols"] = 1024

with open("../src/gauss_seidel_mod.F90") as file:
    source = file.read()

app = Code(OpenACC(), Fortran())
signatures = extract_directive_signature(source)
functions = extract_directive_code(source)
init = extract_initialization_code(source)
data = extract_directive_data(source)

for function in signatures.keys():
    print(f"Tuning {function}")

    args = allocate_signature_memory(data[function], user_dimensions=sizes)
    code = generate_directive_function(
        "",
        signatures[function],
        functions[function],
        data=data[function],
        initialization=init,
        user_dimensions=sizes,
    )

    tune_params = dict()
    tune_params["vlength"] = [2**i for i in range(0, 11)]
    tune_params["collapse_factor"] = [1, 2]

    tune_kernel(
        function,
        code,
        0,
        args,
        tune_params,
        compiler_options=["-fast", "-acc=gpu"],
        compiler="nvfortran",
    )
