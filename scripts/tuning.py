from kernel_tuner import tune_kernel
from kernel_tuner.utils.directives import extract_preprocessor, extract_directive_signature, extract_directive_code, extract_initialization_code, generate_directive_function

with open("../src/color_mod.f90") as file:
    source = file.read()

initialization = extract_initialization_code(source)

with open("../src/grid_mod.f90") as file:
    source = file.read()

initialization += "\n\n" + extract_initialization_code(source)

with open("../src/gauss_seidel_mod.F90") as file:
    source = file.read()

preprocessor = extract_preprocessor(source)
initialization += "\n\n" + extract_initialization_code(source)
signatures = extract_directive_signature(source)
functions = extract_directive_code(source)

for function in signatures.keys():
    print(f"Tuning {function}")
    code = generate_directive_function(preprocessor, signatures[function], functions[function], initialization)
    
    tune_params = dict()
    tune_params["ngangs"] = [2**i for i in range(0, 15)]
    tune_params["vlength"] = [2**i for i in range(0, 11)]

    tune_kernel(
        function,
        code,
        0,
        None,
        tune_params,
        compiler_options=["-fast", "-acc=gpu"],
        compiler="nvfortran",
    )
