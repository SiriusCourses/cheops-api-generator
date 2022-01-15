# Transport type generation

## How to genereate types

It is a must for `genereated-api` directory to be inside this project, due to dependance on `AllOf`, `AnyOf` and `OneOf` implementation from here.  

```bash
mkdir generated-api #output directory
stack build # builds code generation tool
stack exec -- config-generation-exe --input "path/to/api/root" --output "./generated-api" --repository_root "path/to/api/repository/root"
```
Option `repository_root` is needed if there are absolute paths relative to some direcotory in json-sechema includes. In that case this direcory must be supplied in `--reposotory_root` argument.

There are some more options, you can look at them by 
```bash
stack exec -- config-generation-exe --help
```

Now there is `generated-api/src` and `generated-api/test` directories with types and tests for them repectfully.

## How to run generated tests

### Python dependecies

Validation function uses `json` and `jsonschema`, so 
```bash
pip3 install json jsonschema
```
The important part is that if there are no such dependecies installed then `unsafe_validatete` exposed to haskell will just return `False` every time. **WITHOUT** any indication that it is a missing dependancy problem, not a validation one.

### Cbits

Three functions are exposed from Python to C through [pybind11](https://pybind11.readthedocs.io/en/stable/) and then from C to haskell through ffi. 

```haskell
foreign import ccall "unsafe_validate" unsafe_validate :: CString -> CString -> IO CBool
foreign import ccall "start_python" start_python :: IO ()
foreign import ccall "end_python" end_python :: IO ()
```

```c++
extern "C"{
    //initialize python interpretator
    void start_python(); 
    // validate via jsonschema.validate
    bool unsafe_validate(const char* object, const char* scheme); 
    //shut down python interpretator
    void end_python();
};
```
See `test/Spec.hs` to use example. 

### Building cbits

There is pybind11 in use, so you need to install everything it needs. However there is no need to install `pybind`, it will fetch itself locally during `cmake ..`

```bash
cd cbits/c_validate
mkdir cmake-build-debug && cd cmake-build-debug
cmake .. && make
```

### Building Haskell 

1. Add `generated-api/src` to `library.source-dirs` in `package.yaml`
2. Add `generated-api/test` to `tests.source-dirs` in `package.yaml`
3. Commant out `tests.source-dirs.test` item from `package.yaml`
4. Optional: add `-Wno-unused-imports` and `-fno-warn-orphans` to `package.yaml`
```bash
stack test
```
Prepare for a long wait.