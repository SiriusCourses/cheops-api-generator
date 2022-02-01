# Transport type generation

## Overview

This is a tool for type generation with respect to json schema in yaml fromat. 

## Prerequisites
install stack and ghc via ghcup.
```bash
sudo apt-get install cmake gcc g++ curl wget
sudo apt-get install python3-dev libgmp3-dev libtinfo-dev 
```
```bash
pip install jsonschema
```

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
pip install json jsonschema
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
```bash
export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:/abolute/path/to/cbits/c_validate/cmake-build-debug
```

### Building Haskell 

1. Add `generated-api/src` to `library.source-dirs` in `package.yaml`
2. Add `generated-api/test` to `tests.source-dirs` in `package.yaml`
3. Comment out `tests.source-dirs.test` item from `package.yaml`
4. Optional: add `-Wno-unused-imports` and `-fno-warn-orphans` to `package.yaml`
```bash
stack test
```
Prepare for a long wait.


### How does it work

In this part there will be overview of what is going on when this tool is working. I will try my best to specify all corner cases, bug and weird behaviours. At first let me say that there are some.

The tool starts with parsing of all yamls in specified directory. The first gotcha is also here. 

`GOTCHA`: all `yaml` files that do not have `schema` folder in their paths are filtered out. Filteriing is based on absolute path. For details look at function `Main.collectFiles`
### Parsing yaml to `Data.Yaml.Object`
------------

Parsing is done by `libyaml` via some conduit machnery for `include` inlining. During ths phase several things are done:

  * all `!include path` are inclined
  * as `!include path` are inclined new field is injected to remeber where it is inlined from.
  * all `maxItems` and `minItems` fields are dropped
  * all `additionalProperties` fields are dropped
  * all invalid paths are treated as from `repository_root`

`GOTCHA`: If you are getting `/path/to/yaml/path/to/yaml/file.yaml is not found` that usually means that you misspelled `path/to/yaml/file.yaml` in the first place. As it was checked before appending path to the repository root.

For more details go here: `Data.TransportTypes.Parsing.IncludeInjection`
### Transforming `Data.Yaml.Object` to `ParserResult`
------------

As parsing is done then you got `Data.Yaml.Value` on your hands and it transforms via `Data.Yaml.FromJSON` to `Data.TransportTypes.Parsing.ParserResult` object.

In terms of dependencies yaml files form a forest, but at first I thought that tree will be enough. `ParserResult` represents a forst with mutual node to connect all the trees. The node is stored in `mainType` and the forst is in `deps`. Later this node will be discarded in `Data.TransportTypes.CodeGen.Hylo.BuildUp.build` as it is just an artifact of poor design.

The transformation of `Data.Yaml.Value` to `ParserResult` starts in `parseJSON` and using recoursive function `Data.TransportTypes.Parsing.parseDispatch`. The core idea is to check at first if object we are dispatching on is included via `"haskell/origin"` field and at second if we have already met it, thus a state with keymap: `ParserState`.

Then to conclude the parsing some postprocessing is required. It is performed via `Data.TransportTypes.Parsing.postprocessParserResult`. Main goal of such postprocessing is to remove common file prefix of all files. It also changes dashes to underscores.

`GOTCHA`: Dash to underscore transformation is never reverted, so if you had dashes in yamls they are gone forever after this step.

### Transforming `ParserResult` to convinient tree structure
-------

Parsing is done, so it is time for building. Now we have `ParserResult` on ower hands and essencially it represents a tree. A good thing to do with a tree is to traverse it from the leaves to the root. This is exectly what happening during building phase.

`Data.TransportTypes.CodeGen.Hylo.BreakDown` is splitting the tree over `Data.TransportTypes.ModuleParts.ModuleParts` to 

* `Data.TransportTypes.CodeGen.Hylo.Structure.NodeF` - tree itself 
* `Data.TransportTypes.CodeGen.Hylo.Structure.Payload` - stored data

Then `Data.TransportTypes.CodeGen.Hylo.BuildUp.buildUp` is traversing this tree in hylomorphism-like fashion. But I made a design mistake and this tree is actually a forest, so the beauty of hylomorphism is not preserverd. Instead there are a bunch of recursive functions inside `Data.TransportTypes.CodeGen.Hylo.BuildUp` module to build not only interconnected part of the forest, but the disconnected parts also.

One function to rule them all: `Data.TransportTypes.CodeGen.Hylo.BuildUp.build`. The same one which discardes mutual node.

### Traversing tree structure
---------------

Due to the survived nature of hylomorphism traversing process is separated with building process. As our goal is to convert everything to `HsModule'`s and write them to files it is very convinient, because tests and types themselves are generated differently, but traversing process is them same.

Traversing is organized with the help of `Data.TransportTypes.CodeGen.Hylo.BuildUp.Ctx` it is scary, but everything inside it has a perpose.

```haskell
type Ctx a = ReaderT U.ModulePrefix (StateT GeneratorState (Except String)) a
```

Reader is for remebering the path to our module. As we are starting from the leaves there is no way to know the path we got here. Thus we need a Reader to delay the prefix evaluation until we will reach the root.

State is for remebering includes. There is no need to duplicate modules that including in multiple places. Thus the hashmap to remeber which module we have already met and buit.

Except is needed if something will go wrong. 


### Making `GHC.SourceGen` modules
---------------

Test generation is done inside  `Data.TransportTypes.CodeGen.TestGen` module. 

 * `buildSpec` function is building module for `Spec.hs` where all tests are called
 * `buildTest` function making tests themself and `Test.QuickCheck.Arbitrary` instances

    * toJSON test

    At frist the sample is generated via `generic-random` and `quickcheck-instances`. Then it is converted to json and transformed to `std::string` , and them to python `str` along with schema which is stored inside test file as string literal. The sample is validated against schema via python `jsonschema` package. The result is returned as `Bool`.

    `GOTCHA`: There are no exceptions transported between python and haskell, so if something goes wrong validateion result is just `False` with no signal that something went wrong.

    `GOTCHA`: As there are differences in yaml convention in `cheops` project and official json schema draft to have meaningfull validation all `oneOf`'s are transformed to `anyOf`s via string transformation defined in `where` clause of schema literal in test files.
    * fromJSON test

    Checks that `fromJSON . toJSON == (fromJSON . toJSON) . (fromJSON . toJSON)`


Module generation is done inside `Data.TransportTypes.CodeGen.TypeGen`

* `buildTypeDecl` builds declaration for the type
*  `InstanceGen.FromJson.buildFromJSONInstance` builds `FromJSON` instance
*  `InstanceGen.ToJson.buildToJSONInstance` builds `ToJSON` instance
*  `InstanceGen.ToJson.buildToSchemaInstance` builds `ToSchema` instance

There are some commentaries inside these files on cases which are not representable by json schema, but representable via `Data.TransportTypes.TypeRep.TypeRep` data structure. 