//
// Created by Kirill Golubev on 11.01.2022.
//

#include "unsafe_validate.h"
#include <string>
#include <pybind11/embed.h>

namespace py = pybind11;
using namespace py::literals;

bool cpp_validate(const std::string &o, const std::string &sch) {


    py::scoped_interpreter guard{};

    auto locals = py::dict("object"_a = o, "schema"_a = sch);
    try {
    py::exec(R"(
import json
import jsonschema
json_o = json.loads(object)
json_sch = json.loads(schema)
jsonschema.validate(json_o, json_sch)
    )", py::globals(), locals);
    }
    catch (...) {
        return false;
    }
    return true;
}

extern "C" {
bool unsafe_validate(const char *o, const char *sch) {
    return cpp_validate(std::string(o), std::string(sch));
}
}