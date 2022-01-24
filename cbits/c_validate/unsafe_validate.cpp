//
// Created by Kirill Golubev on 11.01.2022.
//

#include "unsafe_validate.h"
#include <string>
#include <pybind11/embed.h>

namespace py = pybind11;
using namespace py::literals;

bool cpp_validate(const std::string &o, const std::string &sch) {
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

std::string cpp_generate(const std::string& sch){
    auto locals = py::dict("schema"_a = sch);
    try {
        py::exec(R"(
from jsf import JSF
import json

json_sch = json.loads(schema)
faker = JSF(json_sch)
fake_json_cnt = faker.generate()
fake_json = json.dumps(fake_json_cnt)
    )", py::globals(), locals);
    }
    catch (...) {
        return "error_inside_python_code";
    }
    return locals["fake_json"].cast<std::string>();
}

extern "C" {
char* unsafe_generate(const char* schema){
    return strdup(cpp_generate(std::string(schema)).c_str());
}

void dispose_generated_object(char* obj){
    free(obj);
}

bool unsafe_validate(const char *o, const char *sch) {
    return cpp_validate(std::string(o), std::string(sch));
}
void start_python(){
    py::initialize_interpreter();
}
void end_python(){
    py::finalize_interpreter();
}
}