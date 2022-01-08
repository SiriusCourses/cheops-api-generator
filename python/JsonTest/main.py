import jsonschema
import json
import sys


def get_schema():
    line = sys.stdin.readline()
    schema = json.loads(line)
    return schema


def get_json():
    line = sys.stdin.readline()
    schema = json.loads(line)
    return schema


if __name__ == '__main__':
    while True:
        json_o = get_json()
        schema = get_schema()
        try:
            jsonschema.validate(instance=json_o, schema=schema)
            print("True")
        except jsonschema.exceptions.ValidationError as err:
            print("False")
            print(err)
