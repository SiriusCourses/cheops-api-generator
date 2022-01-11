import jsonschema
import json
import sys

stop = False

def cprint(*args, **kwargs):
    try:
        print(*args, **kwargs)
    except BrokenPipeError:
        global stop
        stop = True # means that haskell process is not listening
        
def eprint(*args, **kwargs):
    try:
        print(*args, file=sys.stderr, **kwargs)
    except BrokenPipeError:
        pass

if __name__ == '__main__':
    while not stop:
        errObj = None
        errSch = None
        try:
            data = sys.stdin.readline()
            eprint("Got object raw data:")
            eprint(data)
            json_o = json.loads(data)
            eprint("Got json object:")
            eprint(json_o)
        except Exception as err:
            errObj = err
        try:
            data = sys.stdin.readline()
            eprint("Got scheme raw data:")
            eprint(data)
            schema = json.loads(data)
            eprint("Got json scheme:")
            eprint(json_o)
        except Exception as err:
            errSch = err
        if errSch or errObj:
            eprint(errObj, errSch)
            cprint("False")
            continue
        try:
            jsonschema.validate(instance=json_o, schema=schema)
            cprint("True")
        except Exception as err:
            eprint(err)
            cprint("False")
