kvalue = {}
positional = []

def get(key, def_val = None):
  return kvalue.get(key, def_val)

def parse(args):
  for a in args:
    if a == '-':
      positional.append(a)
    elif a[0] == '-':
      if a[1] == '-':
        a = a[2:]
      else:
        a = a[1:]
      if len(a) == 0 or a[0] == '-':
        print '"--" or params starting "-" are not supported'
      else:
        parts = a.split('=', 1)
        if len(parts) == 1: parts.append(True)
        kvalue[parts[0]] = parts[1]
    else:
      positional.append(a)
