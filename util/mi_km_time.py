import sys

HELP = """Usage:
  python mi_km_time.py <time> <mi|km> [<num mi|km>]

Converts (mi,km) time into (km,mi) time and optionally prints total run time of
<num mi|km>.
"""

def convert_int(s):
  try:
    return int(s)
  except ValueError:
    print "Invalid time format:", s
    sys.exit(-1)

args = sys.argv[1:]
if len(args) == 0:
  print HELP,
  sys.exit(0)
if len(args) < 2:
  sys.exit(1)

t = args[0].split(':')
if len(t) > 2:
  print "Invalid time format"
  sys.exit(-1)

t_sec = convert_int(t[0]) * 60
if len(t) > 1:
  t_sec += convert_int(t[1])

kKmInMi = 1.6
if args[1] == "mi":
  t2 = int(t_sec / kKmInMi + 0.5)
  measure = "km"
elif args[1] == "km":
  t2 = int(t_sec * kKmInMi + 0.5)
  measure = "mi"
else:
  print "Only km/mi are allowed"

msg = "{0}:{1:02d} {2}".format(t2 / 60, t2 % 60, measure)
if len(args) >= 3:
  d = convert_int(args[2])
  tt = t_sec * d
  s = tt % 60
  tt /= 60
  m = tt % 60
  tt /= 60
  msg += ", {0} in {1}:{2}:{3}".format(d, tt, m, s)

print msg

