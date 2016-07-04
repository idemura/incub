import sys

HELP = """Usage:
  python mi_km_time.py <time> <mi|km>

Converts (mi,km) time into (km,mi) time.
"""

args = sys.argv[1:]
if len(args) == 0:
  print HELP,
  sys.exit(0)
if len(args) != 2:
  sys.exit(1)

t = args[0].split(':')
if len(t) > 2:
  print "Invalid time format"
  sys.exit(-1)

try:
  t_sec = int(t[0]) * 60;
  if len(t) > 1:
    t_sec += int(t[1])
except ValueError:
  print "Invalid time format"
  sys.exit(-1)

kKmInMi = 1.6
if args[1] == "mi":
  t2 = int(t_sec / kKmInMi + 0.5)
  measure = "km"
elif args[1] == "km":
  t2 = int(t_sec * kKmInMi + 0.5)
  measure = "mi"
else:
  print "Only km/mi are allowed"

print "{0}:{1:02d} {2}".format(t2 / 60, t2 % 60, measure)
