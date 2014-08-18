def error(loc, message):
  m = message
  if loc is not None:
    j = message.find(':')
    if j >= 0:
      m = message[:j] + ' {0}:{1}@{2}'.format(*loc) + message[j:]
    else:
      m = message
  print m
  exit(-1)  # Consider throw.

def fatal(msg):
  print 'Internal compiler error: {0}'.format(msg)
  exit(-1)
