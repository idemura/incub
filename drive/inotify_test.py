from __future__ import print_function
import pyinotify

def handler(event):
    if event.name == 'test.txt':
        print(event.__dict__)

wm = pyinotify.WatchManager()
notifier = pyinotify.Notifier(wm, handler)
wm.add_watch('.', pyinotify.IN_CLOSE_WRITE)
notifier.loop()

