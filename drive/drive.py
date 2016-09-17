from __future__ import print_function
import httplib2
import json
import os
import pprint
import sys
import time

from apiclient import discovery
import oauth2client
from oauth2client import client
from oauth2client import tools
import rfc3339
from googleapiclient.http import MediaFileUpload

try:
    import argparse
    parser = argparse.ArgumentParser(parents=[tools.argparser])
    parser.add_argument("-s", "--silent",
            default=False,
            help="no output to stdout",
            action="store_true")
    flags = parser.parse_args()
except ImportError:
    flags = None

# If modifying these scopes, delete your previously saved credentials
# at ~/.credentials/drive-python-quickstart.json
SCOPES = 'https://www.googleapis.com/auth/drive'
CLIENT_SECRET_FILE = 'client_id.json'
FILE_NAME = 'igor.kdbx'
MIME_TYPE = 'application/octet-stream'
# FILE_NAME = 'test.txt'
# MIME_TYPE = 'text/plain'

def get_credentials():
    """Gets valid user credentials from storage.

    If nothing has been stored, or if the stored credentials are invalid,
    the OAuth2 flow is completed to obtain the new credentials.

    Returns:
        Credentials, the obtained credential.
    """
    home_dir = os.path.expanduser('~')
    credential_dir = os.path.join(home_dir, '.credentials')
    if not os.path.exists(credential_dir):
        os.makedirs(credential_dir)
    credential_path = os.path.join(credential_dir,
                                   'drive-python-quickstart.json')

    store = oauth2client.file.Storage(credential_path)
    credentials = store.get()
    if not credentials or credentials.invalid:
        flow = client.flow_from_clientsecrets(CLIENT_SECRET_FILE, SCOPES)
        flow.user_agent = 'jdrive'
        if flags:
            credentials = tools.run_flow(flow, store, flags)
        else: # Needed only for compatibility with Python 2.6
            credentials = tools.run(flow, store)
        print('Storing credentials to ' + credential_path)
    return credentials

def find_keepass(files):
    for f in files:
        if f['labels'].get('trashed', True):
            continue
        if f.get('originalFilename', '') == FILE_NAME:
            return f
    return None

def output(msg):
    if not flags.silent:
        print(msg)

def sync(service, drive_md, local_file):
    exists = os.path.isfile(local_file)
    try:
        local_tf = int(os.path.getmtime(local_file))
    except OSError:
        local_tf = None

    if drive_md is not None:
        drive_tf = int(rfc3339.tf_from_timestamp(drive_md['modifiedDate']))
        if local_tf == drive_tf:
            output('Up-to-date')
            return
        if local_tf is None or drive_tf > local_tf:
            body = service.files().get_media(
                    fileId=drive_md['id']).execute()
            with open(local_file, 'wb') as f:
                f.write(body)
            output('Download ok')
            return

    if local_tf is None:
        return
    upload_md = {
        'mimeType': MIME_TYPE,
        'modifiedDate': rfc3339.timestamp_from_tf(local_tf),
    }
    f = MediaFileUpload(local_file, mimetype=MIME_TYPE)
    if drive_md is None:
        ids = service.files().generateIds(space='drive', maxResults=1).execute()
        upload_md['id'] = ids['ids'][0]
        upload_md['ownedByMe'] = True
        upload_md['originalFilename'] = local_file
        upload_md['title'] = local_file
        r = service.files().insert(body=upload_md, media_body=f)
        output('Insert ok')
    else:
        upload_md['id'] = drive_md['id']
        r = service.files().update(
            fileId=drive_md['id'],
            body=upload_md,
            media_body=f,
            modifiedDateBehavior='fromBody')
        output('Update ok')
    resp = r.execute()
    # pprint.pprint(resp, indent=2)

def main():
    """Shows basic usage of the Google Drive API.

    Creates a Google Drive API service object and outputs the names and IDs
    for up to 10 files.
    """
    credentials = get_credentials()
    http = credentials.authorize(httplib2.Http())
    service = discovery.build('drive', 'v2', http=http)

    output('Loading files...')
    results = service.files().list().execute()
    items = results.get('items', [])
    if not items:
        output('No files found')
        return False
    sync(service, find_keepass(items), FILE_NAME)
    return True

if __name__ == '__main__':
    if not main():
        sys.exit(1)
