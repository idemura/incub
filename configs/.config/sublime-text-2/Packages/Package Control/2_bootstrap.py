import sys
import threading
import os
import functools
from textwrap import dedent

import sublime

# Clean up the installed and pristine packages for Package Control 2 to
# prevent a downgrade from happening via Sublime Text
if sys.version_info < (3,):
    sublime_dir = os.path.dirname(sublime.packages_path())
    pristine_dir = os.path.join(sublime_dir, 'Pristine Packages')
    installed_dir = os.path.join(sublime_dir, 'Installed Packages')
    pristine_file = os.path.join(pristine_dir, 'Package Control.sublime-package')
    installed_file = os.path.join(installed_dir, 'Package Control.sublime-package')
    if os.path.exists(pristine_file):
        os.remove(pristine_file)
    if os.path.exists(installed_file):
        os.remove(installed_file)

if sys.version_info < (3,):
    from package_control.bootstrap import bootstrap_dependency
    from package_control.package_manager import PackageManager
    from package_control import loader, text
    from package_control.settings import pc_settings_filename, load_list_setting, save_list_setting
    import package_control
else:
    from .package_control.bootstrap import bootstrap_dependency
    from .package_control.package_manager import PackageManager
    from .package_control import loader, text
    from .package_control.settings import pc_settings_filename, load_list_setting, save_list_setting
    from . import package_control


def plugin_loaded():
    manager = PackageManager()
    settings = manager.settings.copy()

    if not loader.exists('package_control'):
        base_loader_code = """
            import sys
            import os
            from os.path import dirname


            # This file adds the package_control subdirectory of Package Control
            # to first in the sys.path so that all other packages may rely on
            # PC for utility functions, such as event helpers, adding things to
            # sys.path, downloading files from the internet, etc


            if sys.version_info >= (3,):
                def decode(path):
                    return path

                def encode(path):
                    return path

                loader_dir = dirname(__file__)

            else:
                def decode(path):
                    if not isinstance(path, unicode):
                        path = path.decode(sys.getfilesystemencoding())
                    return path

                def encode(path):
                    if isinstance(path, unicode):
                        path = path.encode(sys.getfilesystemencoding())
                    return path

                loader_dir = decode(os.getcwd())


            st_dir = dirname(dirname(loader_dir))

            found = False
            if sys.version_info >= (3,):
                installed_packages_dir = os.path.join(st_dir, u'Installed Packages')
                pc_package_path = os.path.join(installed_packages_dir, u'Package Control.sublime-package')
                if os.path.exists(encode(pc_package_path)):
                    found = True

            if not found:
                packages_dir = os.path.join(st_dir, u'Packages')
                pc_package_path = os.path.join(packages_dir, u'Package Control')
                if os.path.exists(encode(pc_package_path)):
                    found = True

            if found:
                if os.name == 'nt':
                    from ctypes import windll, create_unicode_buffer
                    buf = create_unicode_buffer(512)
                    if windll.kernel32.GetShortPathNameW(pc_package_path, buf, len(buf)):
                        pc_package_path = buf.value

                sys.path.insert(0, encode(pc_package_path))
                import package_control
                # We remove the import path right away so as not to screw up
                # Sublime Text and its import machinery
                sys.path.remove(encode(pc_package_path))

            else:
                print(u'Package Control: Error finding main directory from loader')
        """
        base_loader_code = dedent(base_loader_code).lstrip()
        loader.add('00', 'package_control', base_loader_code)

    def pc_bookkeeping(restart_message=None):
        """
        Some settings changes that need to happen on all machines, but should
        only happen after dependencies have been bootstrapped.

        :param restart_message:
            Any message to display to the user once the bookkeeping is complete
        """

        pc_settings = sublime.load_settings(pc_settings_filename())

        # Make sure we are track Package Control itself
        installed_packages = load_list_setting(pc_settings, 'installed_packages')
        if 'Package Control' not in installed_packages:
            params = {
                'package': 'Package Control',
                'operation': 'install',
                'version': package_control.__version__
            }

            def record_usage():
                manager.record_usage(params)
                if restart_message:
                    sublime.set_timeout(lambda: sublime.message_dialog(restart_message), 10)

            # Run recording the usage in a thread to prevent blocking the UI
            # because of a slow internet connection
            threading.Thread(target=record_usage).start()
            installed_packages.append('Package Control')
            save_list_setting(pc_settings, pc_settings_filename(), 'installed_packages', installed_packages)

        # We no longer use the installed_dependencies setting because it is not
        # necessary and created issues with settings shared across operating systems
        if pc_settings.get('installed_dependencies'):
            pc_settings.erase('installed_dependencies')
            sublime.save_settings(pc_settings_filename())

    # SSL support fo Linux
    if sublime.platform() == 'linux':
        linux_ssl_url = u'http://packagecontrol.io/ssl/1.0.1/ssl-linux.sublime-package'
        linux_ssl_hash = u'862d061cbe666777cd1e9cd1cbc7c82f48ad8897dbb68332975f3edf5ce0f38d'
        linux_ssl_priority = u'01'
        linux_ssl_version = '1.0.1'
        linux_ssl_restart_message = text.format(
            u'''
            Package Control

            Package Control just installed or upgraded the missing Python
            _ssl module for Linux since Sublime Text does not include it.

            Please restart Sublime Text to make SSL available to all
            packages.
            '''
        )

        threading.Thread(
            target=bootstrap_dependency,
            args=(
                settings,
                linux_ssl_url,
                linux_ssl_hash,
                linux_ssl_priority,
                linux_ssl_version,
                functools.partial(pc_bookkeeping, linux_ssl_restart_message),
            )
        ).start()

    # SSL support for SHA-2 certificates with ST2 on Windows
    elif sublime.platform() == 'windows' and sys.version_info < (3,):
        win_ssl_url = u'http://packagecontrol.io/ssl/1.0.0/ssl-windows.sublime-package'
        win_ssl_hash = u'3c28982eb400039cfffe53d38510556adead39ba7321f2d15a6770d3ebc75030'
        win_ssl_priority = u'01'
        win_ssl_version = u'1.0.0'
        win_ssl_restart_message = text.format(
            u'''
            Package Control

            Package Control just upgraded the Python _ssl module for ST2 on
            Windows because the bundled one does not include support for
            modern SSL certificates.

            Please restart Sublime Text to complete the upgrade.
            '''
        )

        threading.Thread(
            target=bootstrap_dependency,
            args=(
                settings,
                win_ssl_url,
                win_ssl_hash,
                win_ssl_priority,
                win_ssl_version,
                functools.partial(pc_bookkeeping, win_ssl_restart_message),
            )
        ).start()

    else:
        pc_bookkeeping()

# ST2 compat
if sys.version_info < (3,):
    plugin_loaded()