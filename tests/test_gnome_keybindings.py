"""Exercise GNOME shortcut imports and launcher startup without touching the desktop."""

import os
from pathlib import Path
import shutil
import socket
import subprocess
import tempfile
import time
import unittest


ROOT = Path(__file__).resolve().parents[1]
KEYBINDINGS = ROOT / "scripts/gnome-keybindings.sh"
MEDIA_KEYS = ROOT / "scripts/gnome-media-keys.sh"


class GnomeKeybindingsTests(unittest.TestCase):
    @unittest.skipUnless(shutil.which("dconf") and shutil.which("dbus-run-session"),
                         "dconf and dbus-run-session are required")
    def test_imports_are_active_and_repeatable_in_a_private_database(self):
        with tempfile.TemporaryDirectory() as directory:
            base = Path(directory)
            runtime = base / "runtime"
            runtime.mkdir(mode=0o700)
            profile = base / "profile"
            profile.write_text("user-db:user\n")
            fake_tools = base / "bin"
            fake_tools.mkdir()
            marker = base / "unexpected-settings-write"
            for name in ("gsettings", "xdg-settings"):
                tool = fake_tools / name
                tool.write_text('#!/bin/sh\ntouch "$UNEXPECTED_SETTINGS_MARKER"\nexit 99\n')
                tool.chmod(0o755)
            env = dict(os.environ, XDG_CONFIG_HOME=str(base / "config"),
                       XDG_RUNTIME_DIR=str(runtime), DCONF_PROFILE=str(profile),
                       PATH=str(fake_tools) + os.pathsep + os.environ["PATH"],
                       UNEXPECTED_SETTINGS_MARKER=str(marker))
            env.pop("GSETTINGS_BACKEND", None)
            # Both runs share one private bus and database. The reads are real
            # dconf reads, so loading a dump one directory too deep fails here.
            program = r'''
set -eu
sh "$1" --keybindings-only
first=$(dconf dump /org/gnome/)
sh "$1" --keybindings-only
[ "$first" = "$(dconf dump /org/gnome/)" ]
dconf dump /org/gnome/
'''
            result = subprocess.run(
                ["dbus-run-session", "--", "sh", "-c", program, "test", str(KEYBINDINGS)],
                cwd=base, env=env, text=True, capture_output=True, timeout=30)
            self.assertEqual(result.returncode, 0, result.stderr)
            self.assertIn("[settings-daemon/plugins/media-keys/custom-keybindings/custom0]",
                          result.stdout)
            self.assertIn("command='alacritty'", result.stdout)
            self.assertIn("command='emacsclient -c -a emacs'", result.stdout)
            self.assertIn("custom-keybindings/custom1/'", result.stdout)
            self.assertNotIn("custom-keybindings/custom-keybindings/", result.stdout)
            self.assertIn("switch-windows=['<Alt>Tab']", result.stdout)
            self.assertIn("toggle-tiled-left=['<Alt>Left']", result.stdout)
            self.assertIn("show-screenshot-ui=['<Control><Alt>g']", result.stdout)
            self.assertIn("www=['<Control><Alt>b']", result.stdout)
            self.assertFalse(marker.exists(), "keybindings-only ran unrelated settings commands")

    def test_import_failure_stops_without_reporting_success(self):
        with tempfile.TemporaryDirectory() as directory:
            fake_dconf = Path(directory) / "dconf"
            fake_dconf.write_text("#!/bin/sh\ncat >/dev/null\nexit 23\n")
            fake_dconf.chmod(0o755)
            env = dict(os.environ, PATH=directory + os.pathsep + os.environ["PATH"])
            result = subprocess.run(["sh", str(KEYBINDINGS), "--keybindings-only"],
                                    env=env, text=True, capture_output=True, timeout=5)
            self.assertEqual(result.returncode, 23)
            self.assertNotIn("keybindings applied", result.stdout)
            self.assertNotIn("Setup completed", result.stdout)


class MediaKeysEnvironmentTests(unittest.TestCase):
    def setUp(self):
        self.directory = tempfile.TemporaryDirectory()
        self.addCleanup(self.directory.cleanup)
        self.base = Path(self.directory.name)
        self.env = dict(os.environ, XDG_RUNTIME_DIR=str(self.base), XDG_SESSION_TYPE="wayland")
        for name in ("DISPLAY", "WAYLAND_DISPLAY", "WAYLAND_SOCKET"):
            self.env.pop(name, None)
        self.probe = self.base / "probe"
        self.probe.write_text('#!/bin/sh\nprintf "%s\\n" "${WAYLAND_DISPLAY-unset}" "${DISPLAY-unset}" "$@"\n')
        self.probe.chmod(0o755)

    def bind_socket(self, name):
        server = socket.socket(socket.AF_UNIX)
        self.addCleanup(server.close)
        server.bind(str(self.base / name))
        return server

    def launch(self, *args):
        return subprocess.run(["sh", str(MEDIA_KEYS), str(self.probe), *args],
                              env=self.env, text=True, capture_output=True, timeout=5)

    def test_missing_environment_uses_socket_and_preserves_arguments(self):
        self.bind_socket("wayland-7")
        (self.base / "wayland-7.lock").touch()
        result = self.launch("argument with spaces")
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertEqual(result.stdout.splitlines(), ["wayland-7", "unset", "argument with spaces"])

    def test_existing_wayland_environment_is_preserved(self):
        self.env["WAYLAND_DISPLAY"] = "custom-wayland"
        result = self.launch()
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertEqual(result.stdout.splitlines(), ["custom-wayland", "unset"])

    def test_existing_x11_environment_is_preserved(self):
        self.env["XDG_SESSION_TYPE"] = "x11"
        self.env["DISPLAY"] = ":9"
        result = self.launch()
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertEqual(result.stdout.splitlines(), ["unset", ":9"])

    def test_existing_wayland_socket_does_not_need_discovery(self):
        self.env["WAYLAND_SOCKET"] = "5"
        self.env.pop("XDG_RUNTIME_DIR")
        result = self.launch()
        self.assertEqual(result.returncode, 0, result.stderr)

    def test_ambiguous_displays_fail_without_starting_daemon(self):
        self.bind_socket("wayland-1")
        self.bind_socket("wayland-2")
        result = self.launch()
        self.assertNotEqual(result.returncode, 0)
        self.assertIn("Multiple Wayland displays", result.stderr)
        self.assertEqual(result.stdout, "")

    def test_waits_for_compositor_socket(self):
        timer = self.base / "timer"
        marker = self.base / "timer-used"
        timer.write_text('#!/bin/sh\nprintf "called\\n" > "$TIMER_MARKER"\n'
                         'exec "$REAL_SLEEP" "$@"\n')
        timer.chmod(0o755)
        self.env.update(GNOME_MEDIA_KEYS_SLEEP=str(timer), TIMER_MARKER=str(marker),
                        REAL_SLEEP=shutil.which("sleep"), PATH="")
        process = subprocess.Popen([shutil.which("sh"), str(MEDIA_KEYS), str(self.probe)],
                                   env=self.env, text=True, stdout=subprocess.PIPE,
                                   stderr=subprocess.PIPE)
        try:
            time.sleep(0.1)
            self.assertIsNone(process.poll())
            self.bind_socket("wayland-4")
            stdout, stderr = process.communicate(timeout=5)
            self.assertEqual(process.returncode, 0, stderr)
            self.assertEqual(stdout.splitlines(), ["wayland-4", "unset"])
            self.assertTrue(marker.exists(), "the declared timer was not used")
        finally:
            if process.poll() is None:
                process.kill()
            process.communicate()

    def test_missing_runtime_directory_fails_without_starting_daemon(self):
        self.env.pop("XDG_RUNTIME_DIR")
        result = self.launch()
        self.assertNotEqual(result.returncode, 0)
        self.assertIn("XDG_RUNTIME_DIR is required", result.stderr)
        self.assertEqual(result.stdout, "")


if __name__ == "__main__":
    unittest.main()
