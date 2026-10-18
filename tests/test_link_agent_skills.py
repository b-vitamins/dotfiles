"""Exercise agent skill deployment against a copied repository and a private home."""

import os
from pathlib import Path
import shutil
import subprocess
import tempfile
import unittest


ROOT = Path(__file__).resolve().parents[1]
LINKER = ROOT / "scripts/link-agent-skills.sh"


def write_skill(directory, name=None):
    directory.mkdir(parents=True)
    (directory / "SKILL.md").write_text(
        f"---\nname: {name or directory.name}\ndescription: Test skill.\n---\n\nBody.\n")


class LinkAgentSkillsTests(unittest.TestCase):
    def setUp(self):
        self._tmp = tempfile.TemporaryDirectory()
        base = Path(self._tmp.name).resolve()
        self.repo = base / "dotfiles"
        self.home = base / "home"
        self.home.mkdir()
        (self.repo / "scripts").mkdir(parents=True)
        shutil.copy2(LINKER, self.repo / "scripts" / LINKER.name)
        self.skills = self.repo / "skills"
        write_skill(self.skills / "alpha")
        write_skill(self.skills / "beta")
        # Support directories are never deployed, even if they look like skills.
        write_skill(self.skills / "_support")
        self.targets = [self.home / ".agents/skills", self.home / ".claude/skills"]

    def tearDown(self):
        self._tmp.cleanup()

    def run_linker(self, *args):
        env = dict(os.environ, HOME=str(self.home))
        env.pop("CLAUDE_CONFIG_DIR", None)
        env.pop("CODEX_HOME", None)
        return subprocess.run(["sh", str(self.repo / "scripts" / LINKER.name), *args],
                              env=env, text=True, capture_output=True, timeout=30)

    def assert_deployed(self):
        for target in self.targets:
            for name in ("alpha", "beta"):
                link = target / name
                self.assertTrue(link.is_symlink(), link)
                self.assertEqual(os.readlink(link), str(self.skills / name))
            self.assertFalse((target / "_support").exists())

    def test_links_every_skill_for_each_runtime_and_is_idempotent(self):
        result = self.run_linker()
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assert_deployed()
        result = self.run_linker("--check")
        self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
        self.assertIn("2 skills: up to date", result.stdout)

    def test_dry_run_and_check_change_nothing(self):
        result = self.run_linker("--dry-run")
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertIn("would link:", result.stdout)
        self.assertFalse((self.home / ".agents").exists())
        self.assertFalse((self.home / ".claude").exists())
        self.assertEqual(self.run_linker("--check").returncode, 1)

    def test_migrates_owned_links_and_leaves_foreign_entries_alone(self):
        legacy = self.home / ".codex/skills"
        (legacy / ".system").mkdir(parents=True)
        (legacy / "alpha").symlink_to(self.repo / "codex/skills/alpha")
        elsewhere = self.home / "elsewhere"
        write_skill(elsewhere)
        (legacy / "foreign").symlink_to(elsewhere)
        claude = self.home / ".claude/skills"
        (claude / "synced").mkdir(parents=True)
        (claude / "foreign").symlink_to(elsewhere)
        (claude / "alpha").symlink_to(self.repo / "codex/skills/alpha")
        (claude / "removed").symlink_to(self.skills / "removed")

        result = self.run_linker()
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assert_deployed()
        self.assertFalse((legacy / "alpha").is_symlink())
        self.assertTrue((legacy / ".system").is_dir())
        self.assertEqual(os.readlink(legacy / "foreign"), str(elsewhere))
        self.assertTrue((claude / "synced").is_dir())
        self.assertEqual(os.readlink(claude / "foreign"), str(elsewhere))
        self.assertFalse((claude / "removed").is_symlink())
        self.assertIn("repoint:", result.stdout)
        self.assertIn("remove legacy link:", result.stdout)
        self.assertIn("remove stale link:", result.stdout)

    def test_never_replaces_unmanaged_entries(self):
        own = self.home / ".claude/skills/alpha"
        own.mkdir(parents=True)
        (own / "SKILL.md").write_text("mine\n")
        result = self.run_linker()
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertIn("not managed by this script", result.stderr)
        self.assertFalse(own.is_symlink())
        self.assertEqual((own / "SKILL.md").read_text(), "mine\n")
        self.assertEqual(os.readlink(self.home / ".agents/skills/alpha"),
                         str(self.skills / "alpha"))


if __name__ == "__main__":
    unittest.main()
