import sys

from pathlib import Path

_project_root_dir = Path(__file__).resolve().parents[0]
sys.path.append(str(_project_root_dir))
