# Group Plan Format

Use `plans/groups.csv` to define logical transcription units before parallel work begins.

## CSV Schema

- `group`: Logical unit name (chapter, section, appendix range).
- `start`: First page number (inclusive).
- `end`: Last page number (inclusive).
- `target_tex`: Output `.tex` path for this group.

## Rules

- Use integer page numbers matching rendered filenames (`page-<n>.png`).
- Keep ranges non-overlapping.
- Cover the exact requested scope with no gaps.
- Use stable names that make ownership obvious (for example `chapter-10`).

## Example

```csv
group,start,end,target_tex
chapter-08,121,146,transcriptions/chapter-08.tex
chapter-09,147,187,transcriptions/chapter-09.tex
chapter-10,188,270,transcriptions/chapter-10.tex
```

## Validation and Assignment

Run:

```bash
python3 scripts/assign-groups.py --groups plans/groups.csv --agents 3 --out plans/agent-assignments.csv
```

If overlap exists, `assign-groups.py` exits with an error.
