#!/usr/bin/env python3

# Find YYYY-MM-DD date strings in stdin and pipe them to stdout with week
# numbers and days of the week added.

import re
import sys
from dataclasses import dataclass
from datetime import datetime


@dataclass
class Entry:
    date: str
    full_message: str
    message: str
    project: str


def parse(line: str) -> Entry:
    date_pattern = r"^x (\d{4}-\d{2}-\d{2})"
    match = re.search(date_pattern, line)
    if match:
        date = match.group(1)
        project = ""

        full_message = line.replace(match.group(), "", 1).strip()
        message = full_message

        # First project pattern, if the line ends with " (subdir/xyzzy.ext)", the project is "xyzzy"
        filename_project_pattern = r" \((.*?\/)*([^.) ]+)(\.[^) ]+)\)$"
        filename_match = re.search(filename_project_pattern, line)
        if filename_match:
            project = filename_match.group(2)
            message = line.replace(filename_match.group(), "", 1).strip()
        else:
            # Second project pattern, find the first +tag string in line and use that.
            tag_project_pattern = r" \+(\w+)"
            tag_match = re.search(tag_project_pattern, line)
            if tag_match:
                project = f"+{tag_match.group(1)}"

        return Entry(
            date,
            full_message,
            message,
            project,
        )
    return Entry("", line.strip(), "")


def decorate_dates(entries: list[Entry]):
    """Decorate entries with week numbers and days of the week."""
    last_weeknum = None

    for e in entries:
        if e.date:
            date_obj = datetime.strptime(e.date, "%Y-%m-%d")
            week_number = date_obj.isocalendar()[1]
            day_of_week = date_obj.strftime("%a")
            if last_weeknum is not None and last_weeknum != week_number:
                print()
            last_weeknum = week_number
            print(
                f"x {e.date} w{week_number:02} {day_of_week}  {e.full_message}"
            )
        else:
            print(e.full_message)


def group_projects(entries: list[Entry], max_items: int = 6):
    """List latest done tasks from each separate project."""

    # Group entries by project.
    # Sort by the latest done entry on each project.
    project_entries: dict[str, list[Entry]] = {}
    for e in entries:
        if e.project:
            project_entries.setdefault(e.project, []).append(e)
        else:
            project_entries.setdefault("", []).append(e)
    sorted_projects = sorted(
        project_entries.items(),
        key=lambda item: max(
            (datetime.strptime(e.date, "%Y-%m-%d") for e in item[1] if e.date),
            default=datetime.min,
        ),
    )

    for project, entries in sorted_projects:
        print(f"{project or '-'}")
        for e in entries[-max_items:]:
            print(f"  x {e.date}  {e.message}")
        print()


def command() -> str:
    # If argv[0] does not end in ".py", we're not running as a script, use basename of argv[0] as value.
    if not sys.argv[0].endswith(".py"):
        return sys.argv[0].split("/")[-1]
    if len(sys.argv) > 1:
        return sys.argv[1]
    return ""

if __name__ == "__main__":
    entries: list[Entry] = []
    for line in sys.stdin:
        entries.append(parse(line.strip()))

    cmd = command()
    if cmd == "decorate-dates":
        decorate_dates(entries)
    if cmd == "group-projects":
        group_projects(entries)
