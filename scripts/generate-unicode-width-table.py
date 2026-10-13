#!/usr/bin/env python3
"""Generate the pinned Unicode 17.0.0 table-width Lisp source."""

import argparse
import re
import sys
from pathlib import Path


VERSION = "17.0.0"
# Pinned Unicode 17.0.0 UCD inputs and their SHA-256 digests:
# https://www.unicode.org/Public/17.0.0/ucd/EastAsianWidth.txt
#   ea7ce50f3444a050333448dffef1cadd9325af55cbb764b4a2280faf52170a33
# https://www.unicode.org/Public/17.0.0/ucd/extracted/DerivedGeneralCategory.txt
#   d62e5bab70ca74f099343f71224fa051cb1fdd61a1ab45c0488c44cfc0b6102e
EAST_ASIAN_WIDTH_FILE = "EastAsianWidth.txt"
GENERAL_CATEGORY_FILE = "DerivedGeneralCategory.txt"
WIDE_DEFAULTS = (
    (0x3400, 0x4DBF),
    (0x4E00, 0x9FFF),
    (0xF900, 0xFAFF),
    (0x20000, 0x2FFFD),
    (0x30000, 0x3FFFD),
)

LICENSE_COMMENTS = """;;;; Unicode License v3
;;;; COPYRIGHT AND PERMISSION NOTICE
;;;;
;;;; Copyright © 1991-2026 Unicode, Inc.
;;;;
;;;; Permission is hereby granted, free of charge, to any person obtaining a
;;;; copy of data files and any associated documentation (the "Data Files") or
;;;; software and any associated documentation (the "Software") to deal in the
;;;; Data Files or Software without restriction, including without limitation
;;;; the rights to use, copy, modify, merge, publish, distribute, and/or sell
;;;; copies of the Data Files or Software, and to permit persons to whom the
;;;; Data Files or Software are furnished to do so, provided that either (a) this
;;;; copyright and permission notice appear with all copies of the Data Files or
;;;; Software, or (b) this copyright and permission notice appear in associated
;;;; Documentation.
;;;;
;;;; Source: EastAsianWidth.txt and extracted/DerivedGeneralCategory.txt,
;;;; Unicode Character Database 17.0.0. See THIRD-PARTY-NOTICES.
"""


def code_range(value):
    parts = value.strip().split("..")
    if len(parts) == 1:
        start = end = int(parts[0], 16)
    elif len(parts) == 2:
        start, end = (int(part, 16) for part in parts)
    else:
        raise ValueError("invalid code point range: " + value)
    if start > end or end > 0x10FFFF:
        raise ValueError("invalid code point range: " + value)
    return start, end


def parse_property_file(path, expected_stem):
    text = path.read_text(encoding="utf-8")
    version_header = expected_stem + "-" + VERSION + ".txt"
    if version_header not in text.splitlines()[0]:
        raise ValueError(path.name + " is not the pinned Unicode " + VERSION + " file")
    return text.splitlines()


def merge_intervals(intervals):
    merged = []
    for start, end in sorted(intervals):
        if merged and start <= merged[-1][1] + 1:
            merged[-1] = (merged[-1][0], max(merged[-1][1], end))
        else:
            merged.append((start, end))
    return merged


def property_intervals(lines, selected, include_missing=False):
    intervals = []
    for line in lines:
        if include_missing:
            missing = re.search(r"@missing:\s*([0-9A-Fa-f.]+)\s*;\s*([A-Za-z]+)", line)
            if missing and missing.group(2) in selected:
                intervals.append(code_range(missing.group(1)))
        record = line.split("#", 1)[0].strip()
        if not record or ";" not in record:
            continue
        code_points, property_name = (field.strip() for field in record.split(";", 1))
        if property_name in selected:
            intervals.append(code_range(code_points))
    return merge_intervals(intervals)


def lisp_intervals(name, intervals):
    lines = ["(defparameter " + name, "  #("]
    for start, end in intervals:
        lines.append("    (#x%X . #x%X)" % (start, end))
    lines.extend(["    ))", ""])
    return lines


def render_source(ucd_root):
    east_asian_width = ucd_root / EAST_ASIAN_WIDTH_FILE
    general_category = ucd_root / "extracted" / GENERAL_CATEGORY_FILE
    if not east_asian_width.is_file() or not general_category.is_file():
        raise ValueError(
            "expected " + EAST_ASIAN_WIDTH_FILE + " and extracted/" + GENERAL_CATEGORY_FILE
        )

    east_lines = parse_property_file(east_asian_width, "EastAsianWidth")
    category_lines = parse_property_file(general_category, "DerivedGeneralCategory")
    wide = merge_intervals(
        property_intervals(east_lines, {"W", "F"}, include_missing=True) + list(WIDE_DEFAULTS)
    )
    marks = property_intervals(category_lines, {"Mn", "Mc", "Me"})

    output = [LICENSE_COMMENTS.rstrip(), "", "(in-package #:org-kit)", ""]
    output.extend(["(defparameter +unicode-width-table-version+ \"" + VERSION + "\")", ""])
    output.extend(lisp_intervals("+unicode-17-wide-intervals+", wide))
    output.extend(lisp_intervals("+unicode-17-mark-intervals+", marks))
    output.extend(
        [
            "(defun table-code-point-in-intervals-p (code-point intervals)",
            "  (loop with low = 0",
            "        with high = (length intervals)",
            "        while (< low high)",
            "        for middle = (floor (+ low high) 2)",
            "        for interval = (aref intervals middle)",
            "        do (cond ((< code-point (car interval)) (setf high middle))",
            "                 ((> code-point (cdr interval)) (setf low (1+ middle)))",
            "                 (t (return-from table-code-point-in-intervals-p t)))",
            "        finally (return nil)))",
            "",
            "(defun table-code-point-width (code-point)",
            "  (cond ((table-code-point-in-intervals-p code-point",
            "                                         +unicode-17-mark-intervals+)",
            "         0)",
            "        ((table-code-point-in-intervals-p code-point",
            "                                         +unicode-17-wide-intervals+)",
            "         2)",
            "        (t 1)))",
            "",
            "(defun table-character-width (character)",
            "  (table-code-point-width (char-code character)))",
            "",
            "(defun table-string-width (string)",
            "  (loop for character across string",
            "        sum (table-character-width character)))",
            "",
        ]
    )
    return "\n".join(output)


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("ucd_root", type=Path, help="Unicode 17.0.0 UCD directory")
    outputs = parser.add_mutually_exclusive_group()
    outputs.add_argument("--output", type=Path, default=Path("src/table-width.lisp"))
    outputs.add_argument("--check", type=Path, help="fail if this file differs from generated output")
    args = parser.parse_args()

    try:
        source = render_source(args.ucd_root)
    except (OSError, ValueError) as error:
        parser.error(str(error))

    if args.check:
        try:
            current = args.check.read_text(encoding="utf-8")
        except OSError as error:
            parser.error(str(error))
        if current != source:
            print(str(args.check) + " is not up to date", file=sys.stderr)
            return 1
        return 0

    with args.output.open("w", encoding="utf-8", newline="\n") as output:
        output.write(source)
    return 0


if __name__ == "__main__":
    sys.exit(main())
