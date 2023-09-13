#!/usr/bin/env python3

'''
Reads a NET Data jail activity report *.txt file and
converts it into a *.csv file suitable for further
cleaning and analyis.
'''

import argparse
import os
import sys
import re
import csv
from copy import copy

#  Defining constants for parsing.
#  ==================================================
#  Some are (arrays of) strings, and some are regexes.
#  NET data jail are fixed-width (except blank lines),
#+ which is useful for parsing/detecing if detecting 
#+ if any of the data are corrupted.

FILE_WIDTH = 133
MAIN_HEADER = [
        "NAME---------------------------",
        "ADDRESS-----------------------",
        "CITY-----------------",
        "ST-",
        "ZIP---",
        "S-",
        "RC-",
        "AGE--",
        "ARREST NO-",
        "BOOK-IN----",
        "RELEASE---",
        "\n"
        ]
OFFENSE_HEADER = [
        "CHARGE/OFFENSE                             ",
        "DEG ",
        "ARRESTED BY        ",
        "  WARRANT NO       ",
        "HOW RELEASE           ",
        "BOND AMT   ",
        # TODO:This may break for bonds over $999,999
        "FINE AMT",
        # TODO: This may break for fines over $999,999
        "      ",
        "\n"
        ]
PAGE_HEADER_RE = re.compile(
        # We can probably get away with much less...
        r"^DATE \d{2}/\d{2}/\d{4}\s+"
        # Date
        r"JAIL ACTIVITY REPORT FOR \d{2}/\d{2}/\d{4} "
        # Date again
        r"\d{2}:\d{2} THRU \d{2}/\d{2}/\d{4} \d{2}:\d{2}\s+"
        # Time + date/time
        r"\S+\s*"
        # This seems to identify who ran the report?
        r"PAGE\s*\d+\n$"
        # Page number
        )
SSN_LINE_RE = re.compile(r"^SSN:.{128}\n$")
PAPERREADY_RE = re.compile(
        r"^PAPERREADY: \d{2}/\d{2}/\d{4}.{110}\n$"
        )
ALIAS_RE = re.compile(r"^ALIAS:.{126}\n$")
TOTAL_RE = re.compile(r"^     TOTAL MALES.{116}\n$")

#  Defining classes to manage the information in the
#+ *.txt file.
#  ==================================================

class Offense:
    """
    Reformats a line of text containing an offense as
    structured data.
    """

    def __init__(self, line):
        attributes = [
                "description",
                "degree",
                "arrested_by",
                "warrant_no",
                "how_released",
                "bond_amount",
                "fine_amount",
                "boolean",
                ]
        col_delims = map(len, OFFENSE_HEADER)

        # Read the fixed width line
        pairs = zip(attributes, col_delims)
        for pair in pairs:
            slice_index = pair[1]
            attr = pair[0]
            value = line[:slice_index].strip()
            line = line[slice_index:]
            setattr(self, attr, value)

    def to_dict(self):
        return copy(vars(self))

class Entry:
    """
    Represents a single entry in the jail booking report.
    Personal information is stored as an attribute, and
    offenses are stored in an array.
    """

    def __init__(self, line):
        attributes = [
                "name",
                "add_street",
                "add_city",
                "add_state",
                "add_zip",
                "sex",
                "race",
                "age",
                "arrest_no",
                "book_in_date",
                "release_date"
                ]
        col_delims = map(len, MAIN_HEADER)

        # Read the fixed width line
        pairs = zip(attributes, col_delims)
        for pair in pairs:
            slice_index = pair[1]
            attr = pair[0]
            value = line[:slice_index].strip()
            line = line[slice_index:]
            setattr(self, attr, value)

        # Add the other attributes that are not contained on the line
        self.offenses = []
        self.aliases = []
        self.jail_pop = ''
        self.note = ''

    def append_offense(self, offense):
        assert isinstance(offense, Offense), (
                "Type error: object " + str(offense) +
                " is not an instance of Offense"
                )
        self.offenses.append(offense)

    def append_alias(self, line):
        assert line[0:6] == "ALIAS:", (
                "Type error: object " + str(line) +
                " is not a str or does not contain an alias."
                )
        self.aliases.append(line[6:].strip()) # 6==len("ALIAS:")

    def append_jail_pop(self, line):
        assert line[0:4] == "SSN:", (
                "Attempted to parse line" + line +
                " as SSN line, but failed."
                )
        # 43 == len("SSN: XXX-XX-XXXX  DOB: XX/XX/XXXX JAIL POP:")
        self.jail_pop = line[43:50].strip()
        self.note = line[50:].strip()

    def get_entry_dict(self):
        entry_dict = copy(vars(self))
        entry_dict["aliases"] = "|".join(self.aliases)
        entry_dict.pop("offenses")

        return entry_dict

    def get_offense_dicts(self):
        """
        Creates a dictionary for each offense along with a foreign key to
        identify the offense with the booking.
        """
        offense_dicts = []
        for offense in self.offenses:
            offense_dicts.append(offense.to_dict)

        return offense_dicts


#  Core functions that actually do the splitting and 
#+ parsing of text.
#  ==================================================

def parse_textblocks(rfile):
    """
    Separates the jail report into blocks of data
    corresponding to a single entry.
    """
    rfile.seek(0)

    active_lines = []
    textblocks = []
    total_line = ''

    # Triage the lines
    for line in rfile:
        if line == ''.join(MAIN_HEADER):
            if active_lines != []:
                textblocks.append(active_lines)
            active_lines = [line]
        elif not (
                line in ''.join(OFFENSE_HEADER)
                or PAGE_HEADER_RE.match(line)
                or PAPERREADY_RE.match(line)
                or line in '\r\n'
                or TOTAL_RE.match(line)
                ):
            active_lines.append(line)
        elif TOTAL_RE.match(line):
            total_line = line
            # Otherwise, the last line won't get
            #+appended before loop ends
            textblocks.append(active_lines)

    # Do some rudimentary error checking. The last line
    #+should contain the totals for the sheet.
    try:
        matches = re.findall(r"[\d,]+", total_line)

        assert len(matches) == 3, "Invalid total line.\
                Expected 3 numbers. Found " + str(len(matches)) + "."

        male_total = int(matches[0].replace(",", ""))
        female_total = int(matches[1].replace(",", ""))
        grand_total = int(matches[2].replace(",", ""))

        assert len(textblocks) == grand_total, (
                "Wrong number of textblocks. Expected " +
                str(len(textblocks)) +
                ". Found " +
                str(grand_total) +
                "."
                )

    except AssertionError as error:
        perr("There were parsing errors")
        perr(error)


    return textblocks

def parse_entry(textblock):
    """
    Creates a new entry and its concomitant offenses from a textblock.
    Expects textblocks to be in the form:
        1.          nameline
        (2--n).     aliases
        n+1.        SSN line
        (n+2--end). offenses
    """
    try:
        assert textblock[0] == "".join(MAIN_HEADER), (
                "Entry ill-formatted and does not begin with" +
                " proper header."
                )
        textblock.pop(0)
        new_entry = Entry(textblock.pop(0))
        for line in textblock:
            if ALIAS_RE.match(line):
                new_entry.append_alias(line)
            elif SSN_LINE_RE.match(line):
                new_entry.append_jail_pop(line)
            else:
                new_entry.append_offense(Offense(line))
        return new_entry
    except AssertionError as error:
        perr("There was an error parsing an entry.")
        perr(error)
        perr(textblock)

def process_file(rfile):
    """
    Processes a file and returns a list of entries.

    """
    textblocks = parse_textblocks(rfile)
    return map(parse_entry, textblocks)


def export_csv(rfiles):
    """
    Writes the report data to a csv DictWriter object. Two CSVs are generated
    for each input file: an "entries" csv, and an "offenses" CSV. The foreign
    key for "offenses" is name/arrest_no/book_in_date.
    """

    for rf in rfiles:
        perr("Processing %s..." % rf.name, end = '')
        wfilename = to_csv_ext(rf.name)
        fn = "clean_" + wfilename
        with open(fn, 'w') as entry_wf:
            writer = csv.DictWriter(
                    f = entry_wf,
                    fieldnames = [
                        "name",
                        "race",
                        "sex",
                        "age",
                        "aliases",
                        "arrest_no",
                        "book_in_date",
                        "release_date",
                        "jail_pop",
                        "add_street",
                        "add_city",
                        "add_state",
                        "add_zip",
                        "note",
                        "description",
                        "degree",
                        "arrested_by",
                        "warrant_number",
                        "how_released",
                        "bond_amount",
                        "fine_amount",
                        "warrant_no",
                        "boolean"
                        ]
                    )
            writer.writeheader()

            # Process each entry
            for entry in process_file(rf):
                for offense in entry.offenses:
                    writer.writerow(
                            {**entry.get_entry_dict(), **offense.to_dict()}
                    )

        rf.close()
        perr("done")


def parse_args():
    desc = (
            "Reads a NET Data jail activity report *.txt file and"
            "converts it into a *.csv file suitable for further"
            "cleaning and analyis."
            )
    parser = argparse.ArgumentParser(description = desc)
    parser.add_argument(
            "infiles",
            nargs = "+",
            type = argparse.FileType("r")
            )
    return parser.parse_args()


######################################## UTILS #################################


def perr(*args, **kwargs):
    print(*args, file=sys.stderr, **kwargs)
    sys.stderr.flush()
    return


def to_csv_ext(filename):
    return normalized_basename(filename) + '.csv'


def normalized_basename(filename):
    return normalize(filename_without_ext(filename))


def filename_without_ext(filename):
    return os.path.splitext(os.path.basename(filename))[0]


def normalize(text):
    return text.lower().replace(' ', '_')


######################################## MAIN ##################################


if __name__ == "__main__":
    args = parse_args()
    export_csv(args.infiles)
