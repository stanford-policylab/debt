#!/usr/bin/env python3

"""
Create a new directory structure and template for a given county.

NOTE: If there is already data in the data path, that data will be moved to the
"raw" folder. This is not done recursively to avoid accidental overwrites.
"""

import argparse
import os
import sys

from shutil import copyfile
from utils import chdir_to_debt_root, make_dir, fix_dir_perm, fix_file_perm


def init(args):
    chdir_to_debt_root()
    state = args.state.lower()
    county = '_'.join([x.lower().replace(' ', '_') for x in args.county])
    make_data_dirs(state, county)
    fix_permissions_data_dirs(state, county)

    return


def make_data_dirs(state, county):
    parent_dir = os.path.join('data', 'states', state, county)
    if os.path.exists(parent_dir):
        cur_files = os.listdir(path = parent_dir)
    else:
        cur_files = []
    sub_dirs = [
            'raw',
            'raw_txt',
            'raw_csv',
            'clean',
            ]
    for sub_dir in sub_dirs:
        make_dir(os.path.join(parent_dir, sub_dir))
        if sub_dir in cur_files:
            cur_files.remove(sub_dir)

    # Add any files that were previously in the dir to "raw"
    for f in cur_files:
        dst = os.path.join(parent_dir, "raw", f)
        src = os.path.join(parent_dir, f)
        if os.path.exists(dst):
            perr(dst + " already exists!")
            continue
        os.rename(src, dst)

    return


def fix_permissions_data_dirs(state, county):
    """
    Permissions for files and folders. In particular, files should have the 
    permission structure:
      (1) r-xr-x--- (0550) if they are in a raw folder,
      (2) rwxrwx--- (0770) if they are in a non-raw folder,
    and directories should have the permission structure:
      (1) r-xr-s--- (2550) if they are "*/raw",
      (2) rwxrws--- (2770) if they are a non-raw folder, (e.g., */raw_csv)
      (3) rwxr-s--- (2750) if they are not top-level (e.g., states/tx)
    """

    f_raw_perm=0o0550
    f_gen_perm=0o0770
    d_raw_perm=0o2550
    d_top_perm=0o2770
    d_gen_perm=0o2750

    top_dirs = [
            'raw_txt',
            'raw_csv',
            'clean',
            ]

    parent_dir = os.path.join('data', 'states', state, county)
    raw_dir = os.path.join(parent_dir, "raw")
    sub_dirs = [os.path.join(parent_dir, x) for x in top_dirs]

    # Set directory permissions
    fix_dir_perm(parent_dir, d_gen_perm)
    fix_dir_perm(raw_dir, d_raw_perm)
    for sub_dir in sub_dirs:
        fix_dir_perm(sub_dir, d_top_perm)

    # Set file permissions
    for f in os.listdir(raw_dir):
        f_path = os.path.join(raw_dir, f)
        fix_file_perm(f_path, f_raw_perm)
    for sub_dir in sub_dirs:
        for f in os.listdir(sub_dir):
            f_path = os.path.join(parent_dir, sub_dir, f)
            fix_file_perm(f_path, f_gen_perm)


def parse_args(argv):
    desc = (
            'Initializes the data and lib directories for a new county.\n'
            'NOTE: If the corresponding data dir already contains contents,'
            ' these are moved to the new "raw" folder.'
            )
    parser = argparse.ArgumentParser(prog = argv[0], description = desc)
    parser.add_argument('state')
    parser.add_argument('county', nargs = '+')

    return parser.parse_args(argv[1:])

def perr(*args, **kwargs):
    print(*args, file=sys.stderr, **kwargs)
    sys.stderr.flush()
    return


if __name__ == '__main__':
    init(parse_args(sys.argv))
