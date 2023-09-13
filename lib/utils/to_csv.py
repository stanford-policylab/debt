#!/usr/bin/env python3


'''
converts files from <ext> to csv files

To support <ext> conversions:
1. Add a function named <ext>_to_csv to the CONVERTERS section below
2. You can use the require(<program_name>) to ensure that an executable exists
3. You can use run(<command>) to run a bash command
4. Files should be saved in the current directory with the same filename and a
csv extension, so /path/to/file.xls saves to file.csv
5. You can use to_csv_ext(<filename>) to get <filename> with a csv extension
'''


import argparse
import csv
import os
import re
import shutil
import string
import sys
import tempfile

import pandas as pd
import subprocess as sub
import xml.etree.ElementTree as et

from collections import defaultdict

############################### CONVERTERS ####################################


def csv_to_csv(in_file,
        process_csvs=False,
        csv_null_byte_replace='',
        csv_row_headers=None,
        **kwargs):
    """Normalize a CSV.

    Note this is *not* optimized to a simple copy operation if no corrections
    need to be performed.

    Arguments:
        :process_csvs: Flag to run CSV processing. If this is false (default)
        then this function is a no-op.
        :csv_null_byte_replace: Replace null bytes with the given character.
        :csv_row_headers: The number of cells at the beginning of the file
        which represent the headers. In the normal case, headers are not
        included in each row.

    Examples:
        csv_row_headers is used to fix files that list column headers on each
        row. For example, a CSV with three "real" columns might look like:

            Header1,Header2,Header3,Value1 A,Value2 A,Value3 A
            Header1,Header2,Header3,Value1 B,Value2 B,Value3 B
            Header1,Header2,Header3,Value1 C,Value2 C,Value3 C,Junk

        Setting csv_row_headers=3 will transform this CSV into:

            Header1,Header2,Header3
            Value1 A,Value2 A,Value3 A
            Value1 B,Value2 B,Value3 B
            Value1 C,Value2 C,Value3 C

        Note that the last "Junk" cell from line three was dropped.
    """
    if not process_csvs:
        perr("Ignoring CSV {}".format(in_file))
        return

    real_fn = normalize(
            os.path.join(os.getcwd(), os.path.basename(in_file))
            )
    assert not os.path.exists(real_fn),\
            '{} exists; aborting to avoid overwrite!'.format(real_fn)
    fh_out = tempfile.SpooledTemporaryFile(mode='w+')
    fh_tmp_in = tempfile.SpooledTemporaryFile(mode='w+')

    # Correct null bytes if neessary
    with open(in_file, 'r') as fh_in:
        while True:
            chunk = fh_in.read(1024)
            if not chunk:
                break
            fh_tmp_in.write(chunk.replace('\0', csv_null_byte_replace))
    # Rewind byte-corrected file to read rows
    fh_tmp_in.seek(0)

    # Read file as CSV
    rdr_in = csv.reader(fh_tmp_in)
    rdr_out = csv.writer(fh_out)
    headers = []
    # The total number of cols to pull from each row (None means "all cols")
    ncol = csv_row_headers * 2 if csv_row_headers else None
    # Number of header columns in each row (may be None to mean 0)
    hcol = csv_row_headers
    for i, line in enumerate(rdr_in):
        # Detect headers if necessary
        if not headers:
            headers = line[:hcol]
            rdr_out.writerow(headers)
            # If not using row headers (default), skip to next row
            if not csv_row_headers:
                continue
        # Warn if trimming columns unexpectedly
        if ncol and len(line) != ncol:
            perr('Warning! Line {} has wrong number of rows! '
                    '{}, expected {}. Truncating this row.'.format(i, len(line), ncol))
            rdr_out.writerow(line[hcol:ncol])

    # Move to the real location.
    with open(real_fn, 'w') as fh:
        fh_out.seek(0)
        shutil.copyfileobj(fh_out, fh)

    fh_out.close()
    fh_tmp_in.close()
    return


def xls_to_csv(in_file, **kwargs):
    return ssconvert(in_file, **kwargs)


def ssconvert(in_file, **kwargs):
    require('ssconvert', 'try installing "gnumeric" package on linux')
    out_file = to_csv_ext(in_file)
    run(['ssconvert', '--export-file-per-sheet', in_file, out_file])
    basenames = defaultdict(int)
    for filename in os.listdir('.'):
        if '.csv.' in filename:
            basename, index = filename.split('.csv.')
            basenames[basename] += 1
            new_out_filename = '%s_sheet_%d.csv' % (basename, int(index) + 1)
            os.rename(filename, new_out_filename)
    for basename, count in basenames.items():
        # NOTE: if there is only one sheet output, remove _sheet_1 suffix
        if count == 1:
            os.rename(basename + '_sheet_1.csv', basename + '.csv')
    return


def xlsx_to_csv(in_file, **kwargs):
    return ssconvert(in_file, **kwargs)


def xltx_to_csv(in_file, **kwargs):
    return ssconvert(in_file, **kwargs)


def mdb_to_csv(in_file, **kwargs):
    require('mdb-tables', "try installing 'mdbtools' package on linux")
    require('mdb-export', "try installing 'mdbtools' package on linux")
    basename = normalized_basename(in_file)
    stdout = run(['mdb-tables', '-1', in_file])
    tbls = [tbl for tbl in stdout.split('\n') if tbl != '']
    for tbl in tbls:
        contents = run(['mdb-export', in_file, tbl])
        tablename = normalize(tbl)
        out_file = basename
        if tablename != basename:
            out_file += '_' + normalize(tbl)
        with open(out_file + '.csv', 'w') as f:
            f.write(contents)
    return


def accdb_to_csv(in_file, **kwargs):
    mdb_to_csv(in_file)
    return


def xml_to_csv(in_file, recursive = False, **kwargs):
    '''
    https://stackoverflow.com/questions/41776263/ \
            pandas-read-xml-method-test-strategies
    '''
    tree = et.parse(in_file)
    data = []
    inner = {}
    if recursive:
        xpath = './/*'
    else:
        xpath = '*'
    for el in tree.iterfind("./*"):
        for i in el.iterfind(xpath):
            inner[i.tag] = i.text
        data.append(inner)
        inner = {}
    df = pd.DataFrame(data)
    to_csv(df, in_file)
    return


def txt_to_csv(in_file, sep, **kwargs):
    # NOTE: don't try to infer data types here; read everything as str.
    df = pd.read_table(in_file, sep=sep, dtype=str)
    to_csv(df, in_file)
    return


def dat_to_csv(in_file, **kwargs):
    perr('please pass an additional config file to parse dat!')
    return


def ipums_dat_to_csv(in_file, sas_load_file, **kwargs):
    mapping = sas_extract_mapping(sas_load_file)
    colnames, colspecs, colmaps = sas_extract_column_properties(mapping)
    df = pd.read_fwf(
            in_file,
            header=None,
            names=colnames,
            colspecs=colspecs,
            dtype=str,
            )
    for col in df:
        df[col] = df[col].replace(colmaps[col])
    to_csv(df, in_file)
    return


def sas_extract_column_properties(mapping):
    cols = []
    colmaps = {}
    for name, d in mapping.items():
        colmaps[d['full_name']] = d['translations']
        cols.append((d['start_index'], d['end_index'], d['full_name']))
    cols_sorted = sorted(cols)
    colspecs = []
    colnames = []
    for col in cols_sorted:
        colspecs.append((col[0], col[1]))
        colnames.append(col[2])
    return colnames, colspecs, colmaps


def sas_extract_mapping(sas_load_file):
    with open(sas_load_file) as f:
        text = f.read()
    translations = sas_extract_translations(text)
    labels = sas_extract_labels(text)
    indices = sas_extract_indices(text)
    mapping = {}
    for name, full_name in labels.items():
        mapping[name] = {
                'full_name': full_name,
                'start_index': indices[name]['start'],
                'end_index': indices[name]['end'],
                'translations': translations.get(name, {})
                }
        return mapping


def sas_extract_translations(text):
    blocks = sas_extract_blocks('value', text)
    return dict(map(sas_parse_value_block, blocks))


def sas_extract_blocks(block_type, text):
    return re.findall(block_type + '(.*?)\n;\n', text, re.DOTALL)


def sas_parse_value_block(block):
    name, block = block.split('\n', 1)
    return (name.strip().replace('_f', ''), sas_parse_translation_block(block))


def sas_parse_translation_block(block):
    # re.findall('\s+(\w+)\s+=\s+"(.*?)"(\s+"(.*?)")?', s, re.DOTALL)
    # NOTE: this function only captures the first and second lines of a
    # key-value match (have yet to see one with 3 lines)
    pattern = re.compile(r'''
        \s+         # beginning whitespace 
        (\w+)       # key
        \s+         # space before '='
        =           # key-value delimiter 
        \s+         # space after '='
        "(.*?)"     # non-greedy inside-quotes match for first line
        (           # start of optional match for second line
            \s+     # newline and preceding whitespace for second line
            "(.*?)" # second line match
        )?          # end of optional match for second line
    ''', re.DOTALL | re.VERBOSE)
    d = {}
    for match in re.findall(pattern, block):
        # NOTE: match[2] is optional outer capture for second group
        key, line_1, line_2 = match[0], match[1], match[3]
        d[key] = line_1 + line_2
    return d


def sas_extract_labels(text):
    block = sas_extract_blocks('label', text)[0]
    return sas_parse_translation_block(block)


def sas_extract_indices(text):
    block = sas_extract_blocks('input', text)[0]
    # 'input' line examples:
    # DATANUM 5-6
    # HHWT 15-24 0.2
    # NOTE: care about column name and indices (not the last column, i.e. 0.2)
    lines = [line.strip().split()[:2] for line in block.split('\n') if line]
    def extract_indices(lst):
        name, (start, end) = lst[0], lst[1].split('-')
        # NOTE: return [start, end) for easy use in other python functions
        return (name, {'start': int(start) - 1, 'end': int(end)})
    return dict(map(extract_indices, lines))


def sql_server_to_csv(in_file, sql_server_load_file, **kwargs):
    # NOTE: this works for the versions of files we have, but may need
    # to be extended if the format file structures change
    df_fmt = pd.read_fwf(sql_server_load_file, header=None, skiprows=2)
    width_col = 3
    colname_col = 6
    df = pd.read_fwf(
            in_file,
            header=None,
            names=list(df_fmt[colname_col]),
            widths=list(df_fmt[width_col]),
            dtype=str
            )
    to_csv(df, in_file)
    return


############################### COMMON ########################################


def convert(files_or_dirs, **kwargs):
    for f_or_d in files_or_dirs:
        if not os.path.exists(f_or_d):
            perr(f_or_d + " doesn't exist!")
        elif os.path.isfile(f_or_d):
            convert_file(f_or_d, **kwargs)
        elif os.path.isdir(f_or_d):
            for f in os.listdir(f_or_d):
                convert_file(os.path.join(f_or_d, f), **kwargs)
        else:
            perr(f_or_d + " isn't a file or directory!")
    return


def convert_file(filename, **kwargs):
    perr('converting %s...' % filename, end='')
    file_type = get_file_type(filename)
    if is_supported(file_type):
        get_converter(file_type, **kwargs)(filename, **kwargs)
        perr('done')
        return
    perr(" is not a supported file type for conversion!")
    return


def get_file_type(filename):
    return os.path.splitext(filename)[1].lstrip('.')


def is_supported(file_type):
    return file_type in supported_file_types()


def supported_file_types():
    return [re.sub('_to_csv', '', func) for func in get_to_csv_funcs()]


def get_to_csv_funcs():
    return [k for k in globals().keys() if k.endswith('_to_csv')]


def perr(*args, **kwargs):
    print(*args, file=sys.stderr, **kwargs)
    sys.stderr.flush()
    return


def get_converter(
        file_type,
        sas_load_file=None,
        sql_server_load_file=None,
        **kwargs
        ):
    func_name = file_type + '_to_csv'
    # NOTE: IPUMS provides fixed width dat files with custom load scripts;
    # SAS are the easiest to parse and convert
    if sas_load_file:
        func_name = 'ipums_dat_to_csv'
    # NOTE: sql server files are txt files, but have fixed width fields
    # specified in <file>_format.txt files
    if sql_server_load_file:
        func_name = 'sql_server_to_csv'
    return globals()[func_name]


def require(program, debug=''):
    if not which(program):
        perr(program + ' is a binary required for conversion, and is missing!')
        if debug:
            perr(debug) 
        sys.exit(1)
    return


# source: https://tinyurl.com/yd5f6l6d
def which(program):
    '''
    emulates linux which command to find an executable's full path
    '''
    fpath, _ = os.path.split(program)
    if fpath:
        if is_exe(program):
            return program
    else:
        for path in os.environ["PATH"].split(os.pathsep):
            path = path.strip('"')
            exe_file = os.path.join(path, program)
            if is_exe(exe_file):
                return exe_file
    return


def is_exe(fpath):
    return os.path.isfile(fpath) and os.access(fpath, os.X_OK)


def run(cmd, **kwargs):
    if 'shell' in kwargs:
        # if arguments need to be interpreted or spawn subshells
        # the cmd must be a single string and shell=True
        cmd = ' '.join(cmd)
    p = sub.Popen(cmd, stdout=sub.PIPE, stderr=sub.PIPE, **kwargs)
    so, se = p.communicate()
    ret = p.returncode
    cmd_str = 'COMMAND: ' + str(cmd)
    if ret:  # non-zero return code
        perr(cmd_str + '\n\tFAILED!')
        perr(cmd_str)
        perr('STDOUT:\n' + str(so))
        perr('STDERR:\n' + str(se))
    return so.decode('utf-8')


def to_csv_ext(filename):
    return normalized_basename(filename) + '.csv'

def normalized_basename(filename):
    return normalize(filename_without_ext(filename))


def filename_without_ext(filename):
    return os.path.splitext(os.path.basename(filename))[0]


def normalize(text):
    return text.lower().replace(' ', '_')


def to_csv(df, in_file):
    df.to_csv(to_csv_ext(in_file), index=False)
    return


def parse_args(argv):
    desc = 'supported file types: ' + ', '.join(supported_file_types())
    parser = argparse.ArgumentParser(
            prog=argv[0],
            description=desc,
            formatter_class=argparse.ArgumentDefaultsHelpFormatter
            )
    parser.add_argument(
            'files_or_dirs',
            nargs='+',
            help=', '.join([
                name + '.' + ext for name, ext in zip(
                    list(string.ascii_lowercase),
                    supported_file_types()
                    )
                ] + ['data/'])
            )
    parser.add_argument(
            '-s',
            '--sep',
            default='|',
            help='separator for txt files'
            )
    parser.add_argument(
            '-sas',
            '--sas_load_file',
            help='SAS load file provided by IPUMS to complement dat file'
            )
    parser.add_argument(
            '-ss',
            '--sql_server_load_file',
            help='https://docs.microsoft.com/en-us/sql/relational-databases/'
            'import-export/non-xml-format-files-sql-server to complement '
            'fixed width text file'
            )
    parser.add_argument(
            '-c', '--process_csvs',
            action='store_true',
            help='Process files that are already CSVs. See the csv_* arguments '
            'for more processing options. By default CSVs passed into this '
            'script will be ignored'
            )
    parser.add_argument(
            '--csv_row_headers',
            type=int,
            default=None,
            help="Number of header cells included in each row"
            )
    parser.add_argument(
            '--csv_null_byte_replace',
            default='',
            help='Character to replace null bytes with'
            )
    parser.add_argument(
            '-r', '--recursive',
            action = "store_true",
            help = "Search the XML tree recursively for data fields. Produces "
            "one row for each grandchild, one column for each child thereof."
            )
    return parser.parse_args(argv[1:])


if __name__ == '__main__':
    args = parse_args(sys.argv)
    convert(
            args.files_or_dirs,
            sep=args.sep,
            sas_load_file=args.sas_load_file,
            sql_server_load_file=args.sql_server_load_file,
            process_csvs=args.process_csvs,
            csv_row_headers=args.csv_row_headers,
            csv_null_byte_replace=args.csv_null_byte_replace,
            recursive=args.recursive
            )
