#!/usr/bin/python3

"""
Converts a variety of filetypes to a *.txt file. Based on opp/py/to_csv.py.
"""

import os
import argparse
import re
import string
import sys
import shutil

import subprocess as sub

from xml.sax.saxutils import unescape


############################## CONVERTERS ######################################

# Based on Joe's csv_to_csv, ~/opp/py/to_csv.py.
def txt_to_txt(
        in_file,
        process_txts = False,
        **kwargs
):
    """
    Normalize a TXT.

    Note that this is *not* optimized to a simple copy operation if no
    corrections need to be performed.
    
    Arguments:
        :process_txts: Flag to run TXT processing. If this is false (default)
        then the function is a no-op.
    """
    
    if not process_txts:
        perr("Ignoring TXT {}".format(in_file))
        return

    real_fn = to_txt_ext(in_file)
    assert not os.path.exists(real_fn),\
        "{} already exists; aborting to avoid overwrite!".format(real_fn)

    # Correct null bytes and line breaks
    with open(real_fn, 'w') as fh_out:
        # Deals with encoding issues by replacing non utf-8 characters
        with open(in_file, 'r', errors = 'replace') as fh_in:
            clean_txt_file(fh_in, fh_out, **kwargs)

    return


def clean_text(
        in_text,
        null_byte_replace = '',
        return_replace = '',
        form_feed_replace = '',
        keep_empty_lines = False,
        **kwargs
):
    """
    Sanitizes text ill-formatted for Unix by removing carriage returns, form
    feed characters, and other undesirable bytes.

    Arguments:
        :null_byte_replace: Replace null bytes with the give character.
        :line_break_replace: Replace non-standard line breaks (e.g., "\r")
        :form_feed_replace: Replace form-feed (^L) characters.
        :rm_empty_lines: Flag to remove empty lines (i.e., replace "\n\n" with "\n")
    """
    out_text = in_text \
            .replace('\0', null_byte_replace) \
            .replace('\r', return_replace) \
            .replace('\f', form_feed_replace)

    if not keep_empty_lines:
        out_text = re.sub(r"\n\n+", r"\n", out_text)

    return out_text


def clean_txt_file(
        in_file,
        out_file,
        **kwargs
):
    """
    Sanitizes a (possibly) non-Unix TXT file. Wrapper for 'clean_text'.
    """
    while True:
        chunk = in_file.read(1024)
        if not chunk:
            break
        out_file.write(
                clean_text(chunk, **kwargs)
        )

    return


def pandoc(in_file, **kwargs):
    """
    Uses pandoc to convert a file to text.
    """

    require('pandoc', "Try installing 'pandoc'.")
    stdout = run(["pandoc", in_file, "-t", "plain"])
    # Clean any escaped html
    stdout = unescape(stdout)
    out_file = to_txt_ext(in_file)
    with open(out_file, 'w') as f:
        f.write(clean_text(stdout, **kwargs))

    return


def rtf_to_txt(in_file, **kwargs):
    """
    Converts a rich text file (RTF) to a txt file.
    """

    require('unrtf', "Try installing 'unrtf'.")
    stdout = run(['unrtf', '--nopict', '--text', '--quiet', in_file])
    stdout = unescape(stdout)
    stdout = stdout.split('-----------------')[1]
    out_file = to_txt_ext(in_file)
    with open(out_file, 'w') as f:
        f.write(clean_text(stdout, **kwargs))

    return


def doc_to_txt(in_file, **kwargs):
    """
    Converts a microsoft doc file to a txt file.
    """

    require('catdoc', "try installing 'catdoc'.")
    stdout = run(['antiword', in_file])
    out_file = to_txt_ext(in_file)
    with open(out_file, 'w') as f:
        f.write(clean_text(stdout, **kwargs))

    return


def docx_to_txt(in_file, **kwargs):
    """
    Converts a Microsoft docx file to a txt file.
    """

    pandoc(in_file, **kwargs)

    return


def pdf_to_txt(in_file, **kwargs):
    """
    Convergs _tabular_ data stored in a pdf into a usable txt file.
    """

    require('pdftotext', "Try installing 'pdftotext'.")
    stdout = run(
        ["pdftotext", "-table", "-clip", "-enc", "UTF-8", in_file, "-"]
    )
    out_file = to_txt_ext(in_file)
    with open(out_file, 'w') as f:
        # PDFs must keep page-break (i.e., form feed) characters as a
        # convenience for `pdf.R`.
        kwargs["form_feed_replace"] = "\f"
        f.write(clean_text(stdout, **kwargs))

    return

    

############################## COMMON ##########################################
#  Note: This code was copied from ~/opp/py/to_csv.py, with minor changes to
#+ adapt it to txt files.

def perr(*args, **kwargs):
    print(*args, file=sys.stderr, **kwargs)
    sys.stderr.flush()
    return


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
    return os.path.splitext(filename)[1].lstrip('.').lower()


def is_supported(file_type):
    return file_type in supported_file_types()


def supported_file_types():
    return [re.sub('_to_txt', '', func) for func in get_to_txt_funcs()]


def get_to_txt_funcs():
    return [k for k in globals().keys() if k.endswith('_to_txt')]


def get_converter(file_type, **kwargs):
    func_name = file_type + "_to_txt"
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
    return so.decode('utf-8', 'ignore')

def to_txt_ext(filename):
    return normalized_basename(filename) + ".txt"


def normalized_basename(filename):
    return normalize(filename_without_ext(filename))


def filename_without_ext(filename):
    return os.path.splitext(os.path.basename(filename))[0]


def normalize(text):
    return text.lower().replace(' ', '_')


def parse_args(argv):
    desc = 'supported file types: ' + ', '.join(supported_file_types())
    parser = argparse.ArgumentParser(
        prog=argv[0],
        description=desc,
        formatter_class=argparse.ArgumentDefaultsHelpFormatter
    )
    parser.add_argument(
        'files_or_dirs',
        nargs = '+',
        help = ', '.join([
            name + '.' + ext for name, ext in zip(
                list(string.ascii_lowercase),
                supported_file_types()
            )
        ] + ['data/'])
    )
    parser.add_argument(
        '-t', '--process_txts',
        action = 'store_true',
        help = 'Process files that are already TXTs. See the txt_* arguments '
             'for more processing options. By default TXTs passed into this '
             'script will be ignored.'
    )
    parser.add_argument(
        '--null_byte_replace',
        default = '',
        help = 'Character to replace null bytes with.'
    )
    parser.add_argument(
        '--return_replace',
        default = '',
        help = 'Character to replace carriage returns.'
    )
    parser.add_argument(
        '--form_feed_replace',
        default = '',
        help = 'Character to replace form feeds. NOTE: This is automatically '
            'ignored for PDFs as a convenienve for use with "pdf.R".'
    )
    parser.add_argument(
        '-k', '--keep_empty_lines',
        action = 'store_true',
        help = 'Leave empty lines in output.'
    )

    return parser.parse_args(argv[1:])


if __name__ == '__main__':
    args = parse_args(sys.argv)
    convert(
        args.files_or_dirs,
        null_byte_replace = args.null_byte_replace,
        return_replace = args.return_replace,
        form_feed_replace = args.form_feed_replace,
        keep_empty_lines = args.keep_empty_lines,
        process_txts = args.process_txts
    )
