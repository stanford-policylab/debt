# Liberally copied from opp/py/utils.py

import os
import re
import sys


def chdir_to_debt_root():
    d = debt_root_dir()
    os.chdir(d)
    return d


def debt_root_dir():
    this_dir = os.path.dirname(os.path.realpath(__file__))
    # NOTE: this assumes debt root is two levels up (DEBTROOT/lib/utils/init.py)
    return os.path.abspath(os.path.join(this_dir, os.pardir, os.pardir))


def is_online():
    import socket
    try:
        socket.create_connection(("www.google.com", 80))
        return True
    except OSError:
        pass
    return False


def syntax_highlight_code(code, language):
    from pygments import highlight
    from pygments.lexers import get_lexer_by_name
    from pygments.formatters import Terminal256Formatter
    return highlight(
        code,
        get_lexer_by_name(language),
        Terminal256Formatter()
    )


def syntax_highlight_path(path):
    from pygments import highlight
    from pygments.lexers import get_lexer_for_filename
    from pygments.formatters import Terminal256Formatter
    with open(path) as f:
        code = f.read()
    return highlight(
        code,
        get_lexer_for_filename(filename),
        Terminal256Formatter()
    )


def make_dir(d, mode = 0o2755):
    if not os.path.exists(d):
        os.makedirs(d, mode)
    return

def fix_dir_perm(d, mode):
    assert os.path.isdir(d), d + " should be a directory."
    os.chmod(d, mode)


def fix_file_perm(f, mode):
    assert os.path.isfile(f), f + " should be a regular file."
    os.chmod(f, mode)


def git_pull_rebase(git_dir):
    import git
    git.cmd.Git(git_dir).pull('--rebase')
    return


def git_pull_rebase_if_online(git_dir):
    if (is_online()):
        git_pull_rebase(git_dir)
    return


def strip_margin(text):
    indent = len(min(re.findall('\n[ \t]*(?=\S)', text) or ['']))
    pattern = r'\n[ \t]{%d}' % (indent - 1)
    return re.sub(pattern, '\n', text)


def confirm(question, default='yes'):
    valid = {
        'yes': True,
        'y': True,
        'ye': True,
        'no': False,
        'n': False
    }
    prompt = ' [{yes}/{no}] '.format(
        yes='Y' if default == 'yes' else 'y',
        no='N' if default == 'no' else 'n'
    )
    while True:
        print(question + prompt, end='')
        choice = input().lower()
        if default is not None and choice == '':
            return valid[default]
        elif choice in valid:
            return valid[choice]
        else:
            print('please respond with "[y]es" or "[n]o"')
