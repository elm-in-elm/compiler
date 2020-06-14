#!/usr/bin/env python

import re
import datetime
from lxml import etree
from lxml.builder import E

def CLASS(*args): # class is a reserved word in Python
    return {'class':' '.join(args)}

allowed_status = {
    'notstarted': 'Not started',
    'juststarted': 'Just started',
    'inprogress': 'In progress',
    'nearlydone': 'Nearly done',
    'complete': 'Complete'
}

def filter_comments(lines):
    return [line.strip() for line in lines 
            if line.strip() 
            and not line.startswith('//')]

def verify_status(status):
    if status not in allowed_status:
        raise Exception(f'Status {status} not allowed')

def parsed_line(line):
    title, status, *rest = line.split('|')
    if rest:
        notes = rest[0]
    else:
        notes = ''
    verify_status(status)
    return (title, status, notes)

def to_groups(lines):
    groups = []
    group_contents = []
    for line in lines:
        if line.startswith('#'):
            if group_contents:
                # we've finished the previous group
                groups.append((group_name,group_contents))
                group_contents = []
            group_name = line[2:]
        else:
            group_contents.append(parsed_line(line))
    if group_contents:
        groups.append((group_name, group_contents))
    return groups

def table_of_contents(groups):
    return E.ul(
        *[E.li(E.a(name, href='#'+name))
            for (name,_) in groups]
    )


def transform_gh_links(notes):
    new_notes = re.sub(
        r'#(\d+)',
        r'<a href="https://github.com/elm-in-elm/compiler/pull/\1">#\1</a>',
        notes
    )
    if new_notes:
        return etree.XML(f'<span>{new_notes}</span>')
    else:
        return new_notes

def item_html(item):
    title, status, notes = item
    return E.tr(
        CLASS(status),
        E.td(title),
        E.td(allowed_status[status]),
        E.td(transform_gh_links(notes))
    )

def group_html(group):
    name, contents = group
    return E.div(
        E.h2(name, id=name),
        E.table(
            CLASS('progress'),
            E.tr(
                E.th(
                    CLASS('progresstitle'),
                    'Title'
                ),
                E.th(
                    CLASS('progressstatus'),
                    'Status'
                ),
                E.th('Notes')
            ),
            *[item_html(item) for item in contents]
        )
    )

def last_updated():
    return E.div(f'Last updated: {str(datetime.date.today())}')

def to_html(groups):
    return E.html(
        E.head(
            E.title('elm-in-elm development progress'),
            E.link(rel='stylesheet', href='style.css')
        ),
        E.body(
            E.h1('elm-in-elm development progress'),
            last_updated(),
            table_of_contents(groups),
            *[group_html(group) for group in groups]
        )
    )


with open('data.txt') as f:
    data = f.readlines()

data = filter_comments(data)
groups = to_groups(data)
html = to_html(groups)
output = etree.tostring(html, pretty_print=True)

with open('dist/index.html', 'wb') as f:
    f.write(output)
