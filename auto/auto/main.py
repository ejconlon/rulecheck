from argparse import ArgumentParser
from typing import Any, Dict, List, Optional
from bs4 import BeautifulSoup
import json
import os
import re
import requests as req
import pathlib
import shutil


RESOLVER = 'lts-19.27'
PAGE = f'https://www.stackage.org/{RESOLVER}'


# These can't be unpacked - I think most are boot packages
BLACKLIST = set([
    'array',
    'base',
    'binary',
    'bytestring',
    'Cabal',
    'containers',
    'deepseq',
    'directory',
    'exceptions',
    'filepath',
    'ghc',
    'ghc-bignum',
    'ghc-prim',
    'integer-gmp',
    'mtl',
    'parsec',
    'pretty',
    'process',
    'stm',
    'template-haskell',
    'terminfo',
    'text',
    'time',
    'transformers',
    'unix',
    'xhtml',
])


RULES_RE = re.compile(r'{-#\s+RULES\b', re.MULTILINE)


def parse_package(full_name: str) -> Dict[str, str]:
    name, version = full_name.rsplit('-', maxsplit=1)
    return {'name': name, 'version': version}


def fake(listing: str):
    assert not os.path.exists(listing)
    packages: List[Dict[str, str]] = [{'name': 'vector', 'version': '0.12.3.1'}]
    with open(listing, 'w') as f:
        f.write(json.dumps(packages, sort_keys=True, indent=2))


def find(listing: Optional[str]):
    if listing is not None:
        assert not os.path.exists(listing)
    page = req.get(PAGE).text
    bs = BeautifulSoup(page, features='html.parser')
    links = bs.find_all('a', attrs={'class': 'package-name'})
    packages: List[Dict[str, str]] = []
    for link in links:
        package = parse_package(link.text)
        packages.append(package)
    if listing is None:
        print(packages)
    else:
        with open(listing, 'w') as f:
            f.write(json.dumps(packages, sort_keys=True, indent=2))
    print(f'Found {len(packages)} packages')


def run_unpack(scratch: str, name: str, version: str):
    dest = os.path.join(scratch, f'{name}-{version}')
    if os.path.exists(dest):
        print(f'Skipping {name}')
    else:
        print(f'Unpacking {name}')
        command = f'stack --resolver {RESOLVER} unpack {name} --to {scratch}'
        print(command)
        ret = os.system(command)
        assert ret == 0


def download(listing: str, scratch: str):
    assert os.path.isfile(listing)
    assert not os.path.isfile(scratch)
    packages: List[Dict[str, str]]
    with open(listing, 'r') as f:
        packages = json.load(f)
    if not os.path.exists(scratch):
        os.mkdir(scratch)
    for package in packages:
        name = package['name']
        version = package['version']
        if name not in BLACKLIST:
            run_unpack(scratch, name, version)
            
def repack(listing: str, output: Optional[str]):
    assert os.path.isfile(listing)
    assert os.path.isdir(os.path.dirname(output))
    os.makedirs(output, exist_ok=True)
    packages: List[Dict[str, str]]
    with open(listing, 'r') as f:
        packages = json.load(f)
    for package in packages:
        name = package['name']
        version = package['version']
        dirname = f'{name}-{version}'
        srcname = f'../../haskell-packages/{dirname}'
        assert os.path.isdir(srcname)
        destname = f'{output}/{dirname}'
        os.makedirs(destname, exist_ok=True)
        for fn in package['files']:
            srcfile = f'{srcname}/{fn}'
            assert os.path.isfile(srcfile)
            destfile = f'{destname}/{fn}'
            os.makedirs(os.path.dirname(destfile), exist_ok=True)
            shutil.copy(srcfile, destfile)


def extract(listing: str, scratch: str, output: Optional[str]):
    assert os.path.isfile(listing)
    assert os.path.isdir(scratch)
    if output is not None:
        assert not os.path.exists(output)
    packages: List[Dict[str, str]]
    with open(listing, 'r') as f:
        packages = json.load(f)
    infos: List[Dict[str, Any]] = []
    for package in packages:
        name = package['name']
        version = package['version']
        if name not in BLACKLIST:
            project = os.path.join(scratch, f'{name}-{version}')
            files = []
            for filepath in pathlib.Path(project).rglob('*.hs'):
                try:
                    with open(filepath, 'r') as f:
                        contents = f.read()
                    match = RULES_RE.search(contents)
                    relpath = filepath.relative_to(project)
                    if match is not None:
                        files.append(str(relpath))
                except UnicodeDecodeError:
                    # Not every file is utf-8...
                    pass
            if len(files) > 0:
                print(f'Found package {name} with rewrite rules')
                infos.append({'name': name, 'version': version, 'files': files})
    if output is None:
        print(infos)
    else:
        with open(output, 'w') as f:
            f.write(json.dumps(infos, sort_keys=True, indent=2))
    print(f'Found {len(infos)} packages with rewrite rules')


def build_parser() -> ArgumentParser:
    parser = ArgumentParser('auto')

    op_subparser = parser.add_subparsers(dest='op', required=True)

    fake_parser = op_subparser.add_parser('fake', help='write a fake list of packages')
    fake_parser.add_argument('--listing', required=True, help='file to write package info to')

    find_parser = op_subparser.add_parser('find', help='list all packages in the given resolver')
    find_parser.add_argument('--listing', help='file to write package info to')

    download_parser = op_subparser.add_parser('download', help='download all packages to a local dir')
    download_parser.add_argument('--listing', required=True, help='file to read package info from')
    download_parser.add_argument('--scratch', required=True, help='directory to write packages to')

    extract_parser = op_subparser.add_parser('extract', help='extract info about which projects have rewrite rules')
    extract_parser.add_argument('--listing', required=True, help='file to read package info from')
    extract_parser.add_argument('--scratch', required=True, help='directory to read packages from')
    extract_parser.add_argument('--output', help='file to write info to')

    repack_parser = op_subparser.add_parser('repack', help='repack files that have rewrite rules') 
    repack_parser.add_argument('--listing', required=True, help='file to read package info from')
    repack_parser.add_argument('--output', help='directory to write pack into')

    return parser


def main():
    parser = build_parser()
    args = parser.parse_args()
    match args.op:
        case 'fake': fake(args.listing)
        case 'find': find(args.listing)
        case 'download': download(args.listing, args.scratch)
        case 'extract': extract(args.listing, args.scratch, args.output)
        case 'repack': repack(args.listing, args.output)
        case _: raise Exception(f'Unhandled: {args.op}')


if __name__ == '__main__':
    main()
