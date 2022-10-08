from argparse import ArgumentParser
from typing import Dict, List, Optional
from bs4 import BeautifulSoup
import json
import os
import requests as req


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


def parse_package(full_name: str) -> Dict[str, str]:
    name, version = full_name.rsplit('-', maxsplit=1)
    return {'name': name, 'version': version}


def fake(listing: str):
    assert not os.path.exists(listing)
    packages: List[Dict[str, str]] = [{'name': 'vector', 'version': '0.12.3.1'}]
    with open(listing, 'w') as f:
        f.write(json.dumps(packages))


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
            f.write(json.dumps(packages))
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

    return parser


def main():
    parser = build_parser()
    args = parser.parse_args()
    match args.op:
        case 'fake': fake(args.listing)
        case 'find': find(args.listing)
        case 'download': download(args.listing, args.scratch)
        case _: raise Exception(f'Unhandled: {args.op}')


if __name__ == '__main__':
    main()
