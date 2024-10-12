import glob
from pathlib import Path
from typing import Callable

from parser import Node


def node_to_str(node: Node) -> str:
    res = [node.left_paren]
    if node.text is not None:
        res.append(node.text)
    if node.children is not None:
        for c in node.children:
            res.append(node_to_str(c))
    res.append(node.right_paren)
    return ''.join(res)


def read_text_from_path(path: Path) -> str:
    return path.read_text('utf-8')


def write_text_to_path(path: Path, text: str) -> None:
    path.write_text(text, 'utf-8')


def get_all_rescript_files() -> list[Path]:
    roots = [f'../../**/*.{ext}' for ext in ['res', 'resi']]
    return [Path(file_name) for root in roots for file_name in glob.iglob(root, recursive=True)]


def iterate_nodes_rec(root: Node, consumer: Callable[[Node], None]) -> None:
    consumer(root)
    if root.children is not None:
        for c in root.children:
            iterate_nodes_rec(c, consumer)
