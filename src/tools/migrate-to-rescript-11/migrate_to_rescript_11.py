from __future__ import annotations

from pathlib import Path

from parser import parse, Node
from utils import read_text_from_path, node_to_str, write_text_to_path, get_all_rescript_files, iterate_nodes_rec


def insert_get_unsafe(node: Node) -> None:
    def is_node_to_update(node: Node) -> bool:
        return (
                node.left_paren == '[' and node.right_paren == ']'
                and node.begin[0] == node.end[0]
                and '"' not in node_to_str(node) and '#' not in node_to_str(node)
                and (
                        node.right_sibling is None
                        or node.right_sibling.text is None
                        or '=' not in node.right_sibling.text
                )
        )

    def update_node(node: Node) -> None:
        node.left_paren = '->Array.getUnsafe('
        node.right_paren = ')'

    def process(node: Node) -> None:
        if is_node_to_update(node):
            update_node(node)

    iterate_nodes_rec(node, process)


def rewrite_file(path: Path) -> None:
    parsed = parse(read_text_from_path(path))
    insert_get_unsafe(parsed)
    write_text_to_path(path, node_to_str(parsed))


def main() -> None:
    # rewrite_file(Path('../../metamath/ui/MM_cmp_export_state_to_url.res'))

    for path in get_all_rescript_files():
        print(f'processing: {path.absolute()}')
        rewrite_file(path)


if __name__ == '__main__':
    main()
