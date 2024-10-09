from __future__ import annotations

from pathlib import Path

from parser import parse, Node
from utils import read_text_from_path, node_to_str, write_text_to_path, get_all_rescript_files, iterate_nodes_rec


def insert_get_unsafe(node: Node) -> None:
    def is_node_to_update(n: Node) -> bool:
        return (
                n.left_paren == '[' and n.right_paren == ']'
                and n.begin[0] == n.end[0]
                and '"' not in node_to_str(n) and '#' not in node_to_str(n)
                and (
                        n.right_sibling is None
                        or n.right_sibling.text is None
                        or '=' not in n.right_sibling.text
                )
                and node_to_str(n) != ''
                and (
                        n.left_sibling is not None and n.left_sibling.text is not None
                        and len(n.left_sibling.text) > 0
                        and n.left_sibling.text[-1] not in [' ', '(', '[', '{']
                )
        )

    def update_node(n: Node) -> None:
        n.left_paren = '->Array.getUnsafe('
        n.right_paren = ')'

    def process(n: Node) -> None:
        if is_node_to_update(n):
            update_node(n)

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
