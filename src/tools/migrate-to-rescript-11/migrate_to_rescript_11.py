from __future__ import annotations

from pathlib import Path
from typing import Callable

from parser import parse, Node
from utils import read_text_from_path, node_to_str, write_text_to_path, get_all_rescript_files, iterate_nodes_rec


def process_all_nodes(root: Node, is_node_to_update: Callable[[Node], bool],
                      update_node: Callable[[Node], None]) -> None:
    def process(n: Node) -> None:
        if is_node_to_update(n):
            update_node(n)

    iterate_nodes_rec(root, process)


def insert_get_unsafe(node: Node) -> None:
    def is_assignment(n: Node) -> bool:
        if n.right_sibling is None or n.right_sibling.text is None or '=' not in n.right_sibling.text:
            return False
        if len(n.right_sibling.text) > 1 and n.right_sibling.text[0] == '.':
            return False
        text = n.right_sibling.text
        this_line_text = text[:text.index('\n')] if '\n' in text else text
        return (
                '=' in this_line_text
                and ('!=' not in this_line_text or this_line_text.index('=') < this_line_text.index('!='))
                and ('==' not in this_line_text or this_line_text.index('=') < this_line_text.index('=='))
                and (',' not in this_line_text or this_line_text.index('=') < this_line_text.index(','))
                and ('<=' not in this_line_text or this_line_text.index('=') < this_line_text.index('<='))
                and ('>=' not in this_line_text or this_line_text.index('=') < this_line_text.index('>='))
        )

    def is_node_to_update(n: Node) -> bool:
        return (
                n.left_paren == '[' and n.right_paren == ']'
                and n.begin[0] == n.end[0]
                and '"' not in node_to_str(n) and '#' not in node_to_str(n)
                and not is_assignment(n)
                and node_to_str(n) != ''
                and (
                        n.left_sibling is not None and n.left_sibling.text is not None
                        and len(n.left_sibling.text) > 0
                        and n.left_sibling.text[-1].isalnum()
                )
        )

    def update_node(n: Node) -> None:
        n.left_paren = '->Array.getUnsafe('
        n.right_paren = ')'

    process_all_nodes(node, is_node_to_update, update_node)


def add_parens_to_get_unsafe(node: Node) -> None:
    def is_node_to_update(n: Node) -> bool:
        return (
                n.left_sibling is not None
                and n.left_sibling.text is not None
                and n.left_sibling.text.endswith('->Array.getUnsafe')
                and n.right_sibling is not None
                and n.right_sibling.text is not None
                and n.right_sibling.text.startswith('.')
        )

    def update_node(n: Node) -> None:
        n.right_paren = '))'

    process_all_nodes(node, is_node_to_update, update_node)


def rewrite_array_push(node: Node) -> None:
    def is_node_to_update(n: Node) -> bool:
        return (
                n.left_sibling is not None
                and n.left_sibling.text is not None
                and n.left_sibling.text.endswith('rray2.push')
        )

    def update_node(n: Node) -> None:
        if n.left_sibling is not None and n.left_sibling.text is not None:
            text = n.left_sibling.text
            if text.endswith('->Js.Array2.push') or text.endswith('->Js_array2.push'):
                n.left_sibling.text = text[:-14] + 'Array.push'

        if n.right_sibling is not None and n.right_sibling.text is not None:
            text = n.right_sibling.text
            if text.startswith('->ignore'):
                n.right_sibling.text = text[8:]

    process_all_nodes(node, is_node_to_update, update_node)


def replace_functions(node: Node, replacements: dict[str, str]) -> None:
    def is_node_to_update(n: Node) -> bool:
        if n.left_sibling is None or n.left_sibling.text is None:
            return False
        text = n.left_sibling.text
        for old_func in replacements:
            if text.endswith(old_func):
                return True
        return False

    def update_node(n: Node) -> None:
        if n.left_sibling is not None and n.left_sibling.text is not None:
            text = n.left_sibling.text
            for old_func in replacements:
                if text.endswith(old_func):
                    n.left_sibling.text = text[:-len(old_func)] + replacements[old_func]
                    return

    process_all_nodes(node, is_node_to_update, update_node)


def rewrite_file(path: Path) -> None:
    parsed = parse(read_text_from_path(path))
    # insert_get_unsafe(parsed)
    # add_parens_to_get_unsafe(parsed)
    # rewrite_array_push(parsed)
    # replace_functions(
    #     parsed,
    #     {
    #         'Js.Array2.length': 'Array.length',
    #         'Js_array2.length': 'Array.length',
    #     }
    # )
    write_text_to_path(path, node_to_str(parsed))


def main() -> None:
    # rewrite_file(Path('../../metamath/mm-utils/MM_substitution.res'))

    for path in get_all_rescript_files():
        print(f'processing: {path.absolute()}')
        rewrite_file(path)


if __name__ == '__main__':
    main()
