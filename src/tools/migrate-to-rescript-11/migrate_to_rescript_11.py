from __future__ import annotations

from dataclasses import dataclass, field
from pathlib import Path


@dataclass
class Node:
    left_paren: str = ''
    right_paren: str = ''
    children: list[str | Node] = field(default_factory=lambda: [])


@dataclass
class Paren:
    is_open: bool
    char: str
    opposite: str


def make_parens() -> dict[str, Paren]:
    parens_arr = ['(', ')', '{', '}', '[', ']']
    parens: dict[str, Paren] = {}
    for i in range(int(len(parens_arr) / 2)):
        parens[parens_arr[i * 2]] = Paren(is_open=True, char=parens_arr[i * 2], opposite=parens_arr[i * 2 + 1])
        parens[parens_arr[i * 2 + 1]] = Paren(is_open=False, char=parens_arr[i * 2 + 1], opposite=parens_arr[i * 2])
    return parens


def parse(text: str) -> Node:
    parens = make_parens()
    is_in_str = False
    is_in_multiline_str = False
    is_in_comment = False
    open_comment_text = ''
    begin = 0
    stack: list[Node] = [Node()]
    prev_char = ''
    max_i = len(text) - 1
    i = 0
    char = text[0]
    line_num = 1
    pos = 1

    def process_paren() -> None:
        nonlocal begin
        paren = parens[char]
        cur_node = stack[-1]
        cur_node.children.append(text[begin:i])
        if paren.is_open:
            child_node = Node(left_paren=char)
            cur_node.children.append(child_node)
            stack.append(child_node)
        else:
            if cur_node.left_paren != paren.opposite:
                raise Exception(f'cur_node.left_paren != paren.opposite, {line_num=}, {pos=}')
            else:
                cur_node.right_paren = char
                stack.pop()
        begin = i + 1

    while i <= max_i:
        char = text[i]
        if char == '\n':
            line_num += 1
            pos = 0
        else:
            pos += 1
        if not is_in_comment:
            if prev_char == '/' and char == '/' or prev_char == '/' and char == '*':
                is_in_comment = True
                open_comment_text = prev_char + char
            elif not is_in_str:
                if char == '"':
                    is_in_str = True
                elif not is_in_multiline_str:
                    if char == '`':
                        is_in_multiline_str = True
                    elif char in parens:
                        process_paren()
                    else:
                        pass
                elif char == '`':
                    is_in_multiline_str = False
                else:
                    pass
            elif prev_char != '\\' and char == '"':
                is_in_str = False
            else:
                pass
        elif open_comment_text == '/*' and prev_char == '*' and char == '/' or open_comment_text == '//' and (
                char == '\r' or char == '\n'):
            is_in_comment = False
        else:
            pass

        prev_char = char
        i += 1

    stack[-1].children.append(text[begin:])

    if len(stack) != 1:
        raise Exception(f'len(stack) != 1')
    return stack[-1]


def node_to_str(node: Node) -> str:
    res = [node.left_paren]
    for c in node.children:
        if isinstance(c, str):
            res.append(c)
        else:
            res.append(node_to_str(c))
    res.append(node.right_paren)
    return ''.join(res)


def read_text_from_path(path: Path) -> str:
    return path.read_text('utf-8')


def write_text_to_path(path: Path, text: str) -> None:
    path.write_text(text, 'utf-8')


if __name__ == '__main__':
    path = Path('../../metamath/mm-utils/MM_parser.res')
    parsed = parse(read_text_from_path(path))
    write_text_to_path(path, node_to_str(parsed))
