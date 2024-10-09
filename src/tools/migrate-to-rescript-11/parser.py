from __future__ import annotations

import json
from dataclasses import dataclass
from typing import Tuple, Any


@dataclass
class Node:
    begin: Tuple[int, int] = (-1, -1)
    end: Tuple[int, int] = (-1, -1)
    text: str | None = None
    left_paren: str = ''
    right_paren: str = ''
    parent: Node | None = None
    children: list[Node] | None = None
    left_sibling: Node | None = None
    right_sibling: Node | None = None

    def to_dict(self) -> dict[str, Any]:
        result = vars(self).copy()
        if self.children is not None:
            result['children'] = [c.to_dict() for c in self.children]
        result['begin'] = f'{self.begin[0]}:{self.begin[1]}'
        result['end'] = f'{self.end[0]}:{self.end[1]}'
        return result


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


def nodes_to_json_str(nodes: list[Node]) -> str:
    return json.dumps([n.to_dict() for n in nodes])


def parse(text: str) -> Node:
    parens = make_parens()
    is_in_str = False
    is_in_multiline_str = False
    is_in_comment = False
    open_comment_text = ''
    char = text[0]
    prev_char = ''
    line_num = 1
    pos = 1
    max_i = len(text) - 1
    i = 0
    begin_i = 0
    begin_line_num = 1
    begin_pos = 1
    stack: list[Node] = [Node(begin=(line_num, pos))]

    def save_curr_text() -> None:
        cur_node = stack[-1]
        if cur_node.children is None:
            cur_node.children = []
        cur_node.children.append(Node(
            parent=cur_node,
            begin=(begin_line_num, begin_pos),
            end=(line_num, pos),
            text=text[begin_i:i]
        ))
        if len(cur_node.children) > 1:
            cur_node.children[-2].right_sibling = cur_node.children[-1]
            cur_node.children[-1].left_sibling = cur_node.children[-2]

    def process_paren() -> None:
        nonlocal begin_i
        nonlocal begin_line_num
        nonlocal begin_pos
        paren = parens[char]
        cur_node = stack[-1]
        save_curr_text()
        begin_i = i + 1
        begin_line_num = line_num
        begin_pos = pos + 1
        if paren.is_open:
            child_node = Node(parent=cur_node, left_paren=char, begin=(line_num, pos))
            if cur_node.children is None:
                cur_node.children = []
            cur_node.children.append(child_node)
            stack.append(child_node)
            if len(cur_node.children) > 1:
                cur_node.children[-2].right_sibling = cur_node.children[-1]
                cur_node.children[-1].left_sibling = cur_node.children[-2]
        else:
            if cur_node.left_paren != paren.opposite:
                raise Exception(f'cur_node.left_paren != paren.opposite, {line_num=}, {pos=}')
            else:
                cur_node.right_paren = char
                cur_node.end = (line_num, pos)
                stack.pop()

    while i <= max_i:
        char = text[i]
        if char == '\n':
            line_num += 1
            pos = 0
        else:
            pos += 1
        # if line_num == 17 and pos == 68:
        #     pass
        if is_in_comment:
            if open_comment_text == '/*' and prev_char == '*' and char == '/' or open_comment_text == '//' and (
                    char == '\r' or char == '\n'):
                is_in_comment = False
        elif is_in_str:
            if prev_char != '\\' and char == '"':
                is_in_str = False
        elif is_in_multiline_str:
            if char == '`':
                is_in_multiline_str = False
        elif prev_char == '/' and char == '/' or prev_char == '/' and char == '*':
            is_in_comment = True
            open_comment_text = prev_char + char
        elif char == '"':
            is_in_str = True
        elif char == '`':
            is_in_multiline_str = True
        elif char in parens:
            process_paren()

        prev_char = char
        i += 1

    save_curr_text()

    if len(stack) != 1:
        raise Exception(f'len(stack) != 1\n\n Stack:\n{nodes_to_json_str(stack)}')
    return stack[-1]
