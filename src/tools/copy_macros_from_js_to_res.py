from pathlib import Path


def read_text_from_path(path: str) -> str:
    return Path(path).read_text('utf-8')


def write_text_to_path(path: str, text: str) -> None:
    Path(path).write_text(text, 'utf-8')

def replace_special_characters(text:str) -> str:
    return text.replace('`','[!@#]backtick[!@#]').replace('$','[!@#]dollar[!@#]').replace('\\','[!@#]backslash[!@#]')

def copy_from_js_to_res(js_path: str, res_path: str, var_name:str) -> None:
    js_text = read_text_from_path(js_path)
    res_text = f"let {var_name} = `{replace_special_characters(js_text)}`"
    write_text_to_path(res_path, res_text)

def main() -> None:
    base_dir = r'../metamath/api/macros'
    copy_from_js_to_res(
        js_path=f'{base_dir}/set_mm_example.js',
        res_path=f'{base_dir}/MM_macros_set_mm_example.res',
        var_name='setMmExampleMacros'
    )
    copy_from_js_to_res(
        js_path=f'{base_dir}/mmj2.js',
        res_path=f'{base_dir}/MM_macros_mmj2.res',
        var_name='mmj2Macros'
    )

if __name__ == '__main__':
    main()