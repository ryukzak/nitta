"""
Do not add non-stdlib imports here and be compatible with Python 3.8.

See ml/synthesis/src/scripts/evaluate_nitta_synthesis.py for more info.
"""


def snake_to_lower_camel_case(snake_str: str):
    components = [c for c in snake_str.strip().split("_") if c]
    if not components:
        return ""
    return components[0] + "".join(x[0].title() + x[1:] for x in components[1:])


def camel_case_to_snake(camel_case_str: str):
    return "".join("_" + c.lower() if c.isupper() else c for c in camel_case_str).lstrip("_")
