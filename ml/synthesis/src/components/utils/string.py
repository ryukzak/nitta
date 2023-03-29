def snake_to_lower_camel_case(snake_str: str):
    components = [c for c in snake_str.strip().split('_') if c]
    if not components:
        return ""
    return components[0] + "".join(x[0].title() + x[1:] for x in components[1:])
