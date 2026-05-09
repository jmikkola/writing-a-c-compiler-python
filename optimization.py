import tacky


def optimize(ir: tacky.Program, args):
    optimized = []
    for decl in ir.top_level:
        match decl:
            case tacky.Function():
                optimized.append(optimize_function(decl, args))
            case _:
                optimized.append(decl)
    return tacky.Program(optimized)


def optimize_function(f: tacky.Function, args):
    body = f.body
    if not body:
        return f

    while True:
        if args.fold_constants:
            post_constant_folding = constant_folding(body)
        else:
            post_constant_folding = body

        cfg = make_control_flow_graph(post_constant_folding)

        if args.eliminate_unreachable_code:
            cfg = unreachable_code_elimination(cfg)

        if args.propagate_copies:
            cfg = copy_propagation(cfg)

        if args.eliminate_dead_stores:
            cfg = dead_store_elimination(cfg)

        optimized_function_body = cfg_to_instructions(cfg)

        if optimized_function_body == body:
            break
        if not optimized_function_body:
            break

        body = optimized_function_body

    return tacky.Function(f.name, f.is_global, f.params, body)


def constant_folding(body):
    # TODO
    return body


def make_control_flow_graph(body):
    # TODO
    return body


def unreachable_code_elimination(cfg):
    # TODO
    return cfg


def copy_propagation(cfg):
    # TODO
    return cfg


def dead_store_elimination(cfg):
    # TODO
    return cfg


def cfg_to_instructions(cfg):
    # TODO
    return cfg
