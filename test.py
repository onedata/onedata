#! /usr/bin/env python

import itertools

def generate_configs(params, description_skeleton):
    keys = params.keys()
    configs = {}
    i = 0
    combinations = itertools.product(*params.values())
    print combinations

    for combination in combinations:
        conf_name = 'config{}'.format(i)
        configs[conf_name] = dict()
        new_params = dict(zip(keys, combination))
        print new_params
        description = description_skeleton.format(**new_params)
        for key, value in new_params.items():
            new_params[key] = {'value': value}
        configs[conf_name].update({
            'parameters': new_params,
            'description': description
        })
        i += 1

    return configs



ret = generate_configs({
    'files_number': [10 , 100, 1000],
    'threads_number': [1 , 16],
    'total_size': [10 , 100, 1000],  # in M
    'mode': ["rndrw", "rndrd", "rndwr", "seqwr", "seqrd"]
},
        'SYSBENCH TEST: Files number {files_number} Threads number {threads_number} Total Size {total_size} Mode: {mode}')



print ret
print len(ret.keys())