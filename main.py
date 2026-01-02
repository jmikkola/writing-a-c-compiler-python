import sys

import options
import driver


def main(args):
    arguments = options.Args.parse(args)
    driver.run_compiler(arguments)


if __name__ == "__main__":
    main(sys.argv[1:])
