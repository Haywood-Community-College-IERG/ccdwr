"""Translate a SQL Server (T-SQL) query to DuckDB SQL for the CCDW mirror.

Called by ccdwr::translateSQL_DuckDB().

Usage:
    python sql_to_duckdb.py INFILE OUTFILE [--read tsql] [--drop-catalogs CCDW_HIST,CCDW]

Database (catalog) prefixes listed in --drop-catalogs are removed from
three-part table names, because the mirror combines those databases into
its single catalog.
"""

import argparse
import sys

try:
    import sqlglot
    from sqlglot import exp
except ImportError:
    sys.stderr.write(
        "Python package 'sqlglot' is not installed. Install it with: pip install sqlglot\n"
    )
    sys.exit(2)


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("infile")
    parser.add_argument("outfile")
    parser.add_argument("--read", default="tsql")
    parser.add_argument("--drop-catalogs", default="CCDW_HIST,CCDW")
    args = parser.parse_args()

    drop = {c.strip().upper() for c in args.drop_catalogs.split(",") if c.strip()}

    with open(args.infile, encoding="utf-8-sig") as f:
        sql = f.read()

    try:
        statements = sqlglot.parse(
            sql, read=args.read, error_level=sqlglot.ErrorLevel.RAISE
        )
    except sqlglot.errors.ParseError as e:
        sys.stderr.write(f"{e}\n")
        return 1

    out = []
    for stmt in statements:
        if stmt is None:
            continue
        for table in stmt.find_all(exp.Table):
            if table.catalog and table.catalog.upper() in drop:
                table.set("catalog", None)
        out.append(stmt.sql(dialect="duckdb", pretty=True))

    with open(args.outfile, "w", encoding="utf-8") as f:
        f.write(";\n".join(out))
    return 0


if __name__ == "__main__":
    sys.exit(main())
