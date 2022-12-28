from __future__ import annotations

import abc
import dataclasses
import re
from pathlib import Path
from typing import NamedTuple

import jinja2
import yaml

DATA_FILE = Path("./persistent-api.yaml")

def main():
    api = PersistentAPI.load()
    api.render("Shim.jinja", "../../src/Database/Persist/Monad/Shim.hs")
    api.render("SqlQueryRep.jinja", "../../src/Database/Persist/Monad/SqlQueryRep.hs")
    api.render("SqlShim.jinja", "../../src/Database/Persist/Sql/Shim.hs")
    api.render("TestHelpers.jinja", "../../test/Generated.hs")

class PersistentAPI(NamedTuple):
    functions: list[PersistentFn]

    @classmethod
    def load(cls) -> PersistentAPI:
        data = yaml.safe_load(DATA_FILE.read_text())
        return cls.parse(data)

    @classmethod
    def parse(cls, data: list) -> PersistentAPI:
        return cls(functions=[PersistentFn.parse(d) for d in data])

    def render(self, template_file: str, output_file: str):
        env = jinja2.Environment(
            loader=jinja2.FileSystemLoader("./templates/"),
            keep_trailing_newline=True,
        )
        env.filters["capital_first"] = lambda s: s[0].upper() + s[1:]

        variables = self._asdict()

        template = env.get_template(template_file)
        output = template.render(**variables)
        Path(output_file).write_text(output)

class PersistentFn(NamedTuple):
    name: str
    condition: str | None
    constraints: list[str]
    args: list[str]
    result: str
    conduit_from: str | None

    @classmethod
    def parse(cls, data: dict) -> PersistentFn:
        return cls(
            name=data["name"],
            condition=data.get("condition"),
            constraints=data.get("constraints", []),
            args=data.get("args", []),
            result=data["result"],
            conduit_from=data.get("conduitFrom"),
        )

    @property
    def record_type_vars(self) -> list[str]:
        record_type_vars = set()
        for constraint in self.constraints:
            for word in re.findall(r"\w+", constraint):
                if word.startswith("record"):
                    record_type_vars.add(word)
        return sorted(list(record_type_vars))

    @property
    def shim_constraints(self) -> list[str]:
        typeable_constraints = [
            f"Typeable {type_var}"
            for type_var in self.record_type_vars
        ]

        return self.constraints + typeable_constraints + ["MonadSqlQuery m"]

if __name__ == "__main__":
    main()
