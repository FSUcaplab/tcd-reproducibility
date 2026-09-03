#!/usr/bin/env python3
"""Generate CITATION.ris and CITATION.bib from a CITATION.cff file.

Why this exists instead of `cffconvert`: cffconvert (2.0.0) ignores the
`preferred-citation` block, so for a repo that accompanies a manuscript it
emits a bare `TY  - GEN` software stub with no journal, volume, pages or DOI.
That is the opposite of what a reader clicking "Cite this repository" wants.

This script emits BOTH records when a preferred-citation is present:
the journal article first (TY - JOUR / @article), then the software itself
(TY - COMP / @software). Importing the .ris into EndNote or Zotero therefore
gives the paper AND the code, correctly typed.

Usage:
    python cff2ris.py                    # reads ./CITATION.cff
    python cff2ris.py path/to/CITATION.cff
    python cff2ris.py --stdout           # print instead of writing files
"""

import re
import sys
from pathlib import Path

try:
    import yaml
except ImportError:  # ruamel ships with cffconvert; use it as a fallback
    from ruamel.yaml import YAML

    class _Shim:
        @staticmethod
        def safe_load(text):
            return YAML(typ="safe").load(text)

    yaml = _Shim()


# --- helpers ----------------------------------------------------------------

def author_name(a):
    """CFF author entry -> 'Family, Given' (or the entity name)."""
    if "name" in a:  # entity author, e.g. a lab or consortium
        return a["name"]
    family = " ".join(filter(None, [a.get("name-particle"), a.get("family-names")]))
    given = " ".join(filter(None, [a.get("given-names"), a.get("name-suffix")]))
    return f"{family}, {given}".strip().rstrip(",")


def year_of(entry):
    if entry.get("year"):
        return str(entry["year"])
    released = entry.get("date-released")
    return str(released)[:4] if released else ""


def cite_key(entry):
    """hoch2026sweat -- first author surname + year + first real title word."""
    authors = entry.get("authors") or []
    surname = "anon"
    if authors:
        a = authors[0]
        surname = a.get("family-names") or a.get("name") or "anon"
    surname = re.sub(r"[^a-z]", "", surname.lower()) or "anon"
    stop = {"a", "an", "the", "on", "of", "in", "for", "can", "we", "and", "to"}
    words = re.findall(r"[a-z]+", (entry.get("title") or "").lower())
    word = next((w for w in words if w not in stop and len(w) > 3), "work")
    return f"{surname}{year_of(entry)}{word}"


def flatten(text):
    """Collapse the folded-scalar line breaks CFF uses into one line."""
    return " ".join(str(text).split()) if text else ""


def doi_of(entry):
    if entry.get("doi"):
        return entry["doi"]
    for ident in entry.get("identifiers") or []:
        if ident.get("type") == "doi":
            return ident.get("value")
    return None


# --- RIS --------------------------------------------------------------------

def ris_article(entry):
    out = ["TY  - JOUR", f"TI  - {flatten(entry.get('title'))}"]
    out += [f"AU  - {author_name(a)}" for a in entry.get("authors") or []]
    if entry.get("journal"):
        out.append(f"T2  - {entry['journal']}")
    if year_of(entry):
        out.append(f"PY  - {year_of(entry)}")
    for key, tag in (("volume", "VL"), ("issue", "IS"), ("start", "SP"), ("end", "EP")):
        if entry.get(key):
            out.append(f"{tag}  - {entry[key]}")
    if doi_of(entry):
        out.append(f"DO  - {doi_of(entry)}")
    for ident in entry.get("identifiers") or []:
        if ident.get("type") == "other" and str(ident.get("value", "")).startswith("PMID:"):
            out.append(f"AN  - {ident['value'].split(':', 1)[1].strip()}")
    if entry.get("url"):
        out.append(f"UR  - {entry['url']}")
    if entry.get("abstract"):
        out.append(f"AB  - {flatten(entry['abstract'])}")
    if entry.get("notes"):
        out.append(f"N1  - {flatten(entry['notes'])}")
    out += ["LA  - eng", "ER  - "]
    return "\n".join(out)


def ris_software(cff):
    out = ["TY  - COMP", f"TI  - {flatten(cff.get('title'))}"]
    out += [f"AU  - {author_name(a)}" for a in cff.get("authors") or []]
    if year_of(cff):
        out.append(f"PY  - {year_of(cff)}")
    if cff.get("date-released"):
        out.append(f"DA  - {str(cff['date-released']).replace('-', '/')}/")
    if cff.get("version"):
        out.append(f"ET  - {cff['version']}")  # Zotero maps ET -> version for COMP
    if doi_of(cff):
        out.append(f"DO  - {doi_of(cff)}")
    url = cff.get("repository-code") or cff.get("url")
    if url:
        out.append(f"UR  - {url}")
    if cff.get("abstract"):
        out.append(f"AB  - {flatten(cff['abstract'])}")
    for kw in cff.get("keywords") or []:
        out.append(f"KW  - {kw}")
    if cff.get("license"):
        out.append(f"N1  - License: {cff['license']}")
    out += ["LA  - eng", "ER  - "]
    return "\n".join(out)


# --- BibTeX -----------------------------------------------------------------

def bib_escape(text):
    return flatten(text).replace("&", r"\&").replace("%", r"\%").replace("_", r"\_")


def bib_article(entry):
    fields = [
        ("author", " and ".join(author_name(a) for a in entry.get("authors") or [])),
        ("title", bib_escape(entry.get("title"))),
        ("journal", bib_escape(entry.get("journal"))),
        ("year", year_of(entry)),
        ("volume", entry.get("volume")),
        ("number", entry.get("issue")),
    ]
    if entry.get("start"):
        pages = f"{entry['start']}--{entry['end']}" if entry.get("end") else str(entry["start"])
        fields.append(("pages", pages))
    fields += [("doi", doi_of(entry)), ("url", entry.get("url"))]
    body = ",\n".join(f"  {k} = {{{v}}}" for k, v in fields if v)
    return f"@article{{{cite_key(entry)},\n{body}\n}}"


def bib_software(cff):
    fields = [
        ("author", " and ".join(author_name(a) for a in cff.get("authors") or [])),
        ("title", bib_escape(cff.get("title"))),
        ("year", year_of(cff)),
        ("version", cff.get("version")),
        ("doi", doi_of(cff)),
        ("url", cff.get("repository-code") or cff.get("url")),
        ("license", cff.get("license")),
    ]
    body = ",\n".join(f"  {k} = {{{v}}}" for k, v in fields if v)
    return f"@software{{{cite_key(cff)}software,\n{body}\n}}"


# --- main -------------------------------------------------------------------

def build(cff):
    preferred = cff.get("preferred-citation")
    ris, bib = [], []
    if preferred:
        ris.append(ris_article(preferred))
        bib.append(bib_article(preferred))
    ris.append(ris_software(cff))
    bib.append(bib_software(cff))
    return "\n\n".join(ris) + "\n", "\n\n".join(bib) + "\n"


def main(argv):
    to_stdout = "--stdout" in argv
    args = [a for a in argv if not a.startswith("--")]
    path = Path(args[0]) if args else Path("CITATION.cff")
    if not path.exists():
        sys.exit(f"error: {path} not found")

    cff = yaml.safe_load(path.read_text(encoding="utf-8"))
    ris, bib = build(cff)

    if to_stdout:
        print(ris)
        print(bib)
        return

    for name, text in (("CITATION.ris", ris), ("CITATION.bib", bib)):
        out = path.parent / name
        out.write_text(text, encoding="utf-8")
        print(f"wrote {out}")


if __name__ == "__main__":
    main(sys.argv[1:])
