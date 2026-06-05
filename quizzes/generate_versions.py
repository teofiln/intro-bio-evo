"""
Generate 10 shuffled versions of quiz-01-variacion-evidencia.qmd (versions A–J).
Only question ORDER is randomized; options within each question are unchanged.
Output: quizzes/versions/q01{A..J}.qmd  and  quizzes/versions/q01{A..J}-KEY.qmd
"""

import re, random, pathlib, textwrap

SEED_BASE = 42          # reproducible seeds: version A=42, B=43, …
VERSIONS  = list("ABCDEFGHIJ")
OUT_DIR   = pathlib.Path(__file__).parent / "versions"
OUT_DIR.mkdir(exist_ok=True)

# ── Correct answers for original questions 1-10 ──────────────────────────────
ANSWERS = {
    1:  ("b", "Definición de evolución: cambio hereditario en poblaciones a lo largo de generaciones"),
    2:  ("c", "Plasticidad fenotípica: mismo genotipo, distinto fenotipo según ambiente"),
    3:  ("d", "Aclimatación: (b) cambio reversible no hereditario y (c) respuesta fisiológica/morfológica"),
    4:  ("c", "Selección natural actúa sobre variación preexistente; el antibiótico filtra, no crea el rasgo"),
    5:  ("b", "Homología: misma estructura, diferente función → ancestro común"),
    6:  ("b", "Analogía: función similar, origen evolutivo diferente → evolución convergente"),
    7:  ("c", "Rasgos transicionales consistentes con descendencia con modificación"),
    8:  ("b", "Evolución experimental: cambios hereditarios distintos bajo selección divergente"),
    9:  ("e", "Aptitud: (b) dependencia del entorno y (c) éxito relativo de supervivencia y reproducción"),
    10: ("d", "Tabla aclimatación vs evolución: (I) sí — (II) generaciones — (III) acumulativos e irreversibles"),
}

# ── Read and parse the source quiz ───────────────────────────────────────────
SRC = pathlib.Path(__file__).parent / "quiz-01-variacion-evidencia.qmd"
raw = SRC.read_text(encoding="utf-8")

# Split on the horizontal rules that separate questions
# The file uses lines that are exactly "---"
parts = re.split(r'\n---\n', raw)

# parts[0] = YAML front matter (contains the first ---)
# We need to find where the header ends and questions begin.
# The header block ends after the instructions line.
# Questions are the parts that start with **N.**

header_parts = []
question_blocks = []

for p in parts:
    stripped = p.strip()
    if re.match(r'\*\*\d+\.\*\*', stripped):
        question_blocks.append(stripped)
    else:
        header_parts.append(p)

# The YAML front matter itself contains "---" so re.split produced an extra empty
# leading part. Reconstruct header as everything up to (not including) the first
# question block separator.
# Easier: just find the position of the first question in the raw text.
first_q_match = re.search(r'\n\*\*1\.\*\*', raw)
header_text = raw[:first_q_match.start()].rstrip()

assert len(question_blocks) == 10, f"Expected 10 questions, found {len(question_blocks)}"

# ── Helpers ──────────────────────────────────────────────────────────────────

def renumber_question(block: str, new_num: int) -> str:
    """Replace the leading **N.** with **new_num.**"""
    return re.sub(r'^\*\*\d+\.\*\*', f'**{new_num}.**', block)


def build_quiz(shuffled_indices: list[int]) -> str:
    lines = [header_text, ""]
    for new_num, orig_idx in enumerate(shuffled_indices, start=1):
        block = renumber_question(question_blocks[orig_idx], new_num)
        lines.append("")
        lines.append(block)
        lines.append("")
        lines.append("---")
    return "\n".join(lines)


def build_key(version_letter: str, shuffled_indices: list[int]) -> str:
    title = f"Quiz 1{version_letter} — CLAVE DE RESPUESTAS"
    subtitle = "Variación, Poblaciones y Evidencia de la Evolución · Intro Bio I (B0160)"

    header = textwrap.dedent(f"""\
        ---
        title: "{title}"
        subtitle: "{subtitle}"
        format:
          html:
            toc: false
            number-sections: false
        lang: es
        ---
    """)

    rows = ["| Pregunta | Resp. correcta | Concepto clave |",
            "|:---:|:---:|---|"]
    for new_num, orig_idx in enumerate(shuffled_indices, start=1):
        orig_q_num = orig_idx + 1          # 1-based original question number
        ans, concept = ANSWERS[orig_q_num]
        rows.append(f"| {new_num} | **{ans}** | {concept} |")

    table = "\n".join(rows)

    notes = textwrap.dedent(f"""\

        ---

        *Orden original de las preguntas en esta versión ({version_letter}):*

        | Nº en versión {version_letter} | Nº en versión original |
        |:---:|:---:|
    """)
    for new_num, orig_idx in enumerate(shuffled_indices, start=1):
        notes += f"| {new_num} | {orig_idx + 1} |\n"

    return header + "\n" + table + notes


# ── Generate versions ─────────────────────────────────────────────────────────
for i, letter in enumerate(VERSIONS):
    rng = random.Random(SEED_BASE + i)
    indices = list(range(10))          # 0-based indices into question_blocks
    rng.shuffle(indices)

    quiz_text = build_quiz(indices)
    key_text  = build_key(letter, indices)

    quiz_path = OUT_DIR / f"q01{letter}.qmd"
    key_path  = OUT_DIR / f"q01{letter}-KEY.qmd"

    quiz_path.write_text(quiz_text, encoding="utf-8")
    key_path.write_text(key_text,  encoding="utf-8")

    order = [str(x + 1) for x in indices]
    print(f"Version {letter}: Q order = {', '.join(order)}  →  {quiz_path.name}  +  {key_path.name}")

print(f"\nDone. {len(VERSIONS) * 2} files written to {OUT_DIR}")
