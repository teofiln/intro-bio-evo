"""
Generate shuffled versions of quiz-03-deriva-migracion-especiacion-macroevolucion.qmd.
Only question order is randomized; options within each question are unchanged.
Output: quizzes/q03-versions/q03{A..F}.qmd and quizzes/q03-versions/q03{A..F}-KEY.qmd
"""

import pathlib
import random
import re
import textwrap

SEED_BASE = 2030
VERSIONS = list("ABCDEF")
OUT_DIR = pathlib.Path(__file__).parent / "q03-versions"
OUT_DIR.mkdir(exist_ok=True)

SRC = pathlib.Path(__file__).parent / "quiz-03-deriva-migracion-especiacion-macroevolucion.qmd"
raw = SRC.read_text(encoding="utf-8")

first_q_match = re.search(r"\n\*\*1\.\*\*", raw)
if not first_q_match:
    raise ValueError("Could not find the first question in the source quiz.")

question_blocks = re.findall(
    r"(?ms)^\*\*\d+\.\*\*.*?(?=^\*\*\d+\.\*\*|\Z)",
    raw[first_q_match.start() + 1 :],
)

if len(question_blocks) != 10:
    raise ValueError(f"Expected 10 questions, found {len(question_blocks)}")

ANSWERS = {
    1: ("a", "Deriva genética: cambio aleatorio en frecuencias alélicas por muestreo"),
    2: ("b", "Deriva más intensa en poblaciones pequeñas (N bajo)"),
    3: ("c", "Efecto fundador: muestra pequeña inicial con frecuencias distintas"),
    4: ("a", "Aislamiento postcigótico: inviabilidad, esterilidad y menor aptitud de híbridos"),
    5: ("b", "Flujo génico tiende a homogeneizar poblaciones"),
    6: ("b", "Especiación inicia con reducción de flujo génico"),
    7: ("b", "Alopatría: barrera geográfica y divergencia independiente"),
    8: ("a", "Refuerzo: contacto secundario + híbridos de baja aptitud + barreras prezigóticas"),
    9: ("a", "Radiación adaptativa: diversificación ecológica y morfológica"),
    10: ("c", "Tasas son probabilísticas; una realización no representa el promedio"),
}


def renumber_question(block: str, new_num: int) -> str:
    return re.sub(r"^\*\*\d+\.\*\*", f"**{new_num}.**", block)


def build_quiz(version_letter: str, shuffled_indices: list[int]) -> str:
    title = "Quiz 3: Deriva, Migración, Especiación y Macroevolución"
    subtitle = "Intro Bio I (B0160) - Módulo de Evolución"

    header = textwrap.dedent(
        f"""\
        ---
        title: "{title}"
        subtitle: "{subtitle}"
        format:
          pdf:
            toc: false
            number-sections: false
        lang: es
        ---
    """
    )

    lines = [
        header.rstrip(),
        "",
        "**Nombre:** _________________________________ &emsp; **Carne:** __________________",
        "",
        "*Instrucciones: Elige la única respuesta correcta para cada pregunta.*",
    ]

    for new_num, orig_idx in enumerate(shuffled_indices, start=1):
        lines.append("")
        lines.append(renumber_question(question_blocks[orig_idx].rstrip(), new_num))

    return "\n".join(lines) + "\n"


def build_key(version_letter: str, shuffled_indices: list[int]) -> str:
    title = f"Quiz 3{version_letter} - CLAVE DE RESPUESTAS"
    subtitle = "Deriva, Migración, Especiación y Macroevolución · Intro Bio I (B0160)"

    header = textwrap.dedent(
        f"""\
        ---
        title: "{title}"
        subtitle: "{subtitle}"
        format:
          pdf:
            toc: false
            number-sections: false
        lang: es
        ---
    """
    )

    rows = ["| Pregunta | Respuesta correcta | Concepto clave |", "|:---:|:---:|---|"]
    for new_num, orig_idx in enumerate(shuffled_indices, start=1):
        orig_q_num = orig_idx + 1
        ans, concept = ANSWERS[orig_q_num]
        rows.append(f"| {new_num} | **{ans}** | {concept} |")

    table = "\n".join(rows)

    notes = textwrap.dedent(
        """\

        ## Notas para el docente

        - Cobertura por clase:
          - Preguntas 1-3: Clase 7 (deriva genética, tamaño poblacional, efecto fundador).
          - Pregunta 5: Clase 8 (migración y homogeneización).
          - Preguntas 4, 6-8: Clase 9 (aislamiento reproductivo, flujo génico, alopatría, refuerzo).
          - Preguntas 9-10: Clase 10 (radiación adaptativa y tasas de macroevolución).
        - Distractores principales:
          - Confundir flujo génico con aparición de alelos nuevos por mutación.
          - Interpretar tasas evolutivas como relojes deterministas.
          - Mezclar barreras prezigóticas con procesos postcigóticos.
    """
    )

    return header + "\n" + table + notes


for i, letter in enumerate(VERSIONS):
    rng = random.Random(SEED_BASE + i)
    indices = list(range(10))
    rng.shuffle(indices)

    quiz_text = build_quiz(letter, indices)
    key_text = build_key(letter, indices)

    quiz_path = OUT_DIR / f"q03{letter}.qmd"
    key_path = OUT_DIR / f"q03{letter}-KEY.qmd"

    quiz_path.write_text(quiz_text, encoding="utf-8")
    key_path.write_text(key_text, encoding="utf-8")

    order = [str(x + 1) for x in indices]
    print(f"Version {letter}: Q order = {', '.join(order)} -> {quiz_path.name} + {key_path.name}")

print(f"\nDone. {len(VERSIONS) * 2} files written to {OUT_DIR}")
