"""
Generate 6 shuffled versions of quiz-02-metodos-hw-mutacion-seleccion.qmd (versions A–F).
Only question order is randomized; options within each question are unchanged.
Output: quizzes/q02-versions/q02{A..F}.qmd and quizzes/q02-versions/q02{A..F}-KEY.qmd
"""

import pathlib
import random
import re
import textwrap

SEED_BASE = 2026
VERSIONS = list("ABCDEF")
OUT_DIR = pathlib.Path(__file__).parent / "q02-versions"
OUT_DIR.mkdir(exist_ok=True)

SRC = pathlib.Path(__file__).parent / "quiz-02-metodos-hw-mutacion-seleccion.qmd"
raw = SRC.read_text(encoding="utf-8")

first_q_match = re.search(r'\n\*\*1\.\*\*', raw)
if not first_q_match:
    raise ValueError("Could not find the first question in the source quiz.")

header_text = raw[:first_q_match.start()].rstrip()

question_blocks = re.findall(
    r'(?ms)^\*\*\d+\.\*\*.*?(?=^\*\*\d+\.\*\*|\Z)',
    raw[first_q_match.start() + 1 :],
)

if len(question_blocks) != 10:
    raise ValueError(f"Expected 10 questions, found {len(question_blocks)}")

ANSWERS = {
    1: ("b", "Método científico: hipótesis alternativas y predicciones comprobables contrastadas con datos"),
    2: ("c", "Evidencia integrada de adaptación: relación rasgo-ambiente, heredabilidad y aptitud"),
    3: ("b", "Inserciones/deleciones no múltiplos de 3 en región codificante pueden causar frameshift"),
    4: ("b", "Hardy-Weinberg como modelo nulo para poblaciones"),
    5: ("c", "Bajo HW, heterocigotos = 2pq = 2(0.7)(0.3) = 0.42"),
    6: ("b", "Desviación de HW sugiere violación de uno o más supuestos, no un mecanismo único automático"),
    7: ("b", "Mutación genera variación; selección filtra según diferencias de aptitud"),
    8: ("b", "Solo mutaciones germinales son heredables y contribuyen directamente a evolución poblacional"),
    9: ("c", "Ventaja del heterocigoto en malaria mantiene polimorfismo (selección balanceadora)"),
    10: ("c", "Selección diversificadora favorece extremos y reduce aptitud de intermedios"),
}


def renumber_question(block: str, new_num: int) -> str:
    return re.sub(r'^\*\*\d+\.\*\*', f'**{new_num}.**', block)


def build_quiz(version_letter: str, shuffled_indices: list[int]) -> str:
    title = f"Quiz 2{version_letter}: Métodos, Hardy-Weinberg, Mutación y Selección Natural"
    subtitle = "Intro Bio I (B0160) — Módulo de Evolución"

    header = textwrap.dedent(f"""\
        ---
        title: "{title}"
        subtitle: "{subtitle}"
        format:
          pdf:
            toc: false
            number-sections: false
        lang: es
        ---
    """)

    lines = [header.rstrip(), "", "**Nombre:** _________________________________ &emsp; **Carné:** __________________", "", f"**Versión {version_letter}**", "", "*Instrucciones: Elige la única respuesta correcta para cada pregunta.*"]

    for new_num, orig_idx in enumerate(shuffled_indices, start=1):
        lines.append("")
        lines.append(renumber_question(question_blocks[orig_idx].rstrip(), new_num))

    return "\n".join(lines) + "\n"


def build_key(version_letter: str, shuffled_indices: list[int]) -> str:
    title = f"Quiz 2{version_letter} — CLAVE DE RESPUESTAS"
    subtitle = "Métodos, Hardy-Weinberg, Mutación y Selección Natural · Intro Bio I (B0160)"

    header = textwrap.dedent(f"""\
        ---
        title: "{title}"
        subtitle: "{subtitle}"
        format:
          pdf:
            toc: false
            number-sections: false
        lang: es
        ---
    """)

    rows = ["| Pregunta | Respuesta correcta | Concepto clave |", "|:---:|:---:|---|"]
    for new_num, orig_idx in enumerate(shuffled_indices, start=1):
        orig_q_num = orig_idx + 1
        ans, concept = ANSWERS[orig_q_num]
        rows.append(f"| {new_num} | **{ans}** | {concept} |")

    table = "\n".join(rows)

    notes = textwrap.dedent(f"""\

        ## Notas para el docente

        - Cobertura por clase:
          - Preguntas 1-2: Clase 3 (métodos y evidencia para adaptación).
          - Preguntas 3, 7-8: Clase 5 (tipos de mutación, fuente de variación, germinal vs. somática).
          - Preguntas 4-6: Clase 4 (Hardy-Weinberg, cálculo e interpretación de desviaciones).
          - Preguntas 9-10: Clase 6 (selección balanceadora, tipos de selección).
        - Los distractores atacan errores comunes: confundir desviación de HW con prueba directa de selección, o asumir mutaciones dirigidas por necesidad.
    """)

    return header + "\n" + table + notes


for i, letter in enumerate(VERSIONS):
    rng = random.Random(SEED_BASE + i)
    indices = list(range(10))
    rng.shuffle(indices)

    quiz_text = build_quiz(letter, indices)
    key_text = build_key(letter, indices)

    quiz_path = OUT_DIR / f"q02{letter}.qmd"
    key_path = OUT_DIR / f"q02{letter}-KEY.qmd"

    quiz_path.write_text(quiz_text, encoding="utf-8")
    key_path.write_text(key_text, encoding="utf-8")

    order = [str(x + 1) for x in indices]
    print(f"Version {letter}: Q order = {', '.join(order)} -> {quiz_path.name} + {key_path.name}")

print(f"\nDone. {len(VERSIONS) * 2} files written to {OUT_DIR}")