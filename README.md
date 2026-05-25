# Modulo Evolucion - Intro Bio I (B0160)

Materiales del modulo de Evolucion para Intro Bio I en la Universidad de Costa Rica.

Basado, en parte, en materiales de https://evolution.berkeley.edu/

## Sitio web

El repositorio ahora sigue una estructura de sitio Quarto pre-renderizado, paralela al repositorio fuente `B0406-2026`.

Secciones principales:

- `index.qmd`: pagina de inicio del sitio
- `lectures/`: clases en formato revealjs
- `lectures/index.qmd`: listado de clases
- `course-materials/`: programa del modulo y materiales de apoyo
- `_quarto.yml`: configuracion del sitio y navbar
- `render.sh`: atajo para renderizar todo el sitio

## Renderizado local

```bash
./render.sh
```

O manualmente:

```bash
quarto render . --no-execute
```

## GitHub Pages

La estructura esta preparada para servir archivos pre-renderizados desde la raiz del repositorio usando GitHub Pages.

Pasos tipicos:

```bash
git add .
git commit -m "Update site content"
git push
```
