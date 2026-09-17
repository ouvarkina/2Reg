# 2Reg

Обработка и анализ клинического реестра гемосорбции на R и Quarto. ETL преобразует исходный JSON в таблицы пациентов и процедур; аналитические скрипты читают сохранённые RDS.

## Основные файлы

| Файл / папка | Назначение |
|---|---|
| `01a_build_tidy_from_json.R` | Извлечение данных из JSON в `data/intermediate/` |
| `01b_postprocess_tidy.R` | Нормализация, производные показатели, QC и итоговые RDS/XLSX |
| `03b_severity_and_numeric_outcome.qmd` | Индекс исходной тяжести и outcome score |
| `04_data_structure.qmd` | PCA, кластеры и heatmap пациентов |
| `05_baseline_analysis.qmd` | Описательные таблицы, корреляции и частичные корреляции с поправкой на SOFA |
| `R/` | Общие функции: `etl_helpers.R`, `analysis_helpers.R`, `correlation_helpers.R` |
| `data/2Reg_Dictionary_tidy.xlsx` | Локальный словарь подписей |
| `data/analysis/`, `tables/`, `graphes/` | Результаты анализа, таблицы и графики |

## Подготовка

Нужны **R**, **Quarto** и RStudio либо Positron. Рабочая папка — корень проекта; сохраняйте относительные пути.

Основные зависимости: `dplyr`, `tidyr`, `purrr`, `stringr`, `tibble`, `readr`, `lubridate`, `jsonlite`, `openxlsx`, `ggplot2`, `googlesheets4`, `corrplot`, `gtsummary`, `flextable`, `officer`, `rlang`, `svglite`, `circlize`. `ComplexHeatmap` устанавливается через Bioconductor. Дополнительные зависимости указаны в блоках setup соответствующих скриптов. Если в репозитории есть `renv.lock`, восстановите окружение через `renv::restore()`.

Скрипты `04` и `05` читают Google-таблицу **Dictionary** и требуют доступа к ней: `04` использует настройки выбора переменных, `05` — подписи. Локальный Excel-словарь дополняет отсутствующие подписи в `05`, но не заменяет Google-таблицу.

Для ETL положите в `data/`:

- `registry2-accepted-records.json`;
- `doctors.json`;
- `organizations.json`;
- при наличии ручной разметки — `2Reg_patients_indications.xlsx`.

Данные получают отдельно от репозитория через согласованное защищённое хранилище.

## Запуск

Из корня проекта:

```bash
Rscript 01a_build_tidy_from_json.R
Rscript 01b_postprocess_tidy.R
```

Результат: `data/patients_tidy.rds`, `data/HA_tidy.rds` и Excel-копии. Аналитика использует RDS.

Затем выполните `03b_severity_and_numeric_outcome.qmd`, предварительно проверив его входные файлы в разделе «Входные данные». Для `04` и `05` нужен результат `data/analysis/outcome_propensity_by_patient.rds`.

```bash
quarto render 03b_severity_and_numeric_outcome.qmd
quarto render 04_data_structure.qmd
quarto render 05_baseline_analysis.qmd
```

После подготовки результата `03b` скрипты `04` и `05` можно запускать независимо. Остальные аналитические и презентационные QMD запускаются согласно их разделам входных данных. После изменения ETL пересоберите данные и затронутые результаты анализа.

## Принятые обозначения

- Исходные временные точки: `m12h_0h`, `48h_72h`, `4d_6d`, `7d_10d`. Переноса в клинические окна нет.
- `HA1_` — показатели процедуры «Гемоперфузия 1».
- В коррелограмме `da_` = после − до, `dr_` = после / до для пары `m12h_0h → 48h_72h`. При нулевом знаменателе `dr_ = NA`.
- `*_dur_hours` — полная длительность поддержки: `24 × сутки + часы`; исходные компоненты сохранены в `*_raw`, проверка — в `*_dur_qc`.
- `HA_sorption_dose = HA_avg_blood_flow × HA_duration / BMI`; длительность — в часах, без множителя 60.
- Outcome score — `outcome_propensity`. В `04` он служит только правой аннотацией heatmap и не входит в PCA или кластеризацию.

## Работа с Git

Создавайте отдельную ветку для задачи:

```bash
git switch -c feature/task-name
```

В Git хранятся код, настройки и словарь без персональных данных. Исключайте через `.gitignore` реестр, справочники врачей, пациентские таблицы, промежуточные RDS, результаты анализа, кеши и файлы авторизации. Перед коммитом проверяйте `git diff --cached`. В описании изменения указывайте, какие скрипты проверены и какие результаты нужно пересоздать.
