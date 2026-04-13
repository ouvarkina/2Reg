# Структура Reg2

```
project/
├─ data/
│  ├─ registry2-accepted-records.json
│  ├─ doctors.json
│  ├─ organizations.json
│  ├─ 2Reg_Dictionary_tidy.xlsx
│  ├─ 2Reg_diagnoses.xlsx          # опционально
│  ├─ patients_tidy.rds
│  ├─ HA_tidy.rds
│  ├─ 2Reg_patients_tidy.xlsx
│  └─ 2Reg_HA_tidy.xlsx
├─ tables/
│  ├─ *.xlsx
│  └─ *.pptx
├─ graphes/
│  └─ *.png
├─ 01_build_tidy_from_json.R
└─ 02_analysis_from_rds.qmd
```

Порядок запуска:
1. Положить исходные JSON/XLSX в `data/`.
2. Запустить `01_build_tidy_from_json.R`.
3. После этого рендерить `02_analysis_from_rds.qmd`.
