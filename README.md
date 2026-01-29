# Mental Imagery Data Cleaning & Psychometric Scoring Pipeline

---

## Project Overview

This project provides an automated pipeline for parsing, cleaning, and scoring behavioral data collected via JATOS. The script processes raw JSON-encoded text files, handles complex data nesting, and calculates validated psychometric scores for mental imagery assessments.

## Key Technical Features

* Automated Data Extraction: Iteratively reads and binds multiple raw `.txt` data files into a unified dataset.
* JSON Parsing & Normalization: Utilizes `jsonlite` to decode nested JSON structures into tabular formats, handling varying trial lengths across participants.
* Psychometric Scoring Logic
* VVIQ (Vividness of Visual Imagery Questionnaire): Implements automated 1–5 scale reversal and summation.
* OSIQ (Object-Spatial Imagery Questionnaire): Processes item-level responses to generate new composite scores for analysis.


Data Integrity & Cleaning: Includes "place-then-fill" logic to ensure data alignment across different experimental components (OSPAN, SGT, and Word/Image rating tasks).

## Technologies Used

* R Language
* tidyverse (dplyr, purrr) for data manipulation
* jsonlite for handling JSON data structures

## Data Workflow

1. Read: Load raw JATOS result files from the local directory.
2. Parse: Extract demographic, OSPAN, condition, and imagery task data.
3. Clean: Handle missing values and convert list-columns into atomic vectors for CSV compatibility.
4. Score: Apply reverse-coding and calculate final VVIQ/OSIQ scores.
5. Export: Generate a cleaned `.csv` ready for statistical modeling.
