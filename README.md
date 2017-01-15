This repository contains scripts that have been developed by Geoff Benn and Joel Ledford to assist with analyzing grades for BIS2C at UC Davis.

The contents are as follows:
1. roster_parse.pl: this script converts the detailed, but unwieldy, roster provided the registrar into a form that we can use
as a master grade sheet (MGS). The MGS is also the starting point for most later analysis.
2. BIS2C_grade_stats.R is a script designed to produce a series of pdfs with different visualizations of BIS2C exam results.
This script also provides a basic demographic breakdown of the course. The script requires that you also have major_codes.txt in the same directory - this reads in the UC Davis major codes. BIS2C_grade_stats.v2.R is an updated version with additional features added.
3. Grades_by_section.R is a script that takes smartsite gradebooks (first requiring some manual manipulation by the user) and
then calculates averages by section for a range of assingments and presents them as a table. This allows TAs to see how
their grading compares to other TAs.

Please feel free to copy and use this code as needed. If you have questions or would like to discuss collaborations, please contact us:

Geoff Benn - gkbenn@ucdavis.edu
Joel Ledford - jmledford@ucdavis.edu
