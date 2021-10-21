Pay data from www.phdstipends.com/csv
COL data from https://livingwage.mit.edu/metros/14020
12 or 9-month stipend amounts below 9000 and above 75000 were removed as noise.
Unless otherwise noted, all absolute pay values are medians.

Columns:
    university: University name
    is_unionized: Is campus unionized (includes Northwestern, which is ongoing)
    responses: Individual survey responses in data.
    col: Required annual cost of living, per MIT data
    col_9_mo: col*0.75
    pay_9_equivalent: 9-month pay if reported, otherwise reported 12-month pay multiplied by 3/4
    pay_12_no_summer: 12-month pay if reported, otherwise flat 9-month pay (i.e., assumption is no summer pay unless indicated)
    pay_12_plus_summer: 12-month pay if reported, otherwise 9-month pay plus any reported 3-month pay (may include outside internships; data are noisy)
    min_stipend: minimum stipend reported for the school
    <payment_column>_to_col: ratio of pay to COL over the specified period
