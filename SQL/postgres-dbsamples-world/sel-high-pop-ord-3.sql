WITH ordered_table AS (
    SELECT name, code, population,
    ROW_NUMBER() OVER (ORDER BY population DESC)
    AS rownumber
    FROM country
)

SELECT name, population FROM ordered_table
WHERE rownumber<10;
