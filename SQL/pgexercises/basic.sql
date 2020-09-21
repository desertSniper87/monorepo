SELECT * FROM cd.facilities 
WHERE name LIKE '%Tennis%';



-- Classify
SELECT name, 
CASE 
WHEN monthlymaintenance < 100 
    THEN 'cheap'
ELSE 
    'expensive'
END
FROM cd.facilities;

-- Date

SELECT memid, surname, firstname, joindate
FROM cd.members
WHERE joindate >= '2012-09-01'
;

-- Unique

SELECT DISTINCT surname 
FROM cd.members
ORDER BY surname
limit 10;

-- Union

SELECT surname 
FROM cd.members
UNION 
SELECT name
FROM cd.facilities;

-- Agg

SELECT joindate
FROM cd.members
ORDER BY joindate DESC
limit 1;

SELECT MAX (joindate) AS latest
FROM cd.members;

-- Agg 2

SELECT firstname, surname, joindate
FROM cd.members 
ORDER BY joindate DESC
limit 1;
