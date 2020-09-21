.mode column
.headers on 

CREATE TABLE letters(
    id NUMBER, 
    element TEXT
);

INSERT INTO letters(id, element)
VALUES (1, 'A');

INSERT INTO letters(id, element)
VALUES (2, 'B');

INSERT INTO letters(id, element)
VALUES (3, 'C');

INSERT INTO letters(id, element)
VALUES (4, 'D');

SELECT * FROM letters;

SELECT table1.ELEMENT, table2.ELEMENT
FROM letters table1
JOIN letters table2
ON table1.id!=table2.id
;

SELECT table1.ELEMENT, table2.ELEMENT, table3.ELEMENT
FROM letters table1
JOIN letters table2
ON table1.id!=table2.id
JOIN letters table3
ON table1.id!=table3.id
AND table2.id!=table3.id
;

SELECT table1.ELEMENT, table2.ELEMENT, table3.ELEMENT, table4.ELEMENT
FROM letters table1

JOIN letters table2
ON table1.id!=table2.id

JOIN letters table3
ON table1.id!=table3.id
AND table2.id!=table3.id

JOIN letters table4
ON table1.id!=table4.id
AND table2.id!=table4.id
AND table3.id!=table4.id
;

DROP TABLE letters;

