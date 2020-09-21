NSS Tutorial
Field	Type
ukprn	varchar(8)
institution	varchar(100)
subject	varchar(60)
level	varchar(50)
question	varchar(10)
A_STRONGLY_DISAGREE	int(11)
A_DISAGREE	int(11)
A_NEUTRAL	int(11)
A_AGREE	int(11)
A_STRONGLY_AGREE	int(11)
A_NA	int(11)
CI_MIN	int(11)
score	int(11)
CI_MAX	int(11)
response	int(11)
sample	int(11)
aggregate	char(1)
National Student Survey 2012

The National Student Survey http://www.thestudentsurvey.com/ is presented to thousands of graduating students in UK Higher Education. The survey asks 22 questions, students can respond with STRONGLY DISAGREE, DISAGREE, NEUTRAL, AGREE or STRONGLY AGREE. The values in these columns represent PERCENTAGES of the total students who responded with that answer.

The table nss has one row per institution, subject and question.

Contents [hide] 
1	Check out one row
2	Calculate how many agree or strongly agree
3	Unhappy Computer Students
4	More Computing or Creative Students?
5	Strongly Agree Numbers
6	Strongly Agree, Percentage
7	Scores for Institutions in Manchester
8	Number of Computing Students in Manchester

-- 1.
-- The example shows the number who responded for:

-- question 1
-- at 'Edinburgh Napier University'
-- studying '(8) Computer Science'
-- Show the the percentage who STRONGLY AGREE


SELECT A_STRONGLY_AGREE
FROM nss
WHERE question='Q01'
AND institution='Edinburgh Napier University'
AND subject='(8) Computer Science'

-- 2.
-- Show the institution and subject where the score is at least 100 for question 15.


SELECT institution, subject
FROM nss
WHERE question='Q15'
AND score>=100;

-- 3.
-- Show the institution and score where the score for '(8) Computer Science' is less than 50 for question 'Q15'


SELECT institution, score
FROM nss
WHERE question='Q15'
AND subject='(8) Computer Science'
AND score<50


-- More Computing or Creative Students?
-- 4.
-- Show the subject and total number of students who responded to question 22 for each of the subjects '(8) Computer Science' and '(H) Creative Arts and Design'.

-- HINT

SELECT institution, score
FROM nss
WHERE question='Q22'
AND institution='Edinburgh Napier University'
AND subject='(8) Computer Science'
AND subject='(H) Creative Arts and Design';



-- Strongly Agree Numbers
-- 5.
-- Show the subject and total number of students who A_STRONGLY_AGREE to question 22 for each of the subjects '(8) Computer Science' and '(H) Creative Arts and Design'.

-- HINT

SELECT subject, SUM(A_STRONGLY_AGREE*response/100 )
FROM nss
WHERE question='Q22'
AND (subject='(H) Creative Arts and Design' 
    OR subject='(8) Computer Science')
GROUP BY subject;

-- Strongly Agree, Percentage
-- 6.
-- Show the percentage of students who A_STRONGLY_AGREE to question 22 for the subject '(8) Computer Science' show the same figure for the subject '(H) Creative Arts and Design'.

-- Use the ROUND function to show the percentage without decimal places.

SELECT subject, ROUND(SUM(A_STRONGLY_AGREE*response)/SUM(response))
FROM nss
WHERE question='Q22'
AND (subject='(H) Creative Arts and Design' 
    OR subject='(8) Computer Science')
GROUP BY subject


-- Scores for Institutions in Manchester
-- 7.
-- Show the average scores for question 'Q22' for each institution that include 'Manchester' in the name.

-- The column score is a percentage - you must use the method outlined above to multiply the percentage by the response and divide by the total response. Give your answer rounded to the nearest whole number.


SELECT institution, ROUND(AVG(score)) AS score
FROM nss
WHERE question='Q22'
AND (institution LIKE '%Manchester%')
GROUP BY institution

-- Number of Computing Students in Manchester
-- 8.
-- Show the institution, the total sample size and the number of computing students for institutions in Manchester for 'Q01'.

SELECT total.institution, total.b , comp.sample
FROM 
(
    SELECT institution, SUM(sample) AS b
    FROM nss 
    WHERE question='Q01'
    AND (institution LIKE '%Manchester%')
    GROUP BY institution
)
AS total 
JOIN 
(
    SELECT sample, institution
    FROM nss
    WHERE question='Q01'
    AND institution LIKE '%Manchester%'
    AND subject LIKE '%computer%' 
)
AS comp 
ON total.institution = comp.institution


