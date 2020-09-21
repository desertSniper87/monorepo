SELECT s.hacker_id, Hackers.name, SUM(s.score) 
FROM Hackers 
JOIN
(
    SELECT hacker_id, challenge_id, max(score) AS score FROM submissions
    GROUP BY hacker_id, challenge_id
) AS s
ON Hackers.hacker_id=s.hacker_id
GROUP BY s.hacker_id, Hackers.name
HAVING SUM(s.score)>0
ORDER BY SUM(s.score) DESC
;



