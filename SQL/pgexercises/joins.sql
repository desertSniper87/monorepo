-- Simple Join

SELECT starttime FROM cd.bookings
JOIN cd.members
ON cd.members.memid = cd.bookings.memid
AND cd.members.firstname = 'David'
AND cd.members.surname = 'Farell';


-- Simple Join 2

SELECT starttime AS START, cd.facilities.name
FROM cd.bookings 
JOIN cd.facilities
ON cd.bookings.facid = cd.facilities.facid
WHERE cd.bookings.starttime >= '2012-09-21'
AND cd.bookings.starttime < '2012-09-22'
AND cd.facilities.name LIKE 'Tennis%'
ORDER BY START;

-- Self

SELECT DISTINCT recs.firstname, recs.surname
FROM cd.members mems
JOIN cd.members recs
ON recs.memid = mems.recommendedby
ORDER BY surname, firstname;
