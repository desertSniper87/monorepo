-- Edinburgh Buses
-- Details of the database Looking at the data

-- stops(id, name)
-- route(num,company,pos, stop)
-- stops	route
-- id	num
-- name	company
-- pos
-- stop


-- Summary
-- 1.
-- How many stops are in the database.


select count(*) from stops

-- 2.
-- Find the id value for the stop 'Craiglockhart'


select id from stops where name = 'Craiglockhart'



-- 3.
-- Give the id and the name for the stops on the '4' 'LRT' service.

SELECT id,  name 
FROM stops 
JOIN route
WHERE stops.id=route.stop
AND route.company='LRT'
AND route.num=4
;

-- Routes and stops
-- 4.
-- The query shown gives the number of routes that visit either London Road (149) or Craiglockhart (53). Run the query and notice the two services that link these stops have a count of 2. Add a HAVING clause to restrict the output to these two routes.


SELECT company, num, count(*)
FROM route WHERE stop=149 OR stop=53
GROUP BY company, num
HAVING count(*)=2


-- 5.
-- Execute the self join shown and observe that b.stop gives all the places you can get to from Craiglockhart, without changing routes. Change the query so that it shows the services from Craiglockhart to London Road.


SELECT a.company, a.num, a.stop, b.stop
FROM route a JOIN route b ON
  (a.company=b.company AND a.num=b.num)
WHERE (
    a.stop=53 AND b.stop= 149
)
OR (
    b.stop=149 AND b.stop=53
)

-- 6.
-- The query shown is similar to the previous one, however by joining two copies of the stops table we can refer to stops by name rather than by number. Change the query so that the services between 'Craiglockhart' and 'London Road' are shown. If you are tired of these places try 'Fairmilehead' against 'Tollcross'


SELECT a.company, a.num, stopa.name, stopb.name
FROM route a JOIN route b ON
  (a.company=b.company AND a.num=b.num)
  JOIN stops stopa ON (a.stop=stopa.id)
  JOIN stops stopb ON (b.stop=stopb.id)
WHERE (stopa.name='Craiglockhart'
AND stopb.name='London Road')

-- Using a self join
-- 7.
-- Give a list of all the services which connect stops 115 and 137 ('Haymarket' and 'Leith')

SELECT DISTINCT route_a.company, route_a.num
-- SELECT *
FROM route AS route_a 
JOIN route AS route_b
ON(
    route_a.company=route_b.company AND route_a.num=route_b.num
)
JOIN stops AS stops_a
ON (
    route_a.stop=stops_a.id
)
JOIN stops AS stops_b
ON (
    route_b.stop=stops_b.id
)
WHERE (
    stops_a.id = 115 AND stops_b.id=137
    -- route_a.stop=115 AND routes_b.stop=137
)
-- GROUP BY num
-- OR (
    -- stops_b.id = 115 AND stops_a.id=137
-- )


-- 8.
-- Give a list of the services which connect the stops 'Craiglockhart' and 'Tollcross'

SELECT DISTINCT route_a.company, route_a.num
FROM route AS route_a 
JOIN route AS route_b
ON(
    route_a.company=route_b.company AND route_a.num=route_b.num
)
JOIN stops AS stops_a
ON (
    route_a.stop=stops_a.id
)
JOIN stops AS stops_b
ON (
    route_b.stop=stops_b.id
)
WHERE (
    stops_a.name = 'Craiglockhart' AND stops_b.name='Tollcross'
)




-- 9.
-- Give a distinct list of the stops which may be reached from 'Craiglockhart' by taking one bus, including 'Craiglockhart' itself, offered by the LRT company. Include the company and bus no. of the relevant services.

-- SELECT DISTINCT company 
-- FROM route 
-- JOIN stops
-- ON (
    -- route.stop=stops.id
-- )


SELECT DISTINCT stops_a.name, route_a.company, route_a.num
FROM route AS route_a 
JOIN route AS route_b
ON(
    route_a.company=route_b.company AND route_a.num=route_b.num
)
JOIN stops AS stops_a
ON (
    route_a.stop=stops_a.id
)
JOIN stops AS stops_b
ON (
    route_b.stop=stops_b.id
)
WHERE (
    stops_a.name = 'Craiglockhart' OR stops_b.name = 'Craiglockhart' 
)

-- 10.
-- Find the routes involving two buses that can go from Craiglockhart to Sighthill.
-- Show the bus no. and company for the first bus, the name of the stop for the transfer,
-- and the bus no. and company for the second bus.

-- Hint
SELECT route_clhart.num, route_clhart.company, route_clhart.stops_b_name AS name, route_shill.num, route_shill.company
FROM 
(
    SELECT DISTINCT stops_b.name AS stops_b_name, route_a.num AS num
    ,route_a.company AS company
    FROM route AS route_a 
    JOIN route AS route_b
    ON(
        route_a.company=route_b.company AND route_a.num=route_b.num
    )
    JOIN stops AS stops_a
    ON (
        route_a.stop=stops_a.id
    )
    JOIN stops AS stops_b
    ON (
        route_b.stop=stops_b.id
    )
    WHERE (
        (
            stops_a.name = 'Craiglockhart' 
        )
    )
) AS route_clhart
JOIN (
    SELECT DISTINCT stops_b.name AS stops_b_name, route_a.num AS num
    , route_a.company AS company
    FROM route AS route_a 
    JOIN route AS route_b
    ON(
        route_a.company=route_b.company AND route_a.num=route_b.num
    )
    JOIN stops AS stops_a
    ON (
        route_a.stop=stops_a.id
    )
    JOIN stops AS stops_b
    ON (
        route_b.stop=stops_b.id
    )
    WHERE (
        (
            stops_a.name = 'Sighthill' 
        )
    )
) AS route_shill
ON route_clhart.stops_b_name=route_shill.stops_b_name




