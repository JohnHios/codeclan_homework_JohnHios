--MVP


--Q1: (a). Find the first name, last name and team name of employees who are members of teams.

SELECT
    e.first_name,
    e.last_name,
    e.team_id
FROM employees AS e
LEFT JOIN teams AS t
ON e.team_id = t.id 

-- (b). Find the first name, last name and team name of employees who are members of teams and are enrolled in the pension scheme.

SELECT
    e.first_name,
    e.last_name,
    e.team_id
FROM employees AS e
LEFT JOIN teams AS t
ON e.team_id = t.id 
WHERE e.pension_enrol = TRUE 

-- (c). Find the first name, last name and team name of employees who are members of teams, where their team has a charge cost greater than 80.

SELECT
    e.first_name,
    e.last_name,
    t.name AS team,
    t.charge_cost 
FROM employees AS e
RIGHT JOIN teams AS t
ON e.team_id = t.id 
WHERE CAST(t.charge_cost AS int) > 80 



--Q2: (a). Get a table of all employees details, together with their local_account_no and local_sort_code, if they have them.

SELECT *
FROM employees AS e
LEFT JOIN pay_details AS p
ON e.pay_detail_id  = p.id; 


-- (b). Amend your query above to also return the name of the team that each employee belongs to.

SELECT *
FROM 
(employees AS e
LEFT JOIN pay_details AS p
ON e.pay_detail_id  = p.id)
INNER JOIN teams AS t
ON e.team_id = t.id; 



--Q3. (a). Make a table, which has each employee id along with the team that employee belongs to.

SELECT 
    e.id,
    t.name 
FROM employees AS e
LEFT JOIN teams AS t
ON e.team_id = t.id; 


-- (b). Breakdown the number of employees in each of the teams.

SELECT 
    t.name,
    COUNT(e.id)
FROM employees AS e
LEFT JOIN teams AS t
ON e.team_id = t.id
GROUP BY t.name; 


-- (c). Order the table above by so that the teams with the least employees come first.

SELECT 
    t.name,
    COUNT(e.id)
FROM employees AS e
LEFT JOIN teams AS t
ON e.team_id = t.id
GROUP BY t.name
ORDER BY COUNT(e.id) ASC; 

SELECT 
*
FROM employees

--pay_details

--
-- teams

SELECT 
*
FROM teams

--Q1


--Q1

--Q1


--Q1


--Q1


--Q1

--Q1

