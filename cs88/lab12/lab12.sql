.read data.sql

-- Q2
CREATE TABLE obedience as
  -- REPLACE THIS LINE
  SELECT seven, gerald FROM students;
 


-- Q3
CREATE TABLE blue_dog as
  -- REPLACE THIS LINE
  SELECT color, pet from students where color = "blue" and pet = "dog";


-- Q4
CREATE TABLE smallest_int as
  -- REPLACE THIS LINE
  SELECT time,smallest from students WHERE smallest > 3 order by smallest limit 20;


-- Q5
CREATE TABLE sevens as
  -- REPLACE THIS LINE
  SELECT students.seven FROM students, checkboxes WHERE students.time = checkboxes.time AND checkboxes.'7' = 'True' and students.number = 7;


-- Q6
CREATE TABLE matchmaker as
  -- REPLACE THIS LINE
  SELECT  a.pet, a.song, a.color, b.color FROM students AS a, students AS b
    WHERE  a.time != b.time and a.time < b.time and a.pet = b.pet AND a.song = b.song order by a.time;


-- Q7
CREATE TABLE smallest_int_count as
  -- REPLACE THIS LINE
  SELECT smallest, COUNT(*) from students where smallest > 0 Group by smallest ;

