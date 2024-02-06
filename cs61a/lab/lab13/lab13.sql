.read data.sql


CREATE TABLE bluedog AS
  SELECT color, pet from students where color = 'blue' and pet = 'dog' ;

CREATE TABLE bluedog_songs AS
  SELECT color, pet, song from students where color = 'blue' and pet = 'dog';


CREATE TABLE matchmaker AS
  SELECT a.pet, a.song, a.color, b.color from students as a, students as b
  			where a.song = b.song and a.pet = b.pet and a.time < b.time;


CREATE TABLE sevens AS
  SELECT seven from students, numbers 
  				where students.number = 7 and numbers.'7' ='True' and students.time = numbers.time;


CREATE TABLE favpets AS
  SELECT pet, COUNT(*) AS count FROM students GROUP BY pet ORDER BY count DESC LIMIT 10;


CREATE TABLE dog AS
  SELECT pet, COUNT(*) FROM students WHERE pet = 'dog';


CREATE TABLE bluedog_agg AS
  SELECT song, COUNT(*) AS count FROM bluedog_songs GROUP BY song;


CREATE TABLE instructor_obedience AS
  SELECT seven, instructor, COUNT(*) FROM students WHERE seven = '7'
  			GROUP BY instructor;

