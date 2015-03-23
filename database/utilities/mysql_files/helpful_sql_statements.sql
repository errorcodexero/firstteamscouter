SELECT * FROM firstteamscouter.match_data;
DELETE FROM firstteamscouter.match_data;
UPDATE match_data SET _id=2 WHERE _id=4;
ALTER TABLE match_data AUTO_INCREMENT = 3;