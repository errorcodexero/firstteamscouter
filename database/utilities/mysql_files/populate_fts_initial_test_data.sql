/* Competition Data */
use `firstteamscouter_test`;

INSERT INTO competition_data (`_id`, `tablet_id`, `competition_name`, `competition_location`, `ready_to_export`) VALUES (1, 0, "Lunar District", "Moon", false);
INSERT INTO competition_data (`_id`, `tablet_id`, `competition_name`, `competition_location`, `ready_to_export`) VALUES (2, 0, "Great Spot District", "Jupiter", false);
INSERT INTO competition_data (`_id`, `tablet_id`, `competition_name`, `competition_location`, `ready_to_export`) VALUES (3, 0, "Martian Regional Championships", "Mars", false);


/* Match Data */
INSERT INTO match_data (`_id`, `competition_id`, `match_time`, `match_type`, `match_number`, `red_team_one_id`, `red_team_two_id`, `red_team_three_id`, `blue_team_one_id`, `blue_team_two_id`, `blue_team_three_id`, `ready_to_export`, `tablet_id`, `match_location`) 
				  VALUES (1, 1, "9:00am", "Qualification", 1,  1,  3,  5,  2,  4,  6, false, 0, "Test Track");
INSERT INTO match_data (`_id`, `competition_id`, `match_time`, `match_type`, `match_number`, `red_team_one_id`, `red_team_two_id`, `red_team_three_id`, `blue_team_one_id`, `blue_team_two_id`, `blue_team_three_id`, `ready_to_export`, `tablet_id`, `match_location`) 
				  VALUES (2, 1, "9:07am", "Qualification", 2, 10,  8,  6,  9,  7,  5, false, 0, "Test Track");
INSERT INTO match_data (`_id`, `competition_id`, `match_time`, `match_type`, `match_number`, `red_team_one_id`, `red_team_two_id`, `red_team_three_id`, `blue_team_one_id`, `blue_team_two_id`, `blue_team_three_id`, `ready_to_export`, `tablet_id`, `match_location`) 
				  VALUES (3, 1, "9:14am", "Qualification", 3,  1, 12,  6,  2, 11,  5, false, 0, "Test Track");
INSERT INTO match_data (`_id`, `competition_id`, `match_time`, `match_type`, `match_number`, `red_team_one_id`, `red_team_two_id`, `red_team_three_id`, `blue_team_one_id`, `blue_team_two_id`, `blue_team_three_id`, `ready_to_export`, `tablet_id`, `match_location`) 
				  VALUES (4, 1, "9:21am", "Qualification", 4,  1,  4,  7,  6,  9, 12, false, 0, "Test Track");
INSERT INTO match_data (`_id`, `competition_id`, `match_time`, `match_type`, `match_number`, `red_team_one_id`, `red_team_two_id`, `red_team_three_id`, `blue_team_one_id`, `blue_team_two_id`, `blue_team_three_id`, `ready_to_export`, `tablet_id`, `match_location`) 
				  VALUES (5, 1, "9:28am", "Qualification", 5, 10, 12,  2,  4, 11,  7, false, 0, "Test Track");
INSERT INTO match_data (`_id`, `competition_id`, `match_time`, `match_type`, `match_number`, `red_team_one_id`, `red_team_two_id`, `red_team_three_id`, `blue_team_one_id`, `blue_team_two_id`, `blue_team_three_id`, `ready_to_export`, `tablet_id`, `match_location`) 
				  VALUES (6, 2, "9:00am", "Qualification", 1,  4,  8, 12,  3,  6,  9, false, 0, "Test Track");
INSERT INTO match_data (`_id`, `competition_id`, `match_time`, `match_type`, `match_number`, `red_team_one_id`, `red_team_two_id`, `red_team_three_id`, `blue_team_one_id`, `blue_team_two_id`, `blue_team_three_id`, `ready_to_export`, `tablet_id`, `match_location`) 
				  VALUES (7, 2, "9:07am", "Qualification", 2,  2,  7, 12,  3,  5, 11, false, 0, "Test Track");



/* Team Data */
INSERT INTO team_data (`_id`, `team_number`, `team_sub_number`, `team_name`, `team_city`, `team_state`, `num_team_members`, `team_creation_year`, `ready_to_export`, `tablet_id`) 
				 VALUES (1,  1999, 0, "Prince and the Revolution", "Purpleville", "MA", 6, 1999, false, 0);
INSERT INTO team_data (`_id`, `team_number`, `team_sub_number`, `team_name`, `team_city`, `team_state`, `num_team_members`, `team_creation_year`, `ready_to_export`, `tablet_id`) 
				 VALUES (2,  2178, 0, "Bambi", "Forest", "MD", 4, 2010, false, 0);
INSERT INTO team_data (`_id`, `team_number`, `team_sub_number`, `team_name`, `team_city`, `team_state`, `num_team_members`, `team_creation_year`, `ready_to_export`, `tablet_id`) 
				 VALUES (3,  1638, 0, "Rockets", "Cape Canaveral", "FL", 16, 2008, false, 0);
INSERT INTO team_data (`_id`, `team_number`, `team_sub_number`, `team_name`, `team_city`, `team_state`, `num_team_members`, `team_creation_year`, `ready_to_export`, `tablet_id`) 
				 VALUES (4,  6776, 0, "Mirrors", "New York", "NY", 24, 2017, false, 0);
INSERT INTO team_data (`_id`, `team_number`, `team_sub_number`, `team_name`, `team_city`, `team_state`, `num_team_members`, `team_creation_year`, `ready_to_export`, `tablet_id`) 
				 VALUES (5,  9137, 0, "Newbies Too", "Bothell", "WA", 9, 2020, false, 0);
INSERT INTO team_data (`_id`, `team_number`, `team_sub_number`, `team_name`, `team_city`, `team_state`, `num_team_members`, `team_creation_year`, `ready_to_export`, `tablet_id`) 
				 VALUES (6,  5998, 0, "Newbies", "Lynnwood", "WA", 15, 2016, false, 0);
INSERT INTO team_data (`_id`, `team_number`, `team_sub_number`, `team_name`, `team_city`, `team_state`, `num_team_members`, `team_creation_year`, `ready_to_export`, `tablet_id`) 
				 VALUES (7,   425, 0, "Phoners", "Seattle", "WA", 24, 1996, false, 0);
INSERT INTO team_data (`_id`, `team_number`, `team_sub_number`, `team_name`, `team_city`, `team_state`, `num_team_members`, `team_creation_year`, `ready_to_export`, `tablet_id`) 
				 VALUES (8,   283, 0, "Mobile Shots", "Portland", "OR", 12, 1996, false, 0);
INSERT INTO team_data (`_id`, `team_number`, `team_sub_number`, `team_name`, `team_city`, `team_state`, `num_team_members`, `team_creation_year`, `ready_to_export`, `tablet_id`) 
				 VALUES (9,  6170, 0, "Ringers", "Providence", "RI", 6, 2017, false, 0);
INSERT INTO team_data (`_id`, `team_number`, `team_sub_number`, `team_name`, `team_city`, `team_state`, `num_team_members`, `team_creation_year`, `ready_to_export`, `tablet_id`) 
				 VALUES (10,  574, 0, "Locals", "Essex", "MD", 2, 1997, false, 0);
INSERT INTO team_data (`_id`, `team_number`, `team_sub_number`, `team_name`, `team_city`, `team_state`, `num_team_members`, `team_creation_year`, `ready_to_export`, `tablet_id`) 
				 VALUES (11, 7876, 0, "Grands", "Chicago", "IL", 22, 2018, false, 0);
INSERT INTO team_data (`_id`, `team_number`, `team_sub_number`, `team_name`, `team_city`, `team_state`, `num_team_members`, `team_creation_year`, `ready_to_export`, `tablet_id`) 
				 VALUES (12, 1965, 0, "Old Timers", "Cleveland", "OH", 50, 1965, false, 0);


/* Team Match Data */
INSERT INTO team_match (`_id`, `match_id`, `competition_id`, `team_id`, `alliance_position`) VALUES (1,1,1,1, "Red1");
INSERT INTO team_match (`_id`, `match_id`, `competition_id`, `team_id`, `alliance_position`) VALUES (2, 1, 1, 3, "Red2");
INSERT INTO team_match (`_id`, `match_id`, `competition_id`, `team_id`, `alliance_position`) VALUES (3, 1, 1, 5, "Red3");
INSERT INTO team_match (`_id`, `match_id`, `competition_id`, `team_id`, `alliance_position`) VALUES (4, 1, 1, 2, "Blue1");
INSERT INTO team_match (`_id`, `match_id`, `competition_id`, `team_id`, `alliance_position`) VALUES (5, 1, 1, 4, "Blue2");
INSERT INTO team_match (`_id`, `match_id`, `competition_id`, `team_id`, `alliance_position`) VALUES (6, 1, 1, 6, "Blue3");
INSERT INTO team_match (`_id`, `match_id`, `competition_id`, `team_id`, `alliance_position`) VALUES (7, 2, 1, 10, "Red1");
INSERT INTO team_match (`_id`, `match_id`, `competition_id`, `team_id`, `alliance_position`) VALUES (8, 2, 1, 8, "Red2");
INSERT INTO team_match (`_id`, `match_id`, `competition_id`, `team_id`, `alliance_position`) VALUES (9, 2, 1, 6, "Red3");
INSERT INTO team_match (`_id`, `match_id`, `competition_id`, `team_id`, `alliance_position`) VALUES (10, 2, 1, 9, "Blue1");
INSERT INTO team_match (`_id`, `match_id`, `competition_id`, `team_id`, `alliance_position`) VALUES (11, 2, 1, 7, "Blue2");
INSERT INTO team_match (`_id`, `match_id`, `competition_id`, `team_id`, `alliance_position`) VALUES (12, 2, 1, 5, "Blue3");
INSERT INTO team_match (`_id`, `match_id`, `competition_id`, `team_id`, `alliance_position`) VALUES (13, 3, 1, 1, "Red1");
INSERT INTO team_match (`_id`, `match_id`, `competition_id`, `team_id`, `alliance_position`) VALUES (14, 3, 1, 12, "Red2");
INSERT INTO team_match (`_id`, `match_id`, `competition_id`, `team_id`, `alliance_position`) VALUES (15, 3, 1, 6, "Red3");
INSERT INTO team_match (`_id`, `match_id`, `competition_id`, `team_id`, `alliance_position`) VALUES (16, 3, 1, 2, "Blue1");
INSERT INTO team_match (`_id`, `match_id`, `competition_id`, `team_id`, `alliance_position`) VALUES (17, 3, 1, 11, "Blue2");
INSERT INTO team_match (`_id`, `match_id`, `competition_id`, `team_id`, `alliance_position`) VALUES (18, 3, 1, 5, "Blue3");
INSERT INTO team_match (`_id`, `match_id`, `competition_id`, `team_id`, `alliance_position`) VALUES (19, 4, 1, 1, "Red1");
INSERT INTO team_match (`_id`, `match_id`, `competition_id`, `team_id`, `alliance_position`) VALUES (20, 4, 1, 4, "Red2");
INSERT INTO team_match (`_id`, `match_id`, `competition_id`, `team_id`, `alliance_position`) VALUES (21, 4, 1, 7, "Red3");
INSERT INTO team_match (`_id`, `match_id`, `competition_id`, `team_id`, `alliance_position`) VALUES (22, 4, 1, 6, "Blue1");
INSERT INTO team_match (`_id`, `match_id`, `competition_id`, `team_id`, `alliance_position`) VALUES (23, 4, 1, 9, "Blue2");
INSERT INTO team_match (`_id`, `match_id`, `competition_id`, `team_id`, `alliance_position`) VALUES (24, 4, 1, 12, "Blue3");
INSERT INTO team_match (`_id`, `match_id`, `competition_id`, `team_id`, `alliance_position`) VALUES (25, 5, 1, 10, "Red1");
INSERT INTO team_match (`_id`, `match_id`, `competition_id`, `team_id`, `alliance_position`) VALUES (26, 5, 1, 12, "Red2");
INSERT INTO team_match (`_id`, `match_id`, `competition_id`, `team_id`, `alliance_position`) VALUES (27, 5, 1, 2, "Red3");
INSERT INTO team_match (`_id`, `match_id`, `competition_id`, `team_id`, `alliance_position`) VALUES (28, 5, 1, 4, "Blue1");
INSERT INTO team_match (`_id`, `match_id`, `competition_id`, `team_id`, `alliance_position`) VALUES (29, 5, 1, 11, "Blue2");
INSERT INTO team_match (`_id`, `match_id`, `competition_id`, `team_id`, `alliance_position`) VALUES (30, 5, 1, 7, "Blue3");
INSERT INTO team_match (`_id`, `match_id`, `competition_id`, `team_id`, `alliance_position`) VALUES (31, 6, 2, 4, "Red1");
INSERT INTO team_match (`_id`, `match_id`, `competition_id`, `team_id`, `alliance_position`) VALUES (32, 6, 2, 8, "Red2");
INSERT INTO team_match (`_id`, `match_id`, `competition_id`, `team_id`, `alliance_position`) VALUES (33, 6, 2, 12, "Red3");
INSERT INTO team_match (`_id`, `match_id`, `competition_id`, `team_id`, `alliance_position`) VALUES (34, 6, 2, 3, "Blue1");
INSERT INTO team_match (`_id`, `match_id`, `competition_id`, `team_id`, `alliance_position`) VALUES (35, 6, 2, 6, "Blue2");
INSERT INTO team_match (`_id`, `match_id`, `competition_id`, `team_id`, `alliance_position`) VALUES (36, 6, 2, 9, "Blue3");
INSERT INTO team_match (`_id`, `match_id`, `competition_id`, `team_id`, `alliance_position`) VALUES (37, 7, 2, 2, "Red1");
INSERT INTO team_match (`_id`, `match_id`, `competition_id`, `team_id`, `alliance_position`) VALUES (38, 7, 2, 7, "Red2");
INSERT INTO team_match (`_id`, `match_id`, `competition_id`, `team_id`, `alliance_position`) VALUES (39, 7, 2, 12, "Red3");
INSERT INTO team_match (`_id`, `match_id`, `competition_id`, `team_id`, `alliance_position`) VALUES (40, 7, 2, 3, "Blue1");
INSERT INTO team_match (`_id`, `match_id`, `competition_id`, `team_id`, `alliance_position`) VALUES (41, 7, 2, 5, "Blue2");
INSERT INTO team_match (`_id`, `match_id`, `competition_id`, `team_id`, `alliance_position`) VALUES (42, 7, 2, 11, "Blue3");